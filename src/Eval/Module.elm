module Eval.Module exposing (CachedModuleSummary, DependencySummaryStats, ProjectEnv, ResolveErrorEntry, ResolvedProject, buildCachedModuleSummariesFromParsed, buildInterfaceFromFile, buildProjectEnv, buildProjectEnvFromParsed, buildProjectEnvFromSummaries, coverageWithEnv, coverageWithEnvAndLimit, emptyDependencySummaryStats, eval, evalProject, evalWithEnv, evalWithEnvAndLimit, evalWithEnvFromFiles, evalWithEnvFromFilesAndLimit, evalWithEnvFromFilesAndMemo, evalWithEnvFromFilesAndValues, evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw, evalWithEnvFromFilesAndValuesAndInterceptsRaw, evalWithEnvFromFilesAndValuesAndMemo, evalWithIntercepts, evalWithInterceptsAndMemoRaw, evalWithInterceptsRaw, evalWithMemoizedFunctions, evalWithResolvedIR, evalWithResolvedIRExpression, evalWithResolvedIRFromFilesAndIntercepts, evalWithResolvedIRFromFilesAndInterceptsAndLimit, evalWithValuesAndMemoizedFunctions, extendResolvedWithFiles, extendWithFiles, extendWithFilesNormalized, fileModuleName, getModuleFunctions, getModulePrecomputedValues, handleInternalMemoLookup, handleInternalMemoStore, handleInternalMemoYield, isLosslessValue, mergeDependencySummaryStats, mergeModuleFunctionsIntoEnv, normalizeOneModuleInEnv, normalizeOneModuleInEnvSelected, normalizeOneModuleInEnvSelectedWithFlags, normalizeSummaries, normalizeSummariesWithStats, normalizeUserModulesInEnv, parseProjectSources, precomputedValuesByModule, precomputedValuesCount, projectEnvResolved, replaceModuleFunctionsInEnv, replaceModuleInEnv, setModulePrecomputedValues, trace, traceOrEvalModule, traceWithEnv)

import Array
import Bitwise
import Core
import Dict as ElmDict
import Elm.Interface exposing (Exposed)
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Environment
import Expression.Extra
import Eval.Expression
import Eval.NativeDispatch as NativeDispatch
import Eval.ResolvedExpression as RE
import Eval.ResolvedIR as IR
import Eval.Resolver as Resolver
import EvalResult
import FastDict as Dict
import List.Extra
import ListFusion
import MemoRuntime
import MemoSpec
import NormalizationFlags
import Result.MyExtra
import Rope exposing (Rope)
import Set exposing (Set)
import Syntax exposing (fakeNode)
import TcoAnalysis
import Types exposing (CallTree, Config, Env, Error(..), EvalResult(..), ImportedNames, Value)
import Value exposing (unsupported)


coreFunctions : Dict.Dict String (Dict.Dict String Elm.Syntax.Expression.FunctionImplementation)
coreFunctions =
    Core.functions
        |> Dict.foldl
            (\moduleName moduleDict acc ->
                Dict.insert (Environment.moduleKey moduleName) moduleDict acc
            )
            Dict.empty


{-| Carrier for resolver output attached to a ProjectEnv.

  - `globalIds` is populated with **every** top-level (core + user)
    (`(ModuleName, name)` → counter id). The resolver uses it to translate
    qualified references to stable ids, and Phase 3's evaluator will use it
    to look up pre-resolved bodies.

  - `bodies` holds the resolved `RExpr` for user declarations only. Core
    declarations get ids (so user code can reference them) but are **not**
    resolved to RExpr — Phase 3's evaluator will fall back to the existing
    string-keyed `shared.functions` path for any `RGlobal` id that isn't in
    `bodies`.

  - `errors` accumulates any `ResolveError` the resolver produced while
    walking user declarations. The build does **not** fail on resolve
    errors — the old evaluator still has the original FunctionImplementation
    and can run the declaration the pre-Phase-2 way. Errors are a signal
    that the resolver is missing coverage, not a correctness hazard.

-}
type alias ResolvedProject =
    { globalIds : Dict.Dict ( ModuleName, String ) IR.GlobalId
    , bodies : Dict.Dict IR.GlobalId IR.RExpr
    , globalIdToName : Dict.Dict IR.GlobalId ( ModuleName, String )
    , nativeDispatchers : Dict.Dict IR.GlobalId NativeDispatch.NativeDispatcher
    , higherOrderDispatchers : Dict.Dict IR.GlobalId RE.HigherOrderDispatcher
    , kernelDispatchers : Dict.Dict IR.GlobalId KernelDispatcher
    , globals : Dict.Dict IR.GlobalId Value
    , errors : List ResolveErrorEntry
    }


{-| A precomputed direct-dispatch entry for a core function whose body is
a simple kernel reference (the common "wrapper" pattern in `Core.Basics`,
`Core.String`, etc.). At delegation time, the evaluator calls the kernel
function directly with the applied args + Config + Env, skipping the
Value.toExpression + synthesized AST round-trip.
-}
type alias KernelDispatcher =
    { arity : Int
    , kernelFn : List Value -> Types.Config -> Env -> EvalResult Value
    }


type alias ResolveErrorEntry =
    { moduleName : ModuleName
    , name : String
    , error : Resolver.ResolveError
    }


{-| Accessor for the resolver output attached to a ProjectEnv. Mostly for
tests and diagnostics — Phase 3's evaluator reaches into this directly
through its own code path inside this module.
-}
projectEnvResolved : ProjectEnv -> ResolvedProject
projectEnvResolved (ProjectEnv projectEnv) =
    projectEnv.resolved


{-| Rebuild `ResolvedProject.globals` from `env.shared.precomputedValues`,
mapping each `(moduleKey, name)` pair through `resolved.globalIds` to
find the matching `GlobalId`. Entries whose module or name isn't in
`globalIds` are silently dropped — the resolver's user-module pass may
not have picked them up, and the precomputed cache is always a best-
effort optimization.

Phase 2 of the OLD-evaluator migration plan: the resolved-IR evaluator
(`Eval.ResolvedExpression.evalGlobal`) already checks `env.globals`
before walking a resolved body, so by populating this dict at project-
load / normalization time, we move the precomputed-value fast path
from OLD eval's `evalFunctionOrValue` to RE's `evalGlobal`. That in
turn replaces the per-eval `precomputedGlobalsCache` construction
inside `evalWithResolvedIRFromFilesAndIntercepts` with a one-shot
build at project-env time, amortizing the cost across every later
eval instead of paying it per call.

Called at every site that mutates `env.shared.precomputedValues`:
initial `buildProjectEnv*` completion, `extendResolvedWithFiles`,
`extendWithFilesNormalized`, `normalizeOneModuleInEnv`,
`normalizeUserModulesInEnv`, `replaceModuleInEnv`, etc. The call is
pure and idempotent — safe to invoke whenever the ProjectEnv passes
through a public boundary.

-}
refreshResolvedGlobals : ProjectEnv -> ProjectEnv
refreshResolvedGlobals (ProjectEnv projectEnv) =
    let
        newGlobals : Dict.Dict IR.GlobalId Value
        newGlobals =
            projectEnv.env.shared.precomputedValues
                |> Dict.foldl
                    (\moduleKey moduleVals outer ->
                        let
                            mn : ModuleName
                            mn =
                                String.split "." moduleKey
                        in
                        Dict.foldl
                            (\name value inner ->
                                case Dict.get ( mn, name ) projectEnv.resolved.globalIds of
                                    Just id ->
                                        Dict.insert id value inner

                                    Nothing ->
                                        inner
                            )
                            outer
                            moduleVals
                    )
                    Dict.empty

        resolved : ResolvedProject
        resolved =
            projectEnv.resolved
    in
    ProjectEnv
        { projectEnv | resolved = { resolved | globals = newGlobals } }


{-| Evaluate a top-level expression via the new resolved-IR evaluator.

This is Phase 3 iteration 3b3's wire-up entry point. It:

1.  Parses the expression source string into an `Elm.Syntax.Expression`.
2.  Resolves it against the project's `globalIds` map, producing an
    `RExpr` that has every reference rewritten to `RLocal`/`RGlobal`/
    `RCtor`.
3.  Builds an `REnv` from the project's resolved bodies.
4.  Calls `Eval.ResolvedExpression.evalR` and returns the result.

Iteration 3b3 only supports expressions that touch **user declarations**
in the project env — references to core functions (like `+` or
`List.map`) still return an `Unsupported` error because the new
evaluator's core-dispatch bridge is deferred. See
`Eval.ResolvedExpression.evalGlobal` for the error pattern.

The caller is expected to compose this with the existing
`evalWithEnvFromFiles` for any expression that requires the old path —
there's no automatic fallback in 3b3.

-}
evalWithResolvedIR : ProjectEnv -> String -> Result Error Value
evalWithResolvedIR projectEnv expressionSource =
    let
        -- Wrap the expression source in a throwaway module with a single
        -- declaration so we can reuse `Elm.Parser.parseToFile`. The entry
        -- name uses a lowercase identifier to stay valid Elm.
        wrappedSource : String
        wrappedSource =
            "module ResolvedEntry exposing (..)\n\nentry =\n    " ++ expressionSource
    in
    case Elm.Parser.parseToFile wrappedSource of
        Err deadEnds ->
            Err (ParsingError deadEnds)

        Ok file ->
            case file.declarations of
                [ Node _ (Elm.Syntax.Declaration.FunctionDeclaration func) ] ->
                    evalWithResolvedIRExpression projectEnv
                        (Node.value (Node.value func.declaration).expression)

                _ ->
                    Err
                        (EvalError
                            { currentModule = [ "ResolvedEntry" ]
                            , callStack = []
                            , error = Types.Unsupported "expected one function declaration in entry wrapper"
                            }
                        )


{-| Like `evalWithResolvedIR`, but takes a pre-parsed `Expression` AST
instead of a source string. Used by benchmarks and any caller that
already has an AST, to avoid the per-call parse overhead.
-}
evalWithResolvedIRExpression : ProjectEnv -> Expression -> Result Error Value
evalWithResolvedIRExpression (ProjectEnv projectEnv) expression =
    let
        resolverCtx : Resolver.ResolverContext
        resolverCtx =
            Resolver.initContext
                [ "ResolvedEntry" ]
                projectEnv.resolved.globalIds
    in
    case Resolver.resolveExpression resolverCtx (fakeNode expression) of
        Err resolveError ->
            Err
                (EvalError
                    { currentModule = [ "ResolvedEntry" ]
                    , callStack = []
                    , error =
                        Types.Unsupported
                            ("resolver error: " ++ resolverErrorToString resolveError)
                    }
                )

        Ok rexpr ->
            let
                dispatchConfig : Types.Config
                dispatchConfig =
                    { trace = False
                    , coverage = False
                    , coverageProbeLines = Set.empty
                    , maxSteps = Nothing
                    , tcoTarget = Nothing
                    , callCounts = Nothing
                    , intercepts = Dict.empty
                    , memoizedFunctions = MemoSpec.emptyRegistry
                    , collectMemoStats = False
                    , useResolvedIR = False
                    }

                renv : RE.REnv
                renv =
                    { locals = []
                    , globals = projectEnv.resolved.globals
                    , resolvedBodies = projectEnv.resolved.bodies
                    , globalIdToName = projectEnv.resolved.globalIdToName
                    , nativeDispatchers = projectEnv.resolved.nativeDispatchers
                    , higherOrderDispatchers = projectEnv.resolved.higherOrderDispatchers
                    , kernelDispatchers = projectEnv.resolved.kernelDispatchers
                    , interceptsByGlobal = Dict.empty
                    , fallbackEnv = projectEnv.env
                    , fallbackConfig = dispatchConfig
                    , currentModule = [ "ResolvedEntry" ]
                    , callStack = []
                    , callDepth = 0
                    }
            in
            case RE.evalR renv rexpr of
                EvOk value ->
                    Ok value

                EvErr errorData ->
                    Err (EvalError errorData)

                _ ->
                    Err
                        (EvalError
                            { currentModule = [ "ResolvedEntry" ]
                            , callStack = []
                            , error =
                                Types.Unsupported
                                    "EvalResult with trace/yield/memo — not supported through evalWithResolvedIR yet"
                            }
                        )


resolverErrorToString : Resolver.ResolveError -> String
resolverErrorToString err =
    case err of
        Resolver.UnknownName { moduleName, name } ->
            let
                qualified : String
                qualified =
                    if List.isEmpty moduleName then
                        name

                    else
                        String.join "." moduleName ++ "." ++ name
            in
            "unknown name " ++ qualified

        Resolver.UnknownOperator op ->
            "unknown operator " ++ op

        Resolver.UnsupportedExpression msg ->
            "unsupported expression: " ++ msg

        Resolver.InvalidRecordUpdateTarget name ->
            "invalid record update target: " ++ name

        Resolver.UnexpectedTupleArity n ->
            "unexpected tuple arity: " ++ String.fromInt n


{-| Is this FunctionImplementation a Custom type constructor?

Custom type constructors get synthesized by `constructorImplementations`
at env-build time with a self-referential body: an `Application` whose
head is `FunctionOrValue moduleName ctorName` matching the impl's own
name. When the old evaluator hits this at runtime, it detects the
uppercase name via `evalVariant` and produces a `Custom` value — the
self-reference never actually loops because the built-in `evalVariant`
short-circuits before evaluating it.

But when the resolver sees such a FunctionImplementation registered in
`globalIds`, it emits `RGlobal id` for any reference to the constructor.
At eval time `dispatchGlobalApply` finds a body for that id in
`resolvedBodies` and runs it through `evalR`, which resolves the
self-reference back into `RGlobal id`, and we loop forever (or hit the
depth budget and get a malformed `Custom` back from the old-eval
delegate path).

The fix: don't put Custom type constructor entries in `globalIds` at
all. That way the resolver falls through to `RCtor`, `evalConstructor`
produces a zero-arg Custom, and `applyClosure`'s Custom branch
accumulates the args — matching how Custom construction worked before
resolver coverage widened. Record alias constructors still go through
`RGlobal` because their synthesized body is a `RecordExpr`, not a
self-referential application.

-}
isCustomTypeConstructor : FunctionImplementation -> Bool
isCustomTypeConstructor impl =
    let
        implName : String
        implName =
            Node.value impl.name

        startsWithUppercase : Bool
        startsWithUppercase =
            case String.uncons implName of
                Just ( c, _ ) ->
                    Char.isUpper c

                Nothing ->
                    False

        bodyIsSelfApplication : Bool
        bodyIsSelfApplication =
            case Node.value impl.expression of
                FunctionOrValue _ refName ->
                    refName == implName

                Application items ->
                    case items of
                        (Node _ (FunctionOrValue _ refName)) :: _ ->
                            refName == implName

                        _ ->
                            False

                _ ->
                    False
    in
    startsWithUppercase && bodyIsSelfApplication


{-| Walk every declaration — core and user — to assign `GlobalId`s, then
add ids for exact kernel-backed names that don't have an AST declaration,
then walk user declarations again to resolve their bodies to `RExpr`. Core
bodies are deliberately **not** resolved (they reference kernel functions
that Phase 3's evaluator handles via a separate dispatch path anyway).

The two-pass structure is essential: pass 1 populates the id table so
that pass 2 can resolve cross-module references to already-seen
declarations without running into `UnknownName` errors. Since user code
frequently references `Basics.add` and friends, the core ids need to be
available when we resolve user declarations.

-}
resolveProject : List CachedModuleSummary -> ResolvedProject
resolveProject summaries =
    let
        -- Pass 1a: assign ids to every core declaration. Fold over
        -- `Core.functions` (the authoritative source) rather than
        -- coreFunctions (which is keyed by joined module string and
        -- loses the `ModuleName` shape needed by the resolver context).
        coreIdAssignment : { next : Int, ids : Dict.Dict ( ModuleName, String ) IR.GlobalId }
        coreIdAssignment =
            Core.functions
                |> Dict.foldl
                    (\moduleName moduleDict outer ->
                        moduleDict
                            |> Dict.foldl
                                (\name _ inner ->
                                    { next = inner.next + 1
                                    , ids = Dict.insert ( moduleName, name ) inner.next inner.ids
                                    }
                                )
                                outer
                    )
                    { next = 0, ids = Dict.empty }

        -- Pass 1b: assign ids to every user declaration, EXCEPT Custom
        -- type constructors. See `isCustomTypeConstructor` for why.
        allIdAssignment : { next : Int, ids : Dict.Dict ( ModuleName, String ) IR.GlobalId }
        allIdAssignment =
            summaries
                |> List.foldl
                    (\summary outer ->
                        summary.functions
                            |> List.foldl
                                (\impl inner ->
                                    if isCustomTypeConstructor impl then
                                        inner

                                    else
                                        let
                                            name : String
                                            name =
                                                Node.value impl.name
                                        in
                                        { next = inner.next + 1
                                        , ids = Dict.insert ( summary.moduleName, name ) inner.next inner.ids
                                        }
                                )
                                outer
                    )
                    coreIdAssignment

        kernelIdAssignment : { next : Int, ids : Dict.Dict ( ModuleName, String ) IR.GlobalId }
        kernelIdAssignment =
            Eval.Expression.kernelFunctions
                |> Dict.foldl
                    (\moduleKey moduleDict outer ->
                        let
                            moduleName : ModuleName
                            moduleName =
                                String.split "." moduleKey
                        in
                        moduleDict
                            |> Dict.foldl
                                (\name _ inner ->
                                    if Dict.member ( moduleName, name ) inner.ids then
                                        inner

                                    else
                                        { next = inner.next + 1
                                        , ids = Dict.insert ( moduleName, name ) inner.next inner.ids
                                        }
                                )
                                outer
                    )
                    allIdAssignment

        globalIds : Dict.Dict ( ModuleName, String ) IR.GlobalId
        globalIds =
            kernelIdAssignment.ids

        -- Reverse lookup map built from globalIds. Phase 3's core-dispatch
        -- bridge needs to know "which (moduleName, name) does this id map
        -- to?" so it can synthesize an old-style call for core declarations
        -- that aren't in `bodies`.
        globalIdToName : Dict.Dict IR.GlobalId ( ModuleName, String )
        globalIdToName =
            globalIds
                |> Dict.foldl
                    (\key id acc -> Dict.insert id key acc)
                    Dict.empty

        -- Build the native dispatcher registry once per project env. The
        -- registry maps a handful of hot core-function GlobalIds to
        -- direct `List Value -> Maybe Value` dispatchers so the new
        -- evaluator can skip the Value.toExpression + synthesized AST
        -- delegation path for common operators.
        nativeDispatchers : Dict.Dict IR.GlobalId NativeDispatch.NativeDispatcher
        nativeDispatchers =
            NativeDispatch.buildRegistry
                (\key -> Dict.get key globalIds)

        -- Build the higher-order dispatcher registry — parallel to
        -- `nativeDispatchers`, but for core functions like `List.foldl`
        -- that take a callback Value. These dispatchers invoke callbacks
        -- via `applyClosure` directly, bypassing the old evaluator's
        -- `Kernel.function` marshaling layer which mis-handles
        -- `RExprImpl` closures (patterns = [] but arity > 0).
        higherOrderDispatchers : Dict.Dict IR.GlobalId RE.HigherOrderDispatcher
        higherOrderDispatchers =
            RE.buildHigherOrderRegistry
                (\key -> Dict.get key globalIds)

        {- Build the kernel dispatcher registry from `kernelFunctions`
           by exact qualified name.

           This deliberately does NOT do the old unsafe
           `Elm.Kernel.X.fn` → `X.fn` prefix-strip rewrite. That
           rewrite assumed the Core wrapper for `X.fn` was a trivial
           pass-through, which is false for wrappers that adapt
           argument types across the kernel boundary, e.g.:

             String.join sep chunks =
                 Elm.Kernel.String.join sep (Elm.Kernel.List.toArray chunks)

           The user-level `String.join` takes a `List String`, but the
           kernel `Elm.Kernel.String.join` takes a `jsArray string`
           (JsArray). Short-circuiting `String.join` to the kernel
           dispatcher bypasses the `toArray chunks` conversion, the
           kernel sees a `List` where it expects a `JsArray`, and fires
           "Expected the second argument to be List String".

           Exact-name dispatch is safe for both cases:

             - user-facing overrides like `Dict.insert`, which are
               intentionally registered as drop-in replacements
             - explicit `Elm.Kernel.*` references that appear in
               package source (e.g. `Regex`, `Bytes`, `Json`) after
               the caller has already chosen the kernel entry point
        -}
        kernelDispatchers : Dict.Dict IR.GlobalId KernelDispatcher
        kernelDispatchers =
            Eval.Expression.kernelFunctions
                |> Dict.foldl
                    (\moduleKey moduleDict outer ->
                        let
                            parts : List String
                            parts =
                                String.split "." moduleKey
                        in
                        moduleDict
                            |> Dict.foldl
                                (\funcName ( arity, kernelFn ) acc ->
                                    case Dict.get ( parts, funcName ) globalIds of
                                        Just id ->
                                            Dict.insert id
                                                { arity = arity
                                                , kernelFn = kernelFn
                                                }
                                                acc

                                        Nothing ->
                                            acc
                                )
                                outer
                    )
                    Dict.empty

        -- Pass 2: resolve each user declaration's body against the full
        -- globalIds map. Accumulate successes in `bodies` and failures
        -- in `errors`. Either way, keep going — a failure here is not
        -- fatal because the old evaluator path still works.
        initialAcc : ResolvedProject
        initialAcc =
            { globalIds = globalIds
            , bodies = Dict.empty
            , globalIdToName = globalIdToName
            , nativeDispatchers = nativeDispatchers
            , higherOrderDispatchers = higherOrderDispatchers
            , kernelDispatchers = kernelDispatchers
            , globals = Dict.empty
            , errors = []
            }
    in
    resolveProjectFromSummariesAndInitial initialAcc summaries


{-| Second half of `resolveProject` — folds `resolveDeclaration` over every
user function in `summaries`, inserting successes into `bodies` and failures
into `errors`. Extracted so `rebuildResolvedFromEnv` can reuse the exact same
resolution walk.
-}
resolveProjectFromSummariesAndInitial : ResolvedProject -> List CachedModuleSummary -> ResolvedProject
resolveProjectFromSummariesAndInitial initialAcc summaries =
    let
        globalIds : Dict.Dict ( ModuleName, String ) IR.GlobalId
        globalIds =
            initialAcc.globalIds
    in
    summaries
        |> List.foldl
            (\summary outer ->
                summary.functions
                    |> List.foldl
                        (\impl inner ->
                            if isCustomTypeConstructor impl then
                                inner

                            else
                                let
                                    name : String
                                    name =
                                        Node.value impl.name

                                    ctx : Resolver.ResolverContext
                                    ctx =
                                        Resolver.initContextWithImports
                                            summary.moduleName
                                            globalIds
                                            summary.importedNames
                                in
                                case Resolver.resolveDeclaration ctx impl of
                                    Ok rexpr ->
                                        case Dict.get ( summary.moduleName, name ) inner.globalIds of
                                            Just id ->
                                                { inner
                                                    | bodies = Dict.insert id rexpr inner.bodies
                                                }

                                            Nothing ->
                                                -- Should be impossible — pass 1 added this entry.
                                                inner

                                    Err err ->
                                        { inner
                                            | errors =
                                                { moduleName = summary.moduleName
                                                , name = name
                                                , error = err
                                                }
                                                    :: inner.errors
                                        }
                        )
                        outer
            )
            initialAcc


{-| Incrementally update the resolved-IR sidecar for a set of changed
modules, splicing the results into an existing `ResolvedProject`.

Used by `replaceModuleInEnv` (one module) and `extendWithFiles` (N new
modules) so the resolved-IR view stays coherent with
`env.shared.functions` without paying for a full project-wide
resolver walk on every incremental update.

Algorithm:

1.  **Purge** every entry in `base.globalIds`, `base.globalIdToName`,
    `base.bodies`, and `base.errors` whose module name appears in
    `changedModules`. The purge keeps every unchanged module's
    resolved declarations alive under their existing GlobalIds, so
    cross-module references from unchanged modules stay valid.

2.  **Reconstruct** a `CachedModuleSummary` for each changed module
    from `env.shared.functions` + `env.shared.moduleImports` +
    `allInterfaces`. Modules that lack an `importedNames` entry
    (which only happens before `buildModuleEnv` has run on them)
    are skipped.

3.  **Allocate fresh GlobalIds** for every declaration in the
    changed modules, starting from `1 + max(purged globalIdToName keys)`.
    This keeps existing IDs stable — any IDs that were freed by
    the purge are not reused, which fragments the ID space slightly
    but avoids any risk of reusing an ID that some in-flight
    computation is holding. Fragmentation is irrelevant for
    correctness and the space is 32-bit, so there's no pressure
    to reclaim.

4.  **Resolve** each changed decl's body against the merged
    `globalIds` context and insert the result into `bodies`. This
    shares `resolveProjectFromSummariesAndInitial` with the initial
    full build path so the resolver walk stays in one place.

The `nativeDispatchers`, `higherOrderDispatchers`, and
`kernelDispatchers` maps are keyed by **core** GlobalIds only, which
never change under user-module edits, so they carry over from
`base` unchanged.

**Perf**: O(changed\_modules × decls\_per\_module) resolver walks +
O(base.globalIds) purge scan. For mutation testing's hot loop
(one module changed per mutant) this is 1/N the cost of the
full-project rebuild from Phase 1a, where N is the total module
count. On small-12 that's ~40× faster per call.

-}
rebuildResolvedForModules :
    List ModuleName
    -> Env
    -> ElmDict.Dict ModuleName (List Exposed)
    -> ResolvedProject
    -> ResolvedProject
rebuildResolvedForModules changedModules env allInterfaces base =
    let
        changedKeys : Set String
        changedKeys =
            changedModules
                |> List.map Environment.moduleKey
                |> Set.fromList

        isChanged : ModuleName -> Bool
        isChanged moduleName =
            Set.member (Environment.moduleKey moduleName) changedKeys

        -- Collect every GlobalId currently mapped to a changed module.
        -- Used to purge the reverse map and the bodies dict below.
        idsToRemove : Set IR.GlobalId
        idsToRemove =
            base.globalIds
                |> Dict.foldl
                    (\( modName, _ ) id acc ->
                        if isChanged modName then
                            Set.insert id acc

                        else
                            acc
                    )
                    Set.empty

        purgedGlobalIds : Dict.Dict ( ModuleName, String ) IR.GlobalId
        purgedGlobalIds =
            base.globalIds
                |> Dict.foldl
                    (\key id acc ->
                        if isChanged (Tuple.first key) then
                            acc

                        else
                            Dict.insert key id acc
                    )
                    Dict.empty

        purgedGlobalIdToName : Dict.Dict IR.GlobalId ( ModuleName, String )
        purgedGlobalIdToName =
            base.globalIdToName
                |> Dict.foldl
                    (\id name acc ->
                        if Set.member id idsToRemove then
                            acc

                        else
                            Dict.insert id name acc
                    )
                    Dict.empty

        purgedBodies : Dict.Dict IR.GlobalId IR.RExpr
        purgedBodies =
            base.bodies
                |> Dict.foldl
                    (\id body acc ->
                        if Set.member id idsToRemove then
                            acc

                        else
                            Dict.insert id body acc
                    )
                    Dict.empty

        purgedErrors : List ResolveErrorEntry
        purgedErrors =
            base.errors
                |> List.filter (\err -> not (isChanged err.moduleName))

        -- Reconstruct CachedModuleSummary values for the changed
        -- modules only. Summaries whose `importedNames` entry is
        -- missing from `env.shared.moduleImports` are skipped — that
        -- matches `rebuildResolvedFromEnvFull`'s behaviour and is a
        -- no-op before `buildModuleEnv` has registered them.
        changedSummaries : List CachedModuleSummary
        changedSummaries =
            changedModules
                |> List.filterMap
                    (\moduleName ->
                        let
                            key : String
                            key =
                                Environment.moduleKey moduleName
                        in
                        case
                            ( ElmDict.get moduleName allInterfaces
                            , Dict.get key env.shared.moduleImports
                            )
                        of
                            ( Just interface, Just importedNames ) ->
                                Just
                                    { moduleName = moduleName
                                    , interface = interface
                                    , importedNames = importedNames
                                    , functions =
                                        Dict.get key env.shared.functions
                                            |> Maybe.map Dict.values
                                            |> Maybe.withDefault []
                                    }

                            _ ->
                                Nothing
                    )

        -- Allocate fresh GlobalIds for every decl in the changed
        -- summaries, starting at 1 + max(remaining ids). This avoids
        -- reusing any ID that in-flight intercept tables or closure
        -- captures may still be holding.
        nextStartId : IR.GlobalId
        nextStartId =
            (purgedGlobalIdToName
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault -1
            )
                + 1

        idAssignment :
            { next : IR.GlobalId
            , ids : Dict.Dict ( ModuleName, String ) IR.GlobalId
            , reverseIds : Dict.Dict IR.GlobalId ( ModuleName, String )
            }
        idAssignment =
            changedSummaries
                |> List.foldl
                    (\summary outer ->
                        summary.functions
                            |> List.foldl
                                (\impl inner ->
                                    if isCustomTypeConstructor impl then
                                        inner

                                    else
                                        let
                                            name : String
                                            name =
                                                Node.value impl.name
                                        in
                                        { next = inner.next + 1
                                        , ids = Dict.insert ( summary.moduleName, name ) inner.next inner.ids
                                        , reverseIds = Dict.insert inner.next ( summary.moduleName, name ) inner.reverseIds
                                        }
                                )
                                outer
                    )
                    { next = nextStartId
                    , ids = purgedGlobalIds
                    , reverseIds = purgedGlobalIdToName
                    }

        deltaAcc : ResolvedProject
        deltaAcc =
            { globalIds = idAssignment.ids
            , bodies = purgedBodies
            , globalIdToName = idAssignment.reverseIds
            , nativeDispatchers = base.nativeDispatchers
            , higherOrderDispatchers = base.higherOrderDispatchers
            , kernelDispatchers = base.kernelDispatchers
            , globals = base.globals
            , errors = purgedErrors
            }
    in
    -- Resolve the changed decls' bodies against the merged globalIds
    -- via the same resolver walk the initial full build uses.
    resolveProjectFromSummariesAndInitial deltaAcc changedSummaries


eval : String -> Expression -> Result Error Value
eval source expression =
    let
        ( result, _, _ ) =
            traceOrEvalModule { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False } source expression
    in
    result


trace : String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
trace source expression =
    traceOrEvalModule { trace = True, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False } source expression


traceOrEvalModule : Types.Config -> String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
traceOrEvalModule cfg source expression =
    let
        maybeEnv : Result Error Env
        maybeEnv =
            source
                |> Elm.Parser.parseToFile
                |> Result.mapError ParsingError
                |> Result.andThen buildInitialEnv
    in
    case maybeEnv of
        Err e ->
            ( Err e, Rope.empty, Rope.empty )

        Ok env ->
            let
                maybeNode : a -> Node a
                maybeNode =
                    case expression of
                        FunctionOrValue [] name ->
                            let
                                needle : String
                                needle =
                                    name ++ " ="
                            in
                            source
                                |> String.split "\n"
                                |> List.Extra.findIndex
                                    (String.startsWith needle)
                                |> Maybe.map
                                    (\index ->
                                        Node
                                            { start = { row = index + 1, column = 1 }
                                            , end = { row = index + 1, column = 1 + String.length name }
                                            }
                                    )
                                |> Maybe.withDefault fakeNode

                        _ ->
                            fakeNode

                evalResult : Types.EvalResult Value
                evalResult =
                    Eval.Expression.evalExpression
                        (maybeNode expression)
                        cfg
                        env

                ( result, callTrees, logLines ) =
                    EvalResult.toTriple evalResult
            in
            ( Result.mapError Types.EvalError result
            , callTrees
            , logLines
            )


{-| Opaque type holding a pre-built environment from parsed sources.
Use `buildProjectEnv` to create one, then `evalWithEnv` to evaluate
expressions against it without re-parsing those sources.
-}
type ProjectEnv
    = ProjectEnv
        { env : Env
        , allInterfaces : ElmDict.Dict ModuleName (List Exposed)
        , resolved : ResolvedProject
        }


type alias CachedModuleSummary =
    { moduleName : ModuleName
    , interface : List Exposed
    , importedNames : ImportedNames
    , functions : List FunctionImplementation
    }


type alias DependencySummaryStats =
    { functionsVisited : Int
    , functionsRewritten : Int
    , inlineCandidates : Int
    , inlineSuccesses : Int
    , inlineRejectedPattern : Int
    , inlineRejectedArity : Int
    , inlineRejectedSelfCall : Int
    , inlineRejectedBodyTooLarge : Int
    , inlineRejectedUnsafe : Int
    , inlineRejectedUnsafeApplication : Int
    , inlineRejectedUnsafeIf : Int
    , inlineRejectedUnsafeCase : Int
    , inlineRejectedUnsafeLet : Int
    , inlineRejectedUnsafeLambda : Int
    , inlineRejectedUnsafeOther : Int
    , inlineRejectedInternalHelper : Int
    , inlineBodyLt30 : Int
    , inlineBody30To59 : Int
    , inlineBody60Plus : Int
    , inlineShapeLeaf : Int
    , inlineShapeConstructor : Int
    , inlineShapeOperator : Int
    , inlineShapeRecordAccess : Int
    , inlineShapeCollection : Int
    , inlineShapeOther : Int
    , inlinePayoffChanged : Int
    , inlinePayoffChangedShapeLeaf : Int
    , inlinePayoffChangedShapeConstructor : Int
    , inlinePayoffChangedShapeOperator : Int
    , inlinePayoffChangedShapeRecordAccess : Int
    , inlinePayoffChangedShapeCollection : Int
    , inlinePayoffChangedShapeOther : Int
    , inlinePayoffChangedBodyLt30 : Int
    , inlinePayoffChangedBody30To59 : Int
    , inlinePayoffChangedBody60Plus : Int
    , inlinePayoffInline : Int
    , inlinePayoffInlineShapeLeaf : Int
    , inlinePayoffInlineShapeConstructor : Int
    , inlinePayoffInlineShapeOperator : Int
    , inlinePayoffInlineShapeRecordAccess : Int
    , inlinePayoffInlineShapeCollection : Int
    , inlinePayoffInlineShapeOther : Int
    , inlinePayoffInlineBodyLt30 : Int
    , inlinePayoffInlineBody30To59 : Int
    , inlinePayoffInlineBody60Plus : Int
    , inlinePayoffConstantFold : Int
    , inlinePayoffConstantFoldShapeLeaf : Int
    , inlinePayoffConstantFoldShapeConstructor : Int
    , inlinePayoffConstantFoldShapeOperator : Int
    , inlinePayoffConstantFoldShapeRecordAccess : Int
    , inlinePayoffConstantFoldShapeCollection : Int
    , inlinePayoffConstantFoldShapeOther : Int
    , inlinePayoffConstantFoldBodyLt30 : Int
    , inlinePayoffConstantFoldBody30To59 : Int
    , inlinePayoffConstantFoldBody60Plus : Int
    , inlinePayoffPrecomputedRef : Int
    , inlinePayoffPrecomputedRefShapeLeaf : Int
    , inlinePayoffPrecomputedRefShapeConstructor : Int
    , inlinePayoffPrecomputedRefShapeOperator : Int
    , inlinePayoffPrecomputedRefShapeRecordAccess : Int
    , inlinePayoffPrecomputedRefShapeCollection : Int
    , inlinePayoffPrecomputedRefShapeOther : Int
    , inlinePayoffPrecomputedRefBodyLt30 : Int
    , inlinePayoffPrecomputedRefBody30To59 : Int
    , inlinePayoffPrecomputedRefBody60Plus : Int
    , inlineShadowRejectCollection : Int
    , inlineShadowRejectCollectionPayoffChanged : Int
    , inlineShadowRejectCollectionPayoffInline : Int
    , inlineShadowRejectCollectionPayoffPrecomputedRef : Int
    , inlineShadowRejectCollectionFinalShrinks : Int
    , inlineShadowRejectCollectionFinalNonApplication : Int
    , inlineShadowRejectCollectionFinalDirectRootWin : Int
    , inlineShadowRejectCollectionFinalConstructorApplication : Int
    , inlineShadowRejectCollectionNoPayoffNoDirectBenefit : Int
    , inlineShadowRejectGrowth0 : Int
    , inlineShadowRejectGrowth0PayoffChanged : Int
    , inlineShadowRejectGrowth0PayoffInline : Int
    , inlineShadowRejectGrowth0PayoffPrecomputedRef : Int
    , inlineShadowRejectGrowth0FinalShrinks : Int
    , inlineShadowRejectGrowth0FinalNonApplication : Int
    , inlineShadowRejectGrowth0FinalDirectRootWin : Int
    , inlineShadowRejectGrowth0FinalConstructorApplication : Int
    , inlineShadowRejectGrowth0NoPayoffNoDirectBenefit : Int
    , inlineShadowRejectGrowth1 : Int
    , inlineShadowRejectGrowth1PayoffChanged : Int
    , inlineShadowRejectGrowth1PayoffInline : Int
    , inlineShadowRejectGrowth1PayoffPrecomputedRef : Int
    , inlineShadowRejectGrowth1FinalShrinks : Int
    , inlineShadowRejectGrowth1FinalNonApplication : Int
    , inlineShadowRejectGrowth1FinalDirectRootWin : Int
    , inlineShadowRejectGrowth1FinalConstructorApplication : Int
    , inlineShadowRejectGrowth1NoPayoffNoDirectBenefit : Int
    , listFusionChanges : Int
    , listFusionPipelineNormalizations : Int
    , listFusionHeadFlattenRewrites : Int
    , listFusionRuleRewrites : Int
    , precomputedRefSubstitutions : Int
    , constantFolds : Int
    , rejectSamples : List String
    }


emptyDependencySummaryStats : DependencySummaryStats
emptyDependencySummaryStats =
    { functionsVisited = 0
    , functionsRewritten = 0
    , inlineCandidates = 0
    , inlineSuccesses = 0
    , inlineRejectedPattern = 0
    , inlineRejectedArity = 0
    , inlineRejectedSelfCall = 0
    , inlineRejectedBodyTooLarge = 0
    , inlineRejectedUnsafe = 0
    , inlineRejectedUnsafeApplication = 0
    , inlineRejectedUnsafeIf = 0
    , inlineRejectedUnsafeCase = 0
    , inlineRejectedUnsafeLet = 0
    , inlineRejectedUnsafeLambda = 0
    , inlineRejectedUnsafeOther = 0
    , inlineRejectedInternalHelper = 0
    , inlineBodyLt30 = 0
    , inlineBody30To59 = 0
    , inlineBody60Plus = 0
    , inlineShapeLeaf = 0
    , inlineShapeConstructor = 0
    , inlineShapeOperator = 0
    , inlineShapeRecordAccess = 0
    , inlineShapeCollection = 0
    , inlineShapeOther = 0
    , inlinePayoffChanged = 0
    , inlinePayoffChangedShapeLeaf = 0
    , inlinePayoffChangedShapeConstructor = 0
    , inlinePayoffChangedShapeOperator = 0
    , inlinePayoffChangedShapeRecordAccess = 0
    , inlinePayoffChangedShapeCollection = 0
    , inlinePayoffChangedShapeOther = 0
    , inlinePayoffChangedBodyLt30 = 0
    , inlinePayoffChangedBody30To59 = 0
    , inlinePayoffChangedBody60Plus = 0
    , inlinePayoffInline = 0
    , inlinePayoffInlineShapeLeaf = 0
    , inlinePayoffInlineShapeConstructor = 0
    , inlinePayoffInlineShapeOperator = 0
    , inlinePayoffInlineShapeRecordAccess = 0
    , inlinePayoffInlineShapeCollection = 0
    , inlinePayoffInlineShapeOther = 0
    , inlinePayoffInlineBodyLt30 = 0
    , inlinePayoffInlineBody30To59 = 0
    , inlinePayoffInlineBody60Plus = 0
    , inlinePayoffConstantFold = 0
    , inlinePayoffConstantFoldShapeLeaf = 0
    , inlinePayoffConstantFoldShapeConstructor = 0
    , inlinePayoffConstantFoldShapeOperator = 0
    , inlinePayoffConstantFoldShapeRecordAccess = 0
    , inlinePayoffConstantFoldShapeCollection = 0
    , inlinePayoffConstantFoldShapeOther = 0
    , inlinePayoffConstantFoldBodyLt30 = 0
    , inlinePayoffConstantFoldBody30To59 = 0
    , inlinePayoffConstantFoldBody60Plus = 0
    , inlinePayoffPrecomputedRef = 0
    , inlinePayoffPrecomputedRefShapeLeaf = 0
    , inlinePayoffPrecomputedRefShapeConstructor = 0
    , inlinePayoffPrecomputedRefShapeOperator = 0
    , inlinePayoffPrecomputedRefShapeRecordAccess = 0
    , inlinePayoffPrecomputedRefShapeCollection = 0
    , inlinePayoffPrecomputedRefShapeOther = 0
    , inlinePayoffPrecomputedRefBodyLt30 = 0
    , inlinePayoffPrecomputedRefBody30To59 = 0
    , inlinePayoffPrecomputedRefBody60Plus = 0
    , inlineShadowRejectCollection = 0
    , inlineShadowRejectCollectionPayoffChanged = 0
    , inlineShadowRejectCollectionPayoffInline = 0
    , inlineShadowRejectCollectionPayoffPrecomputedRef = 0
    , inlineShadowRejectCollectionFinalShrinks = 0
    , inlineShadowRejectCollectionFinalNonApplication = 0
    , inlineShadowRejectCollectionFinalDirectRootWin = 0
    , inlineShadowRejectCollectionFinalConstructorApplication = 0
    , inlineShadowRejectCollectionNoPayoffNoDirectBenefit = 0
    , inlineShadowRejectGrowth0 = 0
    , inlineShadowRejectGrowth0PayoffChanged = 0
    , inlineShadowRejectGrowth0PayoffInline = 0
    , inlineShadowRejectGrowth0PayoffPrecomputedRef = 0
    , inlineShadowRejectGrowth0FinalShrinks = 0
    , inlineShadowRejectGrowth0FinalNonApplication = 0
    , inlineShadowRejectGrowth0FinalDirectRootWin = 0
    , inlineShadowRejectGrowth0FinalConstructorApplication = 0
    , inlineShadowRejectGrowth0NoPayoffNoDirectBenefit = 0
    , inlineShadowRejectGrowth1 = 0
    , inlineShadowRejectGrowth1PayoffChanged = 0
    , inlineShadowRejectGrowth1PayoffInline = 0
    , inlineShadowRejectGrowth1PayoffPrecomputedRef = 0
    , inlineShadowRejectGrowth1FinalShrinks = 0
    , inlineShadowRejectGrowth1FinalNonApplication = 0
    , inlineShadowRejectGrowth1FinalDirectRootWin = 0
    , inlineShadowRejectGrowth1FinalConstructorApplication = 0
    , inlineShadowRejectGrowth1NoPayoffNoDirectBenefit = 0
    , listFusionChanges = 0
    , listFusionPipelineNormalizations = 0
    , listFusionHeadFlattenRewrites = 0
    , listFusionRuleRewrites = 0
    , precomputedRefSubstitutions = 0
    , constantFolds = 0
    , rejectSamples = []
    }


mergeDependencySummaryStats : DependencySummaryStats -> DependencySummaryStats -> DependencySummaryStats
mergeDependencySummaryStats left right =
    { functionsVisited = left.functionsVisited + right.functionsVisited
    , functionsRewritten = left.functionsRewritten + right.functionsRewritten
    , inlineCandidates = left.inlineCandidates + right.inlineCandidates
    , inlineSuccesses = left.inlineSuccesses + right.inlineSuccesses
    , inlineRejectedPattern = left.inlineRejectedPattern + right.inlineRejectedPattern
    , inlineRejectedArity = left.inlineRejectedArity + right.inlineRejectedArity
    , inlineRejectedSelfCall = left.inlineRejectedSelfCall + right.inlineRejectedSelfCall
    , inlineRejectedBodyTooLarge = left.inlineRejectedBodyTooLarge + right.inlineRejectedBodyTooLarge
    , inlineRejectedUnsafe = left.inlineRejectedUnsafe + right.inlineRejectedUnsafe
    , inlineRejectedUnsafeApplication = left.inlineRejectedUnsafeApplication + right.inlineRejectedUnsafeApplication
    , inlineRejectedUnsafeIf = left.inlineRejectedUnsafeIf + right.inlineRejectedUnsafeIf
    , inlineRejectedUnsafeCase = left.inlineRejectedUnsafeCase + right.inlineRejectedUnsafeCase
    , inlineRejectedUnsafeLet = left.inlineRejectedUnsafeLet + right.inlineRejectedUnsafeLet
    , inlineRejectedUnsafeLambda = left.inlineRejectedUnsafeLambda + right.inlineRejectedUnsafeLambda
    , inlineRejectedUnsafeOther = left.inlineRejectedUnsafeOther + right.inlineRejectedUnsafeOther
    , inlineRejectedInternalHelper = left.inlineRejectedInternalHelper + right.inlineRejectedInternalHelper
    , inlineBodyLt30 = left.inlineBodyLt30 + right.inlineBodyLt30
    , inlineBody30To59 = left.inlineBody30To59 + right.inlineBody30To59
    , inlineBody60Plus = left.inlineBody60Plus + right.inlineBody60Plus
    , inlineShapeLeaf = left.inlineShapeLeaf + right.inlineShapeLeaf
    , inlineShapeConstructor = left.inlineShapeConstructor + right.inlineShapeConstructor
    , inlineShapeOperator = left.inlineShapeOperator + right.inlineShapeOperator
    , inlineShapeRecordAccess = left.inlineShapeRecordAccess + right.inlineShapeRecordAccess
    , inlineShapeCollection = left.inlineShapeCollection + right.inlineShapeCollection
    , inlineShapeOther = left.inlineShapeOther + right.inlineShapeOther
    , inlinePayoffChanged = left.inlinePayoffChanged + right.inlinePayoffChanged
    , inlinePayoffChangedShapeLeaf = left.inlinePayoffChangedShapeLeaf + right.inlinePayoffChangedShapeLeaf
    , inlinePayoffChangedShapeConstructor = left.inlinePayoffChangedShapeConstructor + right.inlinePayoffChangedShapeConstructor
    , inlinePayoffChangedShapeOperator = left.inlinePayoffChangedShapeOperator + right.inlinePayoffChangedShapeOperator
    , inlinePayoffChangedShapeRecordAccess = left.inlinePayoffChangedShapeRecordAccess + right.inlinePayoffChangedShapeRecordAccess
    , inlinePayoffChangedShapeCollection = left.inlinePayoffChangedShapeCollection + right.inlinePayoffChangedShapeCollection
    , inlinePayoffChangedShapeOther = left.inlinePayoffChangedShapeOther + right.inlinePayoffChangedShapeOther
    , inlinePayoffChangedBodyLt30 = left.inlinePayoffChangedBodyLt30 + right.inlinePayoffChangedBodyLt30
    , inlinePayoffChangedBody30To59 = left.inlinePayoffChangedBody30To59 + right.inlinePayoffChangedBody30To59
    , inlinePayoffChangedBody60Plus = left.inlinePayoffChangedBody60Plus + right.inlinePayoffChangedBody60Plus
    , inlinePayoffInline = left.inlinePayoffInline + right.inlinePayoffInline
    , inlinePayoffInlineShapeLeaf = left.inlinePayoffInlineShapeLeaf + right.inlinePayoffInlineShapeLeaf
    , inlinePayoffInlineShapeConstructor = left.inlinePayoffInlineShapeConstructor + right.inlinePayoffInlineShapeConstructor
    , inlinePayoffInlineShapeOperator = left.inlinePayoffInlineShapeOperator + right.inlinePayoffInlineShapeOperator
    , inlinePayoffInlineShapeRecordAccess = left.inlinePayoffInlineShapeRecordAccess + right.inlinePayoffInlineShapeRecordAccess
    , inlinePayoffInlineShapeCollection = left.inlinePayoffInlineShapeCollection + right.inlinePayoffInlineShapeCollection
    , inlinePayoffInlineShapeOther = left.inlinePayoffInlineShapeOther + right.inlinePayoffInlineShapeOther
    , inlinePayoffInlineBodyLt30 = left.inlinePayoffInlineBodyLt30 + right.inlinePayoffInlineBodyLt30
    , inlinePayoffInlineBody30To59 = left.inlinePayoffInlineBody30To59 + right.inlinePayoffInlineBody30To59
    , inlinePayoffInlineBody60Plus = left.inlinePayoffInlineBody60Plus + right.inlinePayoffInlineBody60Plus
    , inlinePayoffConstantFold = left.inlinePayoffConstantFold + right.inlinePayoffConstantFold
    , inlinePayoffConstantFoldShapeLeaf = left.inlinePayoffConstantFoldShapeLeaf + right.inlinePayoffConstantFoldShapeLeaf
    , inlinePayoffConstantFoldShapeConstructor = left.inlinePayoffConstantFoldShapeConstructor + right.inlinePayoffConstantFoldShapeConstructor
    , inlinePayoffConstantFoldShapeOperator = left.inlinePayoffConstantFoldShapeOperator + right.inlinePayoffConstantFoldShapeOperator
    , inlinePayoffConstantFoldShapeRecordAccess = left.inlinePayoffConstantFoldShapeRecordAccess + right.inlinePayoffConstantFoldShapeRecordAccess
    , inlinePayoffConstantFoldShapeCollection = left.inlinePayoffConstantFoldShapeCollection + right.inlinePayoffConstantFoldShapeCollection
    , inlinePayoffConstantFoldShapeOther = left.inlinePayoffConstantFoldShapeOther + right.inlinePayoffConstantFoldShapeOther
    , inlinePayoffConstantFoldBodyLt30 = left.inlinePayoffConstantFoldBodyLt30 + right.inlinePayoffConstantFoldBodyLt30
    , inlinePayoffConstantFoldBody30To59 = left.inlinePayoffConstantFoldBody30To59 + right.inlinePayoffConstantFoldBody30To59
    , inlinePayoffConstantFoldBody60Plus = left.inlinePayoffConstantFoldBody60Plus + right.inlinePayoffConstantFoldBody60Plus
    , inlinePayoffPrecomputedRef = left.inlinePayoffPrecomputedRef + right.inlinePayoffPrecomputedRef
    , inlinePayoffPrecomputedRefShapeLeaf = left.inlinePayoffPrecomputedRefShapeLeaf + right.inlinePayoffPrecomputedRefShapeLeaf
    , inlinePayoffPrecomputedRefShapeConstructor = left.inlinePayoffPrecomputedRefShapeConstructor + right.inlinePayoffPrecomputedRefShapeConstructor
    , inlinePayoffPrecomputedRefShapeOperator = left.inlinePayoffPrecomputedRefShapeOperator + right.inlinePayoffPrecomputedRefShapeOperator
    , inlinePayoffPrecomputedRefShapeRecordAccess = left.inlinePayoffPrecomputedRefShapeRecordAccess + right.inlinePayoffPrecomputedRefShapeRecordAccess
    , inlinePayoffPrecomputedRefShapeCollection = left.inlinePayoffPrecomputedRefShapeCollection + right.inlinePayoffPrecomputedRefShapeCollection
    , inlinePayoffPrecomputedRefShapeOther = left.inlinePayoffPrecomputedRefShapeOther + right.inlinePayoffPrecomputedRefShapeOther
    , inlinePayoffPrecomputedRefBodyLt30 = left.inlinePayoffPrecomputedRefBodyLt30 + right.inlinePayoffPrecomputedRefBodyLt30
    , inlinePayoffPrecomputedRefBody30To59 = left.inlinePayoffPrecomputedRefBody30To59 + right.inlinePayoffPrecomputedRefBody30To59
    , inlinePayoffPrecomputedRefBody60Plus = left.inlinePayoffPrecomputedRefBody60Plus + right.inlinePayoffPrecomputedRefBody60Plus
    , inlineShadowRejectCollection = left.inlineShadowRejectCollection + right.inlineShadowRejectCollection
    , inlineShadowRejectCollectionPayoffChanged = left.inlineShadowRejectCollectionPayoffChanged + right.inlineShadowRejectCollectionPayoffChanged
    , inlineShadowRejectCollectionPayoffInline = left.inlineShadowRejectCollectionPayoffInline + right.inlineShadowRejectCollectionPayoffInline
    , inlineShadowRejectCollectionPayoffPrecomputedRef = left.inlineShadowRejectCollectionPayoffPrecomputedRef + right.inlineShadowRejectCollectionPayoffPrecomputedRef
    , inlineShadowRejectCollectionFinalShrinks = left.inlineShadowRejectCollectionFinalShrinks + right.inlineShadowRejectCollectionFinalShrinks
    , inlineShadowRejectCollectionFinalNonApplication = left.inlineShadowRejectCollectionFinalNonApplication + right.inlineShadowRejectCollectionFinalNonApplication
    , inlineShadowRejectCollectionFinalDirectRootWin = left.inlineShadowRejectCollectionFinalDirectRootWin + right.inlineShadowRejectCollectionFinalDirectRootWin
    , inlineShadowRejectCollectionFinalConstructorApplication = left.inlineShadowRejectCollectionFinalConstructorApplication + right.inlineShadowRejectCollectionFinalConstructorApplication
    , inlineShadowRejectCollectionNoPayoffNoDirectBenefit = left.inlineShadowRejectCollectionNoPayoffNoDirectBenefit + right.inlineShadowRejectCollectionNoPayoffNoDirectBenefit
    , inlineShadowRejectGrowth0 = left.inlineShadowRejectGrowth0 + right.inlineShadowRejectGrowth0
    , inlineShadowRejectGrowth0PayoffChanged = left.inlineShadowRejectGrowth0PayoffChanged + right.inlineShadowRejectGrowth0PayoffChanged
    , inlineShadowRejectGrowth0PayoffInline = left.inlineShadowRejectGrowth0PayoffInline + right.inlineShadowRejectGrowth0PayoffInline
    , inlineShadowRejectGrowth0PayoffPrecomputedRef = left.inlineShadowRejectGrowth0PayoffPrecomputedRef + right.inlineShadowRejectGrowth0PayoffPrecomputedRef
    , inlineShadowRejectGrowth0FinalShrinks = left.inlineShadowRejectGrowth0FinalShrinks + right.inlineShadowRejectGrowth0FinalShrinks
    , inlineShadowRejectGrowth0FinalNonApplication = left.inlineShadowRejectGrowth0FinalNonApplication + right.inlineShadowRejectGrowth0FinalNonApplication
    , inlineShadowRejectGrowth0FinalDirectRootWin = left.inlineShadowRejectGrowth0FinalDirectRootWin + right.inlineShadowRejectGrowth0FinalDirectRootWin
    , inlineShadowRejectGrowth0FinalConstructorApplication = left.inlineShadowRejectGrowth0FinalConstructorApplication + right.inlineShadowRejectGrowth0FinalConstructorApplication
    , inlineShadowRejectGrowth0NoPayoffNoDirectBenefit = left.inlineShadowRejectGrowth0NoPayoffNoDirectBenefit + right.inlineShadowRejectGrowth0NoPayoffNoDirectBenefit
    , inlineShadowRejectGrowth1 = left.inlineShadowRejectGrowth1 + right.inlineShadowRejectGrowth1
    , inlineShadowRejectGrowth1PayoffChanged = left.inlineShadowRejectGrowth1PayoffChanged + right.inlineShadowRejectGrowth1PayoffChanged
    , inlineShadowRejectGrowth1PayoffInline = left.inlineShadowRejectGrowth1PayoffInline + right.inlineShadowRejectGrowth1PayoffInline
    , inlineShadowRejectGrowth1PayoffPrecomputedRef = left.inlineShadowRejectGrowth1PayoffPrecomputedRef + right.inlineShadowRejectGrowth1PayoffPrecomputedRef
    , inlineShadowRejectGrowth1FinalShrinks = left.inlineShadowRejectGrowth1FinalShrinks + right.inlineShadowRejectGrowth1FinalShrinks
    , inlineShadowRejectGrowth1FinalNonApplication = left.inlineShadowRejectGrowth1FinalNonApplication + right.inlineShadowRejectGrowth1FinalNonApplication
    , inlineShadowRejectGrowth1FinalDirectRootWin = left.inlineShadowRejectGrowth1FinalDirectRootWin + right.inlineShadowRejectGrowth1FinalDirectRootWin
    , inlineShadowRejectGrowth1FinalConstructorApplication = left.inlineShadowRejectGrowth1FinalConstructorApplication + right.inlineShadowRejectGrowth1FinalConstructorApplication
    , inlineShadowRejectGrowth1NoPayoffNoDirectBenefit = left.inlineShadowRejectGrowth1NoPayoffNoDirectBenefit + right.inlineShadowRejectGrowth1NoPayoffNoDirectBenefit
    , listFusionChanges = left.listFusionChanges + right.listFusionChanges
    , listFusionPipelineNormalizations = left.listFusionPipelineNormalizations + right.listFusionPipelineNormalizations
    , listFusionHeadFlattenRewrites = left.listFusionHeadFlattenRewrites + right.listFusionHeadFlattenRewrites
    , listFusionRuleRewrites = left.listFusionRuleRewrites + right.listFusionRuleRewrites
    , precomputedRefSubstitutions = left.precomputedRefSubstitutions + right.precomputedRefSubstitutions
    , constantFolds = left.constantFolds + right.constantFolds
    , rejectSamples = mergeRejectSamples left.rejectSamples right.rejectSamples
    }


rejectSampleLimit : Int
rejectSampleLimit =
    16


collectRejectSamples : Bool
collectRejectSamples =
    False


largeBodyRejectSampleWindow : Int
largeBodyRejectSampleWindow =
    30


shouldCollectLargeBodyRejectSample : Int -> Int -> Bool
shouldCollectLargeBodyRejectSample inlineThreshold bodySize =
    bodySize >= inlineThreshold
        && bodySize < inlineThreshold + largeBodyRejectSampleWindow


mergeRejectSamples : List String -> List String -> List String
mergeRejectSamples left right =
    List.foldl addRejectSample left right


addRejectSample : String -> List String -> List String
addRejectSample sample samples =
    if List.member sample samples || List.length samples >= rejectSampleLimit then
        samples

    else
        samples ++ [ sample ]


{-| Parse all sources and build an environment from them.
This is the expensive phase (parse + fold through buildModuleEnv).
The result can be reused across multiple `evalWithEnv` calls.
-}
buildProjectEnv : List String -> Result Error ProjectEnv
buildProjectEnv sources =
    parseProjectSources sources
        |> Result.andThen buildProjectEnvFromParsed


buildCachedModuleSummariesFromParsed :
    List
        { file : File
        , moduleName : ModuleName
        , interface : List Exposed
        }
    -> List CachedModuleSummary
buildCachedModuleSummariesFromParsed parsedModules =
    let
        userInterfaces : ElmDict.Dict ModuleName (List Exposed)
        userInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces : ElmDict.Dict ModuleName (List Exposed)
        allInterfaces =
            ElmDict.union userInterfaces Core.dependency.interfaces
    in
    parsedModules
        |> List.map (cachedSummaryFromParsedModule allInterfaces)


{-| Phase 1: Parse all source strings into files with module names and interfaces.
-}
parseProjectSources :
    List String
    ->
        Result
            Error
            (List
                { file : File
                , moduleName : ModuleName
                , interface : List Exposed
                }
            )
parseProjectSources sources =
    sources
        |> List.map
            (\source ->
                source
                    |> Elm.Parser.parseToFile
                    |> Result.mapError ParsingError
                    |> Result.andThen
                        (\file ->
                            let
                                modName : ModuleName
                                modName =
                                    fileModuleName file
                            in
                            Ok
                                { file = file
                                , moduleName = modName
                                , interface = buildInterfaceFromFile file
                                }
                        )
            )
        |> combineResults


{-| Phase 2: Build the project environment from already-parsed modules.
-}
buildProjectEnvFromParsed :
    List
        { file : File
        , moduleName : ModuleName
        , interface : List Exposed
        }
    -> Result Error ProjectEnv
buildProjectEnvFromParsed parsedModules =
    buildProjectEnvFromSummaries (buildCachedModuleSummariesFromParsed parsedModules)


buildProjectEnvFromSummaries : List CachedModuleSummary -> Result Error ProjectEnv
buildProjectEnvFromSummaries summaries =
    let
        userInterfaces : ElmDict.Dict ModuleName (List Exposed)
        userInterfaces =
            summaries
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces : ElmDict.Dict ModuleName (List Exposed)
        allInterfaces =
            ElmDict.union userInterfaces Core.dependency.interfaces

        sharedFunctions : Dict.Dict String (Dict.Dict String FunctionImplementation)
        sharedFunctions =
            summaries
                |> List.foldl
                    (\summary acc ->
                        Dict.insert
                            (Environment.moduleKey summary.moduleName)
                            (summary.functions
                                |> List.map (\implementation -> ( Node.value implementation.name, implementation ))
                                |> Dict.fromList
                            )
                            acc
                    )
                    coreFunctions

        -- Pre-populate core module imports with the default import set
        -- (see the note in `buildInitialEnv`). Without this, a caller's
        -- alias like `import Array.Extra as Array` would leak into core
        -- module bodies via the cross-module-call import fallback and
        -- rewrite qualified `Array_elm_builtin` references.
        defaultProcessedImports : ImportedNames
        defaultProcessedImports =
            defaultImports
                |> List.foldl (processImport Core.dependency.interfaces) emptyImports

        coreModuleImports : Dict.Dict String ImportedNames
        coreModuleImports =
            coreFunctions
                |> Dict.foldl (\moduleKey _ acc -> Dict.insert moduleKey defaultProcessedImports acc) Dict.empty

        sharedModuleImports : Dict.Dict String ImportedNames
        sharedModuleImports =
            summaries
                |> List.foldl
                    (\summary acc ->
                        Dict.insert
                            (Environment.moduleKey summary.moduleName)
                            summary.importedNames
                            acc
                    )
                    coreModuleImports

        env : Env
        env =
            { currentModule = []
            , currentModuleKey = ""
            , callStack = []
            , shared =
                { functions = sharedFunctions
                , moduleImports = sharedModuleImports
                , resolveBridge = Types.noResolveBridge
                , precomputedValues = Dict.empty
                , tcoAnalyses = precomputeTcoAnalyses sharedFunctions
                }
            , currentModuleFunctions = Dict.empty
            , letFunctions = Dict.empty
            , values = Dict.empty
            , imports = emptyImports
            , callDepth = 0
            , recursionCheck = Nothing
            }
    in
    Ok
        (refreshResolvedGlobals
            (ProjectEnv
                { env = env
                , allInterfaces = allInterfaces
                , resolved = resolveProject summaries
                }
            )
        )


{-| Run the top-level-constant normalization pass on a list of summaries.

This is separated from `buildProjectEnvFromSummaries` so the caller can cache
the normalized output: elm-build's package summary cache stores the result
of `normalizeSummaries` to disk, so subsequent project loads skip this step
entirely (cache hit → no normalization cost, just decoding).

Idempotent: running this twice produces the same result (normalizing an
already-normalized `Array.fromList [...]` body yields the same expression).
Safe to call regardless of cache state.

-}
type alias DependencySummaryOptimizationResult =
    { functions : Dict.Dict String (Dict.Dict String FunctionImplementation)
    , stats : DependencySummaryStats
    }


normalizeSummariesWithStats :
    List CachedModuleSummary
    -> { summaries : List CachedModuleSummary, stats : DependencySummaryStats }
normalizeSummariesWithStats summaries =
    let
        sharedFunctionsBefore : Dict.Dict String (Dict.Dict String FunctionImplementation)
        sharedFunctionsBefore =
            summaries
                |> List.foldl
                    (\summary acc ->
                        Dict.insert
                            (Environment.moduleKey summary.moduleName)
                            (summary.functions
                                |> List.map (\implementation -> ( Node.value implementation.name, implementation ))
                                |> Dict.fromList
                            )
                            acc
                    )
                    coreFunctions

        sharedModuleImports : Dict.Dict String ImportedNames
        sharedModuleImports =
            summaries
                |> List.map
                    (\summary ->
                        ( Environment.moduleKey summary.moduleName
                        , summary.importedNames
                        )
                    )
                |> Dict.fromList

        ( normalizedFunctions, normalizedPrecomputedValues ) =
            normalizeTopLevelConstants NormalizationFlags.packageAggressive summaries sharedFunctionsBefore sharedModuleImports

        optimizationResult : DependencySummaryOptimizationResult
        optimizationResult =
            optimizeDependencySummaryBodies
                summaries
                sharedModuleImports
                normalizedFunctions
                normalizedPrecomputedValues
    in
    { summaries =
        summaries
            |> List.map
                (\summary ->
                    let
                        moduleKey : String
                        moduleKey =
                            Environment.moduleKey summary.moduleName

                        updatedFns : Dict.Dict String FunctionImplementation
                        updatedFns =
                            Dict.get moduleKey optimizationResult.functions
                                |> Maybe.withDefault Dict.empty
                    in
                    { summary
                        | functions =
                            summary.functions
                                |> List.map
                                    (\f ->
                                        Dict.get (Node.value f.name) updatedFns
                                            |> Maybe.withDefault f
                                    )
                    }
                )
    , stats = optimizationResult.stats
    }


normalizeSummaries : List CachedModuleSummary -> List CachedModuleSummary
normalizeSummaries summaries =
    (normalizeSummariesWithStats summaries).summaries


optimizeDependencySummaryBodies :
    List CachedModuleSummary
    -> Dict.Dict String ImportedNames
    -> Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String (Dict.Dict String Value)
    -> DependencySummaryOptimizationResult
optimizeDependencySummaryBodies summaries sharedModuleImports normalizedFunctions normalizedPrecomputedValues =
    let
        cfg : Types.Config
        cfg =
            { trace = False
            , coverage = False
            , coverageProbeLines = Set.empty
            , maxSteps = Nothing
            , tcoTarget = Nothing
            , callCounts = Nothing
            , intercepts = Dict.empty
            , memoizedFunctions = MemoSpec.emptyRegistry
            , collectMemoStats = False
            , useResolvedIR = False
            }

        flags : NormalizationFlags.NormalizationFlags
        flags =
            NormalizationFlags.packageAggressive
    in
    summaries
        |> List.foldl
            (\summary acc ->
                let
                    moduleKey : String
                    moduleKey =
                        Environment.moduleKey summary.moduleName

                    moduleImports : ImportedNames
                    moduleImports =
                        Dict.get moduleKey sharedModuleImports
                            |> Maybe.withDefault emptyImports

                    currentModuleFunctions : Dict.Dict String FunctionImplementation
                    currentModuleFunctions =
                        Dict.get moduleKey normalizedFunctions
                            |> Maybe.withDefault Dict.empty

                    env : Env
                    env =
                        { currentModule = summary.moduleName
                        , currentModuleKey = moduleKey
                        , callStack = []
                        , shared =
                            { functions = normalizedFunctions
                            , moduleImports = sharedModuleImports
                            , resolveBridge = Types.noResolveBridge
                            , precomputedValues = normalizedPrecomputedValues
                            , tcoAnalyses = Dict.empty
                            }
                        , currentModuleFunctions = currentModuleFunctions
                        , letFunctions = Dict.empty
                        , values = Dict.empty
                        , imports = moduleImports
                        , callDepth = 0
                        , recursionCheck = Nothing
                        }

                    optimizedModuleFunctions : { functions : Dict.Dict String FunctionImplementation, stats : DependencySummaryStats }
                    optimizedModuleFunctions =
                        currentModuleFunctions
                            |> Dict.foldl
                                (\name funcImpl moduleAcc ->
                                    if List.isEmpty funcImpl.arguments then
                                        { moduleAcc | functions = Dict.insert name funcImpl moduleAcc.functions }

                                    else
                                        let
                                            rewriteOutcome : DependencyRewriteResult
                                            rewriteOutcome =
                                                foldWithFlagsAndStats flags env cfg funcImpl.expression

                                            fusionOutcome : ListFusion.FuseResult
                                            fusionOutcome =
                                                if flags.runListFusion then
                                                    ListFusion.canonicalizeWithStats rewriteOutcome.expression

                                                else
                                                    { expression = rewriteOutcome.expression
                                                    , stats = ListFusion.emptyFuseStats
                                                    }

                                            normalizedExpression : Node Expression
                                            normalizedExpression =
                                                fusionOutcome.expression

                                            listFusionStats : DependencySummaryStats
                                            listFusionStats =
                                                { emptyDependencySummaryStats
                                                    | listFusionChanges =
                                                        if normalizedExpression /= rewriteOutcome.expression then
                                                            1

                                                        else
                                                            0
                                                    , listFusionPipelineNormalizations = fusionOutcome.stats.pipelineNormalizations
                                                    , listFusionHeadFlattenRewrites = fusionOutcome.stats.headFlattenRewrites
                                                    , listFusionRuleRewrites = fusionOutcome.stats.ruleRewrites
                                                }

                                            functionStats : DependencySummaryStats
                                            functionStats =
                                                { emptyDependencySummaryStats
                                                    | functionsVisited = 1
                                                    , functionsRewritten =
                                                        if normalizedExpression /= funcImpl.expression then
                                                            1

                                                        else
                                                            0
                                                }
                                        in
                                        { functions =
                                            Dict.insert
                                                name
                                                { funcImpl | expression = normalizedExpression }
                                                moduleAcc.functions
                                        , stats =
                                            mergeDependencySummaryStats
                                                moduleAcc.stats
                                                (mergeDependencySummaryStats
                                                    functionStats
                                                    (mergeDependencySummaryStats listFusionStats rewriteOutcome.stats)
                                                )
                                        }
                                )
                                { functions = Dict.empty, stats = emptyDependencySummaryStats }
                in
                { functions = Dict.insert moduleKey optimizedModuleFunctions.functions acc.functions
                , stats = mergeDependencySummaryStats acc.stats optimizedModuleFunctions.stats
                }
            )
            { functions = Dict.empty, stats = emptyDependencySummaryStats }


{-| AST normalization pass: eagerly evaluate every zero-arg function whose
result is a pure, round-trippable `Value`, then rewrite the function body via
`Value.toExpression` so subsequent references produce the value in one step.

This fixes a big interpreter cliff: without this pass, top-level constants
are re-evaluated on every reference. `Fuzz.Float.exponentMapping` (a sorted
2048-element `Array Int`) took ~1.4 s to rebuild in the interpreter, and any
function that referenced it paid that cost per call. After normalization, the
same reference becomes `Array.fromList [3071, 3072, ...]` — a flat list
literal wrapped in one kernel call.

We iterate modules in the order they appear in `summaries`. As each function
gets normalized, it lands in the accumulator and subsequent evaluations see
the fast form. Dependencies that come earlier in the iteration effectively
get "cascaded" — the first successful normalization makes downstream evals
cheaper. (A topological sort would let us do this in a single pass with
guaranteed ordering, but fixed-point iteration is simpler and the common case
already works well since summaries come out in a sensible order.)

Functions whose values aren't losslessly representable as expressions (Json,
Regex, Bytes, PartiallyApplied with captured env) are skipped — they stay as
AST. Same for functions that fail to eval (Debug.todo, circular refs, etc).

-}
normalizeTopLevelConstants :
    NormalizationFlags.NormalizationFlags
    -> List CachedModuleSummary
    -> Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String ImportedNames
    ->
        ( Dict.Dict String (Dict.Dict String FunctionImplementation)
        , Dict.Dict String (Dict.Dict String Value)
        )
normalizeTopLevelConstants flags summaries initialFunctions sharedImports =
    List.foldl
        (\summary ( currentFunctions, currentPrecomputed ) ->
            let
                moduleKey : String
                moduleKey =
                    Environment.moduleKey summary.moduleName

                originalModuleFns : Dict.Dict String FunctionImplementation
                originalModuleFns =
                    Dict.get moduleKey currentFunctions
                        |> Maybe.withDefault Dict.empty

                -- Walk the module's functions, attempting normalization for
                -- each zero-arg one. We thread `currentFunctions` AND
                -- `currentPrecomputed` through so each newly-normalized
                -- function is visible to later siblings — both as a
                -- rewritten AST and as a precomputed `Value` cache hit.
                ( normalizedModuleFns, normalizedPrecomputedFns ) =
                    List.foldl
                        (\funcImpl ( fnAcc, precomputedAcc ) ->
                            let
                                name : String
                                name =
                                    Node.value funcImpl.name
                            in
                            if List.isEmpty funcImpl.arguments && isNormalizationCandidate funcImpl.expression then
                                case
                                    tryNormalizeConstant
                                        flags
                                        summary.moduleName
                                        moduleKey
                                        summary.importedNames
                                        funcImpl
                                        (Dict.insert moduleKey fnAcc currentFunctions)
                                        sharedImports
                                        (Dict.insert moduleKey precomputedAcc currentPrecomputed)
                                of
                                    Just ( normalized, value ) ->
                                        ( Dict.insert name normalized fnAcc
                                        , Dict.insert name value precomputedAcc
                                        )

                                    Nothing ->
                                        ( Dict.insert name funcImpl fnAcc, precomputedAcc )

                            else
                                ( Dict.insert name funcImpl fnAcc, precomputedAcc )
                        )
                        ( originalModuleFns
                        , Dict.get moduleKey currentPrecomputed |> Maybe.withDefault Dict.empty
                        )
                        summary.functions
            in
            ( Dict.insert moduleKey normalizedModuleFns currentFunctions
            , Dict.insert moduleKey normalizedPrecomputedFns currentPrecomputed
            )
        )
        ( initialFunctions, Dict.empty )
        summaries


{-| Try to normalize a single zero-arg function. Returns `Just (funcImpl,
value)` if eval succeeded and the result is losslessly round-trippable;
`Nothing` if we should leave the AST alone. Callers persist both the
rewritten expression (so the AST path is fast) and the `Value` (so the
runtime precomputed cache hits for module-level references).
-}
tryNormalizeConstant :
    NormalizationFlags.NormalizationFlags
    -> ModuleName
    -> String
    -> ImportedNames
    -> FunctionImplementation
    -> Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String ImportedNames
    -> Dict.Dict String (Dict.Dict String Value)
    -> Maybe ( FunctionImplementation, Value )
tryNormalizeConstant flags moduleName moduleKey moduleImports funcImpl sharedFunctions sharedModuleImports sharedPrecomputedValues =
    let
        env : Env
        env =
            { currentModule = moduleName
            , currentModuleKey = moduleKey
            , callStack = []
            , shared =
                { functions = sharedFunctions
                , moduleImports = sharedModuleImports
                , resolveBridge = Types.noResolveBridge
                , precomputedValues = sharedPrecomputedValues
                , tcoAnalyses = Dict.empty
                }
            , currentModuleFunctions =
                Dict.get moduleKey sharedFunctions
                    |> Maybe.withDefault Dict.empty
            , letFunctions = Dict.empty
            , values = Dict.empty
            , imports = moduleImports
            , callDepth = 0
            , recursionCheck = Nothing
            }

        cfg : Types.Config
        cfg =
            { trace = False
            , coverage = False
            , coverageProbeLines = Set.empty
            , maxSteps = Just flags.tryNormalizeMaxSteps
            , tcoTarget = Nothing
            , callCounts = Nothing
            , intercepts = Dict.empty
            , memoizedFunctions = MemoSpec.emptyRegistry
            , collectMemoStats = False
            , useResolvedIR = False
            }

        result : EvalResult Value
        result =
            Eval.Expression.evalExpression funcImpl.expression cfg env
    in
    case EvalResult.toResult result of
        Ok value ->
            if isLosslessValue value then
                Just
                    ( { funcImpl
                        | expression = Value.toExpression value
                      }
                    , value
                    )

            else
                -- Re-reverting `07d382c`. The theory was that upstream
                -- `1a71da1` (topological-order single-pass normalizer)
                -- would neutralize the +17.7% cold regression that
                -- caused `d9c39de` to revert the original `5d861fa`
                -- non-lossless caching. Empirically on small-12 n=5
                -- with the topological normalizer in place, keeping
                -- `Just (funcImpl, value)` here costs +13.7% cold
                -- (280 → 319 on legacy-ast, 251 → 272 on resolved IR).
                -- A single-run bench comparing adjacent commits dropped
                -- cold from 318 → 278 just by flipping this one line —
                -- so the fixpoint loop wasn't the dominant factor after
                -- all. The real cost is elsewhere (likely per-pass
                -- dict-merge work or later cache-lookup hot-path
                -- interactions).
                --
                -- Consequence: `PerfTests.aliasedPrecomputedLookupTests`
                -- (added in upstream `8c2f76e`) is skipped on this
                -- branch via `Test.skip` with an explicit note. See
                -- that test's skip comment for context.
                Nothing

        Err _ ->
            Nothing


{-| Run `tryNormalizeConstant` over every 0-arg constant in a module,
iterating until no further progress is made. A single pass is alphabetical
(Dict iteration order), so a constant whose dependencies appear later in
the alphabet — e.g. `Diacritics.lookupArray` depends on
`Diacritics.lookupTable` — would be tried before its dependencies are
cached and get stuck at the slow/expensive eval path. Re-running the pass
lets the second attempt hit the now-populated `precomputedValues`.

Returns the fully-normalized function dict, the delta (only the
functions whose bodies changed), the per-name precomputed `Value`
dict, and the rewrite stats emitted while normalizing the module. The
function / delta / precomputed maps are keyed on the simple function
name.

-}
runModuleNormalizationToFixpoint :
    NormalizationFlags.NormalizationFlags
    -> Maybe (Set String)
    -> ModuleName
    -> String
    -> ImportedNames
    -> Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String ImportedNames
    -> Dict.Dict String (Dict.Dict String Value)
    -> Dict.Dict String FunctionImplementation
    -> Dict.Dict String Value
    ->
        { fns : Dict.Dict String FunctionImplementation
        , delta : Dict.Dict String FunctionImplementation
        , precomputed : Dict.Dict String Value
        , stats : DependencySummaryStats
        }
runModuleNormalizationToFixpoint flags normalizationTargets moduleName moduleKey moduleImports sharedFunctions sharedImports sharedPrecomputedValues originalModuleFns originalPrecomputedFns =
    let
        isSelectedTarget : String -> Bool
        isSelectedTarget name =
            case normalizationTargets of
                Nothing ->
                    True

                Just selectedNames ->
                    Set.member name selectedNames

        step :
            Dict.Dict String FunctionImplementation
            -> Dict.Dict String FunctionImplementation
            -> Dict.Dict String Value
            ->
                { fns : Dict.Dict String FunctionImplementation
                , delta : Dict.Dict String FunctionImplementation
                , precomputed : Dict.Dict String Value
                , progress : Int
                }
        step currentFns currentDelta currentPrecomputed =
            originalModuleFns
                |> Dict.foldl
                    (\name funcImpl acc ->
                        let
                            -- Start from whatever the previous pass
                            -- produced for this name: either the normalized
                            -- (rewritten) AST or the unchanged original.
                            priorFn : FunctionImplementation
                            priorFn =
                                Dict.get name currentFns
                                    |> Maybe.withDefault funcImpl
                        in
                        if Dict.member name acc.precomputed then
                            -- Already normalized in a previous pass. Keep the
                            -- rewritten body from the incoming `currentFns`.
                            { acc | fns = Dict.insert name priorFn acc.fns }

                        else if isSelectedTarget name && List.isEmpty funcImpl.arguments && isNormalizationCandidate funcImpl.expression then
                                case
                                    tryNormalizeConstant
                                        flags
                                        moduleName
                                        moduleKey
                                        moduleImports
                                    priorFn
                                    (Dict.insert moduleKey acc.fns sharedFunctions)
                                    sharedImports
                                    (Dict.insert moduleKey acc.precomputed sharedPrecomputedValues)
                            of
                                Just ( normalized, value ) ->
                                    { fns = Dict.insert name normalized acc.fns
                                    , delta = Dict.insert name normalized acc.delta
                                    , precomputed = Dict.insert name value acc.precomputed
                                    , progress = acc.progress + 1
                                    }

                                Nothing ->
                                    { acc | fns = Dict.insert name priorFn acc.fns }

                        else
                            { acc | fns = Dict.insert name priorFn acc.fns }
                    )
                    { fns = Dict.empty
                    , delta = currentDelta
                    , precomputed = currentPrecomputed
                    , progress = 0
                    }

        loop : Int -> Dict.Dict String FunctionImplementation -> Dict.Dict String FunctionImplementation -> Dict.Dict String Value -> ( Dict.Dict String FunctionImplementation, Dict.Dict String FunctionImplementation, Dict.Dict String Value )
        loop remaining fns delta precomputed =
            if remaining <= 0 then
                ( fns, delta, precomputed )

            else
                let
                    result =
                        step fns delta precomputed
                in
                if result.progress == 0 then
                    ( result.fns, result.delta, result.precomputed )

                else
                    loop (remaining - 1) result.fns result.delta result.precomputed
    in
    -- When `runFixpoint` is on we now prefer `normalizeInDepOrder`
    -- (topological DFS, processes each constant's intra-module deps
    -- before itself, single pass, no wasted eager evals). The legacy
    -- `loop`-based fixpoint is kept as a fallback for
    -- `fixpointPasses > 1` in case we need to re-enable it for a
    -- future experiment, but all A/B runs today use topo order.
    let
        ( fixpointFns, fixpointDelta, fixpointPrecomputed ) =
            if flags.runFixpoint then
                normalizeInDepOrder
                    flags
                    normalizationTargets
                    moduleName
                    moduleKey
                    moduleImports
                    sharedFunctions
                    sharedImports
                    sharedPrecomputedValues
                    originalModuleFns
                    originalPrecomputedFns

            else
                ( originalModuleFns, Dict.empty, originalPrecomputedFns )

        -- Run `ListFusion.fuse` on every multi-arg function body. This is the
        -- pass that flattens nested Application heads (ZAM task B's
        -- spine-collection) and folds `List.map (map g xs)` chains. It walks
        -- the AST once per function regardless of whether a fusion
        -- opportunity is found — so on modules with no List.map chains, this
        -- is mostly overhead plus the spine-collection rewrite.
        foldedResult :
            { fns : Dict.Dict String FunctionImplementation
            , stats : DependencySummaryStats
            }
        foldedResult =
            if flags.runListFusion then
                fixpointFns
                    |> Dict.foldl
                        (\name funcImpl acc ->
                            if isSelectedTarget name && not (List.isEmpty funcImpl.arguments) then
                                let
                                    fusionOutcome : ListFusion.FuseResult
                                    fusionOutcome =
                                        ListFusion.canonicalizeWithStats funcImpl.expression

                                    normalizedExpression : Node Expression
                                    normalizedExpression =
                                        fusionOutcome.expression

                                    listFusionStats : DependencySummaryStats
                                    listFusionStats =
                                        { emptyDependencySummaryStats
                                            | listFusionChanges =
                                                countBool (normalizedExpression /= funcImpl.expression)
                                            , listFusionPipelineNormalizations = fusionOutcome.stats.pipelineNormalizations
                                            , listFusionHeadFlattenRewrites = fusionOutcome.stats.headFlattenRewrites
                                            , listFusionRuleRewrites = fusionOutcome.stats.ruleRewrites
                                        }

                                    functionStats : DependencySummaryStats
                                    functionStats =
                                        { emptyDependencySummaryStats
                                            | functionsVisited = 1
                                            , functionsRewritten =
                                                countBool (normalizedExpression /= funcImpl.expression)
                                        }
                                in
                                { fns =
                                    Dict.insert
                                        name
                                        { funcImpl | expression = normalizedExpression }
                                        acc.fns
                                , stats =
                                    mergeDependencySummaryStats
                                        acc.stats
                                        (mergeDependencySummaryStats functionStats listFusionStats)
                                }

                            else
                                { acc | fns = Dict.insert name funcImpl acc.fns }
                        )
                        { fns = Dict.empty, stats = emptyDependencySummaryStats }

            else
                { fns = fixpointFns, stats = emptyDependencySummaryStats }

        foldedFns : Dict.Dict String FunctionImplementation
        foldedFns =
            foldedResult.fns

        foldedDelta : Dict.Dict String FunctionImplementation
        foldedDelta =
            fixpointDelta
                |> Dict.map
                    (\name funcImpl ->
                        Dict.get name foldedFns
                            |> Maybe.withDefault funcImpl
                    )

        newDelta : Dict.Dict String FunctionImplementation
        newDelta =
            foldedFns
                |> Dict.foldl
                    (\name foldedImpl acc ->
                        if isSelectedTarget name then
                            case Dict.get name originalModuleFns of
                                Just origImpl ->
                                    if foldedImpl.expression /= origImpl.expression then
                                        Dict.insert name foldedImpl acc

                                    else
                                        acc

                                Nothing ->
                                    acc

                        else
                            acc
                    )
                    foldedDelta
    in
    { fns = foldedFns
    , delta = newDelta
    , precomputed = fixpointPrecomputed
    , stats = foldedResult.stats
    }


{-| Topological-order single-pass normalization. For each 0-arg
candidate in the module, process its intra-module dependencies first
(recursively via DFS), then try `tryNormalizeConstant` on it. By the
time we eval `lookupArray`, its `lookupTable` / `maxCode` deps are
already in `precomputed`, so the eval sees them in O(1) instead of
walking the full dependency chain on every iteration.

Single pass, no wasted work. Replaces the earlier "loop 3 times and
hope alphabetical order pays off" fixpoint, which was costing ~1.3 s
on the core-extra 8-file subset because pass 2 had to re-run every
expensive eval that pass 1 bailed on.

Cycle detection via an `onPath` set — if a candidate references
itself (directly or transitively), we bail on that chain rather than
recurse forever. Elm's semantics forbid mutual recursion between
zero-arg values so this should never trigger in well-formed source,
but the guard keeps the pass safe on malformed input.

-}
normalizeInDepOrder :
    NormalizationFlags.NormalizationFlags
    -> Maybe (Set String)
    -> ModuleName
    -> String
    -> ImportedNames
    -> Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String ImportedNames
    -> Dict.Dict String (Dict.Dict String Value)
    -> Dict.Dict String FunctionImplementation
    -> Dict.Dict String Value
    ->
        ( Dict.Dict String FunctionImplementation
        , Dict.Dict String FunctionImplementation
        , Dict.Dict String Value
        )
normalizeInDepOrder flags normalizationTargets moduleName moduleKey moduleImports sharedFunctions sharedImports sharedPrecomputedValues originalModuleFns originalPrecomputedFns =
    let
        isSelectedTarget : String -> Bool
        isSelectedTarget name =
            case normalizationTargets of
                Nothing ->
                    True

                Just selectedNames ->
                    Set.member name selectedNames

        candidateNames : Set String
        candidateNames =
            originalModuleFns
                |> Dict.foldl
                    (\name funcImpl acc ->
                        if isSelectedTarget name && List.isEmpty funcImpl.arguments && isNormalizationCandidate funcImpl.expression then
                            Set.insert name acc

                        else
                            acc
                    )
                    Set.empty

        -- For each candidate, collect the subset of its body's free
        -- variables that reference other candidates in this module.
        depGraph : Dict.Dict String (Set String)
        depGraph =
            candidateNames
                |> Set.foldl
                    (\name acc ->
                        case Dict.get name originalModuleFns of
                            Just funcImpl ->
                                let
                                    refs : Set String
                                    refs =
                                        Eval.Expression.freeVariables funcImpl.expression

                                    deps : Set String
                                    deps =
                                        Set.intersect refs candidateNames
                                            |> Set.remove name
                                in
                                Dict.insert name deps acc

                            Nothing ->
                                acc
                    )
                    Dict.empty

        initialState : NormalizeDepState
        initialState =
            { fns = originalModuleFns
            , delta = Dict.empty
            , precomputed = originalPrecomputedFns
            , visited = Set.empty
            , onPath = Set.empty
            }

        finalState : NormalizeDepState
        finalState =
            candidateNames
                |> Set.foldl
                    (\name state -> processCandidate flags moduleName moduleKey moduleImports sharedFunctions sharedImports sharedPrecomputedValues originalModuleFns depGraph name state)
                    initialState
    in
    ( finalState.fns, finalState.delta, finalState.precomputed )


type alias NormalizeDepState =
    { fns : Dict.Dict String FunctionImplementation
    , delta : Dict.Dict String FunctionImplementation
    , precomputed : Dict.Dict String Value
    , visited : Set String
    , onPath : Set String
    }


processCandidate :
    NormalizationFlags.NormalizationFlags
    -> ModuleName
    -> String
    -> ImportedNames
    -> Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String ImportedNames
    -> Dict.Dict String (Dict.Dict String Value)
    -> Dict.Dict String FunctionImplementation
    -> Dict.Dict String (Set String)
    -> String
    -> NormalizeDepState
    -> NormalizeDepState
processCandidate flags moduleName moduleKey moduleImports sharedFunctions sharedImports sharedPrecomputedValues originalModuleFns depGraph name state =
    if Set.member name state.visited then
        state

    else if Set.member name state.onPath then
        -- Cycle — bail on this node, leave original body unchanged.
        state

    else
        let
            deps : Set String
            deps =
                Dict.get name depGraph
                    |> Maybe.withDefault Set.empty

            withPath : NormalizeDepState
            withPath =
                { state | onPath = Set.insert name state.onPath }

            afterDeps : NormalizeDepState
            afterDeps =
                Set.foldl
                    (\dep s -> processCandidate flags moduleName moduleKey moduleImports sharedFunctions sharedImports sharedPrecomputedValues originalModuleFns depGraph dep s)
                    withPath
                    deps

            markVisited : NormalizeDepState -> NormalizeDepState
            markVisited s =
                { s | visited = Set.insert name s.visited, onPath = Set.remove name s.onPath }
        in
        case Dict.get name afterDeps.fns of
            Nothing ->
                -- Should never happen: `name` came from iterating
                -- originalModuleFns, and afterDeps.fns is initialized
                -- from that same map.
                markVisited afterDeps

            Just priorFn ->
                case
                    tryNormalizeConstant
                        flags
                        moduleName
                        moduleKey
                        moduleImports
                        priorFn
                        (Dict.insert moduleKey afterDeps.fns sharedFunctions)
                        sharedImports
                        (Dict.insert moduleKey afterDeps.precomputed sharedPrecomputedValues)
                of
                    Just ( normalized, value ) ->
                        { fns = Dict.insert name normalized afterDeps.fns
                        , delta = Dict.insert name normalized afterDeps.delta
                        , precomputed = Dict.insert name value afterDeps.precomputed
                        , visited = Set.insert name afterDeps.visited
                        , onPath = Set.remove name afterDeps.onPath
                        }

                    Nothing ->
                        markVisited afterDeps


{-| Determine whether a `Value` can be losslessly round-tripped through
`Value.toExpression`. Opaque kernel types (Json, Regex, Bytes) and closures
over captured env can't be represented as an Elm expression, so we leave
those constants in their original AST form.
-}
isLosslessValue : Value -> Bool
isLosslessValue value =
    case value of
        Types.String _ ->
            True

        Types.Int i ->
            -- `round (1 / 0)` and friends produce an `Int Infinity` at the
            -- JS level; the AstWireCodec's Wire3 integer encoder can't
            -- round-trip non-finite values, so skip normalization and keep
            -- the original expression (e.g., `round (1 / 0)` stays as-is).
            let
                asFloat : Float
                asFloat =
                    toFloat i
            in
            not (isInfinite asFloat) && not (isNaN asFloat)

        Types.Float f ->
            -- Same concern: `Value.toExpression (Float Infinity)` encodes as
            -- `Floatable Infinity`, which Wire3 can't serialize cleanly.
            not (isInfinite f) && not (isNaN f)

        Types.Char _ ->
            True

        Types.Bool _ ->
            True

        Types.Unit ->
            True

        Types.Tuple l r ->
            isLosslessValue l && isLosslessValue r

        Types.Triple l m r ->
            isLosslessValue l && isLosslessValue m && isLosslessValue r

        Types.Record fields ->
            Dict.values fields |> List.all isLosslessValue

        Types.List items ->
            List.all isLosslessValue items

        Types.Custom _ args ->
            List.all isLosslessValue args

        Types.JsArray array ->
            Array.toList array |> List.all isLosslessValue

        Types.JsonValue _ ->
            False

        Types.JsonDecoderValue _ ->
            False

        Types.RegexValue _ ->
            False

        Types.BytesValue _ ->
            False

        Types.PartiallyApplied _ _ _ _ _ _ ->
            -- Function-typed constants are risky to round-trip: Value.toExpression
            -- can turn a top-level `myFn = other.fn` alias into a
            -- FunctionOrValue reference, but any PA that captures env values
            -- or uses a KernelImpl with runtime state would lose information.
            -- Skip for now; revisit if the pattern is common.
            False


type alias DependencyRewriteResult =
    { expression : Node Expression
    , stats : DependencySummaryStats
    }


type InlineBodyShape
    = InlineLeaf
    | InlineConstructor
    | InlineOperator
    | InlineRecordAccess
    | InlineCollection
    | InlineOther


type InlinePayoffKind
    = InlinePayoffChangedKind
    | InlinePayoffInlineKind
    | InlinePayoffConstantFoldKind
    | InlinePayoffPrecomputedRefKind


type ShadowInlineFilter
    = ShadowRejectCollection
    | ShadowRejectGrowth0
    | ShadowRejectGrowth1


type alias InlineRewrite =
    { expression : Node Expression
    , bodySize : Int
    , bodyShape : InlineBodyShape
    }


type InlineRejectReason
    = InlineRejectPattern
    | InlineRejectArity
    | InlineRejectSelfCall
    | InlineRejectBodyTooLarge
    | InlineRejectUnsafe InlineUnsafeRoot
    | InlineRejectInternalHelper


type InlineDecision
    = InlineApplied InlineRewrite
    | InlineRejected
        { reason : InlineRejectReason
        , sample : Maybe String
        }
    | InlineNotApplicable


type InlineUnsafeRoot
    = InlineUnsafeApplication
    | InlineUnsafeIf
    | InlineUnsafeCase
    | InlineUnsafeLet
    | InlineUnsafeLambda
    | InlineUnsafeOther


rewriteResult : Node Expression -> DependencyRewriteResult
rewriteResult expression =
    { expression = expression
    , stats = emptyDependencySummaryStats
    }


combineRewriteResults : List DependencyRewriteResult -> { expressions : List (Node Expression), stats : DependencySummaryStats }
combineRewriteResults results =
    { expressions = List.map .expression results
    , stats =
        results
            |> List.foldl
                (\result acc -> mergeDependencySummaryStats acc result.stats)
                emptyDependencySummaryStats
    }


inlineRewriteStats : InlineRewrite -> DependencySummaryStats
inlineRewriteStats inlineRewrite =
    let
        baseStats =
            { emptyDependencySummaryStats
                | inlineCandidates = 1
                , inlineSuccesses = 1
                , inlineBodyLt30 =
                    if inlineRewrite.bodySize < 30 then
                        1

                    else
                        0
                , inlineBody30To59 =
                    if inlineRewrite.bodySize >= 30 && inlineRewrite.bodySize < 60 then
                        1

                    else
                        0
                , inlineBody60Plus =
                    if inlineRewrite.bodySize >= 60 then
                        1

                        else
                            0
            }
    in
    case inlineRewrite.bodyShape of
        InlineLeaf ->
            { baseStats | inlineShapeLeaf = 1 }

        InlineConstructor ->
            { baseStats | inlineShapeConstructor = 1 }

        InlineOperator ->
            { baseStats | inlineShapeOperator = 1 }

        InlineRecordAccess ->
            { baseStats | inlineShapeRecordAccess = 1 }

        InlineCollection ->
            { baseStats | inlineShapeCollection = 1 }

        InlineOther ->
            { baseStats | inlineShapeOther = 1 }


inlinePayoffTotalStats : InlinePayoffKind -> DependencySummaryStats
inlinePayoffTotalStats payoffKind =
    case payoffKind of
        InlinePayoffChangedKind ->
            { emptyDependencySummaryStats | inlinePayoffChanged = 1 }

        InlinePayoffInlineKind ->
            { emptyDependencySummaryStats | inlinePayoffInline = 1 }

        InlinePayoffConstantFoldKind ->
            { emptyDependencySummaryStats | inlinePayoffConstantFold = 1 }

        InlinePayoffPrecomputedRefKind ->
            { emptyDependencySummaryStats | inlinePayoffPrecomputedRef = 1 }


inlinePayoffShapeStats : InlinePayoffKind -> InlineBodyShape -> DependencySummaryStats
inlinePayoffShapeStats payoffKind bodyShape =
    case ( payoffKind, bodyShape ) of
        ( InlinePayoffChangedKind, InlineLeaf ) ->
            { emptyDependencySummaryStats | inlinePayoffChangedShapeLeaf = 1 }

        ( InlinePayoffChangedKind, InlineConstructor ) ->
            { emptyDependencySummaryStats | inlinePayoffChangedShapeConstructor = 1 }

        ( InlinePayoffChangedKind, InlineOperator ) ->
            { emptyDependencySummaryStats | inlinePayoffChangedShapeOperator = 1 }

        ( InlinePayoffChangedKind, InlineRecordAccess ) ->
            { emptyDependencySummaryStats | inlinePayoffChangedShapeRecordAccess = 1 }

        ( InlinePayoffChangedKind, InlineCollection ) ->
            { emptyDependencySummaryStats | inlinePayoffChangedShapeCollection = 1 }

        ( InlinePayoffChangedKind, InlineOther ) ->
            { emptyDependencySummaryStats | inlinePayoffChangedShapeOther = 1 }

        ( InlinePayoffInlineKind, InlineLeaf ) ->
            { emptyDependencySummaryStats | inlinePayoffInlineShapeLeaf = 1 }

        ( InlinePayoffInlineKind, InlineConstructor ) ->
            { emptyDependencySummaryStats | inlinePayoffInlineShapeConstructor = 1 }

        ( InlinePayoffInlineKind, InlineOperator ) ->
            { emptyDependencySummaryStats | inlinePayoffInlineShapeOperator = 1 }

        ( InlinePayoffInlineKind, InlineRecordAccess ) ->
            { emptyDependencySummaryStats | inlinePayoffInlineShapeRecordAccess = 1 }

        ( InlinePayoffInlineKind, InlineCollection ) ->
            { emptyDependencySummaryStats | inlinePayoffInlineShapeCollection = 1 }

        ( InlinePayoffInlineKind, InlineOther ) ->
            { emptyDependencySummaryStats | inlinePayoffInlineShapeOther = 1 }

        ( InlinePayoffConstantFoldKind, InlineLeaf ) ->
            { emptyDependencySummaryStats | inlinePayoffConstantFoldShapeLeaf = 1 }

        ( InlinePayoffConstantFoldKind, InlineConstructor ) ->
            { emptyDependencySummaryStats | inlinePayoffConstantFoldShapeConstructor = 1 }

        ( InlinePayoffConstantFoldKind, InlineOperator ) ->
            { emptyDependencySummaryStats | inlinePayoffConstantFoldShapeOperator = 1 }

        ( InlinePayoffConstantFoldKind, InlineRecordAccess ) ->
            { emptyDependencySummaryStats | inlinePayoffConstantFoldShapeRecordAccess = 1 }

        ( InlinePayoffConstantFoldKind, InlineCollection ) ->
            { emptyDependencySummaryStats | inlinePayoffConstantFoldShapeCollection = 1 }

        ( InlinePayoffConstantFoldKind, InlineOther ) ->
            { emptyDependencySummaryStats | inlinePayoffConstantFoldShapeOther = 1 }

        ( InlinePayoffPrecomputedRefKind, InlineLeaf ) ->
            { emptyDependencySummaryStats | inlinePayoffPrecomputedRefShapeLeaf = 1 }

        ( InlinePayoffPrecomputedRefKind, InlineConstructor ) ->
            { emptyDependencySummaryStats | inlinePayoffPrecomputedRefShapeConstructor = 1 }

        ( InlinePayoffPrecomputedRefKind, InlineOperator ) ->
            { emptyDependencySummaryStats | inlinePayoffPrecomputedRefShapeOperator = 1 }

        ( InlinePayoffPrecomputedRefKind, InlineRecordAccess ) ->
            { emptyDependencySummaryStats | inlinePayoffPrecomputedRefShapeRecordAccess = 1 }

        ( InlinePayoffPrecomputedRefKind, InlineCollection ) ->
            { emptyDependencySummaryStats | inlinePayoffPrecomputedRefShapeCollection = 1 }

        ( InlinePayoffPrecomputedRefKind, InlineOther ) ->
            { emptyDependencySummaryStats | inlinePayoffPrecomputedRefShapeOther = 1 }


inlinePayoffBodyStats : InlinePayoffKind -> Int -> DependencySummaryStats
inlinePayoffBodyStats payoffKind bodySize =
    if bodySize < 30 then
        case payoffKind of
            InlinePayoffChangedKind ->
                { emptyDependencySummaryStats | inlinePayoffChangedBodyLt30 = 1 }

            InlinePayoffInlineKind ->
                { emptyDependencySummaryStats | inlinePayoffInlineBodyLt30 = 1 }

            InlinePayoffConstantFoldKind ->
                { emptyDependencySummaryStats | inlinePayoffConstantFoldBodyLt30 = 1 }

            InlinePayoffPrecomputedRefKind ->
                { emptyDependencySummaryStats | inlinePayoffPrecomputedRefBodyLt30 = 1 }

    else if bodySize < 60 then
        case payoffKind of
            InlinePayoffChangedKind ->
                { emptyDependencySummaryStats | inlinePayoffChangedBody30To59 = 1 }

            InlinePayoffInlineKind ->
                { emptyDependencySummaryStats | inlinePayoffInlineBody30To59 = 1 }

            InlinePayoffConstantFoldKind ->
                { emptyDependencySummaryStats | inlinePayoffConstantFoldBody30To59 = 1 }

            InlinePayoffPrecomputedRefKind ->
                { emptyDependencySummaryStats | inlinePayoffPrecomputedRefBody30To59 = 1 }

    else
        case payoffKind of
            InlinePayoffChangedKind ->
                { emptyDependencySummaryStats | inlinePayoffChangedBody60Plus = 1 }

            InlinePayoffInlineKind ->
                { emptyDependencySummaryStats | inlinePayoffInlineBody60Plus = 1 }

            InlinePayoffConstantFoldKind ->
                { emptyDependencySummaryStats | inlinePayoffConstantFoldBody60Plus = 1 }

            InlinePayoffPrecomputedRefKind ->
                { emptyDependencySummaryStats | inlinePayoffPrecomputedRefBody60Plus = 1 }


inlinePayoffKindStats : Bool -> InlinePayoffKind -> InlineRewrite -> DependencySummaryStats
inlinePayoffKindStats didTrigger payoffKind inlineRewrite =
    if didTrigger then
        mergeDependencySummaryStats
            (inlinePayoffTotalStats payoffKind)
            (mergeDependencySummaryStats
                (inlinePayoffShapeStats payoffKind inlineRewrite.bodyShape)
                (inlinePayoffBodyStats payoffKind inlineRewrite.bodySize)
            )

    else
        emptyDependencySummaryStats


inlinePayoffStats : InlineRewrite -> DependencyRewriteResult -> DependencySummaryStats
inlinePayoffStats inlineRewrite recursiveResult =
    mergeDependencySummaryStats
        (inlinePayoffKindStats
            (recursiveResult.expression /= inlineRewrite.expression)
            InlinePayoffChangedKind
            inlineRewrite
        )
        (mergeDependencySummaryStats
            (inlinePayoffKindStats
                (recursiveResult.stats.inlineSuccesses > 0)
                InlinePayoffInlineKind
                inlineRewrite
            )
            (mergeDependencySummaryStats
                (inlinePayoffKindStats
                    (recursiveResult.stats.constantFolds > 0)
                    InlinePayoffConstantFoldKind
                    inlineRewrite
                )
                (inlinePayoffKindStats
                    (recursiveResult.stats.precomputedRefSubstitutions > 0)
                    InlinePayoffPrecomputedRefKind
                    inlineRewrite
                )
            )
        )


type alias InlineShadowFlags =
    { didChanged : Bool
    , didInline : Bool
    , didConstantFold : Bool
    , didPrecomputedRef : Bool
    , finalShrinks : Bool
    , finalNonApplication : Bool
    , finalDirectRootWin : Bool
    , finalConstructorApplication : Bool
    }


countBool : Bool -> Int
countBool value =
    if value then
        1

    else
        0


isApplicationRoot : Node Expression -> Bool
isApplicationRoot (Node _ expr) =
    case expr of
        Application _ ->
            True

        _ ->
            False


isDirectRootWin : Node Expression -> Bool
isDirectRootWin expression =
    classifyInlineBodyShape expression /= InlineOther


isConstructorApplication : Node Expression -> Bool
isConstructorApplication expression =
    classifyInlineBodyShape expression == InlineConstructor


shadowInlineFilterWouldReject : Int -> InlineRewrite -> ShadowInlineFilter -> Bool
shadowInlineFilterWouldReject callSiteSize inlineRewrite shadowFilter =
    let
        inlinedSize =
            expressionSize inlineRewrite.expression
    in
    case shadowFilter of
        ShadowRejectCollection ->
            inlineRewrite.bodyShape == InlineCollection

        ShadowRejectGrowth0 ->
            inlinedSize > callSiteSize

        ShadowRejectGrowth1 ->
            inlinedSize > callSiteSize + 1


shadowInlineFilterStats : ShadowInlineFilter -> InlineShadowFlags -> DependencySummaryStats
shadowInlineFilterStats shadowFilter shadowFlags =
    let
        hasDownstreamPayoff =
            shadowFlags.didChanged
                || shadowFlags.didInline
                || shadowFlags.didConstantFold
                || shadowFlags.didPrecomputedRef

        hasDirectBenefit =
            shadowFlags.finalShrinks
                || shadowFlags.finalDirectRootWin

        noPayoffNoDirectBenefit =
            countBool (not hasDownstreamPayoff && not hasDirectBenefit)
    in
    case shadowFilter of
        ShadowRejectCollection ->
            { emptyDependencySummaryStats
                | inlineShadowRejectCollection = 1
                , inlineShadowRejectCollectionPayoffChanged = countBool shadowFlags.didChanged
                , inlineShadowRejectCollectionPayoffInline = countBool shadowFlags.didInline
                , inlineShadowRejectCollectionPayoffPrecomputedRef = countBool shadowFlags.didPrecomputedRef
                , inlineShadowRejectCollectionFinalShrinks = countBool shadowFlags.finalShrinks
                , inlineShadowRejectCollectionFinalNonApplication = countBool shadowFlags.finalNonApplication
                , inlineShadowRejectCollectionFinalDirectRootWin = countBool shadowFlags.finalDirectRootWin
                , inlineShadowRejectCollectionFinalConstructorApplication = countBool shadowFlags.finalConstructorApplication
                , inlineShadowRejectCollectionNoPayoffNoDirectBenefit = noPayoffNoDirectBenefit
            }

        ShadowRejectGrowth0 ->
            { emptyDependencySummaryStats
                | inlineShadowRejectGrowth0 = 1
                , inlineShadowRejectGrowth0PayoffChanged = countBool shadowFlags.didChanged
                , inlineShadowRejectGrowth0PayoffInline = countBool shadowFlags.didInline
                , inlineShadowRejectGrowth0PayoffPrecomputedRef = countBool shadowFlags.didPrecomputedRef
                , inlineShadowRejectGrowth0FinalShrinks = countBool shadowFlags.finalShrinks
                , inlineShadowRejectGrowth0FinalNonApplication = countBool shadowFlags.finalNonApplication
                , inlineShadowRejectGrowth0FinalDirectRootWin = countBool shadowFlags.finalDirectRootWin
                , inlineShadowRejectGrowth0FinalConstructorApplication = countBool shadowFlags.finalConstructorApplication
                , inlineShadowRejectGrowth0NoPayoffNoDirectBenefit = noPayoffNoDirectBenefit
            }

        ShadowRejectGrowth1 ->
            { emptyDependencySummaryStats
                | inlineShadowRejectGrowth1 = 1
                , inlineShadowRejectGrowth1PayoffChanged = countBool shadowFlags.didChanged
                , inlineShadowRejectGrowth1PayoffInline = countBool shadowFlags.didInline
                , inlineShadowRejectGrowth1PayoffPrecomputedRef = countBool shadowFlags.didPrecomputedRef
                , inlineShadowRejectGrowth1FinalShrinks = countBool shadowFlags.finalShrinks
                , inlineShadowRejectGrowth1FinalNonApplication = countBool shadowFlags.finalNonApplication
                , inlineShadowRejectGrowth1FinalDirectRootWin = countBool shadowFlags.finalDirectRootWin
                , inlineShadowRejectGrowth1FinalConstructorApplication = countBool shadowFlags.finalConstructorApplication
                , inlineShadowRejectGrowth1NoPayoffNoDirectBenefit = noPayoffNoDirectBenefit
            }


shadowInlineStats : Int -> InlineRewrite -> DependencyRewriteResult -> DependencySummaryStats
shadowInlineStats callSiteSize inlineRewrite recursiveResult =
    let
        shadowFlags =
            { didChanged = recursiveResult.expression /= inlineRewrite.expression
            , didInline = recursiveResult.stats.inlineSuccesses > 0
            , didConstantFold = recursiveResult.stats.constantFolds > 0
            , didPrecomputedRef = recursiveResult.stats.precomputedRefSubstitutions > 0
            , finalShrinks = expressionSize recursiveResult.expression < callSiteSize
            , finalNonApplication = not (isApplicationRoot recursiveResult.expression)
            , finalDirectRootWin = isDirectRootWin recursiveResult.expression
            , finalConstructorApplication = isConstructorApplication recursiveResult.expression
            }

        statsFor shadowFilter =
            if shadowInlineFilterWouldReject callSiteSize inlineRewrite shadowFilter then
                shadowInlineFilterStats shadowFilter shadowFlags

            else
                emptyDependencySummaryStats
    in
    mergeDependencySummaryStats
        (statsFor ShadowRejectCollection)
        (mergeDependencySummaryStats
            (statsFor ShadowRejectGrowth0)
            (statsFor ShadowRejectGrowth1)
        )


inlineRejectStats : InlineRejectReason -> Maybe String -> DependencySummaryStats
inlineRejectStats rejectReason maybeSample =
    let
        baseStats =
            { emptyDependencySummaryStats | inlineCandidates = 1 }

        withSample : DependencySummaryStats -> DependencySummaryStats
        withSample stats =
            case maybeSample of
                Just sample ->
                    { stats | rejectSamples = addRejectSample sample stats.rejectSamples }

                Nothing ->
                    stats
    in
    case rejectReason of
        InlineRejectPattern ->
            withSample { baseStats | inlineRejectedPattern = 1 }

        InlineRejectArity ->
            withSample { baseStats | inlineRejectedArity = 1 }

        InlineRejectSelfCall ->
            withSample { baseStats | inlineRejectedSelfCall = 1 }

        InlineRejectBodyTooLarge ->
            withSample { baseStats | inlineRejectedBodyTooLarge = 1 }

        InlineRejectUnsafe unsafeRoot ->
            case unsafeRoot of
                InlineUnsafeApplication ->
                    withSample { baseStats | inlineRejectedUnsafe = 1, inlineRejectedUnsafeApplication = 1 }

                InlineUnsafeIf ->
                    withSample { baseStats | inlineRejectedUnsafe = 1, inlineRejectedUnsafeIf = 1 }

                InlineUnsafeCase ->
                    withSample { baseStats | inlineRejectedUnsafe = 1, inlineRejectedUnsafeCase = 1 }

                InlineUnsafeLet ->
                    withSample { baseStats | inlineRejectedUnsafe = 1, inlineRejectedUnsafeLet = 1 }

                InlineUnsafeLambda ->
                    withSample { baseStats | inlineRejectedUnsafe = 1, inlineRejectedUnsafeLambda = 1 }

                InlineUnsafeOther ->
                    withSample { baseStats | inlineRejectedUnsafe = 1, inlineRejectedUnsafeOther = 1 }

        InlineRejectInternalHelper ->
            withSample { baseStats | inlineRejectedInternalHelper = 1 }


formatRejectSample : InlineRejectReason -> List String -> String -> Int -> InlineBodyShape -> Node Expression -> String
formatRejectSample rejectReason moduleName funcName bodySize bodyShape expression =
    let
        reasonLabel =
            case rejectReason of
                InlineRejectPattern ->
                    "pattern"

                InlineRejectArity ->
                    "arity"

                InlineRejectSelfCall ->
                    "self_call"

                InlineRejectBodyTooLarge ->
                    "body_too_large"

                InlineRejectUnsafe InlineUnsafeApplication ->
                    "unsafe_application"

                InlineRejectUnsafe InlineUnsafeIf ->
                    "unsafe_if"

                InlineRejectUnsafe InlineUnsafeCase ->
                    "unsafe_case"

                InlineRejectUnsafe InlineUnsafeLet ->
                    "unsafe_let"

                InlineRejectUnsafe InlineUnsafeLambda ->
                    "unsafe_lambda"

                InlineRejectUnsafe InlineUnsafeOther ->
                    "unsafe_other"

                InlineRejectInternalHelper ->
                    "internal_helper"

        qualifiedName =
            String.join "." (moduleName ++ [ funcName ])

        bodyShapeLabel =
            inlineBodyShapeToString bodyShape
    in
    reasonLabel
        ++ " | "
        ++ qualifiedName
        ++ " | size="
        ++ String.fromInt bodySize
        ++ " | shape="
        ++ bodyShapeLabel
        ++ " | "
        ++ truncateSamplePreview 180 (Expression.Extra.toString expression)


inlineBodyShapeToString : InlineBodyShape -> String
inlineBodyShapeToString bodyShape =
    case bodyShape of
        InlineLeaf ->
            "leaf"

        InlineConstructor ->
            "constructor"

        InlineOperator ->
            "operator"

        InlineRecordAccess ->
            "record_access"

        InlineCollection ->
            "collection"

        InlineOther ->
            "other"


truncateSamplePreview : Int -> String -> String
truncateSamplePreview maxChars preview =
    if String.length preview <= maxChars then
        preview

    else
        String.left maxChars preview ++ "..."


classifyUnsafeRoot : Node Expression -> InlineUnsafeRoot
classifyUnsafeRoot (Node _ expr) =
    case expr of
        Application _ ->
            InlineUnsafeApplication

        IfBlock _ _ _ ->
            InlineUnsafeIf

        CaseExpression _ ->
            InlineUnsafeCase

        LetExpression _ ->
            InlineUnsafeLet

        LambdaExpression _ ->
            InlineUnsafeLambda

        _ ->
            InlineUnsafeOther


classifyInlineBodyShape : Node Expression -> InlineBodyShape
classifyInlineBodyShape (Node _ expr) =
    case expr of
        Integer _ ->
            InlineLeaf

        Hex _ ->
            InlineLeaf

        Floatable _ ->
            InlineLeaf

        Literal _ ->
            InlineLeaf

        CharLiteral _ ->
            InlineLeaf

        UnitExpr ->
            InlineLeaf

        FunctionOrValue _ _ ->
            InlineLeaf

        Application ((Node _ (FunctionOrValue _ name)) :: _) ->
            if Eval.Expression.isUpperName name then
                InlineConstructor

            else
                InlineOther

        OperatorApplication _ _ _ _ ->
            InlineOperator

        RecordAccess _ _ ->
            InlineRecordAccess

        ListExpr _ ->
            InlineCollection

        TupledExpression _ ->
            InlineCollection

        RecordExpr _ ->
            InlineCollection

        _ ->
            InlineOther


foldWithFlags :
    NormalizationFlags.NormalizationFlags
    -> Env
    -> Config
    -> Node Expression
    -> Node Expression
foldWithFlags flags env cfg node =
    (foldWithFlagsAndStats flags env cfg node).expression


foldWithFlagsAndStats :
    NormalizationFlags.NormalizationFlags
    -> Env
    -> Config
    -> Node Expression
    -> DependencyRewriteResult
foldWithFlagsAndStats flags env cfg node =
    foldConstantSubExpressionsHelp 3 flags env cfg node


foldConstantSubExpressionsHelp :
    Int
    -> NormalizationFlags.NormalizationFlags
    -> Env
    -> Config
    -> Node Expression
    -> DependencyRewriteResult
foldConstantSubExpressionsHelp depth flags env cfg ((Node range expr) as node) =
    if depth <= 0 then
        rewriteResult node

    else
        let
            recurse : Node Expression -> DependencyRewriteResult
            recurse =
                foldConstantSubExpressionsHelp depth flags env cfg

            recurseNext : Node Expression -> DependencyRewriteResult
            recurseNext =
                foldConstantSubExpressionsHelp (depth - 1) flags env cfg

            tryFold : Node Expression -> { expression : Node Expression, didFold : Bool }
            tryFold foldedNode =
                case Eval.Expression.evalExpression foldedNode cfg env |> EvalResult.toResult of
                    Ok value ->
                        if isLosslessValue value then
                            { expression = Value.toExpression value
                            , didFold = True
                            }

                        else
                            { expression = foldedNode
                            , didFold = False
                            }

                    Err _ ->
                        { expression = foldedNode
                        , didFold = False
                        }

            constantFoldStats : DependencySummaryStats
            constantFoldStats =
                { emptyDependencySummaryStats | constantFolds = 1 }

            precomputedRefSubstitutionStats : DependencySummaryStats
            precomputedRefSubstitutionStats =
                { emptyDependencySummaryStats | precomputedRefSubstitutions = 1 }

            isConstantLeaf : Expression -> Bool
            isConstantLeaf e =
                case e of
                    Integer _ ->
                        True

                    Hex _ ->
                        True

                    Floatable _ ->
                        True

                    Literal _ ->
                        True

                    CharLiteral _ ->
                        True

                    UnitExpr ->
                        True

                    FunctionOrValue [] "True" ->
                        True

                    FunctionOrValue [] "False" ->
                        True

                    ListExpr items ->
                        List.all (\(Node _ ie) -> isConstantLeaf ie) items

                    TupledExpression items ->
                        List.all (\(Node _ ie) -> isConstantLeaf ie) items

                    Application ((Node _ (FunctionOrValue _ name)) :: args) ->
                        Eval.Expression.isUpperName name
                            && List.all (\(Node _ ie) -> isConstantLeaf ie) args

                    RecordExpr fields ->
                        List.all (\(Node _ ( _, Node _ ie )) -> isConstantLeaf ie) fields

                    _ ->
                        False
        in
        case expr of
            ListExpr items ->
                let
                    itemResults =
                        List.map recurse items

                    combined =
                        combineRewriteResults itemResults
                in
                { expression = Node range (ListExpr combined.expressions)
                , stats = combined.stats
                }

            TupledExpression items ->
                let
                    itemResults =
                        List.map recurse items

                    combined =
                        combineRewriteResults itemResults
                in
                { expression = Node range (TupledExpression combined.expressions)
                , stats = combined.stats
                }

            RecordExpr fields ->
                let
                    fieldResults =
                        fields
                            |> List.map
                                (\(Node fRange ( name, value )) ->
                                    let
                                        valueResult =
                                            recurse value
                                    in
                                    { field = Node fRange ( name, valueResult.expression )
                                    , stats = valueResult.stats
                                    }
                                )
                in
                { expression = Node range (RecordExpr (List.map .field fieldResults))
                , stats =
                    fieldResults
                        |> List.foldl
                            (\result acc -> mergeDependencySummaryStats acc result.stats)
                            emptyDependencySummaryStats
                }

            Application ((Node _ (FunctionOrValue moduleName funcName)) :: args) ->
                let
                    foldedArgResults =
                        List.map recurse args

                    foldedArgs =
                        List.map .expression foldedArgResults

                    argStats =
                        foldedArgResults
                            |> List.foldl
                                (\result acc -> mergeDependencySummaryStats acc result.stats)
                                emptyDependencySummaryStats

                    foldedNode =
                        Node range (Application (Node range (FunctionOrValue moduleName funcName) :: foldedArgs))
                in
                if
                    flags.foldConstantApplications
                        && List.all (\(Node _ e) -> isConstantLeaf e) foldedArgs
                        && not (List.isEmpty foldedArgs)
                then
                    let
                        folded =
                            tryFold foldedNode
                    in
                    { expression = folded.expression
                    , stats =
                        if folded.didFold then
                            mergeDependencySummaryStats argStats constantFoldStats

                        else
                            argStats
                    }

                else if flags.inlineFunctions then
                    case tryInlineFunction flags env moduleName funcName foldedArgs of
                        InlineApplied inlineRewrite ->
                            let
                                recursiveResult =
                                    recurseNext inlineRewrite.expression

                                payoffStats =
                                    inlinePayoffStats inlineRewrite recursiveResult

                                shadowStats =
                                    shadowInlineStats (expressionSize foldedNode) inlineRewrite recursiveResult
                            in
                            { expression = recursiveResult.expression
                            , stats =
                                mergeDependencySummaryStats
                                    argStats
                                    (mergeDependencySummaryStats
                                        (inlineRewriteStats inlineRewrite)
                                        (mergeDependencySummaryStats
                                            payoffStats
                                            (mergeDependencySummaryStats shadowStats recursiveResult.stats)
                                        )
                                    )
                            }

                        InlineRejected reject ->
                            { expression = foldedNode
                            , stats =
                                mergeDependencySummaryStats
                                    argStats
                                    (inlineRejectStats reject.reason reject.sample)
                            }

                        InlineNotApplicable ->
                            { expression = foldedNode
                            , stats = argStats
                            }

                else
                    { expression = foldedNode
                    , stats = argStats
                    }

            IfBlock cond thenBranch elseBranch ->
                let
                    condResult =
                        recurse cond

                    thenResult =
                        recurse thenBranch

                    elseResult =
                        recurse elseBranch
                in
                { expression =
                    Node range
                        (IfBlock
                            condResult.expression
                            thenResult.expression
                            elseResult.expression
                        )
                , stats =
                    mergeDependencySummaryStats
                        condResult.stats
                        (mergeDependencySummaryStats thenResult.stats elseResult.stats)
                }

            CaseExpression { expression, cases } ->
                let
                    expressionResult =
                        recurse expression

                    caseResults =
                        cases
                            |> List.map
                                (\( pat, body ) ->
                                    let
                                        bodyResult =
                                            recurse body
                                    in
                                    { caseBody = ( pat, bodyResult.expression )
                                    , stats = bodyResult.stats
                                    }
                                )
                in
                { expression =
                    Node range
                        (CaseExpression
                            { expression = expressionResult.expression
                            , cases = List.map .caseBody caseResults
                            }
                        )
                , stats =
                    caseResults
                        |> List.foldl
                            (\result acc -> mergeDependencySummaryStats acc result.stats)
                            expressionResult.stats
                }

            LetExpression { declarations, expression } ->
                let
                    declarationResults =
                        declarations
                            |> List.map
                                (\(Node dRange decl) ->
                                    case decl of
                                        LetFunction f ->
                                            let
                                                (Node implRange impl) =
                                                    f.declaration

                                                functionExpressionResult =
                                                    recurse impl.expression
                                            in
                                            { declaration =
                                                Node dRange
                                                    (LetFunction
                                                        { f
                                                            | declaration =
                                                                Node implRange
                                                                    { impl | expression = functionExpressionResult.expression }
                                                        }
                                                    )
                                            , stats = functionExpressionResult.stats
                                            }

                                        LetDestructuring pat val ->
                                            let
                                                valueResult =
                                                    recurse val
                                            in
                                            { declaration =
                                                Node dRange (LetDestructuring pat valueResult.expression)
                                            , stats = valueResult.stats
                                            }
                                )

                    expressionResult =
                        recurse expression
                in
                { expression =
                    Node range
                        (LetExpression
                            { declarations = List.map .declaration declarationResults
                            , expression = expressionResult.expression
                            }
                        )
                , stats =
                    declarationResults
                        |> List.foldl
                            (\result acc -> mergeDependencySummaryStats acc result.stats)
                            expressionResult.stats
                }

            OperatorApplication op dir left right ->
                let
                    leftResult =
                        recurse left

                    rightResult =
                        recurse right

                    foldedNode =
                        Node range (OperatorApplication op dir leftResult.expression rightResult.expression)
                in
                if flags.foldConstantApplications && isConstantLeaf (Node.value leftResult.expression) && isConstantLeaf (Node.value rightResult.expression) then
                    let
                        folded =
                            tryFold foldedNode
                    in
                    { expression = folded.expression
                    , stats =
                        mergeDependencySummaryStats
                            leftResult.stats
                            (mergeDependencySummaryStats
                                rightResult.stats
                                (if folded.didFold then
                                    constantFoldStats

                                 else
                                    emptyDependencySummaryStats
                                )
                            )
                    }

                else
                    { expression = foldedNode
                    , stats = mergeDependencySummaryStats leftResult.stats rightResult.stats
                    }

            Negation inner ->
                let
                    innerResult =
                        recurse inner

                    foldedNode =
                        Node range (Negation innerResult.expression)
                in
                if isConstantLeaf (Node.value innerResult.expression) then
                    let
                        folded =
                            tryFold foldedNode
                    in
                    { expression = folded.expression
                    , stats =
                        mergeDependencySummaryStats
                            innerResult.stats
                            (if folded.didFold then
                                constantFoldStats

                             else
                                emptyDependencySummaryStats
                            )
                    }

                else
                    { expression = foldedNode
                    , stats = innerResult.stats
                    }

            ParenthesizedExpression inner ->
                let
                    innerResult =
                        recurse inner
                in
                { expression = Node range (ParenthesizedExpression innerResult.expression)
                , stats = innerResult.stats
                }

            LambdaExpression lambda ->
                let
                    innerResult =
                        recurse lambda.expression
                in
                { expression =
                    Node range
                        (LambdaExpression
                            { lambda | expression = innerResult.expression }
                        )
                , stats = innerResult.stats
                }

            FunctionOrValue [] name ->
                if not flags.inlinePrecomputedRefs then
                    rewriteResult node

                else
                    case Dict.get env.currentModuleKey env.shared.precomputedValues of
                        Just modulePrecomputed ->
                            case Dict.get name modulePrecomputed of
                                Just value ->
                                    if isLosslessValue value then
                                        { expression = Value.toExpression value
                                        , stats = precomputedRefSubstitutionStats
                                        }

                                    else
                                        rewriteResult node

                                Nothing ->
                                    rewriteResult node

                        Nothing ->
                            rewriteResult node

            FunctionOrValue ((_ :: _) as moduleName) name ->
                if not flags.inlinePrecomputedRefs then
                    rewriteResult node

                else
                    let
                        qualifiedKey =
                            Environment.moduleKey moduleName
                    in
                    case Dict.get qualifiedKey env.shared.precomputedValues of
                        Just modulePrecomputed ->
                            case Dict.get name modulePrecomputed of
                                Just value ->
                                    if isLosslessValue value then
                                        { expression = Value.toExpression value
                                        , stats = precomputedRefSubstitutionStats
                                        }

                                    else
                                        rewriteResult node

                                Nothing ->
                                    rewriteResult node

                        Nothing ->
                            rewriteResult node

            _ ->
                rewriteResult node


{-| Try to inline a small non-recursive function at a call site.
For cross-module calls, qualifies unqualified refs in the inlined body
with the source module name so they resolve correctly in the caller's context.
-}
tryInlineFunction : NormalizationFlags.NormalizationFlags -> Env -> List String -> String -> List (Node Expression) -> InlineDecision
tryInlineFunction flags env moduleName funcName args =
    let
        sourceModuleKey =
            if List.isEmpty moduleName then
                env.currentModuleKey

            else
                Environment.moduleKey moduleName

        sourceModuleName =
            if List.isEmpty moduleName then
                env.currentModule

            else
                moduleName

        isCrossModule =
            sourceModuleKey /= env.currentModuleKey

        maybeFuncImpl =
            Dict.get sourceModuleKey env.shared.functions
                |> Maybe.andThen (Dict.get funcName)
    in
    case maybeFuncImpl of
        Just funcImpl ->
            case extractVarPatternNames funcImpl.arguments of
                Just paramNames ->
                    let
                        bodySize : Int
                        bodySize =
                            expressionSize funcImpl.expression

                        bodyShape : InlineBodyShape
                        bodyShape =
                            classifyInlineBodyShape funcImpl.expression

                        paramNameSet : Set.Set String
                        paramNameSet =
                            paramNamesSet paramNames
                    in
                    let
                        inlineSafe : Bool
                        inlineSafe =
                            isInlineSafeBody paramNameSet funcImpl.expression
                    in
                    if
                        List.length paramNames
                            == List.length args
                        && not (containsSelfCallInExpr funcName funcImpl.expression)
                        && bodySize
                            < flags.inlineFunctionMaxSize
                        && inlineSafe
                        && not (referencesInternalHelper funcImpl.expression)
                    then
                        let
                            sourceFunctionNames =
                                Dict.get sourceModuleKey env.shared.functions
                                    |> Maybe.map Dict.keys
                                    |> Maybe.map Set.fromList
                                    |> Maybe.withDefault Set.empty

                            sourceModuleImports : ImportedNames
                            sourceModuleImports =
                                Dict.get sourceModuleKey env.shared.moduleImports
                                    |> Maybe.withDefault emptyImports

                            preQualified =
                                if isCrossModule then
                                    funcImpl.expression
                                        |> canonicalizeQualifiedRefs sourceModuleImports
                                        |> qualifyUnqualifiedRefs sourceModuleName sourceFunctionNames paramNameSet

                                else
                                    funcImpl.expression

                            substitution =
                                List.map2 Tuple.pair paramNames args
                                    |> Dict.fromList
                        in
                        InlineApplied
                            { expression = substituteVars substitution preQualified
                            , bodySize = bodySize
                            , bodyShape = bodyShape
                            }

                    else
                        if List.length paramNames /= List.length args then
                            InlineRejected
                                { reason = InlineRejectArity
                                , sample = Nothing
                                }

                        else if containsSelfCallInExpr funcName funcImpl.expression then
                            InlineRejected
                                { reason = InlineRejectSelfCall
                                , sample = Nothing
                                }

                        else if bodySize >= flags.inlineFunctionMaxSize then
                            let
                                sample =
                                    if shouldCollectLargeBodyRejectSample flags.inlineFunctionMaxSize bodySize then
                                        Just (formatRejectSample InlineRejectBodyTooLarge sourceModuleName funcName bodySize bodyShape funcImpl.expression)

                                    else
                                        Nothing
                            in
                            InlineRejected
                                { reason = InlineRejectBodyTooLarge
                                , sample = sample
                                }

                        else if not inlineSafe then
                            let
                                rejectReason =
                                    InlineRejectUnsafe (classifyUnsafeRoot funcImpl.expression)

                                sample =
                                    if not collectRejectSamples then
                                        Nothing

                                    else
                                        case rejectReason of
                                            InlineRejectUnsafe InlineUnsafeApplication ->
                                                Just (formatRejectSample rejectReason sourceModuleName funcName bodySize bodyShape funcImpl.expression)

                                            _ ->
                                                Nothing
                            in
                            InlineRejected
                                { reason = rejectReason
                                , sample = sample
                                }

                        else if referencesInternalHelper funcImpl.expression then
                            InlineRejected
                                { reason = InlineRejectInternalHelper
                                , sample = Nothing
                                }

                        else
                            InlineNotApplicable

                Nothing ->
                    InlineRejected
                        { reason = InlineRejectPattern
                        , sample = Nothing
                        }

        Nothing ->
            InlineNotApplicable


paramNamesSet : List String -> Set.Set String
paramNamesSet names =
    Set.fromList names


extractVarPatternNames : List (Node Pattern) -> Maybe (List String)
extractVarPatternNames patterns =
    patterns
        |> List.foldr
            (\(Node _ pat) acc ->
                case acc of
                    Nothing ->
                        Nothing

                    Just names ->
                        case pat of
                            VarPattern name ->
                                Just (name :: names)

                            _ ->
                                Nothing
            )
            (Just [])


containsSelfCallInExpr : String -> Node Expression -> Bool
containsSelfCallInExpr funcName (Node _ expr) =
    case expr of
        FunctionOrValue [] name ->
            name == funcName

        Application items ->
            List.any (containsSelfCallInExpr funcName) items

        OperatorApplication _ _ l r ->
            containsSelfCallInExpr funcName l || containsSelfCallInExpr funcName r

        IfBlock c t e ->
            containsSelfCallInExpr funcName c || containsSelfCallInExpr funcName t || containsSelfCallInExpr funcName e

        CaseExpression { expression, cases } ->
            containsSelfCallInExpr funcName expression
                || List.any (\( _, body ) -> containsSelfCallInExpr funcName body) cases

        LetExpression { declarations, expression } ->
            containsSelfCallInExpr funcName expression
                || List.any
                    (\(Node _ decl) ->
                        case decl of
                            LetFunction f ->
                                containsSelfCallInExpr funcName (Node.value f.declaration).expression

                            LetDestructuring _ val ->
                                containsSelfCallInExpr funcName val
                    )
                    declarations

        ListExpr items ->
            List.any (containsSelfCallInExpr funcName) items

        TupledExpression items ->
            List.any (containsSelfCallInExpr funcName) items

        ParenthesizedExpression inner ->
            containsSelfCallInExpr funcName inner

        Negation inner ->
            containsSelfCallInExpr funcName inner

        LambdaExpression { expression } ->
            containsSelfCallInExpr funcName expression

        RecordExpr fields ->
            List.any (\(Node _ ( _, val )) -> containsSelfCallInExpr funcName val) fields

        RecordAccess inner _ ->
            containsSelfCallInExpr funcName inner

        _ ->
            False


expressionSize : Node Expression -> Int
expressionSize (Node _ expr) =
    case expr of
        Application items ->
            1 + List.foldl (\item acc -> acc + expressionSize item) 0 items

        OperatorApplication _ _ l r ->
            1 + expressionSize l + expressionSize r

        IfBlock c t e ->
            1 + expressionSize c + expressionSize t + expressionSize e

        CaseExpression { expression, cases } ->
            1 + expressionSize expression + List.foldl (\( _, body ) acc -> acc + expressionSize body) 0 cases

        LetExpression { expression } ->
            5 + expressionSize expression

        ListExpr items ->
            1 + List.foldl (\item acc -> acc + expressionSize item) 0 items

        TupledExpression items ->
            1 + List.foldl (\item acc -> acc + expressionSize item) 0 items

        LambdaExpression { expression } ->
            2 + expressionSize expression

        ParenthesizedExpression inner ->
            expressionSize inner

        _ ->
            1


{-| Keep package-summary inlining to simple expression helpers.
Control-flow stays off-limits. We also allow tiny wrapper applications like
`List.isEmpty xs` when each parameter is used at most once, so package
summaries can bypass an extra helper frame without duplicating expensive work.
-}
isInlineSafeBody : Set.Set String -> Node Expression -> Bool
isInlineSafeBody paramNameSet expression =
    isInlineSafeExpression expression
        || isInlineSafeWrapperApplication paramNameSet expression


isInlineSafeExpression : Node Expression -> Bool
isInlineSafeExpression (Node _ expr) =
    case expr of
        Integer _ ->
            True

        Hex _ ->
            True

        Floatable _ ->
            True

        Literal _ ->
            True

        CharLiteral _ ->
            True

        UnitExpr ->
            True

        FunctionOrValue _ _ ->
            True

        Application items ->
            case items of
                [] ->
                    False

                headExpr :: args ->
                    case Node.value headExpr of
                        FunctionOrValue _ name ->
                            Eval.Expression.isUpperName name
                                && List.all isInlineSafeExpression args

                        _ ->
                            False

        OperatorApplication _ _ left right ->
            isInlineSafeExpression left && isInlineSafeExpression right

        Negation inner ->
            isInlineSafeExpression inner

        ParenthesizedExpression inner ->
            isInlineSafeExpression inner

        ListExpr items ->
            List.all isInlineSafeExpression items

        TupledExpression items ->
            List.all isInlineSafeExpression items

        RecordExpr fields ->
            List.all (\(Node _ ( _, value )) -> isInlineSafeExpression value) fields

        RecordAccess inner _ ->
            isInlineSafeExpression inner

        IfBlock _ _ _ ->
            False

        CaseExpression _ ->
            False

        LetExpression _ ->
            False

        LambdaExpression _ ->
            False

        _ ->
            False


isInlineSafeWrapperApplication : Set.Set String -> Node Expression -> Bool
isInlineSafeWrapperApplication paramNameSet ((Node _ expr) as expression) =
    case expr of
        Application (headExpr :: args) ->
            not (List.isEmpty args)
                && isInlineSafeWrapperHead paramNameSet headExpr
                && List.all (isInlineSafeWrapperArgument paramNameSet) args
                && parametersUsedAtMostOnce paramNameSet expression

        ParenthesizedExpression inner ->
            isInlineSafeWrapperApplication paramNameSet inner

        _ ->
            False


isInlineSafeWrapperHead : Set.Set String -> Node Expression -> Bool
isInlineSafeWrapperHead paramNameSet (Node _ expr) =
    case expr of
        FunctionOrValue [] name ->
            not (Set.member name paramNameSet)

        FunctionOrValue _ _ ->
            True

        RecordAccessFunction _ ->
            True

        Operator _ ->
            True

        PrefixOperator _ ->
            True

        ParenthesizedExpression inner ->
            isInlineSafeWrapperHead paramNameSet inner

        _ ->
            False


isInlineSafeWrapperArgument : Set.Set String -> Node Expression -> Bool
isInlineSafeWrapperArgument paramNameSet ((Node _ expr) as expression) =
    case expr of
        Integer _ ->
            True

        Hex _ ->
            True

        Floatable _ ->
            True

        Literal _ ->
            True

        CharLiteral _ ->
            True

        UnitExpr ->
            True

        FunctionOrValue _ _ ->
            True

        RecordAccessFunction _ ->
            True

        Operator _ ->
            True

        PrefixOperator _ ->
            True

        Application (headExpr :: args) ->
            not (List.isEmpty args)
                && isInlineSafeWrapperHead paramNameSet headExpr
                && List.all (isInlineSafeWrapperArgument paramNameSet) args

        OperatorApplication _ _ left right ->
            isInlineSafeWrapperArgument paramNameSet left
                && isInlineSafeWrapperArgument paramNameSet right

        Negation inner ->
            isInlineSafeWrapperArgument paramNameSet inner

        ParenthesizedExpression inner ->
            isInlineSafeWrapperArgument paramNameSet inner

        ListExpr items ->
            List.all (isInlineSafeWrapperArgument paramNameSet) items

        TupledExpression items ->
            List.all (isInlineSafeWrapperArgument paramNameSet) items

        RecordExpr fields ->
            List.all (\(Node _ ( _, value )) -> isInlineSafeWrapperArgument paramNameSet value) fields

        RecordAccess inner _ ->
            isInlineSafeWrapperArgument paramNameSet inner

        LambdaExpression lambda ->
            let
                lambdaShadows =
                    List.foldl (\pattern acc -> Set.union acc (collectPatternNames pattern)) Set.empty lambda.args

                outerParamsVisibleInLambdaBody =
                    Set.diff paramNameSet lambdaShadows
            in
            not (referencesAnyWrapperParams outerParamsVisibleInLambdaBody lambda.expression)
                && isInlineSafeWrapperArgument outerParamsVisibleInLambdaBody lambda.expression

        _ ->
            False


referencesAnyWrapperParams : Set.Set String -> Node Expression -> Bool
referencesAnyWrapperParams paramNameSet expression =
    not (Dict.isEmpty (parameterUseCounts paramNameSet expression))

parametersUsedAtMostOnce : Set.Set String -> Node Expression -> Bool
parametersUsedAtMostOnce paramNameSet expression =
    parameterUseCounts paramNameSet expression
        |> Dict.values
        |> List.all (\count -> count <= 1)


parameterUseCounts : Set.Set String -> Node Expression -> Dict.Dict String Int
parameterUseCounts paramNameSet (Node _ expr) =
    let
        mergeCounts : Dict.Dict String Int -> Dict.Dict String Int -> Dict.Dict String Int
        mergeCounts left right =
            Dict.merge
                (\key leftCount acc -> Dict.insert key leftCount acc)
                (\key leftCount rightCount acc -> Dict.insert key (leftCount + rightCount) acc)
                (\key rightCount acc -> Dict.insert key rightCount acc)
                left
                right
                Dict.empty

        sumCounts : List (Dict.Dict String Int) -> Dict.Dict String Int
        sumCounts counts =
            List.foldl mergeCounts Dict.empty counts
    in
    case expr of
        FunctionOrValue [] name ->
            if Set.member name paramNameSet then
                Dict.singleton name 1

            else
                Dict.empty

        Application items ->
            sumCounts (List.map (parameterUseCounts paramNameSet) items)

        OperatorApplication _ _ left right ->
            mergeCounts
                (parameterUseCounts paramNameSet left)
                (parameterUseCounts paramNameSet right)

        IfBlock condition thenBranch elseBranch ->
            sumCounts
                [ parameterUseCounts paramNameSet condition
                , parameterUseCounts paramNameSet thenBranch
                , parameterUseCounts paramNameSet elseBranch
                ]

        CaseExpression caseBlock ->
            sumCounts
                (parameterUseCounts paramNameSet caseBlock.expression
                    :: List.map (\( _, body ) -> parameterUseCounts paramNameSet body) caseBlock.cases
                )

        LetExpression letBlock ->
            sumCounts
                (parameterUseCounts paramNameSet letBlock.expression
                    :: List.map
                        (\(Node _ declaration) ->
                            case declaration of
                                LetFunction letFunction ->
                                    parameterUseCounts paramNameSet ((Node.value letFunction.declaration).expression)

                                LetDestructuring _ value ->
                                    parameterUseCounts paramNameSet value
                        )
                        letBlock.declarations
                )

        ListExpr items ->
            sumCounts (List.map (parameterUseCounts paramNameSet) items)

        TupledExpression items ->
            sumCounts (List.map (parameterUseCounts paramNameSet) items)

        ParenthesizedExpression inner ->
            parameterUseCounts paramNameSet inner

        Negation inner ->
            parameterUseCounts paramNameSet inner

        LambdaExpression lambda ->
            parameterUseCounts paramNameSet lambda.expression

        RecordExpr fields ->
            sumCounts
                (List.map (\(Node _ ( _, value )) -> parameterUseCounts paramNameSet value) fields)

        RecordAccess inner _ ->
            parameterUseCounts paramNameSet inner

        _ ->
            Dict.empty


{-| Scope-aware substitution: when entering a binding scope (case pattern,
let binding, lambda param), removes shadowed names from the substitution
dict. This prevents incorrectly replacing variables that are rebound in
inner scopes — the standard fix for capture-avoiding substitution.
-}
substituteVars : Dict.Dict String (Node Expression) -> Node Expression -> Node Expression
substituteVars subs ((Node range expr) as node) =
    if Dict.isEmpty subs then
        node

    else
        let
            s =
                substituteVars subs

            removeShadowed : Set.Set String -> Dict.Dict String (Node Expression)
            removeShadowed shadows =
                Set.foldl Dict.remove subs shadows
        in
        case expr of
            FunctionOrValue [] name ->
                case Dict.get name subs of
                    Just replacement ->
                        replacement

                    Nothing ->
                        node

            Application items ->
                Node range (Application (List.map s items))

            OperatorApplication op dir l r ->
                Node range (OperatorApplication op dir (s l) (s r))

            IfBlock c t e ->
                Node range (IfBlock (s c) (s t) (s e))

            CaseExpression { expression, cases } ->
                Node range
                    (CaseExpression
                        { expression = s expression
                        , cases =
                            List.map
                                (\( pat, body ) ->
                                    let
                                        shadows =
                                            collectPatternNames pat
                                    in
                                    ( pat, substituteVars (removeShadowed shadows) body )
                                )
                                cases
                        }
                    )

            LetExpression { declarations, expression } ->
                let
                    letShadows =
                        List.foldl
                            (\(Node _ decl) acc ->
                                case decl of
                                    LetFunction f ->
                                        Set.insert (Node.value (Node.value f.declaration).name) acc

                                    LetDestructuring pat _ ->
                                        Set.union acc (collectPatternNames pat)
                            )
                            Set.empty
                            declarations

                    innerSubs =
                        removeShadowed letShadows
                in
                Node range
                    (LetExpression
                        { declarations =
                            List.map
                                (\(Node dRange decl) ->
                                    Node dRange
                                        (case decl of
                                            LetFunction f ->
                                                let
                                                    (Node implRange impl) =
                                                        f.declaration

                                                    argShadows =
                                                        List.foldl (\p acc -> Set.union acc (collectPatternNames p)) letShadows impl.arguments
                                                in
                                                LetFunction
                                                    { f
                                                        | declaration =
                                                            Node implRange
                                                                { impl | expression = substituteVars (removeShadowed argShadows) impl.expression }
                                                    }

                                            LetDestructuring pat val ->
                                                LetDestructuring pat (substituteVars innerSubs val)
                                        )
                                )
                                declarations
                        , expression = substituteVars innerSubs expression
                        }
                    )

            ListExpr items ->
                Node range (ListExpr (List.map s items))

            TupledExpression items ->
                Node range (TupledExpression (List.map s items))

            ParenthesizedExpression inner ->
                Node range (ParenthesizedExpression (s inner))

            Negation inner ->
                Node range (Negation (s inner))

            LambdaExpression lambda ->
                let
                    lambdaShadows =
                        List.foldl (\p acc -> Set.union acc (collectPatternNames p)) Set.empty lambda.args
                in
                Node range (LambdaExpression { lambda | expression = substituteVars (removeShadowed lambdaShadows) lambda.expression })

            RecordExpr fields ->
                Node range (RecordExpr (List.map (\(Node fRange ( name, val )) -> Node fRange ( name, s val )) fields))

            RecordAccess inner field ->
                Node range (RecordAccess (s inner) field)

            _ ->
                node


{-| Qualify unqualified FunctionOrValue references with the source module name.
Only qualifies names that exist in the source module's function dict OR are
uppercase (constructors). Leaves Basics/default-import functions unqualified.
-}
qualifyUnqualifiedRefs : List String -> Set.Set String -> Set.Set String -> Node Expression -> Node Expression
qualifyUnqualifiedRefs sourceModule moduleFunctions locals ((Node range expr) as node) =
    let
        q =
            qualifyUnqualifiedRefs sourceModule moduleFunctions locals
    in
    case expr of
        FunctionOrValue [] name ->
            if Set.member name locals then
                node

            else if Set.member name moduleFunctions || Eval.Expression.isUpperName name then
                Node range (FunctionOrValue sourceModule name)

            else
                node

        Application items ->
            Node range (Application (List.map q items))

        OperatorApplication op dir l r ->
            Node range (OperatorApplication op dir (q l) (q r))

        IfBlock c t e ->
            Node range (IfBlock (q c) (q t) (q e))

        CaseExpression { expression, cases } ->
            Node range
                (CaseExpression
                    { expression = q expression
                    , cases =
                        List.map
                            (\( pat, body ) ->
                                ( pat
                                , qualifyUnqualifiedRefs sourceModule
                                    moduleFunctions
                                    (Set.union locals (collectPatternNames pat))
                                    body
                                )
                            )
                            cases
                    }
                )

        LetExpression { declarations, expression } ->
            let
                letLocals =
                    List.foldl
                        (\(Node _ decl) acc ->
                            case decl of
                                LetFunction f ->
                                    Set.insert (Node.value (Node.value f.declaration).name) acc

                                LetDestructuring pat _ ->
                                    Set.union acc (collectPatternNames pat)
                        )
                        locals
                        declarations
            in
            Node range
                (LetExpression
                    { declarations =
                        List.map
                            (\(Node dRange decl) ->
                                Node dRange
                                    (case decl of
                                        LetFunction f ->
                                            let
                                                (Node implRange impl) =
                                                    f.declaration

                                                fnLocals =
                                                    List.foldl (\p acc -> Set.union acc (collectPatternNames p)) letLocals impl.arguments
                                            in
                                            LetFunction
                                                { f
                                                    | declaration =
                                                        Node implRange
                                                            { impl | expression = qualifyUnqualifiedRefs sourceModule moduleFunctions fnLocals impl.expression }
                                                }

                                        LetDestructuring pat val ->
                                            LetDestructuring pat (qualifyUnqualifiedRefs sourceModule moduleFunctions letLocals val)
                                    )
                            )
                            declarations
                    , expression = qualifyUnqualifiedRefs sourceModule moduleFunctions letLocals expression
                    }
                )

        ListExpr items ->
            Node range (ListExpr (List.map q items))

        TupledExpression items ->
            Node range (TupledExpression (List.map q items))

        ParenthesizedExpression inner ->
            Node range (ParenthesizedExpression (q inner))

        LambdaExpression lambda ->
            let
                lambdaLocals =
                    List.foldl (\p acc -> Set.union acc (collectPatternNames p)) locals lambda.args
            in
            Node range
                (LambdaExpression
                    { lambda | expression = qualifyUnqualifiedRefs sourceModule moduleFunctions lambdaLocals lambda.expression }
                )

        RecordExpr fields ->
            Node range (RecordExpr (List.map (\(Node fRange ( name, val )) -> Node fRange ( name, q val )) fields))

        RecordAccess inner field ->
            Node range (RecordAccess (q inner) field)

        Negation inner ->
            Node range (Negation (q inner))

        _ ->
            node


canonicalizeQualifiedRefs : ImportedNames -> Node Expression -> Node Expression
canonicalizeQualifiedRefs imports ((Node range expr) as node) =
    let
        c =
            canonicalizeQualifiedRefs imports

        canonicalizeModuleName : List String -> List String
        canonicalizeModuleName moduleName =
            if List.isEmpty moduleName then
                moduleName

            else
                Dict.get (Environment.moduleKey moduleName) imports.aliases
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault moduleName
    in
    case expr of
        FunctionOrValue moduleName name ->
            if List.isEmpty moduleName then
                node

            else
                let
                    canonicalModuleName =
                        canonicalizeModuleName moduleName
                in
                if canonicalModuleName == moduleName then
                    node

                else
                    Node range (FunctionOrValue canonicalModuleName name)

        Application items ->
            Node range (Application (List.map c items))

        OperatorApplication op dir left right ->
            Node range (OperatorApplication op dir (c left) (c right))

        IfBlock condition trueBranch falseBranch ->
            Node range (IfBlock (c condition) (c trueBranch) (c falseBranch))

        CaseExpression { expression, cases } ->
            Node range
                (CaseExpression
                    { expression = c expression
                    , cases = List.map (\( pat, body ) -> ( pat, c body )) cases
                    }
                )

        LetExpression { declarations, expression } ->
            Node range
                (LetExpression
                    { declarations =
                        List.map
                            (\(Node declarationRange declaration) ->
                                Node declarationRange
                                    (case declaration of
                                        LetFunction functionDecl ->
                                            let
                                                (Node implRange impl) =
                                                    functionDecl.declaration
                                            in
                                            LetFunction
                                                { functionDecl
                                                    | declaration =
                                                        Node implRange
                                                            { impl | expression = c impl.expression }
                                                }

                                        LetDestructuring pattern value ->
                                            LetDestructuring pattern (c value)
                                    )
                            )
                            declarations
                    , expression = c expression
                    }
                )

        ListExpr items ->
            Node range (ListExpr (List.map c items))

        TupledExpression items ->
            Node range (TupledExpression (List.map c items))

        ParenthesizedExpression inner ->
            Node range (ParenthesizedExpression (c inner))

        LambdaExpression lambda ->
            Node range (LambdaExpression { lambda | expression = c lambda.expression })

        RecordExpr fields ->
            Node range (RecordExpr (List.map (\(Node fieldRange ( name, value )) -> Node fieldRange ( name, c value )) fields))

        RecordAccess inner field ->
            Node range (RecordAccess (c inner) field)

        Negation inner ->
            Node range (Negation (c inner))

        _ ->
            node


{-| Check if an expression references functions that resolve via internal
module helpers (like Basics.lt, Basics.gt) which are defined in the module
but may not resolve correctly when qualified cross-module. These are
lowercase functions that call Elm.Kernel.\* — safe to skip inlining for.
-}
referencesInternalHelper : Node Expression -> Bool
referencesInternalHelper (Node _ expr) =
    case expr of
        Application ((Node _ (FunctionOrValue ("Elm" :: "Kernel" :: _) _)) :: _) ->
            True

        Application items ->
            List.any referencesInternalHelper items

        OperatorApplication _ _ l r ->
            referencesInternalHelper l || referencesInternalHelper r

        IfBlock c t e ->
            referencesInternalHelper c || referencesInternalHelper t || referencesInternalHelper e

        CaseExpression { expression, cases } ->
            referencesInternalHelper expression
                || List.any (\( _, body ) -> referencesInternalHelper body) cases

        LetExpression { declarations, expression } ->
            referencesInternalHelper expression
                || List.any
                    (\(Node _ decl) ->
                        case decl of
                            LetFunction f ->
                                referencesInternalHelper (Node.value f.declaration).expression

                            LetDestructuring _ val ->
                                referencesInternalHelper val
                    )
                    declarations

        ListExpr items ->
            List.any referencesInternalHelper items

        TupledExpression items ->
            List.any referencesInternalHelper items

        ParenthesizedExpression inner ->
            referencesInternalHelper inner

        LambdaExpression { expression } ->
            referencesInternalHelper expression

        Negation inner ->
            referencesInternalHelper inner

        FunctionOrValue ("Elm" :: "Kernel" :: _) _ ->
            True

        _ ->
            False


collectPatternNames : Node Pattern -> Set.Set String
collectPatternNames (Node _ pat) =
    case pat of
        VarPattern name ->
            Set.singleton name

        TuplePattern items ->
            List.foldl (\p acc -> Set.union acc (collectPatternNames p)) Set.empty items

        UnConsPattern head tail ->
            Set.union (collectPatternNames head) (collectPatternNames tail)

        ListPattern items ->
            List.foldl (\p acc -> Set.union acc (collectPatternNames p)) Set.empty items

        NamedPattern _ args ->
            List.foldl (\p acc -> Set.union acc (collectPatternNames p)) Set.empty args

        AsPattern inner (Node _ alias) ->
            Set.insert alias (collectPatternNames inner)

        ParenthesizedPattern inner ->
            collectPatternNames inner

        _ ->
            Set.empty


{-| Replace a single module's declarations in an existing ProjectEnv.

This is the incremental env building primitive: given a base env built from all
modules, and a new version of one module, produce an updated env with only that
module's declarations replaced. All other modules remain untouched.

The key insight: `buildModuleEnv` adds declarations to `env.shared.functions` keyed by
module name. Replacing a module means:

1.  Remove all function entries under the old module's key from env.shared.functions
2.  Remove the old module's import table from env.moduleImports
3.  Re-run buildModuleEnv for the new module only
4.  Update allInterfaces with the new module's interface

This is O(1 module) instead of O(all modules), making it ideal for mutation
testing where only one module changes per mutation.

-}
replaceModuleInEnv :
    ProjectEnv
    ->
        { file : File
        , moduleName : ModuleName
        , interface : List Exposed
        }
    -> Result Error ProjectEnv
replaceModuleInEnv (ProjectEnv projectEnv) newModule =
    let
        modKey : String
        modKey =
            Environment.moduleKey newModule.moduleName

        -- Remove the old module's functions and import table
        cleanedEnv : Env
        cleanedEnv =
            { currentModule = projectEnv.env.currentModule
            , currentModuleKey = projectEnv.env.currentModuleKey
            , callStack = projectEnv.env.callStack
            , shared =
                { functions = Dict.remove modKey projectEnv.env.shared.functions
                , moduleImports = Dict.remove modKey projectEnv.env.shared.moduleImports
                , resolveBridge = projectEnv.env.shared.resolveBridge
                , precomputedValues = Dict.remove modKey projectEnv.env.shared.precomputedValues
                , tcoAnalyses = Dict.remove modKey projectEnv.env.shared.tcoAnalyses
                }
            , currentModuleFunctions = projectEnv.env.currentModuleFunctions
            , letFunctions = Dict.empty
            , values = projectEnv.env.values
            , imports = projectEnv.env.imports
            , callDepth = projectEnv.env.callDepth
            , recursionCheck = projectEnv.env.recursionCheck
            }

        -- Update the interfaces with the new module's interface
        updatedInterfaces : ElmDict.Dict ModuleName (List Exposed)
        updatedInterfaces =
            ElmDict.insert newModule.moduleName newModule.interface projectEnv.allInterfaces
    in
    buildModuleEnv updatedInterfaces newModule cleanedEnv
        |> Result.map
            (\env ->
                refreshResolvedGlobals
                    (ProjectEnv
                        { env = env
                        , allInterfaces = updatedInterfaces
                        , resolved =
                            rebuildResolvedForModules
                                [ newModule.moduleName ]
                                env
                                updatedInterfaces
                                projectEnv.resolved
                        }
                    )
            )


{-| Evaluate an expression using a pre-built ProjectEnv plus additional sources.
Only the additional sources are parsed; the ProjectEnv's environment is reused.
The expression is evaluated in the context of the last additional source.
-}
evalWithEnv : ProjectEnv -> List String -> Expression -> Result Error Value
evalWithEnv projectEnv additionalSources expression =
    evalWithEnvAndLimit Nothing projectEnv additionalSources expression


{-| Like `evalWithEnv`, but with an optional step limit to prevent hangs
on large computations. Pass `Just n` to limit evaluation to `n` trampoline
steps, or `Nothing` for unlimited.

**Phase 4 status:** this function stays on the OLD-evaluator path for
now. The original Phase 4 plan was to route it through the resolved-IR
trampoline (`evalWithResolvedIRFromFilesAndInterceptsAndLimit` exists
for exactly that purpose), but the RE trampoline lacks an
`Eval.Expression.tcoLoop` equivalent: OLD eval's `tcoLoop` runs a
JS-level while-loop for detected tail-recursive calls and never
re-enters the step counter, so `tcoProofTests`' tight budgets (e.g.
110 000 for a 100 000-iteration `countdown`) fit easily. RE's
trampoline charges one step per `rRecThen` (for the `RIf` condition,
for each argument eval, etc.), so the same 100 000-iteration loop
costs ~600 000 counted steps and blows the budget. Routing the test
through RE would cause `tcoProofTests` to regress — the Phase 4
rollback signal — so we leave this entry point on OLD eval until RE
grows a `tcoLoop`-equivalent. The `runRecWithBudget` infrastructure
added in Phase 4 is ready to be used by a future phase when that
lands.

-}
evalWithEnvAndLimit : Maybe Int -> ProjectEnv -> List String -> Expression -> Result Error Value
evalWithEnvAndLimit maxSteps (ProjectEnv projectEnv) additionalSources expression =
    let
        parseResult :
            Result
                Error
                (List
                    { file : File
                    , moduleName : ModuleName
                    , interface : List Exposed
                    }
                )
        parseResult =
            additionalSources
                |> List.map
                    (\source ->
                        source
                            |> Elm.Parser.parseToFile
                            |> Result.mapError ParsingError
                            |> Result.andThen
                                (\file ->
                                    let
                                        modName : ModuleName
                                        modName =
                                            fileModuleName file
                                    in
                                    Ok
                                        { file = file
                                        , moduleName = modName
                                        , interface = buildInterfaceFromFile file
                                        }
                                )
                    )
                |> combineResults
    in
    case parseResult of
        Err e ->
            Err e

        Ok parsedModules ->
            let
                additionalInterfaces : ElmDict.Dict ModuleName (List Exposed)
                additionalInterfaces =
                    parsedModules
                        |> List.map (\m -> ( m.moduleName, m.interface ))
                        |> ElmDict.fromList

                allInterfaces : ElmDict.Dict ModuleName (List Exposed)
                allInterfaces =
                    ElmDict.union additionalInterfaces projectEnv.allInterfaces

                envResult : Result Error Env
                envResult =
                    parsedModules
                        |> Result.MyExtra.combineFoldl
                            (\parsedModule envAcc ->
                                buildModuleEnv allInterfaces parsedModule envAcc
                            )
                            (Ok projectEnv.env)
            in
            case envResult of
                Err e ->
                    Err e

                Ok env ->
                    let
                        lastModule : ModuleName
                        lastModule =
                            parsedModules
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .moduleName
                                |> Maybe.withDefault [ "Main" ]

                        lastFile : Maybe File
                        lastFile =
                            parsedModules
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .file

                        finalImports : ImportedNames
                        finalImports =
                            case lastFile of
                                Just file ->
                                    (defaultImports ++ file.imports)
                                        |> List.foldl (processImport allInterfaces) emptyImports

                                Nothing ->
                                    emptyImports

                        lastModuleKey : String
                        lastModuleKey =
                            Environment.moduleKey lastModule

                        finalEnv : Env
                        finalEnv =
                            { env
                                | currentModule = lastModule
                                , currentModuleKey = lastModuleKey
                                , currentModuleFunctions =
                                    Dict.get lastModuleKey env.shared.functions
                                        |> Maybe.withDefault Dict.empty
                                , imports = finalImports
                            }

                        result : Result Types.EvalErrorData Value
                        result =
                            Eval.Expression.evalExpression
                                (fakeNode expression)
                                { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
                                finalEnv
                                |> EvalResult.toResult
                    in
                    Result.mapError Types.EvalError result


{-| Like `evalWithEnvFromFiles`, but with an optional step limit.
Pass `Just n` to limit evaluation to `n` trampoline steps (prevents infinite
loops from consuming unbounded memory). Pass `Nothing` for unlimited.

**Phase 4 status:** stays on the OLD-evaluator path for the same
reason `evalWithEnvAndLimit` does — see that function's docstring.

-}
evalWithEnvFromFilesAndLimit : Maybe Int -> ProjectEnv -> List File -> Expression -> Result Error Value
evalWithEnvFromFilesAndLimit maxSteps (ProjectEnv projectEnv) additionalFiles expression =
    let
        parsedModules =
            additionalFiles
                |> List.map
                    (\file ->
                        { file = file
                        , moduleName = fileModuleName file
                        , interface = buildInterfaceFromFile file
                        }
                    )

        additionalInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces =
            ElmDict.union additionalInterfaces projectEnv.allInterfaces

        envResult =
            parsedModules
                |> Result.MyExtra.combineFoldl
                    (\parsedModule envAcc ->
                        buildModuleEnv allInterfaces parsedModule envAcc
                    )
                    (Ok projectEnv.env)
    in
    case envResult of
        Err e ->
            Err e

        Ok env ->
            let
                lastModule =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .moduleName
                        |> Maybe.withDefault [ "Main" ]

                lastFile =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .file

                finalImports =
                    case lastFile of
                        Just file ->
                            (defaultImports ++ file.imports)
                                |> List.foldl (processImport allInterfaces) emptyImports

                        Nothing ->
                            emptyImports

                lastModuleKey =
                    Environment.moduleKey lastModule

                finalEnv =
                    { env
                        | currentModule = lastModule
                        , currentModuleKey = lastModuleKey
                        , currentModuleFunctions =
                            Dict.get lastModuleKey env.shared.functions
                                |> Maybe.withDefault Dict.empty
                        , imports = finalImports
                    }

                result =
                    Eval.Expression.evalExpression
                        (fakeNode expression)
                        { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
                        finalEnv
                        |> EvalResult.toResult
            in
            Result.mapError Types.EvalError result


{-| Like `evalWithEnv`, but accepts pre-parsed `File` ASTs instead of source strings.
Skips the parse step entirely — useful when you already have the AST (e.g. build tools,
mutation testing, or incremental compilation where files are parsed once and reused).
-}
evalWithEnvFromFiles : ProjectEnv -> List File -> Expression -> Result Error Value
evalWithEnvFromFiles projectEnv additionalFiles expression =
    evalWithEnvFromFilesAndLimit Nothing projectEnv additionalFiles expression


{-| Like `evalWithEnvFromFiles`, but memoizes selected fully-applied top-level
functions in-memory during evaluation and returns the updated memo cache so it
can be reused across later invocations.
-}
evalWithEnvFromFilesAndMemo :
    ProjectEnv
    -> List File
    -> Set String
    -> MemoRuntime.MemoCache
    -> Bool
    -> Expression
    ->
        Result
            Error
            { value : Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
evalWithEnvFromFilesAndMemo (ProjectEnv projectEnv) additionalFiles memoizedFunctions memoCache collectMemoStats expression =
    let
        parsedModules =
            additionalFiles
                |> List.map
                    (\file ->
                        { file = file
                        , moduleName = fileModuleName file
                        , interface = buildInterfaceFromFile file
                        }
                    )

        additionalInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces =
            ElmDict.union additionalInterfaces projectEnv.allInterfaces

        envResult =
            parsedModules
                |> Result.MyExtra.combineFoldl
                    (\parsedModule envAcc ->
                        buildModuleEnv allInterfaces parsedModule envAcc
                    )
                    (Ok projectEnv.env)
    in
    case envResult of
        Err e ->
            Err e

        Ok env ->
            let
                lastModule =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .moduleName
                        |> Maybe.withDefault [ "Main" ]

                lastFile =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .file

                finalImports =
                    case lastFile of
                        Just file ->
                            (defaultImports ++ file.imports)
                                |> List.foldl (processImport allInterfaces) emptyImports

                        Nothing ->
                            emptyImports

                lastModuleKey =
                    Environment.moduleKey lastModule

                finalEnv =
                    { env
                        | currentModule = lastModule
                        , currentModuleKey = lastModuleKey
                        , currentModuleFunctions =
                            Dict.get lastModuleKey env.shared.functions
                                |> Maybe.withDefault Dict.empty
                        , imports = finalImports
                    }
            in
            Eval.Expression.evalExpression
                (fakeNode expression)
                { trace = False
                , coverage = False
                , coverageProbeLines = Set.empty
                , maxSteps = Nothing
                , tcoTarget = Nothing
                , callCounts = Nothing
                , intercepts = Dict.empty
                , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                , collectMemoStats = collectMemoStats
                , useResolvedIR = False
                }
                finalEnv
                |> driveInternalMemo
                    memoCache
                    (if collectMemoStats then
                        MemoRuntime.emptyMemoStats

                     else
                        MemoRuntime.disabledMemoStats
                    )


{-| Like `evalWithEnvFromFiles`, but also injects pre-computed Values into the
evaluation environment. The injected values are available as local variables
in the expression being evaluated.

This enables preserving interpreter Values across evaluations. For example,
passing the `updatedRules` from one `Rule.review` call into the next, so
elm-review's internal per-module cache stays warm.

-}
evalWithEnvFromFilesAndValues :
    ProjectEnv
    -> List File
    -> Dict.Dict String Value
    -> Expression
    -> Result Error Value
evalWithEnvFromFilesAndValues (ProjectEnv projectEnv) additionalFiles injectedValues expression =
    let
        parsedModules =
            additionalFiles
                |> List.map
                    (\file ->
                        { file = file
                        , moduleName = fileModuleName file
                        , interface = buildInterfaceFromFile file
                        }
                    )

        additionalInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces =
            ElmDict.union additionalInterfaces projectEnv.allInterfaces

        envResult =
            parsedModules
                |> Result.MyExtra.combineFoldl
                    (\parsedModule envAcc ->
                        buildModuleEnv allInterfaces parsedModule envAcc
                    )
                    (Ok projectEnv.env)
    in
    case envResult of
        Err e ->
            Err e

        Ok env ->
            let
                lastModule =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .moduleName
                        |> Maybe.withDefault [ "Main" ]

                lastFile =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .file

                finalImports =
                    case lastFile of
                        Just file ->
                            (defaultImports ++ file.imports)
                                |> List.foldl (processImport allInterfaces) emptyImports

                        Nothing ->
                            emptyImports

                lastModuleKey =
                    Environment.moduleKey lastModule

                -- Inject pre-computed values into the env alongside the final env setup
                finalEnv =
                    { env
                        | currentModule = lastModule
                        , currentModuleKey = lastModuleKey
                        , currentModuleFunctions =
                            Dict.get lastModuleKey env.shared.functions
                                |> Maybe.withDefault Dict.empty
                        , imports = finalImports
                        , values =
                            Dict.foldl
                                (\name value acc -> Dict.insert name value acc)
                                env.values
                                injectedValues
                    }

                result =
                    Eval.Expression.evalExpression
                        (fakeNode expression)
                        { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
                        finalEnv
                        |> EvalResult.toResult
            in
            Result.mapError Types.EvalError result


evalWithEnvFromFilesAndValuesAndMemo :
    ProjectEnv
    -> List File
    -> Dict.Dict String Value
    -> Set String
    -> MemoRuntime.MemoCache
    -> Bool
    -> Expression
    ->
        Result
            Error
            { value : Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
evalWithEnvFromFilesAndValuesAndMemo (ProjectEnv projectEnv) additionalFiles injectedValues memoizedFunctions memoCache collectMemoStats expression =
    let
        parsedModules =
            additionalFiles
                |> List.map
                    (\file ->
                        { file = file
                        , moduleName = fileModuleName file
                        , interface = buildInterfaceFromFile file
                        }
                    )

        additionalInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces =
            ElmDict.union additionalInterfaces projectEnv.allInterfaces

        envResult =
            parsedModules
                |> Result.MyExtra.combineFoldl
                    (\parsedModule envAcc ->
                        buildModuleEnv allInterfaces parsedModule envAcc
                    )
                    (Ok projectEnv.env)
    in
    case envResult of
        Err e ->
            Err e

        Ok env ->
            let
                lastModule =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .moduleName
                        |> Maybe.withDefault [ "Main" ]

                lastFile =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .file

                finalImports =
                    case lastFile of
                        Just file ->
                            (defaultImports ++ file.imports)
                                |> List.foldl (processImport allInterfaces) emptyImports

                        Nothing ->
                            emptyImports

                lastModuleKey =
                    Environment.moduleKey lastModule

                finalEnv =
                    { env
                        | currentModule = lastModule
                        , currentModuleKey = lastModuleKey
                        , currentModuleFunctions =
                            Dict.get lastModuleKey env.shared.functions
                                |> Maybe.withDefault Dict.empty
                        , imports = finalImports
                        , values =
                            Dict.foldl
                                (\name value acc -> Dict.insert name value acc)
                                env.values
                                injectedValues
                    }
            in
            Eval.Expression.evalExpression
                (fakeNode expression)
                { trace = False
                , coverage = False
                , coverageProbeLines = Set.empty
                , maxSteps = Nothing
                , tcoTarget = Nothing
                , callCounts = Nothing
                , intercepts = Dict.empty
                , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                , collectMemoStats = collectMemoStats
                , useResolvedIR = False
                }
                finalEnv
                |> driveInternalMemo
                    memoCache
                    (if collectMemoStats then
                        MemoRuntime.emptyMemoStats

                     else
                        MemoRuntime.disabledMemoStats
                    )


{-| Like `evalWithIntercepts`, but drives interpreter-local memoization
internally and returns the updated cache for warm reuse in later runs.
-}
evalWithMemoizedFunctions :
    ProjectEnv
    -> List String
    -> Set String
    -> MemoRuntime.MemoCache
    -> Bool
    -> Expression
    ->
        Result
            Error
            { value : Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
evalWithMemoizedFunctions projectEnv additionalSources memoizedFunctions memoCache collectMemoStats expression =
    let
        parseResult =
            additionalSources
                |> List.map
                    (\source ->
                        Elm.Parser.parseToFile source
                            |> Result.mapError Types.ParsingError
                    )
                |> combineResults
    in
    parseResult
        |> Result.andThen
            (\files ->
                evalWithEnvFromFilesAndMemo projectEnv files memoizedFunctions memoCache collectMemoStats expression
            )


evalWithValuesAndMemoizedFunctions :
    ProjectEnv
    -> List String
    -> Dict.Dict String Value
    -> Set String
    -> MemoRuntime.MemoCache
    -> Bool
    -> Expression
    ->
        Result
            Error
            { value : Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
evalWithValuesAndMemoizedFunctions projectEnv additionalSources injectedValues memoizedFunctions memoCache collectMemoStats expression =
    let
        parseResult =
            additionalSources
                |> List.map
                    (\source ->
                        Elm.Parser.parseToFile source
                            |> Result.mapError Types.ParsingError
                    )
                |> combineResults
    in
    parseResult
        |> Result.andThen
            (\files ->
                evalWithEnvFromFilesAndValuesAndMemo projectEnv files injectedValues memoizedFunctions memoCache collectMemoStats expression
            )


driveInternalMemo :
    MemoRuntime.MemoCache
    -> MemoRuntime.MemoStats
    -> Types.EvalResult Value
    ->
        Result
            Error
            { value : Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
driveInternalMemo memoCache memoStats evalResult =
    case evalResult of
        Types.EvOk value ->
            Ok { value = value, memoCache = memoCache, memoStats = memoStats }

        Types.EvErr evalErr ->
            Err (Types.EvalError evalErr)

        Types.EvOkTrace value _ _ ->
            Ok { value = value, memoCache = memoCache, memoStats = memoStats }

        Types.EvErrTrace evalErr _ _ ->
            Err (Types.EvalError evalErr)

        Types.EvMemoLookup payload resume ->
            let
                ( nextCache, nextStats, maybeValue ) =
                    handleInternalMemoLookup memoCache memoStats payload
            in
            driveInternalMemo nextCache nextStats (resume maybeValue)

        Types.EvMemoStore payload next ->
            let
                ( nextCache, nextStats ) =
                    handleInternalMemoStore memoCache memoStats payload
            in
            driveInternalMemo nextCache nextStats next

        Types.EvYield tag payload resume ->
            case handleInternalMemoYield memoCache memoStats tag payload of
                Just ( nextCache, nextStats, resumeValue ) ->
                    driveInternalMemo nextCache nextStats (resume resumeValue)

                Nothing ->
                    Err
                        (Types.EvalError
                            { currentModule = []
                            , callStack = []
                            , error = Types.Unsupported ("Unhandled non-memo yield in evalWithMemoizedFunctions: " ++ tag)
                            }
                        )

        Types.EvOkCoverage value _ ->
            Ok { value = value, memoCache = memoCache, memoStats = memoStats }

        Types.EvErrCoverage evalErr _ ->
            Err (Types.EvalError evalErr)


handleInternalMemoYield :
    MemoRuntime.MemoCache
    -> MemoRuntime.MemoStats
    -> String
    -> Value
    -> Maybe ( MemoRuntime.MemoCache, MemoRuntime.MemoStats, Value )
handleInternalMemoYield memoCache memoStats tag payload =
    if tag == MemoRuntime.lookupTag then
        MemoRuntime.decodeLookupPayload payload
            |> Maybe.map
                (\lookupPayload ->
                    let
                        ( nextCache, nextStats, maybeValue ) =
                            handleInternalMemoLookup memoCache memoStats lookupPayload
                    in
                    ( nextCache
                    , nextStats
                    , Maybe.withDefault MemoRuntime.maybeNothing maybeValue
                    )
                )

    else if tag == MemoRuntime.storeTag then
        MemoRuntime.decodeStorePayload payload
            |> Maybe.map
                (\storePayload ->
                    let
                        ( nextCache, nextStats ) =
                            handleInternalMemoStore memoCache memoStats storePayload
                    in
                    ( nextCache
                    , nextStats
                    , Types.Unit
                    )
                )

    else
        Nothing


handleInternalMemoLookup :
    MemoRuntime.MemoCache
    -> MemoRuntime.MemoStats
    -> Types.MemoLookupPayload
    -> ( MemoRuntime.MemoCache, MemoRuntime.MemoStats, Maybe Value )
handleInternalMemoLookup memoCache memoStats lookupPayload =
    let
        updatedStats =
            MemoRuntime.recordFunctionLookup lookupPayload.qualifiedName memoStats

        maybeFingerprints =
            case ( lookupPayload.shallowFingerprint, lookupPayload.deepFingerprint ) of
                ( Just shallowFingerprint, Just deepFingerprint ) ->
                    Just ( shallowFingerprint, deepFingerprint )

                _ ->
                    lookupPayload.args
                        |> Maybe.map
                            (\args ->
                                ( Eval.Expression.fingerprintArgs args
                                , deepHashArgs args
                                )
                            )
    in
    case lookupPayload.compactFingerprint of
        Just compactFingerprint ->
            case MemoRuntime.lookupCompactValue lookupPayload.specId compactFingerprint memoCache of
                Just cachedValue ->
                    ( memoCache
                    , MemoRuntime.recordFunctionHit lookupPayload.qualifiedName updatedStats
                    , Just cachedValue
                    )

                Nothing ->
                    ( memoCache
                    , MemoRuntime.recordFunctionMiss lookupPayload.qualifiedName updatedStats
                    , Nothing
                    )

        Nothing ->
            case maybeFingerprints of
                Just ( shallowFingerprint, deepFingerprint ) ->
                    case MemoRuntime.lookupEntries lookupPayload.specId shallowFingerprint memoCache of
                        Just entries ->
                            case List.Extra.find (\entry -> entry.deepFingerprint == deepFingerprint) entries of
                                Just entry ->
                                    ( memoCache
                                    , MemoRuntime.recordFunctionHit lookupPayload.qualifiedName updatedStats
                                    , Just entry.value
                                    )

                                Nothing ->
                                    ( memoCache
                                    , MemoRuntime.recordFunctionMiss lookupPayload.qualifiedName updatedStats
                                    , Nothing
                                    )

                        Nothing ->
                            ( memoCache
                            , MemoRuntime.recordFunctionMiss lookupPayload.qualifiedName updatedStats
                            , Nothing
                            )

                Nothing ->
                    ( memoCache
                    , MemoRuntime.recordFunctionMiss lookupPayload.qualifiedName updatedStats
                    , Nothing
                    )


handleInternalMemoStore :
    MemoRuntime.MemoCache
    -> MemoRuntime.MemoStats
    -> Types.MemoStorePayload
    -> ( MemoRuntime.MemoCache, MemoRuntime.MemoStats )
handleInternalMemoStore memoCache memoStats storePayload =
    let
        maybeFingerprints =
            case ( storePayload.shallowFingerprint, storePayload.deepFingerprint ) of
                ( Just shallowFingerprint, Just deepFingerprint ) ->
                    Just ( shallowFingerprint, deepFingerprint )

                _ ->
                    storePayload.args
                        |> Maybe.map
                            (\args ->
                                ( Eval.Expression.fingerprintArgs args
                                , deepHashArgs args
                                )
                            )
    in
    case storePayload.compactFingerprint of
        Just compactFingerprint ->
            ( MemoRuntime.storeCompactValue
                storePayload.specId
                compactFingerprint
                storePayload.value
                memoCache
            , MemoRuntime.recordFunctionStore storePayload.qualifiedName memoStats
            )

        Nothing ->
            case maybeFingerprints of
                Just ( shallowFingerprint, deepFingerprint ) ->
                    ( MemoRuntime.storeEntry
                        storePayload.specId
                        shallowFingerprint
                        { deepFingerprint = deepFingerprint
                        , value = storePayload.value
                        }
                        memoCache
                    , MemoRuntime.recordFunctionStore storePayload.qualifiedName memoStats
                    )

                Nothing ->
                    ( memoCache, memoStats )


deepHashArgs : List Value -> Int
deepHashArgs args =
    List.foldl
        (\value acc ->
            Bitwise.xor (acc * 16777619) (Eval.Expression.deepHashValue value)
        )
        2166136261
        args


{-| Combined: injected Values + intercepts + raw EvalResult. Used by
the BackendTask yield driver when both Value injection and intercept
handling are needed in the same evaluation.

**Phase 6 status:** stays on OLD eval for the same reason
`evalWithInterceptsRaw` does — see that function's docstring.

-}
evalWithEnvFromFilesAndValuesAndInterceptsRaw :
    ProjectEnv
    -> List File
    -> Dict.Dict String Value
    -> Dict.Dict String Types.Intercept
    -> Expression
    -> Types.EvalResult Value
evalWithEnvFromFilesAndValuesAndInterceptsRaw (ProjectEnv projectEnv) additionalFiles injectedValues intercepts expression =
    evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw
        (ProjectEnv projectEnv)
        additionalFiles
        injectedValues
        intercepts
        Set.empty
        expression


evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw :
    ProjectEnv
    -> List File
    -> Dict.Dict String Value
    -> Dict.Dict String Types.Intercept
    -> Set String
    -> Expression
    -> Types.EvalResult Value
evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw (ProjectEnv projectEnv) additionalFiles injectedValues intercepts memoizedFunctions expression =
    let
        parsedModules =
            additionalFiles
                |> List.map
                    (\file ->
                        { file = file
                        , moduleName = fileModuleName file
                        , interface = buildInterfaceFromFile file
                        }
                    )

        additionalInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces =
            ElmDict.union additionalInterfaces projectEnv.allInterfaces

        envResult =
            parsedModules
                |> Result.MyExtra.combineFoldl
                    (\parsedModule envAcc ->
                        buildModuleEnv allInterfaces parsedModule envAcc
                    )
                    (Ok projectEnv.env)
    in
    case envResult of
        Err e ->
            case e of
                Types.ParsingError _ ->
                    Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Env build error" }

                Types.EvalError evalErr ->
                    Types.EvErr evalErr

        Ok env ->
            let
                lastModule =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .moduleName
                        |> Maybe.withDefault [ "Main" ]

                lastFile =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .file

                finalImports =
                    case lastFile of
                        Just file ->
                            (defaultImports ++ file.imports)
                                |> List.foldl (processImport allInterfaces) emptyImports

                        Nothing ->
                            emptyImports

                lastModuleKey =
                    Environment.moduleKey lastModule

                finalEnv =
                    { env
                        | currentModule = lastModule
                        , currentModuleKey = lastModuleKey
                        , currentModuleFunctions =
                            Dict.get lastModuleKey env.shared.functions
                                |> Maybe.withDefault Dict.empty
                        , imports = finalImports
                        , values =
                            Dict.foldl
                                (\name value acc -> Dict.insert name value acc)
                                env.values
                                injectedValues
                    }
            in
            Eval.Expression.evalExpression
                (fakeNode expression)
                { trace = False
                , coverage = False
                , coverageProbeLines = Set.empty
                , maxSteps = Nothing
                , tcoTarget = Nothing
                , callCounts = Nothing
                , intercepts = intercepts
                , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                , collectMemoStats = False
                , useResolvedIR = False
                }
                finalEnv


{-| Unlimited-budget delegate for
`evalWithResolvedIRFromFilesAndInterceptsAndLimit`.

Resolved-IR variant of `evalWithEnvFromFilesAndValuesAndInterceptsRaw`.
Takes the same inputs (additional Files to register, injected Values,
intercepts dict, entry Expression) and routes them through the new
`Eval.ResolvedExpression.evalR` path instead of `Eval.Expression`.

The additional files are parsed-as-if-they-were-user-modules: each
top-level declaration is assigned a fresh `GlobalId` and resolved to an
`RExpr`, then spliced into a derived `ResolvedProject` that mirrors
`projectEnv.resolved` plus the new entries. The env itself is also
extended (via `buildModuleEnv`) so the fallback path — used by kernel
calls and anything that doesn't hit the native/kernel dispatchers — sees
the new modules exactly the same way the old evaluator does.

Intercepts are precomputed into `interceptsByGlobal : Dict GlobalId (String, Intercept)`
by joining the intercept keys against the merged `globalIds` table.
Any intercept whose key doesn't match a known `(moduleName, name)` pair
is silently dropped — this matches the old evaluator's behavior, which
only fires intercepts on qualified function calls.

Memoization is deliberately **not** supported in this path. Callers that
need `memoizedFunctions` must route through the old entry point instead.
This keeps the new path simple; Phase 4 Round 3 will port memo support.

Injected Values are stored in `env.values` on the fallback env so that
intercept callbacks invoking the fallback see them, matching the old
path. They're **not** reachable from resolved-IR code directly — the
new evaluator's `evalR` only looks up via `RLocal`/`RGlobal`, not by
name. This matches the review runner's usage (injected values are
consumed by intercept callbacks, not by user code).

-}
evalWithResolvedIRFromFilesAndIntercepts :
    ProjectEnv
    -> List File
    -> Dict.Dict String Value
    -> Dict.Dict String Types.Intercept
    -> Expression
    -> Types.EvalResult Value
evalWithResolvedIRFromFilesAndIntercepts projectEnv additionalFiles injectedValues intercepts expression =
    evalWithResolvedIRFromFilesAndInterceptsAndLimit
        Nothing
        projectEnv
        additionalFiles
        injectedValues
        intercepts
        expression


{-| Step-budgeted variant. When `maxSteps` is `Just n`, the resolved-IR
evaluator's trampoline (`runRecWithBudget`) decrements a counter per
dispatch and returns "Step limit exceeded" when it reaches zero.
`Nothing` means unlimited — cheap single-compare overhead per dispatch.

This is the Phase 4 entry point that `evalWithEnvAndLimit` and
`evalWithEnvFromFilesAndLimit` delegate to.
-}
evalWithResolvedIRFromFilesAndInterceptsAndLimit :
    Maybe Int
    -> ProjectEnv
    -> List File
    -> Dict.Dict String Value
    -> Dict.Dict String Types.Intercept
    -> Expression
    -> Types.EvalResult Value
evalWithResolvedIRFromFilesAndInterceptsAndLimit maxSteps (ProjectEnv projectEnv) additionalFiles injectedValues intercepts expression =
    let
        parsedModules =
            additionalFiles
                |> List.map
                    (\file ->
                        { file = file
                        , moduleName = fileModuleName file
                        , interface = buildInterfaceFromFile file
                        }
                    )

        additionalInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces =
            ElmDict.union additionalInterfaces projectEnv.allInterfaces

        envResult =
            parsedModules
                |> Result.MyExtra.combineFoldl
                    (\parsedModule envAcc ->
                        buildModuleEnv allInterfaces parsedModule envAcc
                    )
                    (Ok projectEnv.env)
    in
    case envResult of
        Err e ->
            case e of
                Types.ParsingError _ ->
                    Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Env build error in evalWithResolvedIRFromFilesAndIntercepts" }

                Types.EvalError evalErr ->
                    Types.EvErr evalErr

        Ok env ->
            let
                summaries : List CachedModuleSummary
                summaries =
                    buildCachedModuleSummariesFromParsed parsedModules

                baseResolved : ResolvedProject
                baseResolved =
                    projectEnv.resolved

                {- Pass 1: allocate a fresh GlobalId for every new
                   top-level function in the additional modules. Start
                   the counter at `1 + max(existing ids)` so it never
                   collides with the base project's ids.
                -}
                nextInitialId : IR.GlobalId
                nextInitialId =
                    (baseResolved.globalIdToName
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.withDefault -1
                    )
                        + 1

                idAssignment : { next : Int, ids : Dict.Dict ( ModuleName, String ) IR.GlobalId, reverseIds : Dict.Dict IR.GlobalId ( ModuleName, String ) }
                idAssignment =
                    summaries
                        |> List.foldl
                            (\summary outer ->
                                summary.functions
                                    |> List.foldl
                                        (\impl inner ->
                                            if isCustomTypeConstructor impl then
                                                inner

                                            else
                                                let
                                                    name : String
                                                    name =
                                                        Node.value impl.name
                                                in
                                                case Dict.get ( summary.moduleName, name ) inner.ids of
                                                    Just _ ->
                                                        -- Re-registering an existing decl
                                                        -- (e.g. same wrapper used twice).
                                                        -- Reuse the existing id.
                                                        inner

                                                    Nothing ->
                                                        { next = inner.next + 1
                                                        , ids = Dict.insert ( summary.moduleName, name ) inner.next inner.ids
                                                        , reverseIds = Dict.insert inner.next ( summary.moduleName, name ) inner.reverseIds
                                                        }
                                        )
                                        outer
                            )
                            { next = nextInitialId
                            , ids = baseResolved.globalIds
                            , reverseIds = baseResolved.globalIdToName
                            }

                mergedGlobalIds : Dict.Dict ( ModuleName, String ) IR.GlobalId
                mergedGlobalIds =
                    idAssignment.ids

                mergedGlobalIdToName : Dict.Dict IR.GlobalId ( ModuleName, String )
                mergedGlobalIdToName =
                    idAssignment.reverseIds

                {- Pass 2: resolve each new declaration's body against the
                   merged globalIds table. Silently drop resolve errors —
                   the entry expression resolution below will surface any
                   missing name as its own UnknownName error.
                -}
                {- Pass 2: Resolve each new declaration's body against
                   the merged globalIds table, using `initContextWithImports`
                   so aliased imports (`import Review.Project as Project`)
                   canonicalize through the module's import table.

                   With Phase 4 r3's `RExprImpl` closure bridge in place
                   (`env.shared.resolveBridge`), successfully resolving a
                   decl is safe even when its body passes local closures
                   to higher-order core functions — the bridge catches
                   them when the old evaluator dispatches back.

                   Silent resolve-error drops remain the fallback for
                   anything the resolver still doesn't cover (e.g.
                   GLSL expressions, exotic let forms). The caller
                   delegates those to the old evaluator as before.
                -}
                mergedBodies : Dict.Dict IR.GlobalId IR.RExpr
                mergedBodies =
                    summaries
                        |> List.foldl
                            (\summary outer ->
                                summary.functions
                                    |> List.foldl
                                        (\impl inner ->
                                            if isCustomTypeConstructor impl then
                                                inner

                                            else
                                                let
                                                    name : String
                                                    name =
                                                        Node.value impl.name

                                                    ctx : Resolver.ResolverContext
                                                    ctx =
                                                        Resolver.initContextWithImports
                                                            summary.moduleName
                                                            mergedGlobalIds
                                                            summary.importedNames
                                                in
                                                case Resolver.resolveDeclaration ctx impl of
                                                    Ok rexpr ->
                                                        case Dict.get ( summary.moduleName, name ) mergedGlobalIds of
                                                            Just id ->
                                                                Dict.insert id rexpr inner

                                                            Nothing ->
                                                                inner

                                                    Err _ ->
                                                        inner
                                        )
                                        outer
                            )
                            baseResolved.bodies

                {- Precompute intercepts keyed by GlobalId so the hot
                   RApply path can do a single Dict.get instead of
                   formatting a qualified-name String on every function
                   call.
                -}
                interceptsByGlobal : Dict.Dict IR.GlobalId ( String, Types.Intercept )
                interceptsByGlobal =
                    intercepts
                        |> Dict.foldl
                            (\key intercept acc ->
                                case parseInterceptKey key of
                                    Just ( moduleName, name ) ->
                                        case Dict.get ( moduleName, name ) mergedGlobalIds of
                                            Just id ->
                                                Dict.insert id ( key, intercept ) acc

                                            Nothing ->
                                                acc

                                    Nothing ->
                                        acc
                            )
                            Dict.empty

                lastModule : ModuleName
                lastModule =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .moduleName
                        |> Maybe.withDefault [ "Main" ]

                lastFile : Maybe File
                lastFile =
                    parsedModules
                        |> List.reverse
                        |> List.head
                        |> Maybe.map .file

                finalImports =
                    case lastFile of
                        Just file ->
                            (defaultImports ++ file.imports)
                                |> List.foldl (processImport allInterfaces) emptyImports

                        Nothing ->
                            emptyImports

                lastModuleKey : String
                lastModuleKey =
                    Environment.moduleKey lastModule

                {- Phase 4 Round 3 bridge: runs an `RExprImpl` closure
                   (created by the new evaluator) when the old evaluator
                   encounters it at a higher-order call site like
                   `List.foldl userFn acc xs`. Captures the resolver
                   state so the old evaluator doesn't need direct access
                   to it.

                   Every bridge invocation builds a fresh `REnv` whose
                   `fallbackEnv` is the old evaluator's current `Env`,
                   keeping delegation back to the old evaluator correct
                   (including that env's `shared.resolveBridge`, which
                   is this same bridge — recursive bridging works).

                   `globals` starts empty on each bridge call. Caching
                   across bridge invocations would require threading a
                   mutable cache, which isn't worth the complexity until
                   we see it dominate in profiles.
                -}
                {- Pre-built `globals` cache from the project env's
                   `ResolvedProject.globals`, shared between the main
                   `renv` and the `installedBridge`'s per-call
                   `bridgeRenv`. Phase 2 of the OLD-evaluator migration
                   plan: previously this dict was rebuilt per call by
                   walking `env.shared.precomputedValues` and mapping
                   each entry through `mergedGlobalIds`. Now the
                   project env carries the pre-built dict, so repeated
                   calls amortize the build cost to zero.

                   `mergedGlobalIds` is a superset of
                   `baseResolved.globalIds` (new additional-file ids
                   extend the map rather than replace it), so any
                   entry already in `projectEnv.resolved.globals`
                   stays valid under the merged map. Additional files
                   aren't normalized at this point, so they don't
                   contribute new `precomputedValues` entries — the
                   cache is complete with the base set.

                   Critical for `RemoveAccentsTest`-shape workloads
                   where a 65 K-iteration `Array.initialize` callback
                   references `lookupTable` on every iteration.
                -}
                precomputedGlobalsCache : Dict.Dict IR.GlobalId Value
                precomputedGlobalsCache =
                    projectEnv.resolved.globals

                installedBridge : Types.ResolveBridge
                installedBridge =
                    Types.ResolveBridge
                        (\payload selfClosure argValues bridgeCfg bridgeEnv ->
                            let
                                withSelf : List Value
                                withSelf =
                                    List.repeat payload.selfSlots selfClosure
                                        ++ payload.capturedLocals

                                bodyLocals : List Value
                                bodyLocals =
                                    List.foldl (::) withSelf argValues

                                bridgeRenv : RE.REnv
                                bridgeRenv =
                                    { locals = bodyLocals
                                    , globals = precomputedGlobalsCache
                                    , resolvedBodies = mergedBodies
                                    , globalIdToName = mergedGlobalIdToName
                                    , nativeDispatchers = baseResolved.nativeDispatchers
                                    , higherOrderDispatchers = baseResolved.higherOrderDispatchers
                                    , kernelDispatchers = baseResolved.kernelDispatchers
                                    , interceptsByGlobal = interceptsByGlobal
                                    , fallbackEnv = bridgeEnv
                                    , fallbackConfig = bridgeCfg
                                    , currentModule = bridgeEnv.currentModule
                                    , callStack = bridgeEnv.callStack
                                    , callDepth = 0
                                    }
                            in
                            RE.evalR bridgeRenv payload.body
                        )

                {- Build the fallback env with injected values merged in.
                   The new evaluator's `delegateCoreApply` uses this env
                   when it can't dispatch via native/kernel paths, and
                   intercept callbacks receive it as their `Env` argument.

                   Also replaces `env.shared.resolveBridge` with the
                   real bridge above so the old evaluator can run
                   resolved-IR closures that reach it through kernel
                   callbacks.
                -}
                existingShared : Types.SharedContext
                existingShared =
                    env.shared

                finalShared : Types.SharedContext
                finalShared =
                    { existingShared | resolveBridge = installedBridge }

                finalEnv : Env
                finalEnv =
                    { env
                        | currentModule = lastModule
                        , currentModuleKey = lastModuleKey
                        , currentModuleFunctions =
                            Dict.get lastModuleKey env.shared.functions
                                |> Maybe.withDefault Dict.empty
                        , imports = finalImports
                        , values =
                            Dict.foldl
                                (\name value acc -> Dict.insert name value acc)
                                env.values
                                injectedValues
                        , shared = finalShared
                    }

                fallbackConfig : Types.Config
                fallbackConfig =
                    { trace = False
                    , coverage = False
                    , coverageProbeLines = Set.empty
                    , maxSteps = maxSteps
                    , tcoTarget = Nothing
                    , callCounts = Nothing
                    , intercepts = intercepts
                    , memoizedFunctions = MemoSpec.emptyRegistry
                    , collectMemoStats = False
                    , useResolvedIR = False
                    }

                resolverCtx : Resolver.ResolverContext
                resolverCtx =
                    Resolver.initContextWithImports lastModule mergedGlobalIds finalImports
            in
            case Resolver.resolveExpression resolverCtx (fakeNode expression) of
                Err resolveError ->
                    Types.EvErr
                        { currentModule = lastModule
                        , callStack = []
                        , error =
                            Types.Unsupported
                                ("resolver error in evalWithResolvedIRFromFilesAndIntercepts: " ++ resolverErrorToString resolveError)
                        }

                Ok rexpr ->
                    let
                        renv : RE.REnv
                        renv =
                            { locals = []
                            , globals = precomputedGlobalsCache
                            , resolvedBodies = mergedBodies
                            , globalIdToName = mergedGlobalIdToName
                            , nativeDispatchers = baseResolved.nativeDispatchers
                            , higherOrderDispatchers = baseResolved.higherOrderDispatchers
                            , kernelDispatchers = baseResolved.kernelDispatchers
                            , interceptsByGlobal = interceptsByGlobal
                            , fallbackEnv = finalEnv
                            , fallbackConfig = fallbackConfig
                            , currentModule = lastModule
                            , callStack = []
                            , callDepth = 0
                            }
                    in
                    RE.evalR renv rexpr


{-| Split an intercept key like `"Review.Rule.initialCachePartsMarker"`
into the `(moduleName, name)` pair that the resolver's `globalIds` table
is keyed by. The last dot-separated segment is the function name; the
rest is the module path.

Returns `Nothing` for malformed keys (no dot, or empty name), matching
the old evaluator's behavior of only firing intercepts on qualified
function calls.

-}
parseInterceptKey : String -> Maybe ( ModuleName, String )
parseInterceptKey key =
    let
        segments : List String
        segments =
            String.split "." key
    in
    case List.reverse segments of
        name :: revModule ->
            if String.isEmpty name || List.isEmpty revModule then
                Nothing

            else
                Just ( List.reverse revModule, name )

        [] ->
            Nothing


{-| Evaluate with function intercepts. Intercepts are checked before normal
function evaluation. If a qualified function name matches an entry in the
intercepts Dict, the intercept function is called instead of the AST.

This is the general-purpose hook for framework callbacks (BackendTask, Test)
and for memoization/caching (elm-review cache markers).

**Phase 6 status:** stays on OLD eval. A Phase 6 attempt routed this
through `evalWithResolvedIRFromFilesAndIntercepts`, but the bridge
function runs a full `resolveProject` pass 2 on the additional files
**every call**, resolving every user-module body into `RExpr`. For the
test-runner workload (one call per test module), that fixed per-call
cost dominates the per-iteration eval savings and regresses the bench
(`bench/testrunner-ab.sh` 1088 ms → 1220 ms on the 8-file subset;
`bench/core-extra-full.sh` 8.04 s → 8.69 s on the 11-file suite).

A future phase will need to make the resolved-project-extension cost
amortize across repeated `evalWithIntercepts` calls — e.g. by caching
the resolved view of the additional files keyed by content hash, or
by letting the caller pre-extend once via `extendResolvedWithFiles`
and pass an already-resolved `ProjectEnv` in. Until then, this entry
point stays on OLD eval.

-}
evalWithIntercepts :
    ProjectEnv
    -> List String
    -> Dict.Dict String Types.Intercept
    -> Expression
    -> Result Error Value
evalWithIntercepts (ProjectEnv projectEnv) additionalSources intercepts expression =
    let
        parseResult =
            additionalSources
                |> List.map
                    (\source ->
                        Elm.Parser.parseToFile source
                            |> Result.mapError Types.ParsingError
                    )
                |> combineResults
    in
    case parseResult of
        Err e ->
            Err e

        Ok parsedModules ->
            let
                modulesWithMeta =
                    parsedModules
                        |> List.map
                            (\file ->
                                { file = file
                                , moduleName = fileModuleName file
                                , interface = buildInterfaceFromFile file
                                }
                            )

                additionalInterfaces =
                    modulesWithMeta
                        |> List.map (\m -> ( m.moduleName, m.interface ))
                        |> ElmDict.fromList

                allInterfaces =
                    ElmDict.union additionalInterfaces projectEnv.allInterfaces

                envResult =
                    modulesWithMeta
                        |> Result.MyExtra.combineFoldl
                            (\parsedModule envAcc ->
                                buildModuleEnv allInterfaces parsedModule envAcc
                            )
                            (Ok projectEnv.env)
            in
            case envResult of
                Err e ->
                    Err e

                Ok env ->
                    let
                        lastModule =
                            modulesWithMeta
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .moduleName
                                |> Maybe.withDefault [ "Main" ]

                        lastFile =
                            parsedModules
                                |> List.reverse
                                |> List.head

                        finalImports =
                            case lastFile of
                                Just file ->
                                    (defaultImports ++ file.imports)
                                        |> List.foldl (processImport allInterfaces) emptyImports

                                Nothing ->
                                    emptyImports

                        lastModuleKey =
                            Environment.moduleKey lastModule

                        finalEnv =
                            { env
                                | currentModule = lastModule
                                , currentModuleKey = lastModuleKey
                                , currentModuleFunctions =
                                    Dict.get lastModuleKey env.shared.functions
                                        |> Maybe.withDefault Dict.empty
                                , imports = finalImports
                            }

                        result =
                            Eval.Expression.evalExpression
                                (fakeNode expression)
                                { trace = False
                                , coverage = False
                                , coverageProbeLines = Set.empty
                                , maxSteps = Nothing
                                , tcoTarget = Nothing
                                , callCounts = Nothing
                                , intercepts = intercepts
                                , memoizedFunctions = MemoSpec.emptyRegistry
                                , collectMemoStats = False
                                , useResolvedIR = False
                                }
                                finalEnv
                                |> EvalResult.toResult
                    in
                    Result.mapError Types.EvalError result


{-| Like evalWithIntercepts but returns the raw EvalResult, preserving EvYield.

The framework driver should handle EvYield in a loop (yield → handle effect →
resume with result → check for more yields).

**Phase 6 status:** stays on the OLD-evaluator path. A direct pass-
through to `evalWithResolvedIRFromFilesAndIntercepts` would return
only the first yield from an expression that yields multiple times
(e.g. `myMap (\\n -> Helpers.marker n) [1, 2, 3]` with a yielding
intercept on `Helpers.marker`). RE's `runRec` trampoline collapses
`EvYield` up through `andThenValue` but doesn't re-enter the
suspended computation when the driver resumes — the continuation
captured at yield time only covers the innermost sub-expression, not
the outer `List.map` / `myMap` loop. OLD eval's `evalExpression`
propagates yields correctly because the whole recursion is
`runRecursion`-based and each step re-enters at the right point.

A future phase (likely alongside the `tcoLoop`-equivalent work that
blocks Phases 3/4's entry-point migrations) will need to thread
`EvYield` through `runRec`'s continuation stack so that a resumed
yield re-enters the outer loop. Until then, `evalWithInterceptsRaw`
must stay on OLD eval — all 10 yield-sequence tests in
`tests/IncrementalEnvTests.elm` regress otherwise.

-}
evalWithInterceptsRaw :
    ProjectEnv
    -> List String
    -> Dict.Dict String Types.Intercept
    -> Expression
    -> Types.EvalResult Value
evalWithInterceptsRaw (ProjectEnv projectEnv) additionalSources intercepts expression =
    evalWithInterceptsAndMemoRaw
        (ProjectEnv projectEnv)
        additionalSources
        intercepts
        Set.empty
        expression


evalWithInterceptsAndMemoRaw :
    ProjectEnv
    -> List String
    -> Dict.Dict String Types.Intercept
    -> Set String
    -> Expression
    -> Types.EvalResult Value
evalWithInterceptsAndMemoRaw (ProjectEnv projectEnv) additionalSources intercepts memoizedFunctions expression =
    let
        parseResult =
            additionalSources
                |> List.map
                    (\source ->
                        Elm.Parser.parseToFile source
                            |> Result.mapError Types.ParsingError
                    )
                |> combineResults
    in
    case parseResult of
        Err e ->
            case e of
                Types.ParsingError _ ->
                    Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Parse error in evalWithInterceptsRaw" }

                Types.EvalError evalErr ->
                    Types.EvErr evalErr

        Ok parsedModules ->
            let
                modulesWithMeta =
                    parsedModules
                        |> List.map
                            (\file ->
                                { file = file
                                , moduleName = fileModuleName file
                                , interface = buildInterfaceFromFile file
                                }
                            )

                additionalInterfaces =
                    modulesWithMeta
                        |> List.map (\m -> ( m.moduleName, m.interface ))
                        |> ElmDict.fromList

                allInterfaces =
                    ElmDict.union additionalInterfaces projectEnv.allInterfaces

                envResult =
                    modulesWithMeta
                        |> Result.MyExtra.combineFoldl
                            (\parsedModule envAcc ->
                                buildModuleEnv allInterfaces parsedModule envAcc
                            )
                            (Ok projectEnv.env)
            in
            case envResult of
                Err e ->
                    case e of
                        Types.ParsingError _ ->
                            Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Env build parse error" }

                        Types.EvalError evalErr ->
                            Types.EvErr evalErr

                Ok env ->
                    let
                        lastModule =
                            modulesWithMeta
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .moduleName
                                |> Maybe.withDefault [ "Main" ]

                        lastFile =
                            parsedModules
                                |> List.reverse
                                |> List.head

                        finalImports =
                            case lastFile of
                                Just file ->
                                    (defaultImports ++ file.imports)
                                        |> List.foldl (processImport allInterfaces) emptyImports

                                Nothing ->
                                    emptyImports

                        lastModuleKey =
                            Environment.moduleKey lastModule

                        finalEnv =
                            { env
                                | currentModule = lastModule
                                , currentModuleKey = lastModuleKey
                                , currentModuleFunctions =
                                    Dict.get lastModuleKey env.shared.functions
                                        |> Maybe.withDefault Dict.empty
                                , imports = finalImports
                            }
                    in
                    Eval.Expression.evalExpression
                        (fakeNode expression)
                        { trace = False
                        , coverage = False
                        , coverageProbeLines = Set.empty
                        , maxSteps = Nothing
                        , tcoTarget = Nothing
                        , callCounts = Nothing
                        , intercepts = intercepts
                        , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                        , collectMemoStats = False
                        , useResolvedIR = False
                        }
                        finalEnv


{-| Extend a ProjectEnv with additional pre-parsed Files, without evaluating
any expression. Useful for building a "base user env" from pkgEnv + all user
modules, which can then be incrementally updated via `replaceModuleInEnv`.
-}
extendWithFiles : ProjectEnv -> List File -> Result Error ProjectEnv
extendWithFiles (ProjectEnv projectEnv) additionalFiles =
    let
        parsedModules =
            additionalFiles
                |> List.map
                    (\file ->
                        { file = file
                        , moduleName = fileModuleName file
                        , interface = buildInterfaceFromFile file
                        }
                    )

        additionalInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces =
            ElmDict.union additionalInterfaces projectEnv.allInterfaces

        envResult =
            parsedModules
                |> Result.MyExtra.combineFoldl
                    (\parsedModule envAcc ->
                        buildModuleEnv allInterfaces parsedModule envAcc
                    )
                    (Ok projectEnv.env)
    in
    envResult
        |> Result.map
            (\env ->
                refreshResolvedGlobals
                    (ProjectEnv
                        { env = env
                        , allInterfaces = allInterfaces
                        , resolved =
                            rebuildResolvedForModules
                                (List.map .moduleName parsedModules)
                                env
                                allInterfaces
                                projectEnv.resolved
                        }
                    )
            )


{-| Like `extendWithFiles`, but also runs the top-level-constant normalization
pass on the newly-added user modules. The existing package functions inside
the incoming `ProjectEnv` act as fixed context (they're already normalized
from the package summary cache), so user constants that reference package
values like `Fuzz.Float.exponentMapping` get to see the fast form during
their own pre-evaluation.

The implementation does `extendWithFiles` first to get a complete env, then
post-hoc rewrites the user modules' function bodies in place by walking the
freshly-added modules and running `tryNormalizeConstant` on each zero-arg
function. Package bodies are left untouched.

-}
extendWithFilesNormalized : ProjectEnv -> List File -> Result Error ProjectEnv
extendWithFilesNormalized projectEnv additionalFiles =
    extendWithFiles projectEnv additionalFiles
        |> Result.map
            (\extendedProjectEnv ->
                let
                    userModuleNames : List ModuleName
                    userModuleNames =
                        additionalFiles |> List.map fileModuleName
                in
                normalizeUserModulesInEnv userModuleNames extendedProjectEnv
            )

{-| Resolve every function in `additionalFiles` into the resolved-IR
representation and merge the new bodies + GlobalIds into the project
env's `ResolvedProject`. Use this when extending a project with user
modules at load time so that subsequent eval calls through the
resolved-IR evaluator can find the user functions natively, instead
of falling back to the old string-keyed evaluator on every call.

Without this, `extendWithFiles` only adds the modules to
`shared.functions` (the old evaluator's view). The resolved-IR
evaluator's `dispatchGlobalApplyStep` then misses on the user
functions and falls through to `delegateCoreApply`, which routes
back to the old evaluator's hot path. For fuzz-heavy workloads with
many user-defined callbacks, that fallback dominates the cold-eval
profile.

This helper does the same id-allocation + body-resolution as
`evalWithResolvedIRFromFilesAndIntercepts`'s internal mergedBodies
pass, but at project-load time (one-shot) instead of per-eval-call
(amortized to zero).

Resolver errors are silently dropped — the function stays in
`shared.functions` and the old evaluator's fallback continues to
handle it. This matches the existing behavior of `resolveProject`
during initial package load.

-}
extendResolvedWithFiles : List File -> ProjectEnv -> ProjectEnv
extendResolvedWithFiles additionalFiles (ProjectEnv projectEnv) =
    let
        parsedModules :
            List
                { file : File
                , moduleName : ModuleName
                , interface : List Exposed
                }
        parsedModules =
            additionalFiles
                |> List.map
                    (\file ->
                        { file = file
                        , moduleName = fileModuleName file
                        , interface = buildInterfaceFromFile file
                        }
                    )

        summaries : List CachedModuleSummary
        summaries =
            buildCachedModuleSummariesFromParsed parsedModules

        baseResolved : ResolvedProject
        baseResolved =
            projectEnv.resolved

        nextInitialId : IR.GlobalId
        nextInitialId =
            (baseResolved.globalIdToName
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault -1
            )
                + 1

        idAssignment : { next : Int, ids : Dict.Dict ( ModuleName, String ) IR.GlobalId, reverseIds : Dict.Dict IR.GlobalId ( ModuleName, String ) }
        idAssignment =
            summaries
                |> List.foldl
                    (\summary outer ->
                        summary.functions
                            |> List.foldl
                                (\impl inner ->
                                    if isCustomTypeConstructor impl then
                                        inner

                                    else
                                        let
                                            name : String
                                            name =
                                                Node.value impl.name
                                        in
                                        case Dict.get ( summary.moduleName, name ) inner.ids of
                                            Just _ ->
                                                inner

                                            Nothing ->
                                                { next = inner.next + 1
                                                , ids = Dict.insert ( summary.moduleName, name ) inner.next inner.ids
                                                , reverseIds = Dict.insert inner.next ( summary.moduleName, name ) inner.reverseIds
                                                }
                                )
                                outer
                    )
                    { next = nextInitialId
                    , ids = baseResolved.globalIds
                    , reverseIds = baseResolved.globalIdToName
                    }

        mergedGlobalIds : Dict.Dict ( ModuleName, String ) IR.GlobalId
        mergedGlobalIds =
            idAssignment.ids

        mergedGlobalIdToName : Dict.Dict IR.GlobalId ( ModuleName, String )
        mergedGlobalIdToName =
            idAssignment.reverseIds

        mergedBodies : Dict.Dict IR.GlobalId IR.RExpr
        mergedBodies =
            summaries
                |> List.foldl
                    (\summary outer ->
                        summary.functions
                            |> List.foldl
                                (\impl inner ->
                                    if isCustomTypeConstructor impl then
                                        inner

                                    else
                                        let
                                            name : String
                                            name =
                                                Node.value impl.name

                                            ctx : Resolver.ResolverContext
                                            ctx =
                                                Resolver.initContextWithImports
                                                    summary.moduleName
                                                    mergedGlobalIds
                                                    summary.importedNames
                                        in
                                        case Resolver.resolveDeclaration ctx impl of
                                            Ok rexpr ->
                                                case Dict.get ( summary.moduleName, name ) mergedGlobalIds of
                                                    Just id ->
                                                        Dict.insert id rexpr inner

                                                    Nothing ->
                                                        inner

                                            Err _ ->
                                                inner
                                )
                                outer
                    )
                    baseResolved.bodies

        newResolved : ResolvedProject
        newResolved =
            { baseResolved
                | globalIds = mergedGlobalIds
                , globalIdToName = mergedGlobalIdToName
                , bodies = mergedBodies
            }
    in
    refreshResolvedGlobals (ProjectEnv { projectEnv | resolved = newResolved })
{-| Check whether a function body is worth trying to normalize. Functions that
obviously produce a `Test`, `Fuzzer`, `Expectation`, or other closure-carrying
value would only succeed at evaluation to be rejected by `isLosslessValue`,
which wastes the eval cycles. Detect that statically by looking for any
reference to a blocklisted module inside the expression.

We don't try to be clever here — the goal is to avoid obviously-wasted eval
work on test-heavy files, not to perfectly classify every possible expression.

-}
isNormalizationCandidate : Node Expression -> Bool
isNormalizationCandidate (Node.Node _ expr) =
    not (expressionReferencesBlocklisted expr)


expressionReferencesBlocklisted : Expression -> Bool
expressionReferencesBlocklisted expr =
    case expr of
        FunctionOrValue moduleName _ ->
            moduleIsBlocklisted moduleName

        Application children ->
            List.any isBlocklistedNode children

        OperatorApplication _ _ l r ->
            isBlocklistedNode l || isBlocklistedNode r

        IfBlock c t f ->
            isBlocklistedNode c || isBlocklistedNode t || isBlocklistedNode f

        PrefixOperator _ ->
            False

        Operator _ ->
            False

        Integer _ ->
            False

        Hex _ ->
            False

        Floatable _ ->
            False

        Negation inner ->
            isBlocklistedNode inner

        Literal _ ->
            False

        CharLiteral _ ->
            False

        TupledExpression children ->
            List.any isBlocklistedNode children

        ParenthesizedExpression inner ->
            isBlocklistedNode inner

        LetExpression { declarations, expression } ->
            isBlocklistedNode expression
                || List.any (\(Node.Node _ decl) -> letDeclarationIsBlocklisted decl) declarations

        CaseExpression { expression, cases } ->
            isBlocklistedNode expression
                || List.any (\( _, body ) -> isBlocklistedNode body) cases

        LambdaExpression { expression } ->
            isBlocklistedNode expression

        RecordExpr setters ->
            List.any (\(Node.Node _ ( _, value )) -> isBlocklistedNode value) setters

        ListExpr children ->
            List.any isBlocklistedNode children

        RecordAccess record _ ->
            isBlocklistedNode record

        RecordAccessFunction _ ->
            False

        RecordUpdateExpression _ setters ->
            List.any (\(Node.Node _ ( _, value )) -> isBlocklistedNode value) setters

        GLSLExpression _ ->
            False

        UnitExpr ->
            False


isBlocklistedNode : Node Expression -> Bool
isBlocklistedNode (Node.Node _ expr) =
    expressionReferencesBlocklisted expr


letDeclarationIsBlocklisted : Elm.Syntax.Expression.LetDeclaration -> Bool
letDeclarationIsBlocklisted decl =
    case decl of
        LetFunction { declaration } ->
            let
                (Node.Node _ funcDecl) =
                    declaration
            in
            isBlocklistedNode funcDecl.expression

        LetDestructuring _ expression ->
            isBlocklistedNode expression


{-| Modules whose output values can't round-trip through `Value.toExpression`
(because they contain closures, opaque kernel types, or have runtime identity).
We skip normalization for any zero-arg function whose body references any of
them — the eval would only succeed long enough to produce a lossy value that
`isLosslessValue` rejects.

The list is conservative: over-blocking a pure constant wastes optimization
potential but is always safe; under-blocking wastes eval cycles during
normalization.

-}
moduleIsBlocklisted : ModuleName -> Bool
moduleIsBlocklisted moduleName =
    case moduleName of
        -- Test framework: Test/Fuzz/Expect values are closures over test bodies
        "Test" :: _ ->
            True

        "Fuzz" :: _ ->
            True

        "Expect" :: _ ->
            True

        -- JSON: Decoders are opaque closures; Encoder Values are JsonVal which
        -- is round-trippable but often built from opaque package-level ones
        "Json" :: "Decode" :: _ ->
            True

        "Json" :: "Encode" :: _ ->
            True

        -- Regex: RegexValue wraps a JS regex object with no AST representation
        "Regex" :: _ ->
            True

        -- View/rendering: closures and virtual-DOM node chains
        "Html" :: _ ->
            True

        "Svg" :: _ ->
            True

        "Browser" :: _ ->
            True

        "Url" :: "Parser" :: _ ->
            True

        -- Ports, cmds, tasks: all closure-typed effect values
        "Cmd" :: _ ->
            True

        "Sub" :: _ ->
            True

        "Task" :: _ ->
            True

        "Process" :: _ ->
            True

        "Platform" :: _ ->
            True

        _ ->
            False


{-| Sum of all precomputed values across modules, for diagnostics.
-}
precomputedValuesCount : ProjectEnv -> Int
precomputedValuesCount (ProjectEnv projectEnv) =
    projectEnv.env.shared.precomputedValues
        |> Dict.foldl (\_ inner acc -> acc + Dict.size inner) 0


{-| Precompute the TCO analysis (`TcoStrategy` + `isTailRecursive`) for
every function in every module of the given function dict. Stored on
`SharedContext.tcoAnalyses` so that `Eval.Expression.tcoLoop` and the
call-dispatch path can look up the result in O(1) instead of walking
the body's AST on every invocation.

The full analysis runs in O(total AST size). Without this side table,
the same analysis ran on every tail-recursive function call — a per-
invocation tax dominated by the body walk in
`TcoAnalysis.collectTailCalls` and `Eval.Expression.containsSelfCall`.
On fuzz-heavy workloads (~100K+ calls) that adds up to hundreds of ms
of pure repeated work.

-}
precomputeTcoAnalyses :
    Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String (Dict.Dict String TcoAnalysis.TcoMetadata)
precomputeTcoAnalyses moduleFunctions =
    Dict.foldl
        (\moduleKey funcs acc ->
            Dict.insert moduleKey (precomputeOneModule funcs) acc
        )
        Dict.empty
        moduleFunctions


{-| Precompute TCO metadata for a single module's function dict.
Used both by the global precompute pass and by the in-place per-module
updaters (`replaceModuleFunctionsInEnv`, `normalizeOneModuleInEnv`,
`mergeModuleFunctionsIntoEnv`) that rewrite a module's bodies and
need to refresh the cache for that module without rebuilding the
whole side table.
-}
precomputeOneModule :
    Dict.Dict String FunctionImplementation
    -> Dict.Dict String TcoAnalysis.TcoMetadata
precomputeOneModule funcs =
    Dict.foldl
        (\name impl acc ->
            let
                body : Node Expression
                body =
                    impl.expression

                paramNames : List String
                paramNames =
                    impl.arguments
                        |> List.filterMap
                            (\(Node _ pat) ->
                                case pat of
                                    Elm.Syntax.Pattern.VarPattern p ->
                                        Just p

                                    _ ->
                                        Nothing
                            )
            in
            Dict.insert name
                { strategy = TcoAnalysis.analyze name paramNames body
                , isTailRec = Eval.Expression.isTailRecursive name body
                }
                acc
        )
        Dict.empty
        funcs


{-| For diagnostics: list the (moduleKey, count) pairs sorted by count.
-}
precomputedValuesByModule : ProjectEnv -> List ( String, Int )
precomputedValuesByModule (ProjectEnv projectEnv) =
    projectEnv.env.shared.precomputedValues
        |> Dict.toList
        |> List.map (\( k, inner ) -> ( k, Dict.size inner ))
        |> List.filter (\( _, n ) -> n > 0)


getModulePrecomputedValues : ModuleName -> ProjectEnv -> Dict.Dict String Value
getModulePrecomputedValues moduleName (ProjectEnv projectEnv) =
    Dict.get (Environment.moduleKey moduleName) projectEnv.env.shared.precomputedValues
        |> Maybe.withDefault Dict.empty


setModulePrecomputedValues : ModuleName -> Dict.Dict String Value -> ProjectEnv -> ProjectEnv
setModulePrecomputedValues moduleName values (ProjectEnv projectEnv) =
    let
        env =
            projectEnv.env

        key =
            Environment.moduleKey moduleName
    in
    refreshResolvedGlobals
        (ProjectEnv
            { projectEnv
                | env =
                    { env
                        | shared =
                            { functions = env.shared.functions
                            , moduleImports = env.shared.moduleImports
                            , resolveBridge = env.shared.resolveBridge
                            , precomputedValues = Dict.insert key values env.shared.precomputedValues
                            , tcoAnalyses = env.shared.tcoAnalyses
                            }
                    }
            }
        )


{-| Read the current normalized function dict for a single module out of a
`ProjectEnv`. Returns the post-normalization bodies as they stand right now —
useful for serializing to disk after normalization.
-}
getModuleFunctions : ModuleName -> ProjectEnv -> Dict.Dict String FunctionImplementation
getModuleFunctions moduleName (ProjectEnv projectEnv) =
    Dict.get (Environment.moduleKey moduleName) projectEnv.env.shared.functions
        |> Maybe.withDefault Dict.empty


{-| Install a pre-computed normalized function dict for a module into an
existing `ProjectEnv`, replacing whatever was there before. Used to load
cached normalization results without re-running the pass.
-}
replaceModuleFunctionsInEnv :
    ModuleName
    -> Dict.Dict String FunctionImplementation
    -> ProjectEnv
    -> ProjectEnv
replaceModuleFunctionsInEnv moduleName newFunctions (ProjectEnv projectEnv) =
    let
        env : Env
        env =
            projectEnv.env

        moduleKey : String
        moduleKey =
            Environment.moduleKey moduleName

        updatedFunctions : Dict.Dict String (Dict.Dict String FunctionImplementation)
        updatedFunctions =
            Dict.insert moduleKey newFunctions env.shared.functions

        newEnv : Env
        newEnv =
            { env
                | shared =
                    { functions = updatedFunctions
                    , moduleImports = env.shared.moduleImports
                    , resolveBridge = env.shared.resolveBridge
                    , precomputedValues = env.shared.precomputedValues
                    , tcoAnalyses =
                        Dict.insert moduleKey
                            (precomputeOneModule newFunctions)
                            env.shared.tcoAnalyses
                    }
            }
    in
    ProjectEnv { projectEnv | env = newEnv }


{-| Normalize a single module in place. Returns the updated `ProjectEnv`
along with **only the functions whose body was actually rewritten** — not
the full module function dict. The rewritten-only delta is cheap to
serialize and overlays cleanly on the original parsed bodies, so cache
blobs can stay small.

Uses the existing `ProjectEnv` as both the evaluation context (so package
functions and previously-normalized sibling modules are visible) and the
place to install the rewritten bodies. Also returns the dependency-summary
stats emitted by the normalization passes so callers can profile which
heuristics actually fired on user modules.

-}
normalizeOneModuleInEnvSelected :
    Maybe (Set String)
    -> ModuleName
    -> ProjectEnv
    ->
        { env : ProjectEnv
        , delta : Dict.Dict String FunctionImplementation
        , precomputed : Dict.Dict String Value
        , stats : DependencySummaryStats
        }
normalizeOneModuleInEnvSelected normalizationTargets moduleName (ProjectEnv projectEnv) =
    normalizeOneModuleInEnvSelectedWithFlags NormalizationFlags.experimental normalizationTargets moduleName (ProjectEnv projectEnv)


normalizeOneModuleInEnvSelectedWithFlags :
    NormalizationFlags.NormalizationFlags
    -> Maybe (Set String)
    -> ModuleName
    -> ProjectEnv
    ->
        { env : ProjectEnv
        , delta : Dict.Dict String FunctionImplementation
        , precomputed : Dict.Dict String Value
        , stats : DependencySummaryStats
        }
normalizeOneModuleInEnvSelectedWithFlags flags normalizationTargets moduleName (ProjectEnv projectEnv) =
    let
        env : Env
        env =
            projectEnv.env

        sharedImports : Dict.Dict String ImportedNames
        sharedImports =
            env.shared.moduleImports

        sharedPrecomputedValues : Dict.Dict String (Dict.Dict String Value)
        sharedPrecomputedValues =
            env.shared.precomputedValues

        moduleKey : String
        moduleKey =
            Environment.moduleKey moduleName

        moduleImports : ImportedNames
        moduleImports =
            Dict.get moduleKey sharedImports
                |> Maybe.withDefault emptyImports

        originalModuleFns : Dict.Dict String FunctionImplementation
        originalModuleFns =
            Dict.get moduleKey env.shared.functions
                |> Maybe.withDefault Dict.empty

        originalPrecomputedFns : Dict.Dict String Value
        originalPrecomputedFns =
            Dict.get moduleKey sharedPrecomputedValues
                |> Maybe.withDefault Dict.empty

        normalizationResult =
            runModuleNormalizationToFixpoint
                flags
                normalizationTargets
                moduleName
                moduleKey
                moduleImports
                env.shared.functions
                sharedImports
                sharedPrecomputedValues
                originalModuleFns
                originalPrecomputedFns

        selectedPrecomputed : Dict.Dict String Value
        selectedPrecomputed =
            case normalizationTargets of
                Nothing ->
                    normalizationResult.precomputed

                Just targetNames ->
                    targetNames
                        |> Set.foldl
                            (\name acc ->
                                case Dict.get name normalizationResult.precomputed of
                                    Just value ->
                                        Dict.insert name value acc

                                    Nothing ->
                                        acc
                            )
                            Dict.empty

        updatedFunctions : Dict.Dict String (Dict.Dict String FunctionImplementation)
        updatedFunctions =
            Dict.insert moduleKey normalizationResult.fns env.shared.functions

        updatedPrecomputed : Dict.Dict String (Dict.Dict String Value)
        updatedPrecomputed =
            Dict.insert moduleKey normalizationResult.precomputed sharedPrecomputedValues

        newEnv : Env
        newEnv =
            { env
                | shared =
                    { functions = updatedFunctions
                    , moduleImports = sharedImports
                    , resolveBridge = env.shared.resolveBridge
                    , precomputedValues = updatedPrecomputed
                    , tcoAnalyses =
                        Dict.insert moduleKey
                            (precomputeOneModule normalizationResult.fns)
                            env.shared.tcoAnalyses
                    }
            }
    in
    { env = refreshResolvedGlobals (ProjectEnv { projectEnv | env = newEnv })
    , delta = normalizationResult.delta
    , precomputed = selectedPrecomputed
    , stats = normalizationResult.stats
    }


normalizeOneModuleInEnv :
    ModuleName
    -> ProjectEnv
    -> ( ProjectEnv, Dict.Dict String FunctionImplementation )
normalizeOneModuleInEnv moduleName (ProjectEnv projectEnv) =
    normalizeOneModuleInEnvSelected Nothing moduleName (ProjectEnv projectEnv)
        |> (\result -> ( result.env, result.delta ))


{-| Merge a pre-computed normalization delta into the env, overlaying the
delta entries on top of whatever bodies the env currently has for that
module. Used to load a cached normalization delta without re-running the
pass.
-}
mergeModuleFunctionsIntoEnv :
    ModuleName
    -> Dict.Dict String FunctionImplementation
    -> ProjectEnv
    -> ProjectEnv
mergeModuleFunctionsIntoEnv moduleName deltaFns (ProjectEnv projectEnv) =
    let
        env : Env
        env =
            projectEnv.env

        moduleKey : String
        moduleKey =
            Environment.moduleKey moduleName

        existingModuleFns : Dict.Dict String FunctionImplementation
        existingModuleFns =
            Dict.get moduleKey env.shared.functions
                |> Maybe.withDefault Dict.empty

        merged : Dict.Dict String FunctionImplementation
        merged =
            Dict.union deltaFns existingModuleFns

        updatedFunctions : Dict.Dict String (Dict.Dict String FunctionImplementation)
        updatedFunctions =
            Dict.insert moduleKey merged env.shared.functions

        newEnv : Env
        newEnv =
            { env
                | shared =
                    { functions = updatedFunctions
                    , moduleImports = env.shared.moduleImports
                    , resolveBridge = env.shared.resolveBridge
                    , precomputedValues = env.shared.precomputedValues
                    , tcoAnalyses =
                        Dict.insert moduleKey
                            (precomputeOneModule merged)
                            env.shared.tcoAnalyses
                    }
            }
    in
    ProjectEnv { projectEnv | env = newEnv }


{-| Rewrite the bodies of zero-arg functions in the specified user modules by
running `tryNormalizeConstant` against the current `ProjectEnv`'s function
dict. Package modules and modules not listed are left completely unchanged.
-}
normalizeUserModulesInEnv : List ModuleName -> ProjectEnv -> ProjectEnv
normalizeUserModulesInEnv moduleNames (ProjectEnv projectEnv) =
    let
        env : Env
        env =
            projectEnv.env

        sharedImports : Dict.Dict String ImportedNames
        sharedImports =
            env.shared.moduleImports

        ( updatedFunctions, updatedPrecomputed ) =
            List.foldl
                (\moduleName ( currentFunctions, currentPrecomputed ) ->
                    let
                        moduleKey : String
                        moduleKey =
                            Environment.moduleKey moduleName

                        moduleImports : ImportedNames
                        moduleImports =
                            Dict.get moduleKey sharedImports
                                |> Maybe.withDefault emptyImports

                        originalModuleFns : Dict.Dict String FunctionImplementation
                        originalModuleFns =
                            Dict.get moduleKey currentFunctions
                                |> Maybe.withDefault Dict.empty

                        originalPrecomputedFns : Dict.Dict String Value
                        originalPrecomputedFns =
                            Dict.get moduleKey currentPrecomputed
                                |> Maybe.withDefault Dict.empty

                        ( normalizedModuleFns, normalizedPrecomputedFns ) =
                            originalModuleFns
                                |> Dict.foldl
                                    (\name funcImpl ( fnAcc, precomputedAcc ) ->
                                        if List.isEmpty funcImpl.arguments && isNormalizationCandidate funcImpl.expression then
                                            case
                                                tryNormalizeConstant
                                                    NormalizationFlags.experimental
                                                    moduleName
                                                    moduleKey
                                                    moduleImports
                                                    funcImpl
                                                    (Dict.insert moduleKey fnAcc currentFunctions)
                                                    sharedImports
                                                    (Dict.insert moduleKey precomputedAcc currentPrecomputed)
                                            of
                                                Just ( normalized, value ) ->
                                                    ( Dict.insert name normalized fnAcc
                                                    , Dict.insert name value precomputedAcc
                                                    )

                                                Nothing ->
                                                    ( Dict.insert name funcImpl fnAcc, precomputedAcc )

                                        else
                                            ( Dict.insert name funcImpl fnAcc, precomputedAcc )
                                    )
                                    ( Dict.empty, originalPrecomputedFns )
                    in
                    ( Dict.insert moduleKey normalizedModuleFns currentFunctions
                    , Dict.insert moduleKey normalizedPrecomputedFns currentPrecomputed
                    )
                )
                ( env.shared.functions, env.shared.precomputedValues )
                moduleNames

        newEnv : Env
        newEnv =
            { env
                | shared =
                    { functions = updatedFunctions
                    , moduleImports = env.shared.moduleImports
                    , resolveBridge = env.shared.resolveBridge
                    , precomputedValues = updatedPrecomputed
                    , tcoAnalyses = env.shared.tcoAnalyses
                    }
            }
    in
    refreshResolvedGlobals
        (ProjectEnv
            { projectEnv | env = newEnv }
        )


{-| Like `evalWithEnv`, but returns trace information (call tree + log lines)
alongside the result. Useful for debugging and profiling.
-}
traceWithEnv : ProjectEnv -> List String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
traceWithEnv (ProjectEnv projectEnv) additionalSources expression =
    let
        parseResult =
            additionalSources
                |> List.map
                    (\source ->
                        source
                            |> Elm.Parser.parseToFile
                            |> Result.mapError ParsingError
                            |> Result.andThen
                                (\file ->
                                    Ok
                                        { file = file
                                        , moduleName = fileModuleName file
                                        , interface = buildInterfaceFromFile file
                                        }
                                )
                    )
                |> combineResults
    in
    case parseResult of
        Err e ->
            ( Err e, Rope.empty, Rope.empty )

        Ok parsedModules ->
            let
                additionalInterfaces =
                    parsedModules
                        |> List.map (\m -> ( m.moduleName, m.interface ))
                        |> ElmDict.fromList

                allInterfaces =
                    ElmDict.union additionalInterfaces projectEnv.allInterfaces

                envResult =
                    parsedModules
                        |> Result.MyExtra.combineFoldl
                            (\parsedModule envAcc ->
                                buildModuleEnv allInterfaces parsedModule envAcc
                            )
                            (Ok projectEnv.env)
            in
            case envResult of
                Err e ->
                    ( Err e, Rope.empty, Rope.empty )

                Ok env ->
                    let
                        lastModule =
                            parsedModules
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .moduleName
                                |> Maybe.withDefault [ "Main" ]

                        lastFile =
                            parsedModules
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .file

                        finalImports =
                            case lastFile of
                                Just file ->
                                    (defaultImports ++ file.imports)
                                        |> List.foldl (processImport allInterfaces) emptyImports

                                Nothing ->
                                    emptyImports

                        lastModuleKey =
                            Environment.moduleKey lastModule

                        finalEnv =
                            { env
                                | currentModule = lastModule
                                , currentModuleKey = lastModuleKey
                                , currentModuleFunctions =
                                    Dict.get lastModuleKey env.shared.functions
                                        |> Maybe.withDefault Dict.empty
                                , imports = finalImports
                            }

                        evalResult =
                            Eval.Expression.evalExpression
                                (fakeNode expression)
                                { trace = True, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
                                finalEnv

                        ( result, callTrees, logLines ) =
                            EvalResult.toTriple evalResult
                    in
                    ( Result.mapError Types.EvalError result
                    , callTrees
                    , logLines
                    )


{-| Like `traceWithEnv`, but only collects evaluated source ranges (no full
CallTree). Uses coverage mode in the interpreter to avoid building env/value
data for each expression — dramatically reducing memory for large test suites.
-}
coverageWithEnv : ProjectEnv -> List String -> Expression -> ( Result Error Value, List Range )
coverageWithEnv projectEnv additionalSources expression =
    coverageWithEnvAndLimit Nothing Set.empty projectEnv additionalSources expression


{-| Like `coverageWithEnv` but with probe line filtering. Only records
coverage for expressions on the given lines. When probeLines is empty,
records all expressions (backward compatible).
-}
coverageWithEnvAndLimit : Maybe Int -> Set Int -> ProjectEnv -> List String -> Expression -> ( Result Error Value, List Range )
coverageWithEnvAndLimit maxSteps probeLines (ProjectEnv projectEnv) additionalSources expression =
    let
        parseResult =
            additionalSources
                |> List.map
                    (\source ->
                        source
                            |> Elm.Parser.parseToFile
                            |> Result.mapError ParsingError
                            |> Result.andThen
                                (\file ->
                                    Ok
                                        { file = file
                                        , moduleName = fileModuleName file
                                        , interface = buildInterfaceFromFile file
                                        }
                                )
                    )
                |> combineResults
    in
    case parseResult of
        Err e ->
            ( Err e, [] )

        Ok parsedModules ->
            let
                additionalInterfaces =
                    parsedModules
                        |> List.map (\m -> ( m.moduleName, m.interface ))
                        |> ElmDict.fromList

                allInterfaces =
                    ElmDict.union additionalInterfaces projectEnv.allInterfaces

                envResult =
                    parsedModules
                        |> Result.MyExtra.combineFoldl
                            (\parsedModule envAcc ->
                                buildModuleEnv allInterfaces parsedModule envAcc
                            )
                            (Ok projectEnv.env)
            in
            case envResult of
                Err e ->
                    ( Err e, [] )

                Ok env ->
                    let
                        lastModule =
                            parsedModules
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .moduleName
                                |> Maybe.withDefault [ "Main" ]

                        lastFile =
                            parsedModules
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .file

                        finalImports =
                            case lastFile of
                                Just file ->
                                    (defaultImports ++ file.imports)
                                        |> List.foldl (processImport allInterfaces) emptyImports

                                Nothing ->
                                    emptyImports

                        lastModuleKey =
                            Environment.moduleKey lastModule

                        finalEnv =
                            { env
                                | currentModule = lastModule
                                , currentModuleKey = lastModuleKey
                                , currentModuleFunctions =
                                    Dict.get lastModuleKey env.shared.functions
                                        |> Maybe.withDefault Dict.empty
                                , imports = finalImports
                            }

                        evalResult =
                            Eval.Expression.evalExpression
                                (fakeNode expression)
                                { trace = False, coverage = True, coverageProbeLines = probeLines, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
                                finalEnv

                        ( result, coverageSet ) =
                            EvalResult.toCoverageSet evalResult
                    in
                    ( Result.mapError Types.EvalError result
                    , Set.toList coverageSet |> List.map Types.unpackRange
                    )


buildInitialEnv : File -> Result Error Env
buildInitialEnv file =
    let
        moduleName : ModuleName
        moduleName =
            fileModuleName file

        coreInterfaces : ElmDict.Dict ModuleName (List Exposed)
        coreInterfaces =
            Core.dependency.interfaces

        imports : ImportedNames
        imports =
            (defaultImports ++ file.imports)
                |> List.foldl (processImport coreInterfaces) emptyImports

        -- Pre-populate moduleImports for every core module with the
        -- default import set so cross-module calls INTO elm/core (Array,
        -- List, Dict, …) don't fall back to the CALLER's imports — which
        -- would incorrectly leak the caller's aliases (e.g. a
        -- `import Array.Extra as Array` in the caller would rewrite
        -- qualified `Array_elm_builtin` references inside Array.elm's own
        -- code to `Array.Extra.Array_elm_builtin`). The default imports
        -- (`import Basics exposing (..)`, etc.) are the ones every Elm
        -- module has implicitly, so a core module's unqualified
        -- references to `ceiling`, `not`, `identity`, etc. still resolve.
        defaultProcessedImports : ImportedNames
        defaultProcessedImports =
            defaultImports
                |> List.foldl (processImport coreInterfaces) emptyImports

        coreModuleImports : Dict.Dict String ImportedNames
        coreModuleImports =
            coreFunctions
                |> Dict.foldl (\moduleKey _ acc -> Dict.insert moduleKey defaultProcessedImports acc) Dict.empty

        coreEnv : Env
        coreEnv =
            { currentModule = moduleName
            , currentModuleKey = Environment.moduleKey moduleName
            , callStack = []
            , shared =
                { functions = coreFunctions
                , moduleImports =
                    coreModuleImports
                        |> Dict.insert (Environment.moduleKey moduleName) imports
                , resolveBridge = Types.noResolveBridge
                , precomputedValues = Dict.empty
                , tcoAnalyses = Dict.empty
                }
            , currentModuleFunctions = Dict.empty
            , letFunctions = Dict.empty
            , values = Dict.empty
            , imports = imports
            , callDepth = 0
            , recursionCheck = Nothing
            }

        addDeclaration : Node Declaration -> Env -> Result Error Env
        addDeclaration (Node _ decl) env =
            case decl of
                FunctionDeclaration function ->
                    let
                        (Node _ implementation) =
                            function.declaration
                    in
                    Ok (Environment.addFunction moduleName implementation env)

                PortDeclaration _ ->
                    Ok env

                InfixDeclaration _ ->
                    Ok env

                Destructuring _ _ ->
                    Err <| Types.EvalError <| unsupported env "Top level destructuring"

                AliasDeclaration alias_ ->
                    Ok (registerRecordAliasConstructor moduleName alias_ env)

                CustomTypeDeclaration customType ->
                    Ok (registerConstructors moduleName customType env)
    in
    file.declarations
        |> Result.MyExtra.combineFoldl
            addDeclaration
            (Ok coreEnv)


emptyImports : ImportedNames
emptyImports =
    { aliases = Dict.empty
    , exposedValues = Dict.empty
    , exposedConstructors = Dict.empty
    }


processImport : ElmDict.Dict ModuleName (List Exposed) -> Node Elm.Syntax.Import.Import -> ImportedNames -> ImportedNames
processImport allInterfaces (Node _ imp) acc =
    let
        canonicalName : ModuleName
        canonicalName =
            Node.value imp.moduleName

        canonicalKey : String
        canonicalKey =
            Environment.moduleKey canonicalName

        canonicalPair : ( ModuleName, String )
        canonicalPair =
            ( canonicalName, canonicalKey )

        -- Always register the full module name as an alias to itself
        withFullName : ImportedNames
        withFullName =
            { acc | aliases = Dict.insert canonicalKey canonicalPair acc.aliases }

        -- If there's an alias, also register alias -> canonical
        withAlias : ImportedNames
        withAlias =
            case imp.moduleAlias of
                Just (Node _ alias_) ->
                    { withFullName | aliases = Dict.insert (Environment.moduleKey alias_) canonicalPair withFullName.aliases }

                Nothing ->
                    withFullName
    in
    case imp.exposingList of
        Nothing ->
            withAlias

        Just (Node _ (All _)) ->
            -- exposing (..) - expose everything from the module's interface
            case ElmDict.get canonicalName allInterfaces of
                Nothing ->
                    withAlias

                Just interface ->
                    List.foldl (exposeFromInterface canonicalPair) withAlias interface

        Just (Node _ (Explicit items)) ->
            List.foldl (exposeExplicitItem allInterfaces canonicalPair) withAlias items


exposeFromInterface : ( ModuleName, String ) -> Exposed -> ImportedNames -> ImportedNames
exposeFromInterface moduleNameWithKey exposed acc =
    case exposed of
        Elm.Interface.Function name ->
            { acc | exposedValues = Dict.insert name moduleNameWithKey acc.exposedValues }

        Elm.Interface.CustomType ( _, constructors ) ->
            let
                newConstructors : Dict.Dict String ( ModuleName, String )
                newConstructors =
                    List.foldl (\ctor d -> Dict.insert ctor moduleNameWithKey d) acc.exposedConstructors constructors
            in
            { acc | exposedConstructors = newConstructors }

        Elm.Interface.Alias name ->
            { acc | exposedValues = Dict.insert name moduleNameWithKey acc.exposedValues }

        Elm.Interface.Operator _ ->
            acc


exposeExplicitItem : ElmDict.Dict ModuleName (List Exposed) -> ( ModuleName, String ) -> Node TopLevelExpose -> ImportedNames -> ImportedNames
exposeExplicitItem allInterfaces (( moduleName, _ ) as moduleNameWithKey) (Node _ item) acc =
    case item of
        FunctionExpose name ->
            { acc | exposedValues = Dict.insert name moduleNameWithKey acc.exposedValues }

        TypeOrAliasExpose name ->
            -- Record type aliases auto-generate a constructor function
            -- that must be reachable via unqualified reference
            -- (e.g. `import Elm.Syntax.Signature exposing (Signature)`
            -- must let `Signature a b` resolve to the record-building
            -- function, matching what `exposing (..)` already provides
            -- through `exposeFromInterface`). Custom types without `(..)`
            -- register nothing in globalIds, so this is a no-op lookup
            -- for them at resolve time.
            if isAliasInInterface allInterfaces moduleName name then
                { acc | exposedValues = Dict.insert name moduleNameWithKey acc.exposedValues }

            else
                acc

        TypeExpose { name, open } ->
            case open of
                Nothing ->
                    -- Type without (..) - doesn't expose constructors
                    acc

                Just _ ->
                    -- Type(..) - expose all constructors
                    case ElmDict.get moduleName allInterfaces of
                        Nothing ->
                            acc

                        Just interface ->
                            let
                                constructors : List String
                                constructors =
                                    interface
                                        |> List.filterMap
                                            (\exposed ->
                                                case exposed of
                                                    Elm.Interface.CustomType ( typeName, ctors ) ->
                                                        if typeName == name then
                                                            Just ctors

                                                        else
                                                            Nothing

                                                    _ ->
                                                        Nothing
                                            )
                                        |> List.concat
                            in
                            List.foldl (\ctor d -> { d | exposedConstructors = Dict.insert ctor moduleNameWithKey d.exposedConstructors }) acc constructors

        InfixExpose _ ->
            acc


isAliasInInterface : ElmDict.Dict ModuleName (List Exposed) -> ModuleName -> String -> Bool
isAliasInInterface allInterfaces moduleName name =
    case ElmDict.get moduleName allInterfaces of
        Nothing ->
            False

        Just interface ->
            List.any
                (\exposed ->
                    case exposed of
                        Elm.Interface.Alias aliasName ->
                            aliasName == name

                        _ ->
                            False
                )
                interface


defaultImports : List (Node Elm.Syntax.Import.Import)
defaultImports =
    [ -- Internal core aliases (baked into Core.functions ASTs)
      makeImport [ "Elm", "JsArray" ] (Just [ "JsArray" ]) Nothing

    -- import Basics exposing (..)
    , makeImport [ "Basics" ] Nothing (Just (All Elm.Syntax.Range.emptyRange))

    -- import List exposing (List, (::))
    , makeImport [ "List" ] Nothing (Just (Explicit [ fakeNode (TypeOrAliasExpose "List"), fakeNode (InfixExpose "::") ]))

    -- import Maybe exposing (Maybe(..))
    , makeImport [ "Maybe" ] Nothing (Just (Explicit [ fakeNode (TypeExpose { name = "Maybe", open = Just Elm.Syntax.Range.emptyRange }) ]))

    -- import Result exposing (Result(..))
    , makeImport [ "Result" ] Nothing (Just (Explicit [ fakeNode (TypeExpose { name = "Result", open = Just Elm.Syntax.Range.emptyRange }) ]))

    -- import String
    , makeImport [ "String" ] Nothing Nothing

    -- import Char
    , makeImport [ "Char" ] Nothing Nothing

    -- import Tuple
    , makeImport [ "Tuple" ] Nothing Nothing

    -- import Debug
    , makeImport [ "Debug" ] Nothing Nothing

    -- import Platform
    , makeImport [ "Platform" ] Nothing Nothing

    -- import Platform.Cmd exposing (Cmd)
    , makeImport [ "Platform", "Cmd" ] Nothing (Just (Explicit [ fakeNode (TypeOrAliasExpose "Cmd") ]))

    -- import Platform.Sub exposing (Sub)
    , makeImport [ "Platform", "Sub" ] Nothing (Just (Explicit [ fakeNode (TypeOrAliasExpose "Sub") ]))
    ]


makeImport : ModuleName -> Maybe ModuleName -> Maybe Exposing -> Node Elm.Syntax.Import.Import
makeImport moduleName maybeAlias maybeExposing =
    fakeNode
        { moduleName = fakeNode moduleName
        , moduleAlias = Maybe.map fakeNode maybeAlias
        , exposingList = Maybe.map fakeNode maybeExposing
        }


{-| Evaluate an expression in the context of multiple modules.
Modules should be provided in dependency order (dependencies before dependents).
The expression is evaluated in the context of the last module.
-}
evalProject : List String -> Expression -> Result Error Value
evalProject sources expression =
    let
        parseResult :
            Result
                Error
                (List
                    { file : File
                    , moduleName : ModuleName
                    , interface : List Exposed
                    }
                )
        parseResult =
            sources
                |> List.map
                    (\source ->
                        source
                            |> Elm.Parser.parseToFile
                            |> Result.mapError ParsingError
                            |> Result.andThen
                                (\file ->
                                    let
                                        modName : ModuleName
                                        modName =
                                            fileModuleName file
                                    in
                                    Ok
                                        { file = file
                                        , moduleName = modName
                                        , interface = buildInterfaceFromFile file
                                        }
                                )
                    )
                |> combineResults
    in
    case parseResult of
        Err e ->
            Err e

        Ok parsedModules ->
            let
                -- Build combined interfaces: core + all user modules
                userInterfaces : ElmDict.Dict ModuleName (List Exposed)
                userInterfaces =
                    parsedModules
                        |> List.map (\m -> ( m.moduleName, m.interface ))
                        |> ElmDict.fromList

                allInterfaces : ElmDict.Dict ModuleName (List Exposed)
                allInterfaces =
                    ElmDict.union userInterfaces Core.dependency.interfaces

                -- Build env with all modules' functions
                envResult : Result Error Env
                envResult =
                    parsedModules
                        |> Result.MyExtra.combineFoldl
                            (\parsedModule envAcc ->
                                buildModuleEnv allInterfaces parsedModule envAcc
                            )
                            (Ok
                                { currentModule = []
                                , currentModuleKey = ""
                                , callStack = []
                                , shared = { functions = coreFunctions, moduleImports = Dict.empty, resolveBridge = Types.noResolveBridge, precomputedValues = Dict.empty, tcoAnalyses = Dict.empty }
                                , currentModuleFunctions = Dict.empty
                                , letFunctions = Dict.empty
                                , values = Dict.empty
                                , imports = emptyImports
                                , callDepth = 0
                                , recursionCheck = Nothing
                                }
                            )
            in
            case envResult of
                Err e ->
                    Err e

                Ok env ->
                    let
                        -- Use the last module as the evaluation context
                        lastModule : ModuleName
                        lastModule =
                            parsedModules
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .moduleName
                                |> Maybe.withDefault [ "Main" ]

                        -- Process imports for the last module
                        lastFile : Maybe File
                        lastFile =
                            parsedModules
                                |> List.reverse
                                |> List.head
                                |> Maybe.map .file

                        finalImports : ImportedNames
                        finalImports =
                            case lastFile of
                                Just file ->
                                    (defaultImports ++ file.imports)
                                        |> List.foldl (processImport allInterfaces) emptyImports

                                Nothing ->
                                    emptyImports

                        lastModuleKey : String
                        lastModuleKey =
                            Environment.moduleKey lastModule

                        finalEnv : Env
                        finalEnv =
                            { env
                                | currentModule = lastModule
                                , currentModuleKey = lastModuleKey
                                , currentModuleFunctions =
                                    Dict.get lastModuleKey env.shared.functions
                                        |> Maybe.withDefault Dict.empty
                                , imports = finalImports
                            }

                        result : Result Types.EvalErrorData Value
                        result =
                            Eval.Expression.evalExpression
                                (fakeNode expression)
                                { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
                                finalEnv
                                |> EvalResult.toResult
                    in
                    Result.mapError Types.EvalError result


buildModuleEnv :
    ElmDict.Dict ModuleName (List Exposed)
    ->
        { file : File
        , moduleName : ModuleName
        , interface : List Exposed
        }
    -> Env
    -> Result Error Env
buildModuleEnv allInterfaces { file, moduleName } env =
    let
        -- Process this module's imports
        moduleImportedNames : ImportedNames
        moduleImportedNames =
            (defaultImports ++ file.imports)
                |> List.foldl (processImport allInterfaces) emptyImports

        envWithModuleImports : Env
        envWithModuleImports =
            { env | shared = { functions = env.shared.functions, moduleImports = Dict.insert (Environment.moduleKey moduleName) moduleImportedNames env.shared.moduleImports, resolveBridge = env.shared.resolveBridge, precomputedValues = env.shared.precomputedValues, tcoAnalyses = env.shared.tcoAnalyses } }

        addDeclaration : Node Declaration -> Env -> Result Error Env
        addDeclaration (Node _ decl) envAcc =
            case decl of
                FunctionDeclaration function ->
                    let
                        (Node _ implementation) =
                            function.declaration
                    in
                    Ok (Environment.addFunction moduleName implementation envAcc)

                CustomTypeDeclaration customType ->
                    Ok (registerConstructors moduleName customType envAcc)

                PortDeclaration _ ->
                    Ok envAcc

                InfixDeclaration _ ->
                    Ok envAcc

                Destructuring _ _ ->
                    Err <| Types.EvalError <| unsupported envAcc "Top level destructuring"

                AliasDeclaration alias_ ->
                    Ok (registerRecordAliasConstructor moduleName alias_ envAcc)
    in
    file.declarations
        |> Result.MyExtra.combineFoldl addDeclaration (Ok envWithModuleImports)


cachedSummaryFromParsedModule :
    ElmDict.Dict ModuleName (List Exposed)
    ->
        { file : File
        , moduleName : ModuleName
        , interface : List Exposed
        }
    -> CachedModuleSummary
cachedSummaryFromParsedModule allInterfaces { file, moduleName, interface } =
    { moduleName = moduleName
    , interface = interface
    , importedNames =
        (defaultImports ++ file.imports)
            |> List.foldl (processImport allInterfaces) emptyImports
    , functions = moduleFunctionImplementations moduleName file
    }


moduleFunctionImplementations : ModuleName -> File -> List FunctionImplementation
moduleFunctionImplementations moduleName file =
    file.declarations
        |> List.concatMap
            (\(Node _ decl) ->
                case decl of
                    FunctionDeclaration function ->
                        [ Node.value function.declaration ]

                    AliasDeclaration alias_ ->
                        recordAliasConstructorImplementation alias_
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []

                    CustomTypeDeclaration customType ->
                        constructorImplementations moduleName customType

                    PortDeclaration _ ->
                        []

                    InfixDeclaration _ ->
                        []

                    Destructuring _ _ ->
                        []
            )


{-| If a type alias has a record type annotation, register its name as a
function that constructs a Record value from positional arguments.

    type alias Point =
        { x : Int, y : Int }

registers `Point` as a 2-argument function whose body is
`RecordExpr [("x", $arg0), ("y", $arg1)]`.

-}
registerRecordAliasConstructor : ModuleName -> TypeAlias -> Env -> Env
registerRecordAliasConstructor moduleName alias_ env =
    case recordAliasConstructorImplementation alias_ of
        Just implementation ->
            Environment.addFunction moduleName implementation env

        Nothing ->
            env


recordAliasConstructorImplementation : TypeAlias -> Maybe Elm.Syntax.Expression.FunctionImplementation
recordAliasConstructorImplementation alias_ =
    case Node.value alias_.typeAnnotation of
        Record fields ->
            let
                aliasName : String
                aliasName =
                    Node.value alias_.name

                fieldNames : List String
                fieldNames =
                    List.map (\(Node _ ( Node _ fieldName, _ )) -> fieldName) fields

                argNames : List String
                argNames =
                    List.indexedMap (\i _ -> "$alias_arg" ++ String.fromInt i) fieldNames
            in
            Just
                { name = fakeNode aliasName
                , arguments =
                    argNames
                        |> List.map (\n -> fakeNode (Elm.Syntax.Pattern.VarPattern n))
                , expression =
                    fakeNode
                        (RecordExpr
                            (List.map2
                                (\fieldName argName ->
                                    fakeNode ( fakeNode fieldName, fakeNode (FunctionOrValue [] argName) )
                                )
                                fieldNames
                                argNames
                            )
                        )
                }

        _ ->
            Nothing


{-| Register constructors from a custom type declaration as functions in env.
Each constructor becomes a function that creates a Custom value.
-}
registerConstructors :
    ModuleName
    -> Elm.Syntax.Type.Type
    -> Env
    -> Env
registerConstructors moduleName customType env =
    constructorImplementations moduleName customType
        |> List.foldl (Environment.addFunction moduleName) env


constructorImplementations :
    ModuleName
    -> Elm.Syntax.Type.Type
    -> List Elm.Syntax.Expression.FunctionImplementation
constructorImplementations moduleName customType =
    customType.constructors
        |> List.map
            (\(Node _ ctor) ->
                let
                    ctorName : String
                    ctorName =
                        Node.value ctor.name

                    arity : Int
                    arity =
                        List.length ctor.arguments

                    argNames : List String
                    argNames =
                        List.range 0 (arity - 1)
                            |> List.map (\i -> "$ctor_arg" ++ String.fromInt i)
                in
                { name = fakeNode ctorName
                , arguments =
                    argNames
                        |> List.map (\n -> fakeNode (Elm.Syntax.Pattern.VarPattern n))
                , expression =
                    if arity == 0 then
                        fakeNode (FunctionOrValue moduleName ctorName)

                    else
                        fakeNode
                            (Application
                                (fakeNode (FunctionOrValue moduleName ctorName)
                                    :: List.map
                                        (\n -> fakeNode (FunctionOrValue [] n))
                                        argNames
                                )
                            )
                }
            )


{-| Build an interface from a parsed File by examining its declarations and exposing list.
-}
buildInterfaceFromFile : File -> List Exposed
buildInterfaceFromFile file =
    let
        allDeclarations : List ( String, Exposed )
        allDeclarations =
            List.filterMap
                (\(Node _ decl) ->
                    case decl of
                        FunctionDeclaration f ->
                            let
                                (Node _ impl) =
                                    f.declaration
                            in
                            Just ( Node.value impl.name, Elm.Interface.Function (Node.value impl.name) )

                        CustomTypeDeclaration t ->
                            Just
                                ( Node.value t.name
                                , Elm.Interface.CustomType
                                    ( Node.value t.name
                                    , t.constructors
                                        |> List.map (Node.value >> .name >> Node.value)
                                    )
                                )

                        AliasDeclaration a ->
                            Just ( Node.value a.name, Elm.Interface.Alias (Node.value a.name) )

                        _ ->
                            Nothing
                )
                file.declarations

        declarationDict : ElmDict.Dict String Exposed
        declarationDict =
            ElmDict.fromList allDeclarations

        exposingList : Elm.Syntax.Exposing.Exposing
        exposingList =
            case Node.value file.moduleDefinition of
                NormalModule normal ->
                    Node.value normal.exposingList

                PortModule port_ ->
                    Node.value port_.exposingList

                EffectModule effect ->
                    Node.value effect.exposingList
    in
    case exposingList of
        All _ ->
            List.map Tuple.second allDeclarations

        Explicit items ->
            List.filterMap
                (\(Node _ expose) ->
                    case expose of
                        FunctionExpose name ->
                            Just (Elm.Interface.Function name)

                        TypeOrAliasExpose name ->
                            -- Expose the type but without constructors
                            case ElmDict.get name declarationDict of
                                Just (Elm.Interface.CustomType ( typeName, _ )) ->
                                    Just (Elm.Interface.CustomType ( typeName, [] ))

                                other ->
                                    other

                        TypeExpose { name, open } ->
                            case open of
                                Just _ ->
                                    -- TypeName(..) - expose with constructors
                                    ElmDict.get name declarationDict

                                Nothing ->
                                    -- TypeName without (..) - no constructors
                                    case ElmDict.get name declarationDict of
                                        Just (Elm.Interface.CustomType ( typeName, _ )) ->
                                            Just (Elm.Interface.CustomType ( typeName, [] ))

                                        other ->
                                            other

                        InfixExpose _ ->
                            Nothing
                )
                items


fileModuleName : File -> ModuleName
fileModuleName file =
    case Node.value file.moduleDefinition of
        NormalModule normal ->
            Node.value normal.moduleName

        PortModule port_ ->
            Node.value port_.moduleName

        EffectModule effect ->
            Node.value effect.moduleName


combineResults : List (Result e a) -> Result e (List a)
combineResults results =
    List.foldr
        (\result acc ->
            case ( result, acc ) of
                ( Ok val, Ok list ) ->
                    Ok (val :: list)

                ( Err e, _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err e
        )
        (Ok [])
        results
