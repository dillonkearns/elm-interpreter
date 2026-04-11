module Eval.Module exposing (CachedModuleSummary, ProjectEnv, ResolveErrorEntry, ResolvedProject, buildCachedModuleSummariesFromParsed, buildInterfaceFromFile, buildProjectEnv, buildProjectEnvFromParsed, buildProjectEnvFromSummaries, eval, evalProject, evalWithEnv, evalWithEnvAndLimit, evalWithEnvFromFiles, evalWithEnvFromFilesAndLimit, evalWithEnvFromFilesAndMemo, evalWithEnvFromFilesAndValues, evalWithEnvFromFilesAndValuesAndMemo, evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw, evalWithEnvFromFilesAndValuesAndInterceptsRaw, evalWithIntercepts, evalWithInterceptsAndMemoRaw, evalWithInterceptsRaw, evalWithMemoizedFunctions, evalWithResolvedIR, evalWithResolvedIRExpression, evalWithResolvedIRFromFilesAndIntercepts, evalWithValuesAndMemoizedFunctions, extendWithFiles, fileModuleName, handleInternalMemoLookup, handleInternalMemoStore, handleInternalMemoYield, parseProjectSources, projectEnvResolved, replaceModuleInEnv, trace, traceOrEvalModule, traceWithEnv)

import Bitwise
import Core
import Dict as ElmDict
import Elm.Interface exposing (Exposed)
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.Import
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Environment
import Eval.Expression
import Eval.NativeDispatch as NativeDispatch
import Eval.ResolvedExpression as RE
import Eval.ResolvedIR as IR
import Eval.Resolver as Resolver
import FastDict as Dict
import List.Extra
import MemoRuntime
import MemoSpec
import Result.MyExtra
import Rope exposing (Rope)
import Set exposing (Set)
import Syntax exposing (fakeNode)
import EvalResult
import Types exposing (CallTree, Env, Error(..), EvalResult(..), ImportedNames, Value)
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


emptyResolvedProject : ResolvedProject
emptyResolvedProject =
    { globalIds = Dict.empty
    , bodies = Dict.empty
    , globalIdToName = Dict.empty
    , nativeDispatchers = Dict.empty
    , higherOrderDispatchers = Dict.empty
    , kernelDispatchers = Dict.empty
    , errors = []
    }


{-| Accessor for the resolver output attached to a ProjectEnv. Mostly for
tests and diagnostics — Phase 3's evaluator reaches into this directly
through its own code path inside this module.
-}
projectEnvResolved : ProjectEnv -> ResolvedProject
projectEnvResolved (ProjectEnv projectEnv) =
    projectEnv.resolved


{-| Detect the "direct kernel wrapper" pattern in a Core-module
FunctionImplementation: the body is a bare `FunctionOrValue
["Elm", "Kernel", ModuleName, ...] name`, with no arguments on the
wrapper itself. Returns `Just (kernelModuleName, kernelName)` when
the pattern matches.

This catches most of `Core.Basics.*`, `Core.String.*`, `Core.Char.*`,
etc. where the wrapper is just a pass-through to the kernel. Core
functions with non-kernel bodies (like `List.map` implemented as a
fold in Elm) don't match and are handled via regular delegation.

-}
extractKernelReference : Elm.Syntax.Expression.FunctionImplementation -> Maybe ( ModuleName, String )
extractKernelReference impl =
    if List.isEmpty impl.arguments then
        case Node.value impl.expression of
            FunctionOrValue (("Elm" :: "Kernel" :: _) as kernelModuleName) kernelName ->
                Just ( kernelModuleName, kernelName )

            _ ->
                Nothing

    else
        Nothing


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
                    , globals = Dict.empty
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
walk user declarations again to resolve their bodies to `RExpr`. Core
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

        globalIds : Dict.Dict ( ModuleName, String ) IR.GlobalId
        globalIds =
            allIdAssignment.ids

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

        -- Build the kernel dispatcher registry by walking Core.functions
        -- and finding entries whose body is a direct kernel reference
        -- (the common "wrapper" pattern). For each such entry, precompute
        -- the (arity, kernelFn) tuple so the new evaluator can call the
        -- kernel function directly at delegation time, skipping the
        -- AST synthesis round-trip.
        kernelDispatchers : Dict.Dict IR.GlobalId KernelDispatcher
        kernelDispatchers =
            Core.functions
                |> Dict.foldl
                    (\coreModuleName moduleDict outer ->
                        moduleDict
                            |> Dict.foldl
                                (\funcName impl acc ->
                                    case extractKernelReference impl of
                                        Just ( kernelModuleName, kernelName ) ->
                                            let
                                                kernelModuleKey : String
                                                kernelModuleKey =
                                                    Environment.moduleKey kernelModuleName
                                            in
                                            case Dict.get kernelModuleKey Eval.Expression.kernelFunctions of
                                                Just kernelModule ->
                                                    case Dict.get kernelName kernelModule of
                                                        Just ( arity, kernelFn ) ->
                                                            case Dict.get ( coreModuleName, funcName ) globalIds of
                                                                Just id ->
                                                                    Dict.insert id
                                                                        { arity = arity
                                                                        , kernelFn = kernelFn
                                                                        }
                                                                        acc

                                                                Nothing ->
                                                                    acc

                                                        Nothing ->
                                                            acc

                                                Nothing ->
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
            , errors = []
            }
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
                                        Resolver.initContext summary.moduleName globalIds
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


eval : String -> Expression -> Result Error Value
eval source expression =
    let
        ( result, _, _ ) =
            traceOrEvalModule { trace = False, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False } source expression
    in
    result


trace : String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
trace source expression =
    traceOrEvalModule { trace = True, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False } source expression


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
        Result Error
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

        env : Env
        env =
            { currentModule = []
            , currentModuleKey = ""
            , callStack = []
            , shared = { functions = sharedFunctions, moduleImports = sharedModuleImports, resolveBridge = Types.noResolveBridge }
            , currentModuleFunctions = Dict.empty
            , letFunctions = Dict.empty
            , values = Dict.empty
            , imports = emptyImports
            , callDepth = 0
            , recursionCheck = Nothing
            }
    in
    Ok
        (ProjectEnv
            { env = env
            , allInterfaces = allInterfaces
            , resolved = resolveProject summaries
            }
        )


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
                ProjectEnv
                    { env = env
                    , allInterfaces = updatedInterfaces
                    , resolved = projectEnv.resolved
                    }
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
-}
evalWithEnvAndLimit : Maybe Int -> ProjectEnv -> List String -> Expression -> Result Error Value
evalWithEnvAndLimit maxSteps (ProjectEnv projectEnv) additionalSources expression =
    let
        parseResult :
            Result Error
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

                -- User interfaces take precedence (first arg of union)
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
                                { trace = False, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
                                finalEnv
                                |> EvalResult.toResult
                    in
                    Result.mapError Types.EvalError result


{-| Like `evalWithEnvFromFiles`, but with an optional step limit.
Pass `Just n` to limit evaluation to `n` trampoline steps (prevents infinite
loops from consuming unbounded memory). Pass `Nothing` for unlimited.
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
                        { trace = False, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
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
        Result Error
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
                        { trace = False, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
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
        Result Error
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
        Result Error
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
        Result Error
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
        Result Error
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


{-| Combined: injected Values + intercepts + raw EvalResult.
Used by the BackendTask yield driver when both Value injection and
intercept handling are needed in the same evaluation.
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
                , maxSteps = Nothing
                , tcoTarget = Nothing
                , callCounts = Nothing
                , intercepts = intercepts
                , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                , collectMemoStats = False
                , useResolvedIR = False
                }
                finalEnv


{-| Resolved-IR variant of `evalWithEnvFromFilesAndValuesAndInterceptsRaw`.

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
evalWithResolvedIRFromFilesAndIntercepts (ProjectEnv projectEnv) additionalFiles injectedValues intercepts expression =
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
                                    , globals = Dict.empty
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
                    , maxSteps = Nothing
                    , tcoTarget = Nothing
                    , callCounts = Nothing
                    , intercepts = intercepts
                    , memoizedFunctions = MemoSpec.emptyRegistry
                    , collectMemoStats = False
                    , useResolvedIR = False
                    }

                resolverCtx : Resolver.ResolverContext
                resolverCtx =
                    Resolver.initContext lastModule mergedGlobalIds
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
                            , globals = Dict.empty
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
                ProjectEnv
                    { env = env
                    , allInterfaces = allInterfaces
                    , resolved = projectEnv.resolved
                    }
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
                                { trace = True, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
                                finalEnv

                        ( result, callTrees, logLines ) =
                            EvalResult.toTriple evalResult
                    in
                    ( Result.mapError Types.EvalError result
                    , callTrees
                    , logLines
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

        coreEnv : Env
        coreEnv =
            { currentModule = moduleName
            , currentModuleKey = Environment.moduleKey moduleName
            , callStack = []
            , shared = { functions = coreFunctions, moduleImports = Dict.singleton (Environment.moduleKey moduleName) imports, resolveBridge = Types.noResolveBridge }
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

        TypeOrAliasExpose _ ->
            -- Type name without constructors - doesn't add values
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
            Result Error
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
                                , shared = { functions = coreFunctions, moduleImports = Dict.empty, resolveBridge = Types.noResolveBridge }
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
                                { trace = False, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False, useResolvedIR = False }
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
            { env | shared = { functions = env.shared.functions, moduleImports = Dict.insert (Environment.moduleKey moduleName) moduleImportedNames env.shared.moduleImports, resolveBridge = env.shared.resolveBridge } }

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

    type alias Point = { x : Int, y : Int }

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
