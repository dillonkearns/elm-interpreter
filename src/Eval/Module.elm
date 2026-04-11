module Eval.Module exposing (CachedModuleSummary, ProjectEnv, buildCachedModuleSummariesFromParsed, buildInterfaceFromFile, buildProjectEnv, buildProjectEnvFromParsed, buildProjectEnvFromSummaries, coverageWithEnv, coverageWithEnvAndLimit, eval, evalProject, evalWithEnv, evalWithEnvAndLimit, evalWithEnvFromFiles, evalWithEnvFromFilesAndLimit, evalWithEnvFromFilesAndMemo, evalWithEnvFromFilesAndValues, evalWithEnvFromFilesAndValuesAndMemo, evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw, evalWithEnvFromFilesAndValuesAndInterceptsRaw, evalWithIntercepts, evalWithInterceptsAndMemoRaw, evalWithInterceptsRaw, evalWithMemoizedFunctions, evalWithValuesAndMemoizedFunctions, extendWithFiles, extendWithFilesNormalized, fileModuleName, getModuleFunctions, handleInternalMemoLookup, handleInternalMemoStore, handleInternalMemoYield, mergeModuleFunctionsIntoEnv, normalizeOneModuleInEnv, normalizeSummaries, normalizeUserModulesInEnv, parseProjectSources, precomputedValuesByModule, precomputedValuesCount, replaceModuleFunctionsInEnv, replaceModuleInEnv, trace, traceOrEvalModule, traceWithEnv)

import Array
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
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Environment
import Eval.Expression
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


eval : String -> Expression -> Result Error Value
eval source expression =
    let
        ( result, _, _ ) =
            traceOrEvalModule { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False } source expression
    in
    result


trace : String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
trace source expression =
    traceOrEvalModule { trace = True, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False } source expression


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
            , shared =
                { functions = sharedFunctions
                , moduleImports = sharedModuleImports
                , precomputedValues = Dict.empty
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
        (ProjectEnv
            { env = env
            , allInterfaces = allInterfaces
            }
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
normalizeSummaries : List CachedModuleSummary -> List CachedModuleSummary
normalizeSummaries summaries =
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

        ( normalizedFunctions, _ ) =
            normalizeTopLevelConstants summaries sharedFunctionsBefore sharedModuleImports
    in
    summaries
        |> List.map
            (\summary ->
                let
                    moduleKey : String
                    moduleKey =
                        Environment.moduleKey summary.moduleName

                    updatedFns : Dict.Dict String FunctionImplementation
                    updatedFns =
                        Dict.get moduleKey normalizedFunctions
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
    List CachedModuleSummary
    -> Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String ImportedNames
    -> ( Dict.Dict String (Dict.Dict String FunctionImplementation)
       , Dict.Dict String (Dict.Dict String Value)
       )
normalizeTopLevelConstants summaries initialFunctions sharedImports =
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
    ModuleName
    -> String
    -> ImportedNames
    -> FunctionImplementation
    -> Dict.Dict String (Dict.Dict String FunctionImplementation)
    -> Dict.Dict String ImportedNames
    -> Dict.Dict String (Dict.Dict String Value)
    -> Maybe ( FunctionImplementation, Value )
tryNormalizeConstant moduleName moduleKey moduleImports funcImpl sharedFunctions sharedModuleImports sharedPrecomputedValues =
    let
        env : Env
        env =
            { currentModule = moduleName
            , currentModuleKey = moduleKey
            , callStack = []
            , shared =
                { functions = sharedFunctions
                , moduleImports = sharedModuleImports
                , precomputedValues = sharedPrecomputedValues
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
            , maxSteps = Just 10000000
            , tcoTarget = Nothing
            , callCounts = Nothing
            , intercepts = Dict.empty
            , memoizedFunctions = MemoSpec.emptyRegistry
            , collectMemoStats = False
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
functions whose bodies changed), and the per-name precomputed `Value`
dict. All three are keyed on the simple function name.
-}
runModuleNormalizationToFixpoint :
    ModuleName
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
runModuleNormalizationToFixpoint moduleName moduleKey moduleImports sharedFunctions sharedImports sharedPrecomputedValues originalModuleFns originalPrecomputedFns =
    let
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

                        else if List.isEmpty funcImpl.arguments && isNormalizationCandidate funcImpl.expression then
                            case
                                tryNormalizeConstant
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
    -- Cap at a small number of passes to bound worst-case time. For any
    -- realistic module this converges in 1-2 passes: the second pass picks
    -- up constants whose alphabetical order placed them before their own
    -- dependencies. The third pass is an "all done" check that short-circuits
    -- when no progress was made in the previous one.
    loop 3 originalModuleFns Dict.empty originalPrecomputedFns


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

        Types.Int _ ->
            True

        Types.Float _ ->
            True

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
                , precomputedValues = Dict.remove modKey projectEnv.env.shared.precomputedValues
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
                                { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False }
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
                        { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False }
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
                , coverage = False
                , coverageProbeLines = Set.empty
                , maxSteps = Nothing
                , tcoTarget = Nothing
                , callCounts = Nothing
                , intercepts = Dict.empty
                , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                , collectMemoStats = collectMemoStats
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
                        { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False }
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
                , coverage = False
                , coverageProbeLines = Set.empty
                , maxSteps = Nothing
                , tcoTarget = Nothing
                , callCounts = Nothing
                , intercepts = Dict.empty
                , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                , collectMemoStats = collectMemoStats
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
                , coverage = False
                , coverageProbeLines = Set.empty
                , maxSteps = Nothing
                , tcoTarget = Nothing
                , callCounts = Nothing
                , intercepts = intercepts
                , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                , collectMemoStats = False
                }
                finalEnv


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
                                , coverage = False
                                , coverageProbeLines = Set.empty
                                , maxSteps = Nothing
                                , tcoTarget = Nothing
                                , callCounts = Nothing
                                , intercepts = intercepts
                                , memoizedFunctions = MemoSpec.emptyRegistry
                                , collectMemoStats = False
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
                        , coverage = False
                        , coverageProbeLines = Set.empty
                        , maxSteps = Nothing
                        , tcoTarget = Nothing
                        , callCounts = Nothing
                        , intercepts = intercepts
                        , memoizedFunctions = MemoSpec.buildRegistry memoizedFunctions
                        , collectMemoStats = False
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
                    }
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
        Elm.Syntax.Expression.LetFunction { declaration } ->
            let
                (Node.Node _ funcDecl) =
                    declaration
            in
            isBlocklistedNode funcDecl.expression

        Elm.Syntax.Expression.LetDestructuring _ expression ->
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


{-| For diagnostics: list the (moduleKey, count) pairs sorted by count.
-}
precomputedValuesByModule : ProjectEnv -> List ( String, Int )
precomputedValuesByModule (ProjectEnv projectEnv) =
    projectEnv.env.shared.precomputedValues
        |> Dict.toList
        |> List.map (\( k, inner ) -> ( k, Dict.size inner ))
        |> List.filter (\( _, n ) -> n > 0)


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
                    , precomputedValues = env.shared.precomputedValues
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
place to install the rewritten bodies.
-}
normalizeOneModuleInEnv :
    ModuleName
    -> ProjectEnv
    -> ( ProjectEnv, Dict.Dict String FunctionImplementation )
normalizeOneModuleInEnv moduleName (ProjectEnv projectEnv) =
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

        ( normalizedModuleFns, delta, normalizedPrecomputedFns ) =
            runModuleNormalizationToFixpoint
                moduleName
                moduleKey
                moduleImports
                env.shared.functions
                sharedImports
                sharedPrecomputedValues
                originalModuleFns
                originalPrecomputedFns

        updatedFunctions : Dict.Dict String (Dict.Dict String FunctionImplementation)
        updatedFunctions =
            Dict.insert moduleKey normalizedModuleFns env.shared.functions

        updatedPrecomputed : Dict.Dict String (Dict.Dict String Value)
        updatedPrecomputed =
            Dict.insert moduleKey normalizedPrecomputedFns sharedPrecomputedValues

        newEnv : Env
        newEnv =
            { env
                | shared =
                    { functions = updatedFunctions
                    , moduleImports = sharedImports
                    , precomputedValues = updatedPrecomputed
                    }
            }
    in
    ( ProjectEnv { projectEnv | env = newEnv }, delta )


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
                    , precomputedValues = env.shared.precomputedValues
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
                    , precomputedValues = updatedPrecomputed
                    }
            }
    in
    ProjectEnv
        { projectEnv | env = newEnv }


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
                                { trace = True, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False }
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
                                { trace = False, coverage = True, coverageProbeLines = probeLines, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False }
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

        coreEnv : Env
        coreEnv =
            { currentModule = moduleName
            , currentModuleKey = Environment.moduleKey moduleName
            , callStack = []
            , shared = { functions = coreFunctions, moduleImports = Dict.singleton (Environment.moduleKey moduleName) imports, precomputedValues = Dict.empty }
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
                                , shared = { functions = coreFunctions, moduleImports = Dict.empty, precomputedValues = Dict.empty }
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
                                { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False }
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
            { env | shared = { functions = env.shared.functions, moduleImports = Dict.insert (Environment.moduleKey moduleName) moduleImportedNames env.shared.moduleImports, precomputedValues = env.shared.precomputedValues } }

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
