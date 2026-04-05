module Eval.Module exposing (ProjectEnv, buildInterfaceFromFile, buildProjectEnv, buildProjectEnvFromParsed, eval, evalProject, evalWithEnv, evalWithEnvAndLimit, evalWithEnvFromFiles, evalWithEnvFromFilesAndLimit, evalWithEnvFromFilesAndValues, evalWithEnvFromFilesAndValuesAndInterceptsRaw, evalWithIntercepts, evalWithInterceptsRaw, extendWithFiles, fileModuleName, parseProjectSources, replaceModuleInEnv, trace, traceOrEvalModule, traceWithEnv)

import Core
import Dict as ElmDict
import Elm.Interface exposing (Exposed)
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..))
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
import FastDict as Dict
import List.Extra
import Result.MyExtra
import Rope exposing (Rope)
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
            traceOrEvalModule { trace = False, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty } source expression
    in
    result


trace : String -> Expression -> ( Result Error Value, Rope CallTree, Rope String )
trace source expression =
    traceOrEvalModule { trace = True, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty } source expression


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


{-| Parse all sources and build an environment from them.
This is the expensive phase (parse + fold through buildModuleEnv).
The result can be reused across multiple `evalWithEnv` calls.
-}
buildProjectEnv : List String -> Result Error ProjectEnv
buildProjectEnv sources =
    parseProjectSources sources
        |> Result.andThen buildProjectEnvFromParsed


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
    let
        userInterfaces : ElmDict.Dict ModuleName (List Exposed)
        userInterfaces =
            parsedModules
                |> List.map (\m -> ( m.moduleName, m.interface ))
                |> ElmDict.fromList

        allInterfaces : ElmDict.Dict ModuleName (List Exposed)
        allInterfaces =
            ElmDict.union userInterfaces Core.dependency.interfaces

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
                        , shared = { functions = coreFunctions, moduleImports = Dict.empty }
                        , currentModuleFunctions = Dict.empty
                        , letFunctions = Dict.empty
                        , values = Dict.empty
                        , imports = emptyImports
                        , callDepth = 0
                        , recursionCheck = Nothing
                        }
                    )
    in
    envResult
        |> Result.map
            (\env ->
                ProjectEnv
                    { env = env
                    , allInterfaces = allInterfaces
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
                                { trace = False, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty }
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
                        { trace = False, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty }
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
                        { trace = False, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty }
                        finalEnv
                        |> EvalResult.toResult
            in
            Result.mapError Types.EvalError result


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
                                , maxSteps = Nothing
                                , tcoTarget = Nothing
                                , callCounts = Nothing
                                , intercepts = intercepts
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
                                { trace = True, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty }
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
            , shared = { functions = coreFunctions, moduleImports = Dict.singleton (Environment.moduleKey moduleName) imports }
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
                                , shared = { functions = coreFunctions, moduleImports = Dict.empty }
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
                                { trace = False, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty }
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
            { env | shared = { functions = env.shared.functions, moduleImports = Dict.insert (Environment.moduleKey moduleName) moduleImportedNames env.shared.moduleImports } }

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


{-| If a type alias has a record type annotation, register its name as a
function that constructs a Record value from positional arguments.

    type alias Point = { x : Int, y : Int }

registers `Point` as a 2-argument function whose body is
`RecordExpr [("x", $arg0), ("y", $arg1)]`.

-}
registerRecordAliasConstructor : ModuleName -> TypeAlias -> Env -> Env
registerRecordAliasConstructor moduleName alias_ env =
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

                implementation : Elm.Syntax.Expression.FunctionImplementation
                implementation =
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
            in
            Environment.addFunction moduleName implementation env

        _ ->
            env


{-| Register constructors from a custom type declaration as functions in env.
Each constructor becomes a function that creates a Custom value.
-}
registerConstructors :
    ModuleName
    -> Elm.Syntax.Type.Type
    -> Env
    -> Env
registerConstructors moduleName customType env =
    let
        addConstructor : Node Elm.Syntax.Type.ValueConstructor -> Env -> Env
        addConstructor (Node _ ctor) envAcc =
            let
                ctorName : String
                ctorName =
                    Node.value ctor.name

                arity : Int
                arity =
                    List.length ctor.arguments

                -- Generate argument pattern names: $ctor_arg0, $ctor_arg1, ...
                argNames : List String
                argNames =
                    List.range 0 (arity - 1)
                        |> List.map (\i -> "$ctor_arg" ++ String.fromInt i)

                -- Build the function implementation
                implementation : Elm.Syntax.Expression.FunctionImplementation
                implementation =
                    { name = fakeNode ctorName
                    , arguments =
                        argNames
                            |> List.map (\n -> fakeNode (Elm.Syntax.Pattern.VarPattern n))
                    , expression =
                        if arity == 0 then
                            -- Zero-arg constructor: just the constructor itself
                            fakeNode (FunctionOrValue moduleName ctorName)

                        else
                            -- N-arg constructor: application of constructor to args
                            fakeNode
                                (Application
                                    (fakeNode (FunctionOrValue moduleName ctorName)
                                        :: List.map
                                            (\n -> fakeNode (FunctionOrValue [] n))
                                            argNames
                                    )
                                )
                    }
            in
            Environment.addFunction moduleName implementation envAcc
    in
    List.foldl addConstructor env customType.constructors


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
