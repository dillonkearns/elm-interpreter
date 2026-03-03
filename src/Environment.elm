module Environment exposing (addFunction, addValue, call, callKernel, callKernelNoStack, callNoStack, empty, moduleKey, with)

import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import FastDict as Dict
import Types exposing (Env, EnvValues, Value)


moduleKey : ModuleName -> String
moduleKey moduleName =
    String.join "." moduleName


addValue : String -> Value -> Env -> Env
addValue name value env =
    { currentModule = env.currentModule
    , currentModuleKey = env.currentModuleKey
    , callStack = env.callStack
    , functions = env.functions
    , currentModuleFunctions = env.currentModuleFunctions
    , values = Dict.insert name value env.values
    , imports = env.imports
    , moduleImports = env.moduleImports
    }


addFunction : ModuleName -> FunctionImplementation -> Env -> Env
addFunction moduleName function env =
    let
        funcName : String
        funcName =
            Node.value function.name

        key : String
        key =
            moduleKey moduleName

        isCurrentModule : Bool
        isCurrentModule =
            key == env.currentModuleKey

        currentInner : Dict.Dict String FunctionImplementation
        currentInner =
            if isCurrentModule then
                env.currentModuleFunctions

            else
                Maybe.withDefault Dict.empty
                    (Dict.get key env.functions)

        newInner : Dict.Dict String FunctionImplementation
        newInner =
            Dict.insert funcName function currentInner
    in
    { env
        | functions = Dict.insert key newInner env.functions
        , currentModuleFunctions =
            if isCurrentModule then
                newInner

            else
                env.currentModuleFunctions
    }


with : EnvValues -> Env -> Env
with newValues old =
    { currentModule = old.currentModule
    , currentModuleKey = old.currentModuleKey
    , callStack = old.callStack
    , functions = old.functions
    , currentModuleFunctions = old.currentModuleFunctions
    , values = Dict.union newValues old.values
    , imports = old.imports
    , moduleImports = old.moduleImports
    }


empty : ModuleName -> Env
empty moduleName =
    { currentModule = moduleName
    , currentModuleKey = moduleKey moduleName
    , callStack = []
    , functions = Dict.empty
    , currentModuleFunctions = Dict.empty
    , values = Dict.empty
    , imports = emptyImports
    , moduleImports = Dict.empty
    }


emptyImports : Types.ImportedNames
emptyImports =
    { aliases = Dict.empty
    , exposedValues = Dict.empty
    , exposedConstructors = Dict.empty
    }


{-| Optimized call for kernel functions. Kernel modules (Elm.Kernel.*)
are never the current user module, so we skip the equality check and
the moduleImports lookup (kernel modules have no user imports).
-}
callKernel : ModuleName -> String -> Env -> Env
callKernel moduleName name env =
    let
        key : String
        key =
            moduleKey moduleName
    in
    { currentModule = moduleName
    , currentModuleKey = key
    , callStack =
        { moduleName = moduleName, name = name }
            :: env.callStack
    , functions = env.functions
    , currentModuleFunctions =
        Dict.get key env.functions
            |> Maybe.withDefault Dict.empty
    , values = env.values
    , imports = env.imports
    , moduleImports = env.moduleImports
    }


call : ModuleName -> String -> Env -> Env
call moduleName name env =
    if moduleName == env.currentModule then
        { currentModule = moduleName
        , currentModuleKey = env.currentModuleKey
        , callStack =
            { moduleName = moduleName, name = name }
                :: env.callStack
        , functions = env.functions
        , currentModuleFunctions = env.currentModuleFunctions
        , values = env.values
        , imports = env.imports
        , moduleImports = env.moduleImports
        }

    else
        let
            key : String
            key =
                moduleKey moduleName
        in
        { currentModule = moduleName
        , currentModuleKey = key
        , callStack =
            { moduleName = moduleName, name = name }
                :: env.callStack
        , functions = env.functions
        , currentModuleFunctions =
            Dict.get key env.functions
                |> Maybe.withDefault Dict.empty
        , values = env.values
        , imports =
            Dict.get key env.moduleImports
                |> Maybe.withDefault env.imports
        , moduleImports = env.moduleImports
        }


{-| Like callKernel but skips callStack update. Used when trace is off.
-}
callKernelNoStack : ModuleName -> String -> Env -> Env
callKernelNoStack moduleName _ env =
    let
        key : String
        key =
            moduleKey moduleName
    in
    { currentModule = moduleName
    , currentModuleKey = key
    , callStack = env.callStack
    , functions = env.functions
    , currentModuleFunctions =
        Dict.get key env.functions
            |> Maybe.withDefault Dict.empty
    , values = env.values
    , imports = env.imports
    , moduleImports = env.moduleImports
    }


{-| Like call but skips callStack update. Used when trace is off.
-}
callNoStack : ModuleName -> String -> Env -> Env
callNoStack moduleName _ env =
    if moduleName == env.currentModule then
        env

    else
        let
            key : String
            key =
                moduleKey moduleName
        in
        { currentModule = moduleName
        , currentModuleKey = key
        , callStack = env.callStack
        , functions = env.functions
        , currentModuleFunctions =
            Dict.get key env.functions
                |> Maybe.withDefault Dict.empty
        , values = env.values
        , imports =
            Dict.get key env.moduleImports
                |> Maybe.withDefault env.imports
        , moduleImports = env.moduleImports
        }
