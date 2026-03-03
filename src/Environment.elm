module Environment exposing (addFunction, addValue, call, empty, with)

import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import FastDict as Dict
import Types exposing (Env, EnvValues, Value)


addValue : String -> Value -> Env -> Env
addValue name value env =
    { currentModule = env.currentModule
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

        isCurrentModule : Bool
        isCurrentModule =
            moduleName == env.currentModule

        currentInner : Dict.Dict String FunctionImplementation
        currentInner =
            if isCurrentModule then
                env.currentModuleFunctions

            else
                Maybe.withDefault Dict.empty
                    (Dict.get moduleName env.functions)

        newInner : Dict.Dict String FunctionImplementation
        newInner =
            Dict.insert funcName function currentInner
    in
    { env
        | functions = Dict.insert moduleName newInner env.functions
        , currentModuleFunctions =
            if isCurrentModule then
                newInner

            else
                env.currentModuleFunctions
    }


with : EnvValues -> Env -> Env
with newValues old =
    { currentModule = old.currentModule
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


call : ModuleName -> String -> Env -> Env
call moduleName name env =
    if moduleName == env.currentModule then
        { currentModule = moduleName
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
        { currentModule = moduleName
        , callStack =
            { moduleName = moduleName, name = name }
                :: env.callStack
        , functions = env.functions
        , currentModuleFunctions =
            Dict.get moduleName env.functions
                |> Maybe.withDefault Dict.empty
        , values = env.values
        , imports =
            Dict.get moduleName env.moduleImports
                |> Maybe.withDefault env.imports
        , moduleImports = env.moduleImports
        }
