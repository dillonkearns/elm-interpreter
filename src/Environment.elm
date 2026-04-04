module Environment exposing (addFunction, addLocalFunction, addValue, call, callKernel, callKernelNoStack, callNoStack, empty, moduleKey, replaceValues, with, withBindings)

import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import FastDict as Dict
import Types exposing (Env, EnvValues, Value)


moduleKey : ModuleName -> String
moduleKey moduleName =
    case moduleName of
        [ a ] ->
            a

        [ a, b ] ->
            a ++ "." ++ b

        [ a, b, c ] ->
            a ++ "." ++ b ++ "." ++ c

        _ ->
            String.join "." moduleName


addValue : String -> Value -> Env -> Env
addValue name value env =
    { currentModule = env.currentModule
    , currentModuleKey = env.currentModuleKey
    , callStack = env.callStack
    , shared = env.shared
    , currentModuleFunctions = env.currentModuleFunctions
    , values = Dict.insert name value env.values
    , imports = env.imports
    , callDepth = env.callDepth
    , recursionCheck = env.recursionCheck
    }


{-| Replace the values dict entirely (explicit construction, avoids _Utils_update).
-}
replaceValues : EnvValues -> Env -> Env
replaceValues newValues env =
    { currentModule = env.currentModule
    , currentModuleKey = env.currentModuleKey
    , callStack = env.callStack
    , shared = env.shared
    , currentModuleFunctions = env.currentModuleFunctions
    , values = newValues
    , imports = env.imports
    , callDepth = env.callDepth
    , recursionCheck = env.recursionCheck
    }


{-| Add a function only to currentModuleFunctions, NOT to the global
functions dict. Used for let-functions where recursive self-reference
is needed but the function shouldn't pollute the global namespace.
-}
addLocalFunction : FunctionImplementation -> Env -> Env
addLocalFunction function env =
    { env
        | currentModuleFunctions =
            Dict.insert (Node.value function.name) function env.currentModuleFunctions
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
                    (Dict.get key env.shared.functions)

        newInner : Dict.Dict String FunctionImplementation
        newInner =
            Dict.insert funcName function currentInner
    in
    { env
        | shared =
            { functions = Dict.insert key newInner env.shared.functions
            , moduleImports = env.shared.moduleImports
            }
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
    , shared = old.shared
    , currentModuleFunctions = old.currentModuleFunctions
    , values = Dict.union newValues old.values
    , imports = old.imports
    , callDepth = old.callDepth
    , recursionCheck = old.recursionCheck
    }


{-| Add a list of bindings to env.values. Used by match results to avoid
building an intermediate Dict that would just be unioned into values anyway.
-}
withBindings : List ( String, Value ) -> Env -> Env
withBindings bindings old =
    { currentModule = old.currentModule
    , currentModuleKey = old.currentModuleKey
    , callStack = old.callStack
    , shared = old.shared
    , currentModuleFunctions = old.currentModuleFunctions
    , values = List.foldl (\( k, v ) acc -> Dict.insert k v acc) old.values bindings
    , imports = old.imports
    , callDepth = old.callDepth
    , recursionCheck = old.recursionCheck
    }


empty : ModuleName -> Env
empty moduleName =
    { currentModule = moduleName
    , currentModuleKey = moduleKey moduleName
    , callStack = []
    , shared = { functions = Dict.empty, moduleImports = Dict.empty }
    , currentModuleFunctions = Dict.empty
    , values = Dict.empty
    , imports = emptyImports
    , callDepth = 0
    , recursionCheck = Nothing
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
    , shared = env.shared
    , currentModuleFunctions =
        Dict.get key env.shared.functions
            |> Maybe.withDefault Dict.empty
    , values = env.values
    , imports = env.imports
    , callDepth = env.callDepth + 1
    , recursionCheck = env.recursionCheck
    }


call : ModuleName -> String -> Env -> Env
call moduleName name env =
    let
        key : String
        key =
            moduleKey moduleName
    in
    if key == env.currentModuleKey then
        { currentModule = moduleName
        , currentModuleKey = key
        , callStack =
            { moduleName = moduleName, name = name }
                :: env.callStack
        , shared = env.shared
        , currentModuleFunctions = env.currentModuleFunctions
        , values = env.values
        , imports = env.imports
        , callDepth = env.callDepth + 1
        , recursionCheck = env.recursionCheck
        }

    else
        { currentModule = moduleName
        , currentModuleKey = key
        , callStack =
            { moduleName = moduleName, name = name }
                :: env.callStack
        , shared = env.shared
        , currentModuleFunctions =
            Dict.get key env.shared.functions
                |> Maybe.withDefault Dict.empty
        , values = env.values
        , imports =
            Dict.get key env.shared.moduleImports
                |> Maybe.withDefault env.imports
        , callDepth = env.callDepth + 1
        , recursionCheck = env.recursionCheck
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
    , shared = env.shared
    , currentModuleFunctions =
        Dict.get key env.shared.functions
            |> Maybe.withDefault Dict.empty
    , values = env.values
    , imports = env.imports
    , callDepth = env.callDepth
    , recursionCheck = env.recursionCheck
    }


{-| Like call but skips callStack update. Used when trace is off.
-}
callNoStack : ModuleName -> String -> Env -> Env
callNoStack moduleName _ env =
    let
        key : String
        key =
            moduleKey moduleName
    in
    if key == env.currentModuleKey then
        { env | callDepth = env.callDepth + 1 }

    else
        { currentModule = moduleName
        , currentModuleKey = key
        , callStack = env.callStack
        , shared = env.shared
        , currentModuleFunctions =
            Dict.get key env.shared.functions
                |> Maybe.withDefault Dict.empty
        , values = env.values
        , imports =
            Dict.get key env.shared.moduleImports
                |> Maybe.withDefault env.imports
        , callDepth = env.callDepth + 1
        , recursionCheck = env.recursionCheck
        }
