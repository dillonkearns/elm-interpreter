module Environment exposing (addFunction, addLetFunction, addLocalFunction, addValue, call, callKernel, callKernelNoStack, callModuleFn, callModuleFnNoStack, callNoStack, empty, moduleKey, replaceValues, with, withBindings)

import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import FastDict as Dict
import Types exposing (Env, EnvValues, Value, noResolveBridge)


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
    , letFunctions = env.letFunctions
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
    , letFunctions = env.letFunctions
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


{-| Add a let-defined function to the letFunctions dict.
This is used for self-recursive lookups within the function's own body.
Unlike addLocalFunction, letFunctions is reset by callNoStack for
same-module calls, preventing name collisions across different scopes.
-}
addLetFunction : FunctionImplementation -> Env -> Env
addLetFunction function env =
    { env
        | letFunctions =
            Dict.insert (Node.value function.name) function env.letFunctions
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
            , resolveBridge = env.shared.resolveBridge
            , precomputedValues = env.shared.precomputedValues
            , tcoAnalyses = env.shared.tcoAnalyses
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
    , letFunctions = old.letFunctions
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
    , letFunctions = old.letFunctions
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
    , shared = { functions = Dict.empty, moduleImports = Dict.empty, resolveBridge = noResolveBridge, precomputedValues = Dict.empty, tcoAnalyses = Dict.empty }
    , currentModuleFunctions = Dict.empty
    , letFunctions = Dict.empty
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


{-| Optimized call for kernel functions. Kernel modules (Elm.Kernel.\*)
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
    , letFunctions = Dict.empty
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
        , letFunctions = env.letFunctions
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
        , letFunctions = Dict.empty
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
    , letFunctions = Dict.empty
    , values = env.values
    , imports = env.imports
    , callDepth = env.callDepth
    , recursionCheck = env.recursionCheck
    }


{-| Flat-closure variant of `call` for **top-level module function** lookups.

A top-level module function's body can only reference:

  - its own parameters
  - its own let bindings
  - module-level names (`currentModuleFunctions` / `shared.functions`)
  - imports

It _never_ legitimately references the caller's local bindings, so the
caller's `env.values` is dead weight when stored on the `PartiallyApplied`
that represents the bare function reference. This variant clears `values`
to `Dict.empty` at the point where we build the closure env, keeping
subsequent `bindSimplePatterns` inserts (for the function's own
parameters) and TCO fingerprint walks proportional to the function's
own argument count instead of the caller's scope size.

**Do not** use this for let-defined functions — those genuinely close
over their enclosing scope's `values`.

-}
callModuleFn : ModuleName -> String -> Env -> Env
callModuleFn moduleName name env =
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
        , letFunctions = Dict.empty
        , values = Dict.empty
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
        , letFunctions = Dict.empty
        , values = Dict.empty
        , imports =
            Dict.get key env.shared.moduleImports
                |> Maybe.withDefault env.imports
        , callDepth = env.callDepth + 1
        , recursionCheck = env.recursionCheck
        }


{-| Like `callModuleFn` but skips the `callStack` update — used when trace
is off. See `callModuleFn` for the flat-closure rationale.
-}
callModuleFnNoStack : ModuleName -> String -> Env -> Env
callModuleFnNoStack moduleName _ env =
    let
        key : String
        key =
            moduleKey moduleName
    in
    if key == env.currentModuleKey then
        { currentModule = env.currentModule
        , currentModuleKey = env.currentModuleKey
        , callStack = env.callStack
        , shared = env.shared
        , currentModuleFunctions = env.currentModuleFunctions
        , letFunctions = Dict.empty
        , values = Dict.empty
        , imports = env.imports
        , callDepth = env.callDepth + 1
        , recursionCheck = env.recursionCheck
        }

    else
        { currentModule = moduleName
        , currentModuleKey = key
        , callStack = env.callStack
        , shared = env.shared
        , currentModuleFunctions =
            Dict.get key env.shared.functions
                |> Maybe.withDefault Dict.empty
        , letFunctions = Dict.empty
        , values = Dict.empty
        , imports =
            Dict.get key env.shared.moduleImports
                |> Maybe.withDefault env.imports
        , callDepth = env.callDepth + 1
        , recursionCheck = env.recursionCheck
        }


{-| Like call but skips callStack update. Used when trace is off.
-}
callNoStack : ModuleName -> String -> Env -> Env
callNoStack moduleName name env =
    let
        key : String
        key =
            moduleKey moduleName
    in
    if key == env.currentModuleKey then
        -- Reset letFunctions when calling a MODULE-LEVEL function
        -- (one that exists in the shared functions dict). This prevents
        -- let-function names from leaking across scope boundaries.
        -- When calling a LET-function (not in shared dict), keep
        -- letFunctions so self-recursion works.
        --
        -- Same-module case: env.currentModuleFunctions is already
        -- env.shared.functions[key], so skip the redundant Dict.get.
        if Dict.member name env.currentModuleFunctions then
            { currentModule = env.currentModule
            , currentModuleKey = env.currentModuleKey
            , callStack = env.callStack
            , shared = env.shared
            , currentModuleFunctions = env.currentModuleFunctions
            , letFunctions = Dict.empty
            , values = env.values
            , imports = env.imports
            , callDepth = env.callDepth + 1
            , recursionCheck = env.recursionCheck
            }

        else
            { currentModule = env.currentModule
            , currentModuleKey = env.currentModuleKey
            , callStack = env.callStack
            , shared = env.shared
            , currentModuleFunctions = env.currentModuleFunctions
            , letFunctions = env.letFunctions
            , values = env.values
            , imports = env.imports
            , callDepth = env.callDepth + 1
            , recursionCheck = env.recursionCheck
            }

    else
        { currentModule = moduleName
        , currentModuleKey = key
        , callStack = env.callStack
        , shared = env.shared
        , currentModuleFunctions =
            Dict.get key env.shared.functions
                |> Maybe.withDefault Dict.empty
        , letFunctions = Dict.empty
        , values = env.values
        , imports =
            Dict.get key env.shared.moduleImports
                |> Maybe.withDefault env.imports
        , callDepth = env.callDepth + 1
        , recursionCheck = env.recursionCheck
        }
