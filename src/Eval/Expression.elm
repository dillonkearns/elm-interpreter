module Eval.Expression exposing (evalExpression, evalFunction)

import Core
import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration)
import Elm.Syntax.Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range as Range
import Environment
import Eval.Types as Types
import EvalResult
import FastDict as Dict exposing (Dict)
import Kernel
import List.Extra
import Recursion
import Result.MyExtra
import Rope exposing (Rope)
import Set exposing (Set)
import Syntax exposing (fakeNode)
import TopologicalSort
import Types exposing (CallTree(..), Config, Env, EnvValues, Eval, EvalErrorData, EvalResult(..), PartialEval, PartialResult, Value(..))
import Value exposing (nameError, typeError, unsupported)


evalExpression : Node Expression -> Eval Value
evalExpression initExpression initCfg initEnv =
    Recursion.runRecursion
        (\( Node range expression, cfg, env ) ->
            let
                result : PartialResult Value
                result =
                    case expression of
                        Expression.UnitExpr ->
                            Types.succeedPartial Unit

                        Expression.Integer i ->
                            Types.succeedPartial <| Int i

                        Expression.Hex i ->
                            Types.succeedPartial <| Int i

                        Expression.Floatable f ->
                            Types.succeedPartial <| Float f

                        Expression.Literal string ->
                            Types.succeedPartial <| String string

                        Expression.CharLiteral c ->
                            Types.succeedPartial <| Char c

                        Expression.OperatorApplication "||" _ l r ->
                            evalShortCircuitOr l r cfg env

                        Expression.OperatorApplication "&&" _ l r ->
                            evalShortCircuitAnd l r cfg env

                        Expression.OperatorApplication "<|" _ l r ->
                            evalApplication l [ r ] cfg env

                        Expression.OperatorApplication "|>" _ l r ->
                            evalApplication r [ l ] cfg env

                        Expression.OperatorApplication opName _ l r ->
                            evalOperatorApplication opName l r cfg env

                        Expression.Application [] ->
                            Types.failPartial <| typeError env "Empty application"

                        Expression.Application (first :: rest) ->
                            evalApplication first rest cfg env

                        Expression.FunctionOrValue moduleName name ->
                            evalFunctionOrValue moduleName name cfg env

                        Expression.IfBlock cond true false ->
                            evalIfBlock cond true false cfg env

                        Expression.PrefixOperator opName ->
                            evalOperator opName cfg env

                        Expression.Operator opName ->
                            evalOperator opName cfg env

                        Expression.Negation child ->
                            evalNegation child cfg env

                        Expression.TupledExpression exprs ->
                            evalTuple exprs cfg env

                        Expression.ParenthesizedExpression child ->
                            Recursion.recurse ( child, cfg, env )

                        Expression.LetExpression letBlock ->
                            evalLetBlock letBlock cfg env

                        Expression.CaseExpression caseExpr ->
                            evalCase caseExpr cfg env

                        Expression.LambdaExpression lambda ->
                            Types.succeedPartial <| PartiallyApplied env [] lambda.args Nothing lambda.expression

                        Expression.RecordExpr fields ->
                            evalRecord fields cfg env

                        Expression.ListExpr elements ->
                            evalList elements cfg env

                        Expression.RecordAccess recordExpr field ->
                            evalRecordAccess recordExpr field cfg env

                        Expression.RecordAccessFunction field ->
                            Types.succeedPartial <| evalRecordAccessFunction field

                        Expression.RecordUpdateExpression name setters ->
                            evalRecordUpdate name setters cfg env

                        Expression.GLSLExpression _ ->
                            Types.failPartial <| unsupported env "GLSL not supported"
            in
            if cfg.trace then
                result
                    |> Recursion.map
                        (\evalResult ->
                            let
                                ( value, trees, logs ) =
                                    EvalResult.toTriple evalResult

                                callTree : Rope CallTree
                                callTree =
                                    Rope.singleton
                                        (CallNode
                                            { env = env
                                            , expression = Node range expression
                                            , children = trees
                                            , result = value
                                            }
                                        )
                            in
                            case value of
                                Ok v ->
                                    EvOkTrace v callTree logs

                                Err e ->
                                    EvErrTrace e callTree logs
                        )

            else
                result
        )
        ( initExpression, initCfg, initEnv )


{-| Evaluate an expression, using inline evaluation for simple cases
(literals, variables) to skip the trampoline round-trip. Falls back
to recurseThen for complex expressions.
-}
evalOrRecurse : ( Node Expression, Config, Env ) -> (Value -> PartialResult Value) -> PartialResult Value
evalOrRecurse ( (Node _ expr) as fullExpr, cfg, env ) continuation =
    case expr of
        Expression.Integer i ->
            continuation (Int i)

        Expression.Hex i ->
            continuation (Int i)

        Expression.Floatable f ->
            continuation (Float f)

        Expression.Literal s ->
            continuation (String s)

        Expression.CharLiteral c ->
            continuation (Char c)

        Expression.UnitExpr ->
            continuation Unit

        Expression.FunctionOrValue [] name ->
            case Dict.get name env.values of
                Just (PartiallyApplied _ [] [] _ _) ->
                    -- Zero-arg thunk, needs call path via trampoline
                    Types.recurseThen ( fullExpr, cfg, env ) continuation

                Just value ->
                    continuation value

                Nothing ->
                    Types.recurseThen ( fullExpr, cfg, env ) continuation

        Expression.ParenthesizedExpression inner ->
            evalOrRecurse ( inner, cfg, env ) continuation

        Expression.LambdaExpression lambda ->
            continuation (PartiallyApplied env [] lambda.args Nothing lambda.expression)

        Expression.RecordAccessFunction field ->
            continuation (evalRecordAccessFunction field)

        Expression.Negation (Node _ (Expression.Integer i)) ->
            continuation (Int -i)

        Expression.Negation (Node _ (Expression.Floatable f)) ->
            continuation (Float -f)

        Expression.ListExpr [] ->
            continuation (List [])

        Expression.TupledExpression [] ->
            continuation Unit

        _ ->
            Types.recurseThen ( fullExpr, cfg, env ) continuation


evalShortCircuitAnd : Node Expression -> Node Expression -> PartialEval Value
evalShortCircuitAnd l r cfg env =
    evalOrRecurse ( l, cfg, env )
        (\value ->
            case value of
                Bool False ->
                    Types.succeedPartial <| Bool False

                Bool True ->
                    Recursion.recurse ( r, cfg, env )

                v ->
                    Types.failPartial <| typeError env <| "&& applied to non-Bool " ++ Value.toString v
        )


evalShortCircuitOr : Node Expression -> Node Expression -> PartialEval Value
evalShortCircuitOr l r cfg env =
    evalOrRecurse ( l, cfg, env )
        (\value ->
            case value of
                Bool True ->
                    Types.succeedPartial <| Bool True

                Bool False ->
                    Recursion.recurse ( r, cfg, env )

                v ->
                    Types.failPartial <| typeError env <| "|| applied to non-Bool " ++ Value.toString v
        )


evalTuple : List (Node Expression) -> PartialEval Value
evalTuple exprs cfg env =
    case exprs of
        [] ->
            Types.succeedPartial Unit

        [ c ] ->
            Recursion.recurse ( c, cfg, env )

        [ l, r ] ->
            evalOrRecurse ( l, cfg, env )
                (\lValue ->
                    evalOrRecurse ( r, cfg, env )
                        (\rValue ->
                            Types.succeedPartial (Tuple lValue rValue)
                        )
                )

        [ l, m, r ] ->
            evalOrRecurse ( l, cfg, env )
                (\lValue ->
                    evalOrRecurse ( m, cfg, env )
                        (\mValue ->
                            evalOrRecurse ( r, cfg, env )
                                (\rValue ->
                                    Types.succeedPartial (Triple lValue mValue rValue)
                                )
                        )
                )

        _ :: _ :: _ :: _ :: _ ->
            Types.failPartial <| typeError env "Tuples with more than three elements are not supported"


operatorKernelFunctions : Dict String ( ModuleName, List Value -> Eval Value )
operatorKernelFunctions =
    Dict.foldl
        (\opName ref acc ->
            let
                kernelModuleName : ModuleName
                kernelModuleName =
                    "Elm" :: "Kernel" :: ref.moduleName
            in
            case Dict.get (Environment.moduleKey kernelModuleName) kernelFunctions of
                Nothing ->
                    acc

                Just kernelModule ->
                    case Dict.get ref.name kernelModule of
                        Nothing ->
                            acc

                        Just ( _, f ) ->
                            Dict.insert opName ( kernelModuleName, f ) acc
        )
        Dict.empty
        Core.operators


evalOperatorApplication : String -> Node Expression -> Node Expression -> PartialEval Value
evalOperatorApplication opName l r cfg env =
    case Dict.get opName operatorKernelFunctions of
        Just ( kernelModuleName, kernelFn ) ->
            evalOrRecurse ( l, cfg, env )
                (\lValue ->
                    evalOrRecurse ( r, cfg, env )
                        (\rValue ->
                            if cfg.trace then
                                let
                                    childEnv : Env
                                    childEnv =
                                        Environment.callKernel kernelModuleName opName env

                                    kernelEvalResult : EvalResult Value
                                    kernelEvalResult =
                                        kernelFn [ lValue, rValue ] cfg childEnv

                                    ( result, children, logLines ) =
                                        EvalResult.toTriple kernelEvalResult

                                    callTree : Rope CallTree
                                    callTree =
                                        Rope.singleton
                                            (CallNode
                                                { env = childEnv
                                                , expression = fakeNode <| Expression.OperatorApplication opName Elm.Syntax.Infix.Non l r
                                                , result = result
                                                , children = children
                                                }
                                            )
                                in
                                Recursion.base
                                    (case result of
                                        Ok v ->
                                            EvOkTrace v callTree logLines

                                        Err e ->
                                            EvErrTrace e callTree logLines
                                    )

                            else
                                Recursion.base (kernelFn [ lValue, rValue ] cfg env)
                        )
                )

        Nothing ->
            -- Fallback for operators not in kernel (e.g., <<, >>)
            let
                first : Node Expression
                first =
                    fakeNode <| Expression.Operator opName
            in
            evalApplication first [ l, r ] cfg env


evalApplication : Node Expression -> List (Node Expression) -> PartialEval Value
evalApplication first rest cfg env =
    let
        inner : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Node Expression -> PartialResult Value
        inner localEnv oldArgs patterns maybeQualifiedName implementation =
            case ( oldArgs, rest ) of
                ( [], [ singleArg ] ) ->
                    -- Fast path: single argument, no oldArgs, exactly 1 new arg
                    -- Pattern match on patterns list to avoid List.length
                    case patterns of
                        [ _ ] ->
                            -- Exactly 1 pattern: fully applied
                            evalOrRecurse ( singleArg, cfg, env )
                                (\argValue ->
                                    evalFullyApplied localEnv [ argValue ] patterns maybeQualifiedName implementation cfg env
                                )

                        _ :: _ :: _ ->
                            -- 2+ patterns: still partially applied after 1 arg
                            evalOrRecurse ( singleArg, cfg, env )
                                (\argValue ->
                                    Types.succeedPartial <| PartiallyApplied localEnv [ argValue ] patterns maybeQualifiedName implementation
                                )

                        [] ->
                            -- 0 patterns with 1 arg: need general path (splitting)
                            evalApplicationGeneralCompute first rest oldArgs localEnv patterns maybeQualifiedName implementation cfg env

                ( [], [ arg1, arg2 ] ) ->
                    -- Fast path: two arguments, no oldArgs
                    case patterns of
                        [ _, _ ] ->
                            -- Exactly 2 patterns: fully applied
                            evalOrRecurse ( arg1, cfg, env )
                                (\val1 ->
                                    evalOrRecurse ( arg2, cfg, env )
                                        (\val2 ->
                                            evalFullyApplied localEnv [ val1, val2 ] patterns maybeQualifiedName implementation cfg env
                                        )
                                )

                        _ :: _ :: _ :: _ ->
                            -- 3+ patterns: still partially applied after 2 args
                            evalOrRecurse ( arg1, cfg, env )
                                (\val1 ->
                                    evalOrRecurse ( arg2, cfg, env )
                                        (\val2 ->
                                            Types.succeedPartial <| PartiallyApplied localEnv [ val1, val2 ] patterns maybeQualifiedName implementation
                                        )
                                )

                        _ ->
                            -- 0 or 1 pattern with 2 args: need general path (splitting)
                            evalApplicationGeneralCompute first rest oldArgs localEnv patterns maybeQualifiedName implementation cfg env

                _ ->
                    evalApplicationGeneralCompute first rest oldArgs localEnv patterns maybeQualifiedName implementation cfg env
    in
    evalOrRecurse ( first, cfg, env )
        (\firstValue ->
            case firstValue of
                Custom name customArgs ->
                    Types.recurseMapThen ( rest, cfg, env )
                        (\values -> Types.succeedPartial <| Custom name (customArgs ++ values))

                PartiallyApplied localEnv oldArgs patterns maybeQualifiedName implementation ->
                    inner localEnv oldArgs patterns maybeQualifiedName implementation

                other ->
                    Types.failPartial <|
                        typeError env <|
                            "Trying to apply "
                                ++ Value.toString other
                                ++ ", which is a non-lambda non-variant"
        )


evalApplicationGeneralCompute : Node Expression -> List (Node Expression) -> List Value -> Env -> List (Node Pattern) -> Maybe QualifiedNameRef -> Node Expression -> PartialEval Value
evalApplicationGeneralCompute first rest oldArgs localEnv patterns maybeQualifiedName implementation cfg env =
    evalApplicationGeneral first rest oldArgs (List.length oldArgs) (List.length patterns) localEnv patterns maybeQualifiedName implementation cfg env


evalApplicationGeneral : Node Expression -> List (Node Expression) -> List Value -> Int -> Int -> Env -> List (Node Pattern) -> Maybe QualifiedNameRef -> Node Expression -> PartialEval Value
evalApplicationGeneral first rest oldArgs oldArgsLength patternsLength localEnv patterns maybeQualifiedName implementation cfg env =
    let
        ( used, leftover ) =
            List.Extra.splitAt (patternsLength - oldArgsLength) rest
    in
    if not (List.isEmpty leftover) then
        -- Too many args, we split
        Recursion.recurse
            ( fakeNode <|
                Expression.Application
                    (fakeNode
                        (Expression.Application (first :: used))
                        :: leftover
                    )
            , cfg
            , env
            )

    else
        let
            -- Since leftover is empty, rest has exactly (patternsLength - oldArgsLength) elements.
            -- So oldArgsLength + restLength == patternsLength when leftover is empty,
            -- or restLength < patternsLength - oldArgsLength is impossible (splitAt would have given us leftover).
            -- But rest could be shorter than (patternsLength - oldArgsLength) meaning still not enough args.
            restLength : Int
            restLength =
                List.length rest
        in
        Types.recurseMapThen ( rest, cfg, env )
            (\values ->
                let
                    args : List Value
                    args =
                        oldArgs ++ values
                in
                if oldArgsLength + restLength < patternsLength then
                    -- Still not enough
                    Types.succeedPartial <| PartiallyApplied localEnv args patterns maybeQualifiedName implementation

                else
                    -- Just right, we special case this for TCO
                    evalFullyApplied localEnv args patterns maybeQualifiedName implementation cfg env
            )


evalFullyApplied : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Node Expression -> PartialEval Value
evalFullyApplied localEnv args patterns maybeQualifiedName implementation cfg env =
    -- Fast path: if all patterns are simple VarPatterns or AllPatterns,
    -- bind directly via addValue instead of building an intermediate Dict.
    case bindSimplePatterns patterns args localEnv of
        Just boundEnv ->
            evalFullyAppliedWithEnv boundEnv args maybeQualifiedName implementation cfg env

        Nothing ->
            let
                maybeNewEnvValues : Result EvalErrorData (Maybe EnvValues)
                maybeNewEnvValues =
                    match env
                        (fakeNode <| ListPattern patterns)
                        (List args)
            in
            case maybeNewEnvValues of
                Err e ->
                    Types.failPartial e

                Ok Nothing ->
                    Types.failPartial <| typeError env "Could not match lambda patterns"

                Ok (Just newEnvValues) ->
                    evalFullyAppliedWithEnv (Environment.with newEnvValues localEnv) args maybeQualifiedName implementation cfg env


{-| Try to bind all patterns directly via addValue. Returns Just the new env
if all patterns are simple (VarPattern, AllPattern, ParenthesizedPattern wrapping one).
Returns Nothing for complex patterns that need full match.
-}
bindSimplePatterns : List (Node Pattern) -> List Value -> Env -> Maybe Env
bindSimplePatterns patterns args env =
    case ( patterns, args ) of
        ( [], [] ) ->
            Just env

        ( (Node _ (VarPattern name)) :: ptail, value :: atail ) ->
            bindSimplePatterns ptail atail (Environment.addValue name value env)

        ( (Node _ AllPattern) :: ptail, _ :: atail ) ->
            bindSimplePatterns ptail atail env

        ( (Node _ (ParenthesizedPattern inner)) :: ptail, value :: atail ) ->
            bindSimplePatterns (inner :: ptail) (value :: atail) env

        _ ->
            Nothing


evalFullyAppliedWithEnv : Env -> List Value -> Maybe QualifiedNameRef -> Node Expression -> PartialEval Value
evalFullyAppliedWithEnv boundEnv args maybeQualifiedName implementation cfg env =
    case implementation of
        Node range (FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
            let
                qualifiedName : QualifiedNameRef
                qualifiedName =
                    { moduleName = moduleName
                    , name = name
                    }

                fullName : String
                fullName =
                    Syntax.qualifiedNameToString qualifiedName

                key : String
                key =
                    Environment.moduleKey moduleName
            in
            case Dict.get key kernelFunctions of
                Nothing ->
                    Types.failPartial <| nameError env fullName

                Just kernelModule ->
                    case Dict.get name kernelModule of
                        Nothing ->
                            Types.failPartial <| nameError env fullName

                        Just ( _, f ) ->
                            if cfg.trace then
                                let
                                    childEnv : Env
                                    childEnv =
                                        Environment.callKernel moduleName name env

                                    kernelEvalResult : EvalResult Value
                                    kernelEvalResult =
                                        f args cfg childEnv

                                    ( kernelResult, children, logLines ) =
                                        EvalResult.toTriple kernelEvalResult

                                    callTree : Rope CallTree
                                    callTree =
                                        Rope.singleton
                                            (CallNode
                                                { env = childEnv
                                                , expression =
                                                    Node range <|
                                                        Application <|
                                                            Node range (FunctionOrValue moduleName name)
                                                                :: List.map Value.toExpression args
                                                , result = kernelResult
                                                , children = children
                                                }
                                            )
                                in
                                (case kernelResult of
                                    Ok v ->
                                        EvOkTrace v callTree logLines

                                    Err e ->
                                        EvErrTrace e callTree logLines
                                )
                                    |> Recursion.base

                            else
                                Recursion.base (f args cfg env)

        _ ->
            call
                maybeQualifiedName
                implementation
                cfg
                boundEnv


call : Maybe QualifiedNameRef -> Node Expression -> PartialEval Value
call maybeQualifiedName implementation cfg env =
    case maybeQualifiedName of
        Just qualifiedName ->
            let
                callFn =
                    if cfg.trace then
                        Environment.call

                    else
                        Environment.callNoStack
            in
            Recursion.recurse
                ( implementation
                , cfg
                , callFn qualifiedName.moduleName qualifiedName.name env
                )

        Nothing ->
            Recursion.recurse ( implementation, cfg, env )


evalFunctionOrValue : ModuleName -> String -> PartialEval Value
evalFunctionOrValue moduleName name cfg env =
    case moduleName of
        [] ->
            -- Fast path: local value lookup before isVariant check.
            -- Avoids String.uncons + Char.toCode for the most common case.
            case Dict.get name env.values of
                Just (PartiallyApplied localEnv [] [] maybeName implementation) ->
                    call maybeName implementation cfg localEnv

                Just value ->
                    Types.succeedPartial value

                Nothing ->
                    evalQualifiedOrVariant [] name cfg env

        _ ->
            evalQualifiedOrVariant moduleName name cfg env


evalQualifiedOrVariant : ModuleName -> String -> PartialEval Value
evalQualifiedOrVariant moduleName name cfg env =
    case moduleName of
        [] ->
            -- For unqualified names: check currentModuleFunctions first to
            -- skip the expensive isVariant check (String.uncons + Char.toCode)
            case Dict.get name env.currentModuleFunctions of
                Just function ->
                    let
                        qualifiedNameRef : QualifiedNameRef
                        qualifiedNameRef =
                            { moduleName = env.currentModule, name = name }

                        callFn =
                            if cfg.trace then
                                Environment.call

                            else
                                Environment.callNoStack
                    in
                    if List.isEmpty function.arguments then
                        call (Just qualifiedNameRef) function.expression cfg env

                    else
                        PartiallyApplied
                            (callFn env.currentModule name env)
                            []
                            function.arguments
                            (Just qualifiedNameRef)
                            function.expression
                            |> Types.succeedPartial

                Nothing ->
                    evalQualifiedOrVariantSlow moduleName name cfg env

        _ ->
            evalQualifiedOrVariantSlow moduleName name cfg env


evalQualifiedOrVariantSlow : ModuleName -> String -> PartialEval Value
evalQualifiedOrVariantSlow moduleName name cfg env =
    if isVariant name then
        case findRecordAliasConstructor moduleName name env of
            Just ( resolvedModule, function ) ->
                if List.isEmpty function.arguments then
                    call (Just { moduleName = resolvedModule, name = name }) function.expression cfg env

                else
                    PartiallyApplied
                        (Environment.call resolvedModule name env)
                        []
                        function.arguments
                        (Just { moduleName = resolvedModule, name = name })
                        function.expression
                        |> Types.succeedPartial

            Nothing ->
                evalVariant moduleName env name

    else
        evalNonVariant moduleName name cfg env


{-| Look up a record alias constructor function for a given variant name.
Returns the function only if its body is a RecordExpr, to avoid infinite
loops with regular custom type constructors.
-}
findRecordAliasConstructor : ModuleName -> String -> Env -> Maybe ( ModuleName, Expression.FunctionImplementation )
findRecordAliasConstructor moduleName name env =
    let
        ( resolvedModule, resolvedModuleKey ) =
            if List.isEmpty moduleName then
                ( env.currentModule, env.currentModuleKey )

            else
                fixModuleName moduleName env

        isRecordExprBody : Expression.FunctionImplementation -> Bool
        isRecordExprBody func =
            case Node.value func.expression of
                Expression.RecordExpr _ ->
                    True

                _ ->
                    False

        maybeFunc : Maybe Expression.FunctionImplementation
        maybeFunc =
            if List.isEmpty moduleName then
                Dict.get name env.currentModuleFunctions

            else
                Dict.get resolvedModuleKey env.functions
                    |> Maybe.andThen (Dict.get name)
    in
    maybeFunc
        |> Maybe.andThen
            (\f ->
                if isRecordExprBody f then
                    Just ( resolvedModule, f )

                else
                    Nothing
            )


fixModuleName : ModuleName -> Env -> ( ModuleName, String )
fixModuleName moduleName env =
    if List.isEmpty moduleName then
        ( env.currentModule, env.currentModuleKey )

    else
        let
            key : String
            key =
                Environment.moduleKey moduleName
        in
        case Dict.get key env.imports.aliases of
            Just canonicalWithKey ->
                canonicalWithKey

            Nothing ->
                ( moduleName, key )


evalVariant : ModuleName -> Env -> String -> PartialResult Value
evalVariant moduleName env name =
    -- True/False must remain as Bool values (not Custom) because
    -- the evaluator's if/then/else, &&, || depend on matching Bool
    case ( moduleName, name ) of
        ( [], "True" ) ->
            Types.succeedPartial <| Bool True

        ( [], "False" ) ->
            Types.succeedPartial <| Bool False

        ( "Basics" :: _, "True" ) ->
            Types.succeedPartial <| Bool True

        ( "Basics" :: _, "False" ) ->
            Types.succeedPartial <| Bool False

        _ ->
            let
                resolvedModuleName : ModuleName
                resolvedModuleName =
                    if List.isEmpty moduleName then
                        -- Unqualified constructor: check exposed constructors first
                        case Dict.get name env.imports.exposedConstructors of
                            Just ( sourceModule, _ ) ->
                                sourceModule

                            Nothing ->
                                env.currentModule

                    else
                        Tuple.first (fixModuleName moduleName env)

                qualifiedNameRef : QualifiedNameRef
                qualifiedNameRef =
                    { moduleName = resolvedModuleName, name = name }
            in
            Types.succeedPartial <| Custom qualifiedNameRef []


evalNonVariant : ModuleName -> String -> PartialEval Value
evalNonVariant moduleName name cfg env =
    case moduleName of
        "Elm" :: "Kernel" :: _ ->
            -- Check native kernel functions first; they take precedence over
            -- generated AST because they handle edge cases (e.g. Unicode
            -- surrogate pairs, stable sort) that the generated AST may not.
            let
                key : String
                key =
                    Environment.moduleKey moduleName
            in
            case Dict.get key kernelFunctions of
                Just nativeModule ->
                    case Dict.get name nativeModule of
                        Just _ ->
                            evalKernelFunctionWithKey key moduleName name cfg env

                        Nothing ->
                            evalKernelFunctionFromAstWithKey key moduleName name cfg env

                Nothing ->
                    evalKernelFunctionFromAstWithKey key moduleName name cfg env

        _ ->
            -- Note: env.values lookup for moduleName=[] is already done in
            -- evalFunctionOrValue before calling this function, so skip it here.
            let
                        maybeFunction : Maybe ( ModuleName, String, Expression.FunctionImplementation )
                        maybeFunction =
                            if List.isEmpty moduleName then
                                -- Unqualified: currentModuleFunctions was already checked
                                -- in evalQualifiedOrVariant, so go straight to imports.
                                case Dict.get name env.imports.exposedValues of
                                    Just ( sourceModule, sourceModuleKey ) ->
                                        Dict.get sourceModuleKey env.functions
                                            |> Maybe.andThen (Dict.get name)
                                            |> Maybe.map (\f -> ( sourceModule, sourceModuleKey, f ))

                                    Nothing ->
                                        Nothing

                            else
                                let
                                    moduleNameKey : String
                                    moduleNameKey =
                                        Environment.moduleKey moduleName
                                in
                                -- Try direct lookup first (common case: module not aliased)
                                case Dict.get moduleNameKey env.functions of
                                    Just moduleDict ->
                                        case Dict.get name moduleDict of
                                            Just f ->
                                                Just ( moduleName, moduleNameKey, f )

                                            Nothing ->
                                                -- Module exists but function not found.
                                                -- Could be aliased to a different module.
                                                case Dict.get moduleNameKey env.imports.aliases of
                                                    Just ( canonical, canonicalKey ) ->
                                                        Dict.get canonicalKey env.functions
                                                            |> Maybe.andThen (Dict.get name)
                                                            |> Maybe.map (\f -> ( canonical, canonicalKey, f ))

                                                    Nothing ->
                                                        Nothing

                                    Nothing ->
                                        -- Module not found directly; try alias resolution
                                        case Dict.get moduleNameKey env.imports.aliases of
                                            Just ( canonical, canonicalKey ) ->
                                                Dict.get canonicalKey env.functions
                                                    |> Maybe.andThen (Dict.get name)
                                                    |> Maybe.map (\f -> ( canonical, canonicalKey, f ))

                                            Nothing ->
                                                Nothing
                    in
                    case maybeFunction of
                        Just ( resolvedModule, resolvedModuleKey, function ) ->
                            let
                                qualifiedNameRef : QualifiedNameRef
                                qualifiedNameRef =
                                    { moduleName = resolvedModule, name = name }
                            in
                            if resolvedModule == env.currentModule then
                                -- Same module: keep local values, use existing caches
                                if List.isEmpty function.arguments then
                                    call (Just qualifiedNameRef) function.expression cfg env

                                else
                                    let
                                        callFn =
                                            if cfg.trace then
                                                Environment.call

                                            else
                                                Environment.callNoStack
                                    in
                                    PartiallyApplied
                                        (callFn resolvedModule name env)
                                        []
                                        function.arguments
                                        (Just qualifiedNameRef)
                                        function.expression
                                        |> Types.succeedPartial

                            else
                                -- Cross-module: combine clearing values + Environment.call
                                -- into a single record construction. We know resolvedModule /=
                                -- env.currentModule, so we inline the cross-module branch
                                -- of Environment.call directly.
                                let
                                    callEnv : Env
                                    callEnv =
                                        { currentModule = resolvedModule
                                        , currentModuleKey = resolvedModuleKey
                                        , callStack =
                                            if cfg.trace then
                                                { moduleName = resolvedModule, name = name }
                                                    :: env.callStack

                                            else
                                                env.callStack
                                        , functions = env.functions
                                        , currentModuleFunctions =
                                            Dict.get resolvedModuleKey env.functions
                                                |> Maybe.withDefault Dict.empty
                                        , values = Dict.empty
                                        , imports =
                                            Dict.get resolvedModuleKey env.moduleImports
                                                |> Maybe.withDefault env.imports
                                        , moduleImports = env.moduleImports
                                        }
                                in
                                if List.isEmpty function.arguments then
                                    Recursion.recurse ( function.expression, cfg, callEnv )

                                else
                                    PartiallyApplied
                                        callEnv
                                        []
                                        function.arguments
                                        (Just qualifiedNameRef)
                                        function.expression
                                        |> Types.succeedPartial

                        Nothing ->
                            Syntax.qualifiedNameToString
                                { moduleName = Tuple.first (fixModuleName moduleName env)
                                , name = name
                                }
                                |> nameError env
                                |> Types.failPartial


evalIfBlock : Node Expression -> Node Expression -> Node Expression -> PartialEval Value
evalIfBlock cond true false cfg env =
    evalOrRecurse ( cond, cfg, env )
        (\condValue ->
            case condValue of
                Bool True ->
                    Recursion.recurse ( true, cfg, env )

                Bool False ->
                    Recursion.recurse ( false, cfg, env )

                _ ->
                    Types.failPartial <| typeError env "ifThenElse condition was not a boolean"
        )


evalList : List (Node Expression) -> PartialEval Value
evalList elements cfg env =
    Types.recurseMapThen ( elements, cfg, env )
        (\values -> Types.succeedPartial <| List values)


evalRecord : List (Node Expression.RecordSetter) -> PartialEval Value
evalRecord fields cfg env =
    let
        ( fieldNames, expressions ) =
            fields
                |> List.map (\(Node _ ( Node _ name, expression )) -> ( name, expression ))
                |> List.unzip
    in
    Types.recurseMapThen ( expressions, cfg, env )
        (\tuples ->
            tuples
                |> List.map2 Tuple.pair fieldNames
                |> Dict.fromList
                |> Record
                |> Types.succeedPartial
        )


kernelFunctions : Dict String (Dict String ( Int, List Value -> Eval Value ))
kernelFunctions =
    Kernel.functions evalFunction
        |> Dict.foldl
            (\moduleName moduleDict acc ->
                Dict.insert (String.join "." moduleName) moduleDict acc
            )
            Dict.empty



evalFunction : Kernel.EvalFunction
evalFunction oldArgs patterns functionName implementation cfg localEnv =
    let
        oldArgsLength : Int
        oldArgsLength =
            List.length oldArgs

        patternsLength : Int
        patternsLength =
            List.length patterns
    in
    if oldArgsLength < patternsLength then
        -- Still not enough
        EvalResult.succeed <| PartiallyApplied localEnv oldArgs patterns functionName implementation

    else
        -- Just right, we special case this for TCO
        case bindSimplePatterns patterns oldArgs localEnv of
            Just boundEnv ->
                case implementation of
                    Node _ (Expression.FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
                        let
                            fullName : String
                            fullName =
                                Syntax.qualifiedNameToString { moduleName = moduleName, name = name }

                            key : String
                            key =
                                Environment.moduleKey moduleName
                        in
                        case Dict.get key kernelFunctions of
                            Nothing ->
                                EvalResult.fail <| nameError localEnv fullName

                            Just kernelModule ->
                                case Dict.get name kernelModule of
                                    Nothing ->
                                        EvalResult.fail <| nameError localEnv fullName

                                    Just ( _, f ) ->
                                        if cfg.trace then
                                            f oldArgs
                                                cfg
                                                (Environment.callKernel moduleName name localEnv)

                                        else
                                            f oldArgs cfg localEnv

                    _ ->
                        evalExpression implementation cfg boundEnv

            Nothing ->
                let
                    maybeNewEnvValues : Result EvalErrorData (Maybe EnvValues)
                    maybeNewEnvValues =
                        match localEnv
                            (Node (Range.combine (List.map Node.range patterns)) <| ListPattern patterns)
                            (List oldArgs)
                in
                case maybeNewEnvValues of
                    Err e ->
                        EvalResult.fail e

                    Ok Nothing ->
                        EvalResult.fail <| typeError localEnv "Could not match lambda patterns"

                    Ok (Just newEnvValues) ->
                        case implementation of
                            Node _ (Expression.FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name) ->
                                let
                                    fullName : String
                                    fullName =
                                        Syntax.qualifiedNameToString { moduleName = moduleName, name = name }

                                    key : String
                                    key =
                                        Environment.moduleKey moduleName
                                in
                                case Dict.get key kernelFunctions of
                                    Nothing ->
                                        EvalResult.fail <| nameError localEnv fullName

                                    Just kernelModule ->
                                        case Dict.get name kernelModule of
                                            Nothing ->
                                                EvalResult.fail <| nameError localEnv fullName

                                            Just ( _, f ) ->
                                                if cfg.trace then
                                                    f oldArgs
                                                        cfg
                                                        (Environment.callKernel moduleName name localEnv)

                                                else
                                                    f oldArgs cfg localEnv

                            _ ->
                                -- This is fine because it's never going to be recursive. FOR NOW. TODO: fix
                                evalExpression implementation
                                    cfg
                                    (localEnv |> Environment.with newEnvValues)


evalKernelFunction : ModuleName -> String -> PartialEval Value
evalKernelFunction moduleName name cfg env =
    evalKernelFunctionWithKey (Environment.moduleKey moduleName) moduleName name cfg env


evalKernelFunctionWithKey : String -> ModuleName -> String -> PartialEval Value
evalKernelFunctionWithKey key moduleName name cfg env =
    case Dict.get key kernelFunctions of
        Nothing ->
            Types.failPartial <| nameError env (String.join "." moduleName)

        Just kernelModule ->
            case Dict.get name kernelModule of
                Nothing ->
                    Types.failPartial <| nameError env <| Syntax.qualifiedNameToString { moduleName = moduleName, name = name }

                Just ( argCount, f ) ->
                    if argCount == 0 then
                        if cfg.trace then
                            let
                                kernelEvalResult : EvalResult Value
                                kernelEvalResult =
                                    f [] cfg (Environment.callKernel moduleName name env)

                                ( result, callTrees, logLines ) =
                                    EvalResult.toTriple kernelEvalResult

                                callTreeRope : Rope CallTree
                                callTreeRope =
                                    Rope.singleton
                                        (CallNode
                                            { env = env
                                            , expression = fakeNode <| FunctionOrValue moduleName name
                                            , result = result
                                            , children = callTrees
                                            }
                                        )
                            in
                            Recursion.base
                                (case result of
                                    Ok v ->
                                        EvOkTrace v callTreeRope logLines

                                    Err e ->
                                        EvErrTrace e callTreeRope logLines
                                )

                        else
                            Recursion.base (f [] cfg env)

                    else
                        PartiallyApplied (Environment.empty moduleName)
                            []
                            (List.repeat argCount (fakeNode AllPattern))
                            (Just { moduleName = moduleName, name = name })
                            (fakeNode <| Expression.FunctionOrValue moduleName name)
                            |> Types.succeedPartial


{-| Fall back to generated AST for kernel functions not in native kernel.
-}
evalKernelFunctionFromAst : ModuleName -> String -> PartialEval Value
evalKernelFunctionFromAst moduleName name cfg env =
    evalKernelFunctionFromAstWithKey (Environment.moduleKey moduleName) moduleName name cfg env


evalKernelFunctionFromAstWithKey : String -> ModuleName -> String -> PartialEval Value
evalKernelFunctionFromAstWithKey key moduleName name cfg env =
    case Dict.get key env.functions of
        Nothing ->
            evalKernelFunctionWithKey key moduleName name cfg env

        Just kernelModule ->
            case Dict.get name kernelModule of
                Nothing ->
                    evalKernelFunctionWithKey key moduleName name cfg env

                Just function ->
                    let
                        callFn =
                            if cfg.trace then
                                Environment.callKernel

                            else
                                Environment.callKernelNoStack
                    in
                    PartiallyApplied
                        (callFn moduleName name env)
                        []
                        function.arguments
                        (Just { moduleName = moduleName, name = name })
                        function.expression
                        |> Types.succeedPartial


evalNegation : Node Expression -> PartialEval Value
evalNegation child cfg env =
    evalOrRecurse ( child, cfg, env )
        (\value ->
            case value of
                Int i ->
                    Types.succeedPartial <| Int -i

                Float f ->
                    Types.succeedPartial <| Float -f

                _ ->
                    Types.failPartial <| typeError env "Trying to negate a non-number"
        )


evalLetBlock : Expression.LetBlock -> PartialEval Value
evalLetBlock letBlock cfg env =
    case letBlock.declarations of
        [ singleDecl ] ->
            -- Fast path: single declaration needs no topological sort
            evalLetBlockSingle singleDecl letBlock.expression cfg env

        _ ->
            evalLetBlockFull letBlock cfg env


evalLetBlockSingle : Node LetDeclaration -> Node Expression -> PartialEval Value
evalLetBlockSingle declaration body cfg env =
    let
        newEnv : EvalResult Env
        newEnv =
            addLetDeclaration declaration cfg env
    in
    case newEnv of
        EvOk ne ->
            Recursion.recurse ( body, cfg, ne )

        EvErr e ->
            Recursion.base (EvErr e)

        EvOkTrace ne trees logs ->
            Types.recurseThen ( body, cfg, ne )
                (\res ->
                    Recursion.base (EvOkTrace res trees logs)
                )

        EvErrTrace e trees logs ->
            Recursion.base (EvErrTrace e trees logs)


evalLetBlockFull : Expression.LetBlock -> PartialEval Value
evalLetBlockFull letBlock cfg env =
    let
        envDefs : Set String
        envDefs =
            Set.diff
                (Set.union
                    (Dict.keys env.currentModuleFunctions |> Set.fromList)
                    (Dict.keys env.values |> Set.fromList)
                )
                allDefVars

        allDefVars : Set String
        allDefVars =
            letBlock.declarations
                |> List.foldl (\e -> Set.union (declarationDefinedVariables e)) Set.empty

        sortedDeclarations : Result TopologicalSort.SortError (List (Node LetDeclaration))
        sortedDeclarations =
            letBlock.declarations
                |> List.indexedMap
                    (\id declaration ->
                        { id = id + 1
                        , declaration = declaration
                        , defVars = declarationDefinedVariables declaration
                        , refVars = Set.diff (declarationFreeVariables declaration) envDefs
                        , cycleAllowed = isLetDeclarationFunction declaration
                        }
                    )
                |> TopologicalSort.sort
                    { id = .id
                    , defVars = .defVars
                    , refVars = .refVars
                    , cycleAllowed = .cycleAllowed
                    }
                |> Result.map (List.map .declaration >> List.reverse)

        newEnv : EvalResult Env
        newEnv =
            case sortedDeclarations of
                Err TopologicalSort.IllegalCycle ->
                    EvalResult.fail <| typeError env "illegal cycle in let block"

                Err TopologicalSort.InternalError ->
                    EvalResult.fail <| typeError env "internal error in let block"

                Ok sd ->
                    -- We can't use combineMap and need to fold
                    -- because we need to change the environment for each call
                    List.foldl
                        (\declaration acc ->
                            EvalResult.andThen
                                (\e -> addLetDeclaration declaration cfg e)
                                acc
                        )
                        (EvalResult.succeed env)
                        sd
    in
    case newEnv of
        EvOk ne ->
            Recursion.recurse ( letBlock.expression, cfg, ne )

        EvErr e ->
            Recursion.base (EvErr e)

        EvOkTrace ne trees logs ->
            Types.recurseThen ( letBlock.expression, cfg, ne )
                (\res -> Recursion.base (EvOkTrace res trees logs))

        EvErrTrace e trees logs ->
            Recursion.base (EvErrTrace e trees logs)


isLetDeclarationFunction : Node LetDeclaration -> Bool
isLetDeclarationFunction (Node _ d) =
    case d of
        Expression.LetFunction { declaration } ->
            not (List.isEmpty (Node.value declaration).arguments)

        _ ->
            False


addLetDeclaration : Node LetDeclaration -> Eval Env
addLetDeclaration ((Node _ letDeclaration) as node) cfg env =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            case declaration of
                Node _ ({ name, expression } as implementation) ->
                    if isLetDeclarationFunction node then
                        EvalResult.succeed <| Environment.addFunction env.currentModule implementation env

                    else
                        evalExpression expression cfg env
                            |> EvalResult.map (\value -> Environment.addValue (Node.value name) value env)

        Expression.LetDestructuring letPattern letExpression ->
            evalExpression letExpression cfg env
                |> EvalResult.onValue
                    (\letValue ->
                        case match env letPattern letValue of
                            Err e ->
                                Err e

                            Ok Nothing ->
                                Err <| typeError env "Could not match pattern inside let"

                            Ok (Just patternEnv) ->
                                Ok (Environment.with patternEnv env)
                    )


declarationFreeVariables : Node LetDeclaration -> Set String
declarationFreeVariables (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            let
                { name, arguments, expression } =
                    Node.value declaration
            in
            Set.diff (freeVariables expression)
                (List.foldl (\p -> Set.union (patternDefinedVariables p))
                    (Set.singleton (Node.value name))
                    arguments
                )

        Expression.LetDestructuring pattern expression ->
            Set.diff (freeVariables expression) (patternDefinedVariables pattern)


letFreeVariables : Expression.LetBlock -> Set String
letFreeVariables { declarations, expression } =
    Set.diff
        (List.foldl (\d -> Set.union (declarationFreeVariables d)) (freeVariables expression) declarations)
        (List.foldl (\d -> Set.union (declarationDefinedVariables d)) Set.empty declarations)


caseFreeVariables : Expression.Case -> Set String
caseFreeVariables ( pattern, expression ) =
    Set.diff (freeVariables expression) (patternDefinedVariables pattern)


freeVariables : Node Expression -> Set String
freeVariables (Node _ expr) =
    case expr of
        Expression.Application expressions ->
            List.foldl (\e -> Set.union (freeVariables e)) Set.empty expressions

        Expression.OperatorApplication _ _ l r ->
            Set.union (freeVariables l) (freeVariables r)

        Expression.FunctionOrValue [] name ->
            if isVariant name then
                Set.empty

            else
                Set.singleton name

        Expression.IfBlock cond true false ->
            Set.union (freeVariables cond) (Set.union (freeVariables true) (freeVariables false))

        Expression.Negation child ->
            freeVariables child

        Expression.TupledExpression expressions ->
            List.foldl (\e -> Set.union (freeVariables e)) Set.empty expressions

        Expression.ParenthesizedExpression child ->
            freeVariables child

        Expression.LetExpression block ->
            letFreeVariables block

        Expression.CaseExpression { expression, cases } ->
            List.foldl (\c -> Set.union (caseFreeVariables c)) (freeVariables expression) cases

        Expression.LambdaExpression { expression, args } ->
            Set.diff (freeVariables expression)
                (List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty args)

        Expression.RecordExpr setters ->
            List.foldl (\(Node _ ( _, e )) -> Set.union (freeVariables e)) Set.empty setters

        Expression.ListExpr expressions ->
            List.foldl (\e -> Set.union (freeVariables e)) Set.empty expressions

        Expression.RecordAccess record _ ->
            freeVariables record

        Expression.RecordUpdateExpression (Node _ s) setters ->
            List.foldl (\(Node _ ( _, e )) -> Set.union (freeVariables e)) (Set.singleton s) setters

        _ ->
            Set.empty


patternDefinedVariables : Node Pattern -> Set String
patternDefinedVariables (Node _ pattern) =
    case pattern of
        TuplePattern patterns ->
            List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty patterns

        RecordPattern fields ->
            List.foldl (\(Node _ s) -> Set.insert s) Set.empty fields

        UnConsPattern head tail ->
            Set.union (patternDefinedVariables head) (patternDefinedVariables tail)

        ListPattern patterns ->
            List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty patterns

        VarPattern name ->
            Set.singleton name

        NamedPattern _ patterns ->
            List.foldl (\p -> Set.union (patternDefinedVariables p)) Set.empty patterns

        AsPattern p (Node _ s) ->
            Set.insert s (patternDefinedVariables p)

        ParenthesizedPattern p ->
            patternDefinedVariables p

        _ ->
            Set.empty


declarationDefinedVariables : Node LetDeclaration -> Set String
declarationDefinedVariables (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            Set.singleton <| Node.value (Node.value declaration).name

        Expression.LetDestructuring letPattern _ ->
            patternDefinedVariables letPattern


evalRecordAccess : Node Expression -> Node String -> PartialEval Value
evalRecordAccess recordExpr (Node _ field) cfg env =
    evalOrRecurse ( recordExpr, cfg, env )
        (\value ->
            case value of
                Record fields ->
                    case Dict.get field fields of
                        Just fieldValue ->
                            Types.succeedPartial fieldValue

                        Nothing ->
                            Types.failPartial <| typeError env <| "Field " ++ field ++ " not found [record access]"

                _ ->
                    Types.failPartial <| typeError env "Trying to access a field on a non-record value"
        )


evalRecordAccessFunction : String -> Value
evalRecordAccessFunction field =
    PartiallyApplied
        (Environment.empty [])
        []
        [ fakeNode (VarPattern "$r") ]
        Nothing
        (fakeNode <|
            Expression.RecordAccess
                (fakeNode <| Expression.FunctionOrValue [] "$r")
                (fakeNode <| String.dropLeft 1 field)
        )


evalRecordUpdate : Node String -> List (Node Expression.RecordSetter) -> PartialEval Value
evalRecordUpdate (Node range name) setters cfg env =
    evalOrRecurse ( Node range <| Expression.FunctionOrValue [] name, cfg, env )
        (\value ->
            case value of
                Record originalFields ->
                    let
                        ( fieldNames, fieldExpressions ) =
                            setters
                                |> List.map
                                    (\(Node _ ( Node _ fieldName, fieldExpression )) ->
                                        ( fieldName
                                        , fieldExpression
                                        )
                                    )
                                |> List.unzip
                    in
                    Types.recurseMapThen ( fieldExpressions, cfg, env )
                        (\fieldValues ->
                            let
                                updates : Dict String Value
                                updates =
                                    List.map2 Tuple.pair fieldNames fieldValues
                                        |> Dict.fromList
                            in
                            Dict.union updates originalFields
                                |> Record
                                |> Types.succeedPartial
                        )

                _ ->
                    Types.failPartial <| typeError env "Trying to update fields on a value which is not a record"
        )


evalOperator : String -> PartialEval Value
evalOperator opName cfg env =
    case Dict.get opName Core.operators of
        Nothing ->
            Types.failPartial <| nameError env opName

        Just kernelFunction ->
            let
                callFn =
                    if cfg.trace then
                        Environment.call

                    else
                        Environment.callNoStack
            in
            PartiallyApplied
                (callFn kernelFunction.moduleName opName env)
                []
                [ fakeNode <| VarPattern "$l", fakeNode <| VarPattern "$r" ]
                Nothing
                (fakeNode <|
                    Expression.Application
                        [ fakeNode <| Expression.FunctionOrValue kernelFunction.moduleName kernelFunction.name
                        , fakeNode <| Expression.FunctionOrValue [] "$l"
                        , fakeNode <| Expression.FunctionOrValue [] "$r"
                        ]
                )
                |> Types.succeedPartial


{-| Check if name starts with uppercase (variant/constructor).
Uses String.left 1 + native string comparison instead of
String.uncons + Char.toCode to avoid Char boxing overhead.
-}
isVariant : String -> Bool
isVariant name =
    let
        firstChar : String
        firstChar =
            String.left 1 name
    in
    "A" <= firstChar && firstChar <= "Z"


evalCase : Expression.CaseBlock -> PartialEval Value
evalCase { expression, cases } cfg env =
    evalOrRecurse ( expression, cfg, env )
        (\exprValue ->
            let
                maybePartial : Result EvalErrorData (Maybe ( EnvValues, Node Expression ))
                maybePartial =
                    Result.MyExtra.combineFoldl
                        (\( pattern, branchExpression ) acc ->
                            case acc of
                                Just _ ->
                                    Ok acc

                                Nothing ->
                                    case match env pattern exprValue of
                                        Err e ->
                                            Err e

                                        Ok Nothing ->
                                            Ok Nothing

                                        Ok (Just additionalEnv) ->
                                            Ok <| Just ( additionalEnv, branchExpression )
                        )
                        (Ok Nothing)
                        cases
            in
            case maybePartial of
                Ok Nothing ->
                    Types.failPartial <| typeError env <| "Missing case branch for " ++ Value.toString exprValue

                Ok (Just ( additionalEnv, branchExpression )) ->
                    if Dict.isEmpty additionalEnv then
                        Recursion.recurse ( branchExpression, cfg, env )

                    else
                        Recursion.recurse
                            ( branchExpression
                            , cfg
                            , Environment.with additionalEnv env
                            )

                Err e ->
                    Types.failPartial e
        )


matchOk : a -> Result error (Maybe a)
matchOk val =
    Ok (Just val)


matchNoMatch : Result error (Maybe a)
matchNoMatch =
    Ok Nothing


matchAndThen : (a -> Result error (Maybe a)) -> Result error (Maybe a) -> Result error (Maybe a)
matchAndThen f v =
    case v of
        Err _ ->
            v

        Ok Nothing ->
            v

        Ok (Just w) ->
            f w


match : Env -> Node Pattern -> Value -> Result EvalErrorData (Maybe EnvValues)
match env (Node _ pattern) value =
    case ( pattern, value ) of
        ( UnitPattern, Unit ) ->
            matchOk Dict.empty

        ( UnitPattern, _ ) ->
            matchNoMatch

        ( AllPattern, _ ) ->
            matchOk Dict.empty

        ( ParenthesizedPattern subPattern, _ ) ->
            match env subPattern value

        ( NamedPattern { name } [], Bool True ) ->
            if name == "True" then
                matchOk Dict.empty

            else
                matchNoMatch

        ( NamedPattern { name } [], Bool False ) ->
            if name == "False" then
                matchOk Dict.empty

            else
                matchNoMatch

        ( NamedPattern namePattern argsPatterns, Custom variant args ) ->
            -- Two names from different modules can never have the same type
            -- so if we assume the code typechecks we can skip the module name check
            if namePattern.name == variant.name then
                case ( argsPatterns, args ) of
                    ( [], [] ) ->
                        -- Zero-arg constructor like True, False, Nothing
                        matchOk Dict.empty

                    ( [ singlePattern ], [ singleArg ] ) ->
                        -- Single-arg constructor like Just x, Ok x — very common
                        match env singlePattern singleArg

                    _ ->
                        let
                            matchNamedPatternHelper :
                                EnvValues
                                -> ( List (Node Pattern), List Value )
                                -> Result EvalErrorData (Maybe EnvValues)
                            matchNamedPatternHelper envValues queue =
                                case queue of
                                    ( [], [] ) ->
                                        matchOk envValues

                                    ( patternHead :: patternTail, argHead :: argTail ) ->
                                        match env patternHead argHead
                                            |> matchAndThen
                                                (\newEnvValues ->
                                                    matchNamedPatternHelper (Dict.union newEnvValues envValues) ( patternTail, argTail )
                                                )

                                    _ ->
                                        Err <| typeError env "Mismatched number of arguments to variant"
                        in
                        matchNamedPatternHelper Dict.empty ( argsPatterns, args )

            else
                matchNoMatch

        ( NamedPattern _ _, _ ) ->
            matchNoMatch

        ( ListPattern patterns, List values ) ->
            matchListHelp env Dict.empty patterns values

        ( UnConsPattern (Node _ (VarPattern headName)) (Node _ (VarPattern tailName)), List (listHead :: listTail) ) ->
            -- Fast path: x :: xs with VarPatterns — avoid match calls
            matchOk <| Dict.insert tailName (List listTail) (Dict.singleton headName listHead)

        ( UnConsPattern patternHead patternTail, List (listHead :: listTail) ) ->
            match env patternHead listHead
                |> matchAndThen
                    (\headEnv ->
                        match env patternTail (List listTail)
                            |> matchAndThen
                                (\tailEnv ->
                                    matchOk
                                        (Dict.union headEnv tailEnv)
                                )
                    )

        ( UnConsPattern _ _, _ ) ->
            matchNoMatch

        ( VarPattern name, _ ) ->
            matchOk <| Dict.singleton name value

        ( ListPattern _, _ ) ->
            matchNoMatch

        ( CharPattern c, Char d ) ->
            if c == d then
                matchOk Dict.empty

            else
                matchNoMatch

        ( CharPattern _, _ ) ->
            matchNoMatch

        ( StringPattern c, String d ) ->
            if c == d then
                matchOk Dict.empty

            else
                matchNoMatch

        ( StringPattern _, _ ) ->
            matchNoMatch

        ( IntPattern c, Int d ) ->
            if c == d then
                matchOk Dict.empty

            else
                matchNoMatch

        ( IntPattern _, _ ) ->
            matchNoMatch

        ( HexPattern c, Int d ) ->
            if c == d then
                matchOk Dict.empty

            else
                matchNoMatch

        ( HexPattern _, _ ) ->
            matchNoMatch

        ( FloatPattern c, Float d ) ->
            if c == d then
                matchOk Dict.empty

            else
                matchNoMatch

        ( FloatPattern _, _ ) ->
            matchNoMatch

        ( TuplePattern [ Node _ (VarPattern lname), Node _ (VarPattern rname) ], Tuple lvalue rvalue ) ->
            -- Fast path: two VarPatterns — avoid match calls entirely
            matchOk <| Dict.insert rname rvalue (Dict.singleton lname lvalue)

        ( TuplePattern [ lpattern, rpattern ], Tuple lvalue rvalue ) ->
            match env lpattern lvalue
                |> matchAndThen
                    (\lenv ->
                        match env rpattern rvalue
                            |> matchAndThen
                                (\renv ->
                                    matchOk <| Dict.union lenv renv
                                )
                    )

        ( TuplePattern [ Node _ (VarPattern lname), Node _ (VarPattern mname), Node _ (VarPattern rname) ], Triple lvalue mvalue rvalue ) ->
            -- Fast path: three VarPatterns — avoid match calls entirely
            matchOk <| Dict.insert rname rvalue (Dict.insert mname mvalue (Dict.singleton lname lvalue))

        ( TuplePattern [ lpattern, mpattern, rpattern ], Triple lvalue mvalue rvalue ) ->
            match env lpattern lvalue
                |> matchAndThen
                    (\lenv ->
                        match env mpattern mvalue
                            |> matchAndThen
                                (\menv ->
                                    match env rpattern rvalue
                                        |> matchAndThen
                                            (\renv ->
                                                matchOk <| Dict.union lenv (Dict.union menv renv)
                                            )
                                )
                    )

        ( TuplePattern _, _ ) ->
            matchNoMatch

        ( AsPattern childPattern (Node _ asName), _ ) ->
            match env childPattern value
                |> matchAndThen
                    (\e -> matchOk <| Dict.insert asName value e)

        ( RecordPattern fields, Record fieldValues ) ->
            List.foldl
                (\(Node _ fieldName) ->
                    matchAndThen
                        (\acc ->
                            case Dict.get fieldName fieldValues of
                                Nothing ->
                                    Err <| typeError env <| "Field " ++ fieldName ++ " not found in record"

                                Just fieldValue ->
                                    matchOk <| Dict.insert fieldName fieldValue acc
                        )
                )
                (matchOk Dict.empty)
                fields

        ( RecordPattern _, _ ) ->
            matchNoMatch


matchListHelp : Env -> EnvValues -> List (Node Pattern) -> List Value -> Result EvalErrorData (Maybe EnvValues)
matchListHelp env acc patterns values =
    case ( patterns, values ) of
        ( [], [] ) ->
            Ok (Just acc)

        ( patternHead :: patternTail, valueHead :: valueTail ) ->
            case match env patternHead valueHead of
                Err e ->
                    Err e

                Ok Nothing ->
                    Ok Nothing

                Ok (Just headEnv) ->
                    matchListHelp env (Dict.union headEnv acc) patternTail valueTail

        _ ->
            Ok Nothing
