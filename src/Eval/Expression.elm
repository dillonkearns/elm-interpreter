module Eval.Expression exposing (evalExpression, evalFunction)

import Array
import Bitwise
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
import Kernel.Utils
import List.Extra
import Recursion
import Result.MyExtra
import Rope exposing (Rope)
import Set exposing (Set)
import Syntax exposing (fakeNode)
import TopologicalSort
import Types exposing (CallTree(..), Config, Env, EnvValues, Eval, EvalErrorData, EvalErrorKind(..), EvalResult(..), Implementation(..), PartialEval, PartialResult, Value(..))
import Value exposing (nameError, typeError, unsupported)


{-| Cheap fingerprint of a list of argument values — uses only top-level
structure (type tag, length, first element) so it's O(args) with O(1) per value.
-}
fingerprintArgs : List Value -> Int
fingerprintArgs args =
    List.foldl (\v acc -> Bitwise.xor (acc * 31) (fingerprintValue v)) 5381 args


fingerprintValue : Value -> Int
fingerprintValue value =
    case value of
        Int i ->
            i * 37 + 1

        Float f ->
            round (f * 1000) + 2

        String s ->
            String.length s * 41 + 3

        Char c ->
            Char.toCode c * 43 + 4

        Bool True ->
            5

        Bool False ->
            6

        Unit ->
            7

        List items ->
            List.length items * 53 + 8

        Tuple a _ ->
            fingerprintValue a * 59 + 9

        Triple a _ _ ->
            fingerprintValue a * 61 + 10

        Custom ref args ->
            List.length args * 67 + 11

        Record dict ->
            -- Include fingerprints of values, not just size, so records with
            -- different field values (e.g., { pos = 1 } vs { pos = 2 })
            -- produce different fingerprints for bounded-progress detection.
            Dict.foldl (\_ v acc -> Bitwise.xor (acc * 31) (fingerprintValue v)) (Dict.size dict * 71 + 12) dict

        JsArray arr ->
            Array.length arr * 73 + 13

        PartiallyApplied closureEnv args _ maybeName _ arity ->
            -- Fingerprint based on arity, applied args, function identity,
            -- AND captured environment values. This ensures closures at
            -- different parser positions (different captured state) produce
            -- different fingerprints, preventing false cycle detection.
            let
                nameHash =
                    case maybeName of
                        Just name ->
                            String.length name.name * 79

                        Nothing ->
                            0

                argsHash =
                    List.foldl (\a acc -> Bitwise.xor (acc * 31) (fingerprintValue a)) 0 args

                -- Include captured env values in fingerprint.
                -- This is the key fix for parser combinators: the parser state
                -- (position offset) lives in the captured env, not in the args.
                envHash =
                    Dict.foldl
                        (\_ v acc ->
                            case v of
                                PartiallyApplied _ _ _ _ _ _ ->
                                    -- Don't recurse into nested closures (expensive + cycles)
                                    acc + 17

                                _ ->
                                    Bitwise.xor (acc * 31) (fingerprintValue v)
                        )
                        0
                        closureEnv.values
            in
            Bitwise.xor (arity * 83 + List.length args * 89 + nameHash + 14) (Bitwise.xor argsHash envHash)

        _ ->
            0


{-| Cheap size estimate of argument values — sum of top-level sizes.
Used to detect Category B loops (args change but total size grows
monotonically, meaning no argument is getting structurally smaller).
-}
sizeOfArgs : List Value -> Int
sizeOfArgs args =
    List.foldl (\v acc -> acc + sizeOfValue v) 0 args


sizeOfValue : Value -> Int
sizeOfValue value =
    case value of
        List items ->
            List.length items

        String s ->
            String.length s

        Tuple _ _ ->
            2

        Triple _ _ _ ->
            3

        Custom _ args ->
            List.length args

        Record dict ->
            Dict.size dict

        JsArray arr ->
            Array.length arr

        _ ->
            1


{-| Combined check + update for cycle detection. Returns either:
- CycleDetected: infinite loop found, bail out
- Continue updatedCheck: no cycle, proceed with updated check state
Computes fingerprint and does Dict lookup only ONCE.

Detects two patterns:
- Category A: identical fingerprint for 3 consecutive calls (same args = guaranteed loop)
- Category B: fingerprint changes but total value size never decreases for 50 consecutive
  calls (args growing monotonically = no progress toward base case)
-}
type alias RecursionCheckDict =
    Dict String { fingerprint : Int, depth : Int, count : Int, size : Int, growCount : Int, argSizes : List Int, argFingerprints : List Int }


type CycleResult
    = CycleDetected
    | Continue RecursionCheckDict


checkAndUpdateCycle : QualifiedNameRef -> List Value -> Int -> RecursionCheckDict -> CycleResult
checkAndUpdateCycle qualifiedName args callDepth checkDict =
    let
        fnKey =
            Syntax.qualifiedNameToString qualifiedName

        fp =
            fingerprintArgs args

        sz =
            sizeOfArgs args

        perArgSzs =
            List.map sizeOfValue args

        perArgFps =
            List.map fingerprintValue args
    in
    case Dict.get fnKey checkDict of
        Nothing ->
            Continue (Dict.insert fnKey { fingerprint = fp, depth = callDepth, count = 1, size = sz, growCount = 0, argSizes = perArgSzs, argFingerprints = perArgFps } checkDict)

        Just entry ->
            if entry.depth >= callDepth then
                -- Not recursive (function returned and was called again)
                Continue (Dict.insert fnKey { fingerprint = fp, depth = callDepth, count = 1, size = sz, growCount = 0, argSizes = perArgSzs, argFingerprints = perArgFps } checkDict)

            else if entry.fingerprint == fp then
                -- Category A: identical fingerprint (same args)
                -- Use higher threshold when args contain functions (closures),
                -- because closure fingerprinting can't distinguish nested state
                -- changes (e.g., parser combinators, wrapped counters).
                let
                    hasClosureArgs =
                        List.any
                            (\a ->
                                case a of
                                    PartiallyApplied _ _ _ _ _ _ ->
                                        True

                                    _ ->
                                        False
                            )
                            args

                    threshold =
                        if hasClosureArgs then
                            500

                        else
                            3
                in
                if entry.count >= threshold then
                    CycleDetected

                else
                    Continue (Dict.insert fnKey { entry | count = entry.count + 1 } checkDict)

            else
                -- Fingerprint changed. Check Category B: is total size growing?
                let
                    -- "Bounded progress" = any arg has constant size but changing fingerprint.
                    -- This detects countdown variables (Int decreasing toward base case)
                    -- whose size stays 1 but whose value changes each call.
                    boundedProgress =
                        hasBoundedProgress entry.argSizes entry.argFingerprints perArgSzs perArgFps

                    newGrowCount =
                        if boundedProgress then
                            -- A scalar argument is changing value — likely a countdown
                            0

                        else if sz > entry.size then
                            -- Size strictly increased, no scalar progress — args growing
                            entry.growCount + 1

                        else
                            -- Size stayed same or decreased — making progress or stable
                            0
                in
                if newGrowCount >= 50 then
                    CycleDetected

                else
                    Continue (Dict.insert fnKey { fingerprint = fp, depth = callDepth, count = 1, size = sz, growCount = newGrowCount, argSizes = perArgSzs, argFingerprints = perArgFps } checkDict)


{-| Check if any argument has constant size but changing fingerprint.
This detects "countdown" variables — scalars whose value changes each call
(making progress toward a base case) even though their size stays at 1.
-}
hasBoundedProgress : List Int -> List Int -> List Int -> List Int -> Bool
hasBoundedProgress prevSizes prevFps newSizes newFps =
    case ( prevSizes, prevFps ) of
        ( ps :: psTail, pf :: pfTail ) ->
            case ( newSizes, newFps ) of
                ( ns :: nsTail, nf :: nfTail ) ->
                    (ps == ns && pf /= nf)
                        || hasBoundedProgress psTail pfTail nsTail nfTail

                _ ->
                    False

        _ ->
            False


evalExpression : Node Expression -> Eval Value
evalExpression initExpression initCfg initEnv =
    Recursion.runRecursion
        (\( Node range expression, cfg0, env ) ->
            case cfg0.maxSteps of
                Just 0 ->
                    Recursion.base
                        (EvErr
                            { currentModule = env.currentModule
                            , callStack = env.callStack
                            , error = Unsupported "Step limit exceeded"
                            }
                        )

                _ ->
                    let
                        cfg : Config
                        cfg =
                            case cfg0.maxSteps of
                                Nothing ->
                                    cfg0

                                Just n ->
                                    { cfg0 | maxSteps = Just (n - 1) }

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
                                    Types.succeedPartial <| Value.mkPartiallyApplied env [] lambda.args Nothing (AstImpl lambda.expression)

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
                Just (PartiallyApplied _ [] [] _ _ _) ->
                    -- Zero-arg thunk, needs call path via trampoline
                    Types.recurseThen ( fullExpr, cfg, env ) continuation

                Just value ->
                    continuation value

                Nothing ->
                    -- Check currentModuleFunctions before trampoline
                    case Dict.get name env.currentModuleFunctions of
                        Just function ->
                            if List.isEmpty function.arguments then
                                -- Zero-arg thunk, needs trampoline for body eval
                                Types.recurseThen ( fullExpr, cfg, env ) continuation

                            else
                                -- Has args, return PartiallyApplied directly
                                continuation
                                    (PartiallyApplied
                                        (if cfg.trace then
                                            Environment.call env.currentModule name env

                                         else
                                            Environment.callNoStack env.currentModule name env
                                        )
                                        []
                                        function.arguments
                                        (Just { moduleName = env.currentModule, name = name })
                                        (AstImpl function.expression)
                                        (List.length function.arguments)
                                    )

                        Nothing ->
                            -- Need full resolution (imports, aliases, variants, etc.)
                            Types.recurseThen ( fullExpr, cfg, env ) continuation

        Expression.ParenthesizedExpression inner ->
            evalOrRecurse ( inner, cfg, env ) continuation

        Expression.LambdaExpression lambda ->
            continuation (Value.mkPartiallyApplied env [] lambda.args Nothing (AstImpl lambda.expression))

        Expression.RecordAccessFunction field ->
            continuation (evalRecordAccessFunction field)

        Expression.Negation child ->
            case evalSimple child env of
                Just value ->
                    case value of
                        Int i ->
                            continuation (Int -i)

                        Float f ->
                            continuation (Float -f)

                        _ ->
                            Types.failPartial <| typeError env "Trying to negate a non-number"

                Nothing ->
                    Types.recurseThen ( fullExpr, cfg, env ) continuation

        Expression.OperatorApplication opName _ l r ->
            if cfg.trace then
                Types.recurseThen ( fullExpr, cfg, env ) continuation

            else
                case opName of
                    "<|" ->
                        Types.recurseThen ( fullExpr, cfg, env ) continuation

                    "|>" ->
                        Types.recurseThen ( fullExpr, cfg, env ) continuation

                    "||" ->
                        case evalSimple l env of
                            Just (Bool True) ->
                                continuation (Bool True)

                            Just (Bool False) ->
                                Types.recurseThen ( r, cfg, env ) continuation

                            Just v ->
                                Types.failPartial <| typeError env <| "|| applied to non-Bool " ++ Value.toString v

                            Nothing ->
                                -- Left complex: trampoline left, then short-circuit
                                Types.recurseThen ( l, cfg, env )
                                    (\lValue ->
                                        case lValue of
                                            Bool True ->
                                                continuation (Bool True)

                                            Bool False ->
                                                Types.recurseThen ( r, cfg, env ) continuation

                                            v ->
                                                Types.failPartial <| typeError env <| "|| applied to non-Bool " ++ Value.toString v
                                    )

                    "&&" ->
                        case evalSimple l env of
                            Just (Bool False) ->
                                continuation (Bool False)

                            Just (Bool True) ->
                                Types.recurseThen ( r, cfg, env ) continuation

                            Just v ->
                                Types.failPartial <| typeError env <| "&& applied to non-Bool " ++ Value.toString v

                            Nothing ->
                                -- Left complex: trampoline left, then short-circuit
                                Types.recurseThen ( l, cfg, env )
                                    (\lValue ->
                                        case lValue of
                                            Bool False ->
                                                continuation (Bool False)

                                            Bool True ->
                                                Types.recurseThen ( r, cfg, env ) continuation

                                            v ->
                                                Types.failPartial <| typeError env <| "&& applied to non-Bool " ++ Value.toString v
                                    )

                    "==" ->
                        -- Fast path for equality: inline primitive comparison
                        case evalSimple l env of
                            Just lValue ->
                                case evalSimple r env of
                                    Just rValue ->
                                        case Kernel.Utils.equal lValue rValue env of
                                            Ok True ->
                                                continuation (Bool True)

                                            Ok False ->
                                                continuation (Bool False)

                                            Err e ->
                                                Recursion.base (EvErr e)

                                    Nothing ->
                                        Types.recurseThen ( r, cfg, env )
                                            (\rValue ->
                                                case Kernel.Utils.equal lValue rValue env of
                                                    Ok True ->
                                                        continuation (Bool True)

                                                    Ok False ->
                                                        continuation (Bool False)

                                                    Err e ->
                                                        Recursion.base (EvErr e)
                                            )

                            Nothing ->
                                Types.recurseThen ( fullExpr, cfg, env ) continuation

                    "/=" ->
                        case evalSimple l env of
                            Just lValue ->
                                case evalSimple r env of
                                    Just rValue ->
                                        case Kernel.Utils.equal lValue rValue env of
                                            Ok True ->
                                                continuation (Bool False)

                                            Ok False ->
                                                continuation (Bool True)

                                            Err e ->
                                                Recursion.base (EvErr e)

                                    Nothing ->
                                        Types.recurseThen ( fullExpr, cfg, env ) continuation

                            Nothing ->
                                Types.recurseThen ( fullExpr, cfg, env ) continuation

                    _ ->
                        case resolveOperator opName of
                            Just ( _, kernelFn ) ->
                                case evalSimple l env of
                                    Just lValue ->
                                        case evalSimple r env of
                                            Just rValue ->
                                                -- Both simple: fully inline
                                                case kernelFn [ lValue, rValue ] cfg env of
                                                    EvOk v ->
                                                        continuation v

                                                    err ->
                                                        Recursion.base err

                                            Nothing ->
                                                -- Left simple, right complex: trampoline right only
                                                Types.recurseThen ( r, cfg, env )
                                                    (\rValue ->
                                                        case kernelFn [ lValue, rValue ] cfg env of
                                                            EvOk v ->
                                                                continuation v

                                                            err ->
                                                                Recursion.base err
                                                    )

                                    Nothing ->
                                        case evalSimple r env of
                                            Just rValue ->
                                                -- Left complex, right simple: trampoline left only
                                                Types.recurseThen ( l, cfg, env )
                                                    (\lValue ->
                                                        case kernelFn [ lValue, rValue ] cfg env of
                                                            EvOk v ->
                                                                continuation v

                                                            err ->
                                                                Recursion.base err
                                                    )

                                            Nothing ->
                                                -- Both complex: full trampoline
                                                Types.recurseThen ( fullExpr, cfg, env ) continuation

                            Nothing ->
                                Types.recurseThen ( fullExpr, cfg, env ) continuation

        Expression.RecordAccess recordExpr (Node _ field) ->
            case evalSimple recordExpr env of
                Just (Record fields) ->
                    case Dict.get field fields of
                        Just fieldValue ->
                            continuation fieldValue

                        Nothing ->
                            Types.failPartial <| typeError env <| "Field " ++ field ++ " not found [record access]"

                Just _ ->
                    Types.failPartial <| typeError env "Trying to access a field on a non-record value"

                Nothing ->
                    Types.recurseThen ( fullExpr, cfg, env ) continuation

        Expression.TupledExpression exprs ->
            case exprs of
                [] ->
                    continuation Unit

                [ c ] ->
                    case evalSimple c env of
                        Just v ->
                            continuation v

                        Nothing ->
                            Types.recurseThen ( fullExpr, cfg, env ) continuation

                [ l, r ] ->
                    case evalSimple l env of
                        Just lValue ->
                            case evalSimple r env of
                                Just rValue ->
                                    continuation (Tuple lValue rValue)

                                Nothing ->
                                    Types.recurseThen ( fullExpr, cfg, env ) continuation

                        Nothing ->
                            Types.recurseThen ( fullExpr, cfg, env ) continuation

                [ l, m, r ] ->
                    case evalSimple l env of
                        Just lValue ->
                            case evalSimple m env of
                                Just mValue ->
                                    case evalSimple r env of
                                        Just rValue ->
                                            continuation (Triple lValue mValue rValue)

                                        Nothing ->
                                            Types.recurseThen ( fullExpr, cfg, env ) continuation

                                Nothing ->
                                    Types.recurseThen ( fullExpr, cfg, env ) continuation

                        Nothing ->
                            Types.recurseThen ( fullExpr, cfg, env ) continuation

                _ ->
                    Types.failPartial <| typeError env "Tuples with more than three elements are not supported"

        Expression.IfBlock cond true false ->
            if cfg.trace then
                Types.recurseThen ( fullExpr, cfg, env ) continuation

            else
                case evalSimple cond env of
                    Just (Bool True) ->
                        Types.recurseThen ( true, cfg, env ) continuation

                    Just (Bool False) ->
                        Types.recurseThen ( false, cfg, env ) continuation

                    Just _ ->
                        Types.failPartial <| typeError env "ifThenElse condition was not a boolean"

                    Nothing ->
                        Types.recurseThen ( fullExpr, cfg, env ) continuation

        Expression.ListExpr [] ->
            continuation (List [])

        _ ->
            Types.recurseThen ( fullExpr, cfg, env ) continuation


{-| Try to evaluate a simple expression without recursion.
Returns Just value for literals and variable lookups, Nothing otherwise.
This is used to avoid stack-depth issues with recursive evalOrRecurse calls.
-}
evalSimple : Node Expression -> Env -> Maybe Value
evalSimple (Node _ expr) env =
    case expr of
        Expression.Integer i ->
            Just (Int i)

        Expression.Hex i ->
            Just (Int i)

        Expression.Floatable f ->
            Just (Float f)

        Expression.Literal s ->
            Just (String s)

        Expression.CharLiteral c ->
            Just (Char c)

        Expression.UnitExpr ->
            Just Unit

        Expression.FunctionOrValue [] name ->
            case Dict.get name env.values of
                Just (PartiallyApplied _ [] [] _ _ _) ->
                    Nothing

                Just value ->
                    Just value

                Nothing ->
                    Nothing

        Expression.ParenthesizedExpression inner ->
            evalSimple inner env

        _ ->
            Nothing


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
    case resolveOperator opName of
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


utilsKernelModuleName : ModuleName
utilsKernelModuleName =
    [ "Elm", "Kernel", "Utils" ]


resolveOperator : String -> Maybe ( ModuleName, List Value -> Eval Value )
resolveOperator opName =
    case opName of
        -- Comparison operators
        "==" ->
            Just ( utilsKernelModuleName, comparisonEq )

        "/=" ->
            Just ( utilsKernelModuleName, comparisonNeq )

        "<" ->
            Just ( utilsKernelModuleName, comparisonLt )

        ">" ->
            Just ( utilsKernelModuleName, comparisonGt )

        "<=" ->
            Just ( utilsKernelModuleName, comparisonLe )

        ">=" ->
            Just ( utilsKernelModuleName, comparisonGe )

        -- Append
        "++" ->
            Just ( utilsKernelModuleName, kernelAppend )

        -- All other operators: use the pre-built Dict
        _ ->
            Dict.get opName operatorKernelFunctions


{-| Pre-computed EvalResult values for Bool True/False.
Avoids allocating both the Bool wrapper and EvOk wrapper on every comparison.
-}
evalResultTrue : EvalResult Value
evalResultTrue =
    EvOk (Bool True)


evalResultFalse : EvalResult Value
evalResultFalse =
    EvOk (Bool False)


boolResult : Bool -> EvalResult Value
boolResult b =
    if b then
        evalResultTrue

    else
        evalResultFalse


comparisonEq : List Value -> Eval Value
comparisonEq args _ env =
    case args of
        [ Int li, Int ri ] ->
            boolResult (li == ri)

        [ String ls, String rs ] ->
            boolResult (ls == rs)

        [ l, r ] ->
            case Kernel.Utils.innerCompare l r env of
                Ok order ->
                    boolResult (order == EQ)

                Err e ->
                    EvErr e

        _ ->
            EvalResult.fail <| typeError env "Comparison needs exactly two arguments"


comparisonNeq : List Value -> Eval Value
comparisonNeq args _ env =
    case args of
        [ Int li, Int ri ] ->
            boolResult (li /= ri)

        [ String ls, String rs ] ->
            boolResult (ls /= rs)

        [ l, r ] ->
            case Kernel.Utils.innerCompare l r env of
                Ok order ->
                    boolResult (order /= EQ)

                Err e ->
                    EvErr e

        _ ->
            EvalResult.fail <| typeError env "Comparison needs exactly two arguments"


comparisonLt : List Value -> Eval Value
comparisonLt args _ env =
    case args of
        [ Int li, Int ri ] ->
            boolResult (li < ri)

        [ String ls, String rs ] ->
            boolResult (ls < rs)

        [ l, r ] ->
            case Kernel.Utils.innerCompare l r env of
                Ok order ->
                    boolResult (order == LT)

                Err e ->
                    EvErr e

        _ ->
            EvalResult.fail <| typeError env "Comparison needs exactly two arguments"


comparisonGt : List Value -> Eval Value
comparisonGt args _ env =
    case args of
        [ Int li, Int ri ] ->
            boolResult (li > ri)

        [ String ls, String rs ] ->
            boolResult (ls > rs)

        [ l, r ] ->
            case Kernel.Utils.innerCompare l r env of
                Ok order ->
                    boolResult (order == GT)

                Err e ->
                    EvErr e

        _ ->
            EvalResult.fail <| typeError env "Comparison needs exactly two arguments"


comparisonLe : List Value -> Eval Value
comparisonLe args _ env =
    case args of
        [ Int li, Int ri ] ->
            boolResult (li <= ri)

        [ String ls, String rs ] ->
            boolResult (ls <= rs)

        [ l, r ] ->
            case Kernel.Utils.innerCompare l r env of
                Ok order ->
                    boolResult (order /= GT)

                Err e ->
                    EvErr e

        _ ->
            EvalResult.fail <| typeError env "Comparison needs exactly two arguments"


comparisonGe : List Value -> Eval Value
comparisonGe args _ env =
    case args of
        [ Int li, Int ri ] ->
            boolResult (li >= ri)

        [ String ls, String rs ] ->
            boolResult (ls >= rs)

        [ l, r ] ->
            case Kernel.Utils.innerCompare l r env of
                Ok order ->
                    boolResult (order /= LT)

                Err e ->
                    EvErr e

        _ ->
            EvalResult.fail <| typeError env "Comparison needs exactly two arguments"


kernelAppend : List Value -> Eval Value
kernelAppend args cfg env =
    case args of
        [ l, r ] ->
            Kernel.Utils.append l r cfg env

        _ ->
            EvalResult.fail <| typeError env "Append needs exactly two arguments"


evalApplication : Node Expression -> List (Node Expression) -> PartialEval Value
evalApplication first rest cfg env =
    let
        inner : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Implementation -> PartialResult Value
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
                                    Types.succeedPartial <| Value.mkPartiallyApplied localEnv [ argValue ] patterns maybeQualifiedName implementation
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
                                            Types.succeedPartial <| Value.mkPartiallyApplied localEnv [ val1, val2 ] patterns maybeQualifiedName implementation
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

                PartiallyApplied localEnv oldArgs patterns maybeQualifiedName implementation _ ->
                    inner localEnv oldArgs patterns maybeQualifiedName implementation

                other ->
                    Types.failPartial <|
                        typeError env <|
                            "Trying to apply "
                                ++ Value.toString other
                                ++ ", which is a non-lambda non-variant"
        )


evalApplicationGeneralCompute : Node Expression -> List (Node Expression) -> List Value -> Env -> List (Node Pattern) -> Maybe QualifiedNameRef -> Implementation -> PartialEval Value
evalApplicationGeneralCompute first rest oldArgs localEnv patterns maybeQualifiedName implementation cfg env =
    evalApplicationGeneral first rest oldArgs (List.length oldArgs) (List.length patterns) localEnv patterns maybeQualifiedName implementation cfg env


evalApplicationGeneral : Node Expression -> List (Node Expression) -> List Value -> Int -> Int -> Env -> List (Node Pattern) -> Maybe QualifiedNameRef -> Implementation -> PartialEval Value
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
                    Types.succeedPartial <| Value.mkPartiallyApplied localEnv args patterns maybeQualifiedName implementation

                else
                    -- Just right, we special case this for TCO
                    evalFullyApplied localEnv args patterns maybeQualifiedName implementation cfg env
            )


evalFullyApplied : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Implementation -> PartialEval Value
evalFullyApplied localEnv args patterns maybeQualifiedName implementation cfg env =
    -- Fast path: if all patterns are simple VarPatterns or AllPatterns,
    -- bind directly via addValue instead of building an intermediate Dict.
    case bindSimplePatterns patterns args localEnv of
        Just boundEnv ->
            evalFullyAppliedWithEnv boundEnv args maybeQualifiedName implementation cfg env

        Nothing ->
            let
                maybeNewBindings : Result EvalErrorData (Maybe (List ( String, Value )))
                maybeNewBindings =
                    match env
                        (fakeNode <| ListPattern patterns)
                        (List args)
            in
            case maybeNewBindings of
                Err e ->
                    Types.failPartial e

                Ok Nothing ->
                    Types.failPartial <| typeError env "Could not match lambda patterns"

                Ok (Just newBindings) ->
                    evalFullyAppliedWithEnv (Environment.withBindings newBindings localEnv) args maybeQualifiedName implementation cfg env


{-| Try to bind all patterns directly via addValue. Returns Just the new env
if all patterns are simple (VarPattern, AllPattern, ParenthesizedPattern wrapping one).
Returns Nothing for complex patterns that need full match.

Batches dict insertions to avoid creating intermediate env records.
-}
bindSimplePatterns : List (Node Pattern) -> List Value -> Env -> Maybe Env
bindSimplePatterns patterns args env =
    bindSimplePatternsHelp patterns args env.values
        |> Maybe.map (\newValues -> Environment.replaceValues newValues env)


bindSimplePatternsHelp : List (Node Pattern) -> List Value -> EnvValues -> Maybe EnvValues
bindSimplePatternsHelp patterns args values =
    case ( patterns, args ) of
        ( [], [] ) ->
            Just values

        ( (Node _ (VarPattern name)) :: ptail, value :: atail ) ->
            bindSimplePatternsHelp ptail atail (Dict.insert name value values)

        ( (Node _ AllPattern) :: ptail, _ :: atail ) ->
            bindSimplePatternsHelp ptail atail values

        ( (Node _ (ParenthesizedPattern inner)) :: ptail, value :: atail ) ->
            bindSimplePatternsHelp (inner :: ptail) (value :: atail) values

        _ ->
            Nothing


evalFullyAppliedWithEnv : Env -> List Value -> Maybe QualifiedNameRef -> Implementation -> PartialEval Value
evalFullyAppliedWithEnv boundEnv args maybeQualifiedName implementation cfg env =
    case implementation of
        KernelImpl moduleName name f ->
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
                                    fakeNode <|
                                        Application <|
                                            fakeNode (FunctionOrValue moduleName name)
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

        AstImpl (Node range (FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name)) ->
            -- Fallback for AST-based kernel references (shouldn't happen often with KernelImpl)
            let
                key : String
                key =
                    Environment.moduleKey moduleName
            in
            case Dict.get key kernelFunctions of
                Nothing ->
                    Types.failPartial <| nameError env (Syntax.qualifiedNameToString { moduleName = moduleName, name = name })

                Just kernelModule ->
                    case Dict.get name kernelModule of
                        Nothing ->
                            Types.failPartial <| nameError env (Syntax.qualifiedNameToString { moduleName = moduleName, name = name })

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
            case maybeQualifiedName of
                Just qualifiedName ->
                    if env.callDepth < 200 then
                        call maybeQualifiedName implementation cfg boundEnv

                    else
                        let
                            currentCheck =
                                Maybe.withDefault Dict.empty env.recursionCheck
                        in
                        case checkAndUpdateCycle qualifiedName args env.callDepth currentCheck of
                            CycleDetected ->
                                Types.failPartial <|
                                    typeError env
                                        ("Infinite recursion detected: "
                                            ++ Syntax.qualifiedNameToString qualifiedName
                                            ++ " called with identical arguments"
                                        )

                            Continue updatedCheck ->
                                call maybeQualifiedName implementation cfg { boundEnv | recursionCheck = Just updatedCheck }

                Nothing ->
                    call maybeQualifiedName implementation cfg boundEnv


call : Maybe QualifiedNameRef -> Implementation -> PartialEval Value
call maybeQualifiedName implementation cfg env =
    case implementation of
        AstImpl expr ->
            case maybeQualifiedName of
                Just qualifiedName ->
                    let
                        callFn =
                            if cfg.trace then
                                Environment.call

                            else
                                Environment.callNoStack

                        newEnv =
                            callFn qualifiedName.moduleName qualifiedName.name env
                    in
                    if cfg.tcoTarget == Just qualifiedName.name then
                        -- Inside a tcoLoop: signal TailCall to the loop
                        Recursion.base
                            (EvErr
                                { currentModule = env.currentModule
                                , callStack = env.callStack
                                , error = TailCall env.values
                                }
                            )

                    else if isTailRecursive qualifiedName.name expr then
                        -- Static analysis confirmed tail-recursive: use tcoLoop
                        let
                            tcoCfg =
                                { cfg | tcoTarget = Just qualifiedName.name }

                            limit =
                                case cfg.maxSteps of
                                    Just n ->
                                        n

                                    Nothing ->
                                        -- No step limit: use generous default for legitimate programs.
                                        -- Infinite loops are caught by cycle detection in tcoLoop.
                                        500000
                        in
                        Recursion.base (tcoLoop qualifiedName.name expr limit tcoCfg newEnv)

                    else
                        -- Not tail-recursive: normal trampoline
                        Recursion.recurse ( expr, cfg, newEnv )

                Nothing ->
                    Recursion.recurse ( expr, cfg, env )

        KernelImpl moduleName name f ->
            if cfg.trace then
                let
                    childEnv : Env
                    childEnv =
                        Environment.callKernel moduleName name env
                in
                Recursion.base (f [] cfg childEnv)

            else
                Recursion.base (f [] cfg env)



{-| Static analysis: check if a function body is tail-recursive with respect
to a given function name. Returns True only if:
1. The body CONTAINS at least one self-call (otherwise it's not recursive at all)
2. ALL self-calls appear in tail position (outermost expression of if/case branches)

Functions with zero self-calls return False — they're non-recursive and don't
need TCO. This avoids wrapping every non-recursive function in a tcoLoop.
-}
isTailRecursive : String -> Node Expression -> Bool
isTailRecursive funcName ((Node _ expr) as node) =
    -- First: must actually contain a self-call somewhere
    containsSelfCall funcName expr
        && isTailRecursiveHelper funcName node


{-| Check the structure of a function body that's known to contain self-calls.
Verifies all self-calls are in tail position.
-}
isTailRecursiveHelper : String -> Node Expression -> Bool
isTailRecursiveHelper funcName (Node _ expr) =
    case expr of
        -- If/else: both branches must be tail-safe
        Expression.IfBlock _ (Node _ trueExpr) (Node _ falseExpr) ->
            isTailSafe funcName trueExpr && isTailSafe funcName falseExpr

        -- Case: all branches must be tail-safe
        Expression.CaseExpression { cases } ->
            List.all (\( _, Node _ branchExpr ) -> isTailSafe funcName branchExpr) cases

        -- Let: declarations must NOT contain self-calls (they're not in tail position),
        -- and the body expression (the "in" part) must be tail-recursive
        Expression.LetExpression { declarations, expression } ->
            not (letDeclarationsContainSelfCall funcName declarations)
                && isTailRecursiveHelper funcName expression

        -- A bare self-call at the top level is tail-recursive
        Expression.Application ((Node _ (Expression.FunctionOrValue [] name)) :: _) ->
            name == funcName

        -- Anything else with self-calls: NOT tail-recursive
        _ ->
            False


{-| Check if an expression in tail position is safe. An expression is tail-safe if:
1. It's a self-call (tail call — good), OR
2. It doesn't contain any self-calls (base case — good), OR
3. It's a branching expression where all branches are tail-safe (if/case/let)
-}
isTailSafe : String -> Expression -> Bool
isTailSafe funcName expr =
    case expr of
        -- Self-call in tail position: the call itself is tail, but the ARGUMENTS
        -- must not contain non-tail self-calls (e.g. Dict.foldl calls itself
        -- in an argument: foldl func (... (foldl func acc left)) right)
        Expression.Application ((Node _ (Expression.FunctionOrValue [] name)) :: args) ->
            if name == funcName then
                -- Tail self-call, but verify args don't have nested self-calls
                not (List.any (\(Node _ a) -> containsSelfCall funcName a) args)

            else
                -- Not a self-call: safe only if no self-calls anywhere
                not (containsSelfCall funcName expr)

        -- If/else: recurse into branches
        Expression.IfBlock _ (Node _ trueExpr) (Node _ falseExpr) ->
            isTailSafe funcName trueExpr && isTailSafe funcName falseExpr

        -- Case: recurse into all branches
        Expression.CaseExpression { cases } ->
            List.all (\( _, Node _ branchExpr ) -> isTailSafe funcName branchExpr) cases

        -- Let: declarations must not contain self-calls, body must be tail-safe
        Expression.LetExpression { declarations, expression } ->
            let
                (Node _ bodyExpr) =
                    expression
            in
            not (letDeclarationsContainSelfCall funcName declarations)
                && isTailSafe funcName bodyExpr

        -- Parenthesized: unwrap
        Expression.ParenthesizedExpression (Node _ inner) ->
            isTailSafe funcName inner

        -- Anything else: safe only if no self-calls
        _ ->
            not (containsSelfCall funcName expr)


{-| Check if an expression contains a call to funcName anywhere (not in tail position).
-}
containsSelfCall : String -> Expression -> Bool
containsSelfCall funcName expr =
    case expr of
        Expression.Application ((Node _ (Expression.FunctionOrValue [] name)) :: args) ->
            name == funcName || List.any (\(Node _ e) -> containsSelfCall funcName e) args

        Expression.Application exprs ->
            List.any (\(Node _ e) -> containsSelfCall funcName e) exprs

        Expression.IfBlock (Node _ cond) (Node _ t) (Node _ f) ->
            containsSelfCall funcName cond || containsSelfCall funcName t || containsSelfCall funcName f

        Expression.CaseExpression { expression, cases } ->
            let
                (Node _ caseExpr) =
                    expression
            in
            containsSelfCall funcName caseExpr
                || List.any (\( _, Node _ branchExpr ) -> containsSelfCall funcName branchExpr) cases

        Expression.OperatorApplication _ _ (Node _ l) (Node _ r) ->
            containsSelfCall funcName l || containsSelfCall funcName r

        Expression.FunctionOrValue [] name ->
            name == funcName

        Expression.LetExpression { declarations, expression } ->
            let
                (Node _ bodyExpr) =
                    expression
            in
            containsSelfCall funcName bodyExpr

        Expression.TupledExpression exprs ->
            List.any (\(Node _ e) -> containsSelfCall funcName e) exprs

        Expression.ParenthesizedExpression (Node _ inner) ->
            containsSelfCall funcName inner

        Expression.ListExpr exprs ->
            List.any (\(Node _ e) -> containsSelfCall funcName e) exprs

        Expression.RecordExpr fields ->
            List.any (\(Node _ ( _, Node _ e )) -> containsSelfCall funcName e) fields

        Expression.Negation (Node _ inner) ->
            containsSelfCall funcName inner

        Expression.LambdaExpression { expression } ->
            let
                (Node _ lambdaBody) =
                    expression
            in
            containsSelfCall funcName lambdaBody

        Expression.RecordAccess (Node _ inner) _ ->
            containsSelfCall funcName inner

        _ ->
            False


{-| Check if any let declaration body contains a self-call.
-}
letDeclarationsContainSelfCall : String -> List (Node Expression.LetDeclaration) -> Bool
letDeclarationsContainSelfCall funcName declarations =
    List.any
        (\(Node _ decl) ->
            case decl of
                Expression.LetFunction { declaration } ->
                    let
                        (Node _ funcDecl) =
                            declaration
                    in
                    containsSelfCall funcName (Node.value funcDecl.expression)

                Expression.LetDestructuring _ (Node _ expr) ->
                    containsSelfCall funcName expr
        )
        declarations


{-| Check if args of a self-call application contain further self-calls.
-}
containsSelfCallInArgs : String -> Expression -> Bool
containsSelfCallInArgs funcName expr =
    case expr of
        Expression.Application (_ :: args) ->
            List.any (\(Node _ e) -> containsSelfCall funcName e) args

        _ ->
            False


{-| TCO loop: evaluates body expression in a tight loop using nested
evalExpression calls. Each iteration is a complete evaluation; self-calls
are detected and caught as TailCall errors, then rebound and looped.

Cycle detection:
- Category A: identical values between iterations → immediate detection
- Category B: values grow monotonically for 50+ iterations → likely infinite

This is tail-recursive in Elm, compiled to a while loop by the host compiler.
-}
tcoLoop : String -> Node Expression -> Int -> Config -> Env -> EvalResult Value
tcoLoop funcName body remaining cfg env =
    tcoLoopHelp funcName body remaining 0 0 (fingerprintValues env.values) cfg env


tcoLoopHelp : String -> Node Expression -> Int -> Int -> Int -> Int -> Config -> Env -> EvalResult Value
tcoLoopHelp funcName body remaining lastSize growCount lastFingerprint cfg env =
    if remaining <= 0 then
        EvErr
            { currentModule = env.currentModule
            , callStack = env.callStack
            , error = Unsupported "Step limit exceeded"
            }

    else
        let
            innerCfg =
                { cfg | maxSteps = Nothing }

            result =
                evalExpression body innerCfg env
        in
        case tcoExtractTailCall result of
            Just newValues ->
                -- Got a TailCall signal
                if newValues == env.values then
                    -- Category A: identical args → infinite loop
                    EvErr
                        { currentModule = env.currentModule
                        , callStack = env.callStack
                        , error = TypeError ("Infinite recursion detected: " ++ funcName ++ " called with identical arguments")
                        }

                else
                    let
                        newSize =
                            valuesSize newValues

                        newFingerprint =
                            fingerprintValues newValues

                        -- "Bounded progress": at least one value has constant size
                        -- but changing fingerprint. This detects countdown variables
                        -- (Int changing value but constant size=1), indicating
                        -- progress toward a base case even if total size grows.
                        boundedProgress =
                            hasBoundedProgressInValues env.values newValues

                        newGrowCount =
                            if boundedProgress then
                                -- A scalar argument is changing — likely a countdown
                                0

                            else if newSize > lastSize && lastSize > 0 then
                                growCount + 1

                            else
                                0
                    in
                    if newGrowCount >= 50 then
                        -- Category B: growing without bound
                        EvErr
                            { currentModule = env.currentModule
                            , callStack = env.callStack
                            , error = TypeError ("Infinite recursion detected: " ++ funcName ++ " arguments growing without bound")
                            }

                    else
                        -- Continue loop (this IS the tail call for Elm's TCO)
                        tcoLoopHelp funcName body (remaining - 1) newSize newGrowCount newFingerprint cfg (Environment.replaceValues newValues env)

            Nothing ->
                -- Not a TailCall: return the result as-is
                result


{-| Extract TailCall values from an EvalResult, or Nothing if not a TailCall.
-}
tcoExtractTailCall : EvalResult Value -> Maybe EnvValues
tcoExtractTailCall result =
    case result of
        EvErr { error } ->
            case error of
                TailCall newValues ->
                    Just newValues

                _ ->
                    Nothing

        EvErrTrace { error } _ _ ->
            case error of
                TailCall newValues ->
                    Just newValues

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Compute total size of all values in an env's values dict.
-}
valuesSize : EnvValues -> Int
valuesSize values =
    Dict.foldl (\_ v acc -> acc + sizeOfValue v) 0 values


{-| Cheap fingerprint of all values in an env's values dict.
-}
fingerprintValues : EnvValues -> Int
fingerprintValues values =
    Dict.foldl (\k v acc -> Bitwise.xor (acc * 31) (fingerprintValue v + String.length k)) 5381 values


{-| Check if any value has constant size but changing fingerprint between
two env value dicts. This detects "countdown" variables that indicate
progress toward a base case.
-}
hasBoundedProgressInValues : EnvValues -> EnvValues -> Bool
hasBoundedProgressInValues oldValues newValues =
    Dict.foldl
        (\key newVal found ->
            if found then
                True

            else
                case Dict.get key oldValues of
                    Just oldVal ->
                        let
                            oldSz =
                                sizeOfValue oldVal

                            newSz =
                                sizeOfValue newVal
                        in
                        -- Same size but different fingerprint = bounded progress
                        oldSz == newSz && fingerprintValue oldVal /= fingerprintValue newVal

                    Nothing ->
                        False
        )
        False
        newValues


evalFunctionOrValue : ModuleName -> String -> PartialEval Value
evalFunctionOrValue moduleName name cfg env =
    case moduleName of
        [] ->
            -- Fast path: local value lookup before isVariant check.
            -- Avoids String.uncons + Char.toCode for the most common case.
            case Dict.get name env.values of
                Just (PartiallyApplied localEnv [] [] maybeName implementation _) ->
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
                        call (Just qualifiedNameRef) (AstImpl function.expression) cfg env

                    else
                        PartiallyApplied
                            (callFn env.currentModule name env)
                            []
                            function.arguments
                            (Just qualifiedNameRef)
                            (AstImpl function.expression)
                            (List.length function.arguments)
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
                    call (Just { moduleName = resolvedModule, name = name }) (AstImpl function.expression) cfg env

                else
                    PartiallyApplied
                        (Environment.call resolvedModule name env)
                        []
                        function.arguments
                        (Just { moduleName = resolvedModule, name = name })
                        (AstImpl function.expression)
                        (List.length function.arguments)
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
                Dict.get resolvedModuleKey env.shared.functions
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
                                        Dict.get sourceModuleKey env.shared.functions
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
                                case Dict.get moduleNameKey env.shared.functions of
                                    Just moduleDict ->
                                        case Dict.get name moduleDict of
                                            Just f ->
                                                Just ( moduleName, moduleNameKey, f )

                                            Nothing ->
                                                -- Module exists but function not found.
                                                -- Could be aliased to a different module.
                                                case Dict.get moduleNameKey env.imports.aliases of
                                                    Just ( canonical, canonicalKey ) ->
                                                        Dict.get canonicalKey env.shared.functions
                                                            |> Maybe.andThen (Dict.get name)
                                                            |> Maybe.map (\f -> ( canonical, canonicalKey, f ))

                                                    Nothing ->
                                                        Nothing

                                    Nothing ->
                                        -- Module not found directly; try alias resolution
                                        case Dict.get moduleNameKey env.imports.aliases of
                                            Just ( canonical, canonicalKey ) ->
                                                Dict.get canonicalKey env.shared.functions
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
                            if resolvedModuleKey == env.currentModuleKey then
                                -- Same module: keep local values, use existing caches
                                if List.isEmpty function.arguments then
                                    call (Just qualifiedNameRef) (AstImpl function.expression) cfg env

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
                                        (AstImpl function.expression)
                                        (List.length function.arguments)
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
                                        , shared = env.shared
                                        , currentModuleFunctions =
                                            Dict.get resolvedModuleKey env.shared.functions
                                                |> Maybe.withDefault Dict.empty
                                        , values = Dict.empty
                                        , imports =
                                            Dict.get resolvedModuleKey env.shared.moduleImports
                                                |> Maybe.withDefault env.imports
                                        , callDepth = env.callDepth + 1
                                        , recursionCheck = env.recursionCheck
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
                                        (AstImpl function.expression)
                                        (List.length function.arguments)
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
        EvalResult.succeed <| Value.mkPartiallyApplied localEnv oldArgs patterns functionName implementation

    else
        -- Just right, we special case this for TCO
        case bindSimplePatterns patterns oldArgs localEnv of
            Just boundEnv ->
                case implementation of
                    KernelImpl moduleName name f ->
                        if cfg.trace then
                            f oldArgs
                                cfg
                                (Environment.callKernel moduleName name localEnv)

                        else
                            f oldArgs cfg localEnv

                    AstImpl expr ->
                        evalExpression expr cfg boundEnv

            Nothing ->
                let
                    maybeNewBindings : Result EvalErrorData (Maybe (List ( String, Value )))
                    maybeNewBindings =
                        match localEnv
                            (Node (Range.combine (List.map Node.range patterns)) <| ListPattern patterns)
                            (List oldArgs)
                in
                case maybeNewBindings of
                    Err e ->
                        EvalResult.fail e

                    Ok Nothing ->
                        EvalResult.fail <| typeError localEnv "Could not match lambda patterns"

                    Ok (Just newBindings) ->
                        case implementation of
                            KernelImpl moduleName name f ->
                                if cfg.trace then
                                    f oldArgs
                                        cfg
                                        (Environment.callKernel moduleName name localEnv)

                                else
                                    f oldArgs cfg localEnv

                            AstImpl expr ->
                                -- This is fine because it's never going to be recursive. FOR NOW. TODO: fix
                                evalExpression expr
                                    cfg
                                    (localEnv |> Environment.withBindings newBindings)


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
                            (KernelImpl moduleName name f)
                            argCount
                            |> Types.succeedPartial


{-| Fall back to generated AST for kernel functions not in native kernel.
-}
evalKernelFunctionFromAst : ModuleName -> String -> PartialEval Value
evalKernelFunctionFromAst moduleName name cfg env =
    evalKernelFunctionFromAstWithKey (Environment.moduleKey moduleName) moduleName name cfg env


evalKernelFunctionFromAstWithKey : String -> ModuleName -> String -> PartialEval Value
evalKernelFunctionFromAstWithKey key moduleName name cfg env =
    case Dict.get key env.shared.functions of
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
                        (AstImpl function.expression)
                        (List.length function.arguments)
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
                    -- Two-pass processing for mutual recursion support:
                    -- Pass 1: register all function declarations in currentModuleFunctions
                    --         so they can find each other
                    -- Pass 2: create PartiallyApplied values and evaluate non-function decls
                    let
                        envWithAllFunctions =
                            List.foldl
                                (\(Node _ decl) e ->
                                    case decl of
                                        Expression.LetFunction { declaration } ->
                                            let
                                                impl =
                                                    Node.value declaration
                                            in
                                            if not (List.isEmpty impl.arguments) then
                                                Environment.addLocalFunction impl e

                                            else
                                                e

                                        _ ->
                                            e
                                )
                                env
                                sd
                    in
                    List.foldl
                        (\declaration acc ->
                            EvalResult.andThen
                                (\e -> addLetDeclaration declaration cfg e)
                                acc
                        )
                        (EvalResult.succeed envWithAllFunctions)
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
                        let
                            fnName : String
                            fnName =
                                Node.value name

                            -- Check if the function body references itself
                            -- (excluding parameter names which shadow outer scope).
                            -- Always register in currentModuleFunctions so the
                            -- body can find itself (self-recursion) AND so sibling
                            -- let-functions can find it (mutual recursion).
                            envWithFn : Env
                            envWithFn =
                                Environment.addLocalFunction implementation env

                            arity : Int
                            arity =
                                List.length implementation.arguments
                        in
                        EvalResult.succeed <|
                            Environment.addValue fnName
                                (PartiallyApplied envWithFn [] implementation.arguments Nothing (AstImpl implementation.expression) arity)
                                envWithFn

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

                            Ok (Just patternBindings) ->
                                Ok (Environment.withBindings patternBindings env)
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
        (AstImpl <|
            fakeNode <|
                Expression.RecordAccess
                    (fakeNode <| Expression.FunctionOrValue [] "$r")
                    (fakeNode <| String.dropLeft 1 field)
        )
        1


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

        Just operatorRef ->
            -- Look up the actual function implementation to avoid circular
            -- references (e.g., Parser.keeper = (|=) which maps back to keeper).
            -- Resolve through the functions dictionary to get the real implementation.
            let
                resolvedRef : { moduleName : ModuleName, name : String, function : Maybe Expression.FunctionImplementation }
                resolvedRef =
                    resolveOperatorFunction operatorRef
            in
            case resolvedRef.function of
                Just function ->
                    if List.isEmpty function.arguments then
                        -- Zero-arg function (eta-reduced): evaluate its body directly
                        -- This handles cases like `keeper = A.keeper` or `keeper = (|=)`
                        Recursion.recurse
                            ( function.expression
                            , cfg
                            , { env
                                | currentModule = resolvedRef.moduleName
                                , currentModuleKey = Environment.moduleKey resolvedRef.moduleName
                                , currentModuleFunctions =
                                    Dict.get (Environment.moduleKey resolvedRef.moduleName) env.shared.functions
                                        |> Maybe.withDefault Dict.empty
                                , values = Dict.empty
                                , imports =
                                    Dict.get (Environment.moduleKey resolvedRef.moduleName) env.shared.moduleImports
                                        |> Maybe.withDefault env.imports
                              }
                            )

                    else
                        let
                            callFn =
                                if cfg.trace then
                                    Environment.call

                                else
                                    Environment.callNoStack
                        in
                        PartiallyApplied
                            (callFn resolvedRef.moduleName resolvedRef.name env)
                            []
                            function.arguments
                            (Just { moduleName = resolvedRef.moduleName, name = resolvedRef.name })
                            (AstImpl function.expression)
                            (List.length function.arguments)
                            |> Types.succeedPartial

                Nothing ->
                    -- Fallback: function not found in coreFunctions, use indirect call
                    let
                        callFn =
                            if cfg.trace then
                                Environment.call

                            else
                                Environment.callNoStack
                    in
                    PartiallyApplied
                        (callFn operatorRef.moduleName opName env)
                        []
                        [ fakeNode <| VarPattern "$l", fakeNode <| VarPattern "$r" ]
                        Nothing
                        (AstImpl <|
                            fakeNode <|
                                Expression.Application
                                    [ fakeNode <| Expression.FunctionOrValue operatorRef.moduleName operatorRef.name
                                    , fakeNode <| Expression.FunctionOrValue [] "$l"
                                    , fakeNode <| Expression.FunctionOrValue [] "$r"
                                    ]
                        )
                        2
                        |> Types.succeedPartial


{-| Resolve an operator reference through the function chain.
Follows zero-arg functions that are just re-exports (like Parser.keeper = (|=))
until we find the real implementation with arguments.
-}
resolveOperatorFunction : QualifiedNameRef -> { moduleName : ModuleName, name : String, function : Maybe Expression.FunctionImplementation }
resolveOperatorFunction ref =
    case Dict.get ref.moduleName Core.functions |> Maybe.andThen (Dict.get ref.name) of
        Just function ->
            if List.isEmpty function.arguments then
                -- Zero-arg function: check if body is another reference we can follow
                case Node.value function.expression of
                    Expression.FunctionOrValue qualModule qualName ->
                        if not (List.isEmpty qualModule) then
                            -- Follow the chain (e.g., keeper = A.keeper)
                            resolveOperatorFunction { moduleName = qualModule, name = qualName }

                        else
                            { moduleName = ref.moduleName, name = ref.name, function = Just function }

                    Expression.PrefixOperator _ ->
                        -- Body is (|=) or (|.) - this is a re-export like `keeper = (|=)`.
                        -- Search all modules for the actual implementation of this function name.
                        resolveOperatorAcrossModules ref.name

                    _ ->
                        { moduleName = ref.moduleName, name = ref.name, function = Just function }

            else
                { moduleName = ref.moduleName, name = ref.name, function = Just function }

        Nothing ->
            { moduleName = ref.moduleName, name = ref.name, function = Nothing }


{-| Search all Core modules for a function with the given name that has a real
implementation (non-empty arguments), skipping re-exports like `keeper = (|=)`.
-}
resolveOperatorAcrossModules : String -> { moduleName : ModuleName, name : String, function : Maybe Expression.FunctionImplementation }
resolveOperatorAcrossModules name =
    Core.functions
        |> Dict.foldl
            (\moduleName moduleDict acc ->
                case acc of
                    Just found ->
                        Just found

                    Nothing ->
                        case Dict.get name moduleDict of
                            Just function ->
                                if not (List.isEmpty function.arguments) then
                                    Just { moduleName = moduleName, name = name, function = Just function }

                                else
                                    Nothing

                            Nothing ->
                                Nothing
            )
            Nothing
        |> Maybe.withDefault { moduleName = [], name = name, function = Nothing }


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
                maybePartial : Result EvalErrorData (Maybe ( List ( String, Value ), Node Expression ))
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

                Ok (Just ( additionalBindings, branchExpression )) ->
                    case additionalBindings of
                        [] ->
                            Recursion.recurse ( branchExpression, cfg, env )

                        _ ->
                            Recursion.recurse
                                ( branchExpression
                                , cfg
                                , Environment.withBindings additionalBindings env
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


match : Env -> Node Pattern -> Value -> Result EvalErrorData (Maybe (List ( String, Value )))
match env (Node _ pattern) value =
    case ( pattern, value ) of
        ( UnitPattern, Unit ) ->
            matchOkEmpty

        ( UnitPattern, _ ) ->
            matchNoMatch

        ( AllPattern, _ ) ->
            matchOkEmpty

        ( ParenthesizedPattern subPattern, _ ) ->
            match env subPattern value

        ( NamedPattern { name } [], Bool True ) ->
            if name == "True" then
                matchOkEmpty

            else
                matchNoMatch

        ( NamedPattern { name } [], Bool False ) ->
            if name == "False" then
                matchOkEmpty

            else
                matchNoMatch

        ( NamedPattern namePattern argsPatterns, Custom variant args ) ->
            -- Two names from different modules can never have the same type
            -- so if we assume the code typechecks we can skip the module name check
            if namePattern.name == variant.name then
                case ( argsPatterns, args ) of
                    ( [], [] ) ->
                        -- Zero-arg constructor like True, False, Nothing
                        matchOkEmpty

                    ( [ singlePattern ], [ singleArg ] ) ->
                        -- Single-arg constructor like Just x, Ok x — very common
                        match env singlePattern singleArg

                    _ ->
                        let
                            matchNamedPatternHelper :
                                List ( String, Value )
                                -> ( List (Node Pattern), List Value )
                                -> Result EvalErrorData (Maybe (List ( String, Value )))
                            matchNamedPatternHelper bindings queue =
                                case queue of
                                    ( [], [] ) ->
                                        matchOk bindings

                                    ( patternHead :: patternTail, argHead :: argTail ) ->
                                        match env patternHead argHead
                                            |> matchAndThen
                                                (\newBindings ->
                                                    matchNamedPatternHelper (newBindings ++ bindings) ( patternTail, argTail )
                                                )

                                    _ ->
                                        Err <| typeError env "Mismatched number of arguments to variant"
                        in
                        matchNamedPatternHelper [] ( argsPatterns, args )

            else
                matchNoMatch

        ( NamedPattern _ _, _ ) ->
            matchNoMatch

        ( ListPattern patterns, List values ) ->
            matchListHelp env [] patterns values

        ( UnConsPattern (Node _ (VarPattern headName)) (Node _ (VarPattern tailName)), List (listHead :: listTail) ) ->
            -- Fast path: x :: xs with VarPatterns — avoid match calls
            matchOk [ ( headName, listHead ), ( tailName, List listTail ) ]

        ( UnConsPattern patternHead patternTail, List (listHead :: listTail) ) ->
            match env patternHead listHead
                |> matchAndThen
                    (\headBindings ->
                        match env patternTail (List listTail)
                            |> matchAndThen
                                (\tailBindings ->
                                    matchOk (headBindings ++ tailBindings)
                                )
                    )

        ( UnConsPattern _ _, _ ) ->
            matchNoMatch

        ( VarPattern name, _ ) ->
            matchOk [ ( name, value ) ]

        ( ListPattern _, _ ) ->
            matchNoMatch

        ( CharPattern c, Char d ) ->
            if c == d then
                matchOkEmpty

            else
                matchNoMatch

        ( CharPattern _, _ ) ->
            matchNoMatch

        ( StringPattern c, String d ) ->
            if c == d then
                matchOkEmpty

            else
                matchNoMatch

        ( StringPattern _, _ ) ->
            matchNoMatch

        ( IntPattern c, Int d ) ->
            if c == d then
                matchOkEmpty

            else
                matchNoMatch

        ( IntPattern _, _ ) ->
            matchNoMatch

        ( HexPattern c, Int d ) ->
            if c == d then
                matchOkEmpty

            else
                matchNoMatch

        ( HexPattern _, _ ) ->
            matchNoMatch

        ( FloatPattern c, Float d ) ->
            if c == d then
                matchOkEmpty

            else
                matchNoMatch

        ( FloatPattern _, _ ) ->
            matchNoMatch

        ( TuplePattern [ Node _ (VarPattern lname), Node _ (VarPattern rname) ], Tuple lvalue rvalue ) ->
            -- Fast path: two VarPatterns — avoid match calls entirely
            matchOk [ ( lname, lvalue ), ( rname, rvalue ) ]

        ( TuplePattern [ lpattern, rpattern ], Tuple lvalue rvalue ) ->
            match env lpattern lvalue
                |> matchAndThen
                    (\lbindings ->
                        match env rpattern rvalue
                            |> matchAndThen
                                (\rbindings ->
                                    matchOk (lbindings ++ rbindings)
                                )
                    )

        ( TuplePattern [ Node _ (VarPattern lname), Node _ (VarPattern mname), Node _ (VarPattern rname) ], Triple lvalue mvalue rvalue ) ->
            -- Fast path: three VarPatterns — avoid match calls entirely
            matchOk [ ( lname, lvalue ), ( mname, mvalue ), ( rname, rvalue ) ]

        ( TuplePattern [ lpattern, mpattern, rpattern ], Triple lvalue mvalue rvalue ) ->
            match env lpattern lvalue
                |> matchAndThen
                    (\lbindings ->
                        match env mpattern mvalue
                            |> matchAndThen
                                (\mbindings ->
                                    match env rpattern rvalue
                                        |> matchAndThen
                                            (\rbindings ->
                                                matchOk (lbindings ++ mbindings ++ rbindings)
                                            )
                                )
                    )

        ( TuplePattern _, _ ) ->
            matchNoMatch

        ( AsPattern childPattern (Node _ asName), _ ) ->
            match env childPattern value
                |> matchAndThen
                    (\bindings -> matchOk (( asName, value ) :: bindings))

        ( RecordPattern fields, Record fieldValues ) ->
            List.foldl
                (\(Node _ fieldName) ->
                    matchAndThen
                        (\acc ->
                            case Dict.get fieldName fieldValues of
                                Nothing ->
                                    Err <| typeError env <| "Field " ++ fieldName ++ " not found in record"

                                Just fieldValue ->
                                    matchOk (( fieldName, fieldValue ) :: acc)
                        )
                )
                matchOkEmpty
                fields

        ( RecordPattern _, _ ) ->
            matchNoMatch


{-| Cached constant for pattern matches with no bindings.
Avoids allocating Ok(Just []) on every literal/unit/wildcard match.
-}
matchOkEmpty : Result error (Maybe (List a))
matchOkEmpty =
    Ok (Just [])


matchListHelp : Env -> List ( String, Value ) -> List (Node Pattern) -> List Value -> Result EvalErrorData (Maybe (List ( String, Value )))
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

                Ok (Just headBindings) ->
                    matchListHelp env (headBindings ++ acc) patternTail valueTail

        _ ->
            Ok Nothing
