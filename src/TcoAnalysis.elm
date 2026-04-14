module TcoAnalysis exposing (ListDrainInfo, TcoMetadata, TcoStrategy(..), analyze)

{-| Static analysis of tail-recursive function bodies to determine
whether cycle detection can be safely skipped at runtime, and to
extract structural info for body specialization.
-}

import Elm.Syntax.Expression exposing (CaseBlock, Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Set exposing (Set)


type TcoStrategy
    = TcoListDrain ListDrainInfo
    | TcoSafe
    | TcoGeneral


type alias ListDrainInfo =
    { listArgName : String
    , headBindingName : Maybe String
    , tailBindingName : String
    , baseCaseBody : Node Expression
    , consCaseBody : Node Expression
    }


{-| Per-function metadata cached on `SharedContext.tcoAnalyses` so that
`tcoLoop` and the call-dispatch path can look up tail-recursion shape
info in O(1) instead of walking the body's AST on every call.
Populated once during project load by `Eval.Module.precomputeTcoAnalyses`.
-}
type alias TcoMetadata =
    { strategy : TcoStrategy
    , isTailRec : Bool
    }


analyze : String -> List String -> Node Expression -> TcoStrategy
analyze qualifiedFuncName paramNames body =
    let
        funcName =
            qualifiedFuncName
                |> String.split "."
                |> List.reverse
                |> List.head
                |> Maybe.withDefault qualifiedFuncName
    in
    case analyzeListDrain funcName paramNames body of
        Just info ->
            TcoListDrain info

        Nothing ->
            let
                tailCalls =
                    collectTailCalls funcName body
            in
            if List.isEmpty tailCalls then
                TcoGeneral

            else if List.all (callHasShrinkingArg body) tailCalls then
                TcoSafe

            else
                TcoGeneral


analyzeListDrain : String -> List String -> Node Expression -> Maybe ListDrainInfo
analyzeListDrain funcName paramNames body =
    case unwrapLets body of
        Node _ (CaseExpression caseBlock) ->
            analyzeListDrainCase funcName paramNames caseBlock

        _ ->
            Nothing


unwrapLets : Node Expression -> Node Expression
unwrapLets ((Node _ expr) as node) =
    case expr of
        LetExpression { expression } ->
            unwrapLets expression

        _ ->
            node


analyzeListDrainCase : String -> List String -> CaseBlock -> Maybe ListDrainInfo
analyzeListDrainCase funcName paramNames { expression, cases } =
    case expression of
        Node _ (FunctionOrValue [] listArgName) ->
            if not (List.member listArgName paramNames) then
                Nothing

            else
                case ( findBaseCase funcName cases, findConsCase cases ) of
                    ( Just baseCaseBody, Just ( headBinding, tailBinding, consCaseBody ) ) ->
                        if List.all (callPassesTailVar tailBinding) (collectTailCalls funcName consCaseBody) then
                            Just
                                { listArgName = listArgName
                                , headBindingName = headBinding
                                , tailBindingName = tailBinding
                                , baseCaseBody = baseCaseBody
                                , consCaseBody = consCaseBody
                                }

                        else
                            Nothing

                    _ ->
                        Nothing

        _ ->
            Nothing


findBaseCase : String -> List ( Node Pattern, Node Expression ) -> Maybe (Node Expression)
findBaseCase funcName cases =
    List.filterMap
        (\( Node _ pat, body ) ->
            case pat of
                ListPattern [] ->
                    if List.isEmpty (collectTailCalls funcName body) then
                        Just body

                    else
                        Nothing

                _ ->
                    Nothing
        )
        cases
        |> List.head


findConsCase : List ( Node Pattern, Node Expression ) -> Maybe ( Maybe String, String, Node Expression )
findConsCase cases =
    List.filterMap
        (\( Node _ pat, body ) ->
            case pat of
                UnConsPattern (Node _ headPat) (Node _ (VarPattern tailName)) ->
                    Just
                        ( case headPat of
                            VarPattern n ->
                                Just n

                            AllPattern ->
                                Nothing

                            _ ->
                                Just "__head__"
                        , tailName
                        , body
                        )

                _ ->
                    Nothing
        )
        cases
        |> List.head


callPassesTailVar : String -> List (Node Expression) -> Bool
callPassesTailVar tailVarName args =
    List.any
        (\(Node _ argExpr) ->
            case argExpr of
                FunctionOrValue [] name ->
                    name == tailVarName

                _ ->
                    False
        )
        args


collectTailCalls : String -> Node Expression -> List (List (Node Expression))
collectTailCalls funcName (Node _ expr) =
    case expr of
        IfBlock _ thenBranch elseBranch ->
            collectTailCalls funcName thenBranch ++ collectTailCalls funcName elseBranch

        CaseExpression { cases } ->
            List.concatMap (\( _, branchBody ) -> collectTailCalls funcName branchBody) cases

        LetExpression { expression } ->
            collectTailCalls funcName expression

        Application ((Node _ (FunctionOrValue [] name)) :: args) ->
            if name == funcName then
                [ args ]

            else
                []

        _ ->
            []


callHasShrinkingArg : Node Expression -> List (Node Expression) -> Bool
callHasShrinkingArg body callArgs =
    let
        consTailVars =
            collectConsTailBindings body |> Set.map Tuple.second
    in
    List.any
        (\(Node _ argExpr) ->
            case argExpr of
                FunctionOrValue [] varName ->
                    Set.member varName consTailVars

                _ ->
                    False
        )
        callArgs


collectConsTailBindings : Node Expression -> Set ( String, String )
collectConsTailBindings (Node _ expr) =
    case expr of
        CaseExpression { expression, cases } ->
            case expression of
                Node _ (FunctionOrValue [] sVar) ->
                    List.foldl
                        (\( Node _ pattern, branchBody ) acc ->
                            Set.union (Set.union (extractConsTail sVar pattern) (collectConsTailBindings branchBody)) acc
                        )
                        Set.empty
                        cases

                Node _ (TupledExpression tupleParts) ->
                    -- Idiomatic multi-list recursion dispatches via a
                    -- tuple scrutinee: `case ( prefix, list ) of
                    -- ( p :: ps, x :: xs ) -> ...`. Extract each
                    -- component's scrutinee variable (Nothing for
                    -- non-var components — function calls, literals,
                    -- etc.) and walk each case branch's tuple pattern
                    -- component-wise. Cheap to skip when this never
                    -- fires because the cache lookup happens at project
                    -- load, not per-call.
                    let
                        tupleVars : List (Maybe String)
                        tupleVars =
                            List.map tupleComponentVar tupleParts
                    in
                    List.foldl
                        (\( Node _ pattern, branchBody ) acc ->
                            Set.union (Set.union (extractConsTailTuple tupleVars pattern) (collectConsTailBindings branchBody)) acc
                        )
                        Set.empty
                        cases

                _ ->
                    List.foldl (\( _, branchBody ) acc -> Set.union (collectConsTailBindings branchBody) acc) Set.empty cases

        IfBlock _ thenBranch elseBranch ->
            Set.union (collectConsTailBindings thenBranch) (collectConsTailBindings elseBranch)

        LetExpression { expression } ->
            collectConsTailBindings expression

        _ ->
            Set.empty


tupleComponentVar : Node Expression -> Maybe String
tupleComponentVar (Node _ expr) =
    case expr of
        FunctionOrValue [] v ->
            Just v

        _ ->
            Nothing


{-| Match a `TuplePattern` against the flattened scrutinee variable
names and pull every `(scrutineeVar, tailVarName)` pair where the
corresponding component is a `x :: tail` destructure. Returns empty
if the pattern isn't a `TuplePattern` or the arities don't match.
-}
extractConsTailTuple : List (Maybe String) -> Pattern -> Set ( String, String )
extractConsTailTuple tupleVars pattern =
    case pattern of
        TuplePattern subPatterns ->
            if List.length tupleVars == List.length subPatterns then
                List.map2
                    (\maybeVar (Node _ subPattern) ->
                        case maybeVar of
                            Just sVar ->
                                extractConsTail sVar subPattern

                            Nothing ->
                                Set.empty
                    )
                    tupleVars
                    subPatterns
                    |> List.foldl Set.union Set.empty

            else
                Set.empty

        _ ->
            Set.empty


extractConsTail : String -> Pattern -> Set ( String, String )
extractConsTail scrutineeVar pattern =
    case pattern of
        UnConsPattern _ (Node _ (VarPattern tailName)) ->
            Set.singleton ( scrutineeVar, tailName )

        _ ->
            Set.empty
