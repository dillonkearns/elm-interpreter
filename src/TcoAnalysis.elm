module TcoAnalysis exposing (TcoStrategy(..), analyze)

{-| Static analysis of tail-recursive function bodies to determine
whether cycle detection can be safely skipped at runtime.

Computed once per function (during normalization or at first call),
then stored/cached so tcoLoop can dispatch to a cycle-check-free
fast path for provably-terminating patterns.

-}

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Set exposing (Set)


type TcoStrategy
    = TcoSafe
    | TcoGeneral


{-| Analyze a tail-recursive function body. Given the function name and
its parameter names, determine if every tail-recursive call passes a
structurally-smaller argument for at least one parameter (guaranteeing
termination).

Currently detects:
- List drain: a parameter matched via `x :: rest` in a case, where the
  tail call passes `rest` for that parameter
- Countdown: a parameter compared via `n <= 0` (or similar), where the
  tail call passes `n - 1` for that parameter

-}
analyze : String -> List String -> Node Expression -> TcoStrategy
analyze funcName paramNames body =
    let
        tailCalls =
            collectTailCalls funcName body
    in
    if List.isEmpty tailCalls then
        TcoGeneral

    else if List.all (callHasShrinkingArg paramNames body) tailCalls then
        TcoSafe

    else
        TcoGeneral


{-| Collect all tail-recursive call argument lists from the body.
-}
collectTailCalls : String -> Node Expression -> List (List (Node Expression))
collectTailCalls funcName (Node _ expr) =
    case expr of
        IfBlock _ (Node _ trueExpr) (Node _ falseExpr) ->
            collectTailCallsFromExpr funcName trueExpr
                ++ collectTailCallsFromExpr funcName falseExpr

        CaseExpression { cases } ->
            List.concatMap (\( _, Node _ branchExpr ) -> collectTailCallsFromExpr funcName branchExpr) cases

        LetExpression { expression } ->
            collectTailCalls funcName expression

        Application ((Node _ (FunctionOrValue [] name)) :: args) ->
            if name == funcName then
                [ args ]

            else
                []

        _ ->
            []


collectTailCallsFromExpr : String -> Expression -> List (List (Node Expression))
collectTailCallsFromExpr funcName expr =
    case expr of
        IfBlock _ (Node _ trueExpr) (Node _ falseExpr) ->
            collectTailCallsFromExpr funcName trueExpr
                ++ collectTailCallsFromExpr funcName falseExpr

        CaseExpression { cases } ->
            List.concatMap (\( _, Node _ branchExpr ) -> collectTailCallsFromExpr funcName branchExpr) cases

        LetExpression { expression } ->
            collectTailCalls funcName expression

        Application ((Node _ (FunctionOrValue [] name)) :: args) ->
            if name == funcName then
                [ args ]

            else
                []

        _ ->
            []


{-| Check if a tail call has at least one argument that is a cons-tail
variable from a case match on one of the parameters. Order-independent:
doesn't rely on argument position matching parameter position.
-}
callHasShrinkingArg : List String -> Node Expression -> List (Node Expression) -> Bool
callHasShrinkingArg _ body callArgs =
    let
        consTailVars =
            collectConsTailBindings body
                |> Set.map Tuple.second
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


{-| Walk the body and collect (listParam, tailVar) pairs from
`case listParam of ... x :: tailVar -> ...` patterns.
-}
collectConsTailBindings : Node Expression -> Set ( String, String )
collectConsTailBindings (Node _ expr) =
    case expr of
        CaseExpression { expression, cases } ->
            let
                scrutineeVar =
                    case expression of
                        Node _ (FunctionOrValue [] name) ->
                            Just name

                        _ ->
                            Nothing
            in
            case scrutineeVar of
                Just sVar ->
                    cases
                        |> List.foldl
                            (\( Node _ pattern, branchBody ) acc ->
                                let
                                    tailBindings =
                                        extractConsTail sVar pattern

                                    innerBindings =
                                        collectConsTailBindings branchBody
                                in
                                Set.union (Set.union tailBindings innerBindings) acc
                            )
                            Set.empty

                Nothing ->
                    List.foldl
                        (\( _, branchBody ) acc -> Set.union (collectConsTailBindings branchBody) acc)
                        Set.empty
                        cases

        IfBlock _ thenBranch elseBranch ->
            Set.union (collectConsTailBindings thenBranch) (collectConsTailBindings elseBranch)

        LetExpression { expression } ->
            collectConsTailBindings expression

        _ ->
            Set.empty


{-| From a case pattern matching on `scrutineeVar`, extract (scrutineeVar, tailVar)
if the pattern is `_ :: tailVar` or `x :: tailVar`.
-}
extractConsTail : String -> Pattern -> Set ( String, String )
extractConsTail scrutineeVar pattern =
    case pattern of
        UnConsPattern _ (Node _ (VarPattern tailName)) ->
            Set.singleton ( scrutineeVar, tailName )

        UnConsPattern _ (Node _ (AsPattern _ (Node _ tailName))) ->
            Set.singleton ( scrutineeVar, tailName )

        NamedPattern _ subPatterns ->
            List.foldl
                (\(Node _ subPat) acc ->
                    case subPat of
                        UnConsPattern _ (Node _ (VarPattern tailName)) ->
                            Set.insert ( scrutineeVar, tailName ) acc

                        _ ->
                            acc
                )
                Set.empty
                subPatterns

        _ ->
            Set.empty
