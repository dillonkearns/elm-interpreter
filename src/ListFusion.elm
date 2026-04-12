module ListFusion exposing (fuse)

{-| List fusion AST pass: recognizes chains like
`List.map f (List.map g xs)` and rewrites them to
`List.map (\x -> f (g x)) xs` to eliminate intermediate lists.

Bottom-up walk: fuses inner expressions first so longer chains
collapse to a single map in one pass.

Only targets `List.map` chains for now. Other patterns (filter-map,
concat-map, pipeline style) could be added later.

-}

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (emptyRange)


fuse : Node Expression -> Node Expression
fuse node =
    fuseHelp (foldChildren (normalizePipeline node))


{-| Normalize `xs |> List.map f` → `List.map f xs` so fusion can match
against direct application form. Also handles `List.map f <| xs`.
Only touches the top-level `|>` / `<|` shape; recursive walking happens
in `foldChildren`.
-}
normalizePipeline : Node Expression -> Node Expression
normalizePipeline ((Node range expr) as node) =
    case expr of
        OperatorApplication "|>" _ leftArg rightFunc ->
            case rightFunc of
                Node _ (Application items) ->
                    Node range (Application (items ++ [ leftArg ]))

                Node _ (FunctionOrValue _ _) ->
                    Node range (Application [ rightFunc, leftArg ])

                _ ->
                    node

        OperatorApplication "<|" _ leftFunc rightArg ->
            case leftFunc of
                Node _ (Application items) ->
                    Node range (Application (items ++ [ rightArg ]))

                Node _ (FunctionOrValue _ _) ->
                    Node range (Application [ leftFunc, rightArg ])

                _ ->
                    node

        _ ->
            node


{-| Walk children first (bottom-up), then try to fuse at this level.
-}
foldChildren : Node Expression -> Node Expression
foldChildren ((Node range expr) as node) =
    case expr of
        Application items ->
            Node range (Application (List.map fuse items))

        OperatorApplication op dir l r ->
            Node range (OperatorApplication op dir (fuse l) (fuse r))

        IfBlock c t e ->
            Node range (IfBlock (fuse c) (fuse t) (fuse e))

        CaseExpression { expression, cases } ->
            Node range
                (CaseExpression
                    { expression = fuse expression
                    , cases = List.map (\( pat, body ) -> ( pat, fuse body )) cases
                    }
                )

        LetExpression { declarations, expression } ->
            Node range
                (LetExpression
                    { declarations =
                        List.map
                            (\(Node dRange decl) ->
                                Node dRange
                                    (case decl of
                                        LetFunction f ->
                                            let
                                                (Node implRange impl) =
                                                    f.declaration
                                            in
                                            LetFunction
                                                { f
                                                    | declaration =
                                                        Node implRange { impl | expression = fuse impl.expression }
                                                }

                                        LetDestructuring pat val ->
                                            LetDestructuring pat (fuse val)
                                    )
                            )
                            declarations
                    , expression = fuse expression
                    }
                )

        ListExpr items ->
            Node range (ListExpr (List.map fuse items))

        TupledExpression items ->
            Node range (TupledExpression (List.map fuse items))

        ParenthesizedExpression inner ->
            Node range (ParenthesizedExpression (fuse inner))

        LambdaExpression lambda ->
            Node range (LambdaExpression { lambda | expression = fuse lambda.expression })

        RecordExpr fields ->
            Node range
                (RecordExpr
                    (List.map
                        (\(Node fRange ( name, val )) ->
                            Node fRange ( name, fuse val )
                        )
                        fields
                    )
                )

        RecordAccess inner field ->
            Node range (RecordAccess (fuse inner) field)

        RecordUpdateExpression name setters ->
            Node range
                (RecordUpdateExpression name
                    (List.map
                        (\(Node fRange ( fname, val )) ->
                            Node fRange ( fname, fuse val )
                        )
                        setters
                    )
                )

        Negation inner ->
            Node range (Negation (fuse inner))

        _ ->
            node


{-| Try to fuse at the current expression level.

Pattern: `List.map outerFn (List.map innerFn xs)` — both args are
direct applications of List.map. Rewrites to:
`List.map (\x__fuse -> outerFn (innerFn x__fuse)) xs`.
-}
fuseHelp : Node Expression -> Node Expression
fuseHelp ((Node range expr) as node) =
    case expr of
        Application [ Node _ (FunctionOrValue [ "List" ] "map"), outerFn, inner ] ->
            case inner of
                Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "map"), innerFn, xs ]) ->
                    let
                        varName =
                            "__x_fused"

                        varRef =
                            Node emptyRange (FunctionOrValue [] varName)

                        composedBody =
                            Node emptyRange
                                (Application
                                    [ outerFn
                                    , Node emptyRange
                                        (ParenthesizedExpression
                                            (Node emptyRange (Application [ innerFn, varRef ]))
                                        )
                                    ]
                                )

                        fusedLambda =
                            Node emptyRange
                                (LambdaExpression
                                    { args = [ Node emptyRange (VarPattern varName) ]
                                    , expression = composedBody
                                    }
                                )
                    in
                    Node range
                        (Application
                            [ Node emptyRange (FunctionOrValue [ "List" ] "map")
                            , fusedLambda
                            , xs
                            ]
                        )

                _ ->
                    node

        _ ->
            node
