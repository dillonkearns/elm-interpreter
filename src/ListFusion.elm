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
import Elm.Syntax.Infix
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


{-| Try to fuse at the current expression level. Each pattern is an
independent rule; we try them in order and return the first that matches.
-}
fuseHelp : Node Expression -> Node Expression
fuseHelp ((Node range expr) as node) =
    case expr of
        -- Pattern: List.map identity xs → xs
        Application [ Node _ (FunctionOrValue [ "List" ] "map"), Node _ (FunctionOrValue [] "identity"), xs ] ->
            xs

        Application [ Node _ (FunctionOrValue [ "List" ] "map"), Node _ (FunctionOrValue [ "Basics" ] "identity"), xs ] ->
            xs

        -- Pattern: List.reverse (List.reverse xs) → xs
        Application [ Node _ (FunctionOrValue [ "List" ] "reverse"), Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "reverse"), xs ]) ] ->
            xs

        -- Pattern: List.concat (List.map f xs) → List.concatMap f xs
        Application [ Node _ (FunctionOrValue [ "List" ] "concat"), Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "map"), f, xs ]) ] ->
            Node range
                (Application
                    [ Node emptyRange (FunctionOrValue [ "List" ] "concatMap")
                    , f
                    , xs
                    ]
                )

        -- Pattern: List.filterMap identity (List.map f xs) → List.filterMap f xs
        Application [ Node _ (FunctionOrValue [ "List" ] "filterMap"), Node _ (FunctionOrValue [] "identity"), Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "map"), f, xs ]) ] ->
            Node range
                (Application
                    [ Node emptyRange (FunctionOrValue [ "List" ] "filterMap")
                    , f
                    , xs
                    ]
                )

        -- Pattern: List.foldl f init (List.reverse xs) → List.foldr f init xs
        Application [ Node _ (FunctionOrValue [ "List" ] "foldl"), f, init, Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "reverse"), xs ]) ] ->
            Node range
                (Application
                    [ Node emptyRange (FunctionOrValue [ "List" ] "foldr")
                    , f
                    , init
                    , xs
                    ]
                )

        -- Pattern: List.map outerFn (List.map innerFn xs) → List.map (\x -> outerFn (innerFn x)) xs
        Application [ Node _ (FunctionOrValue [ "List" ] "map"), outerFn, Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "map"), innerFn, xs ]) ] ->
            Node range
                (Application
                    [ Node emptyRange (FunctionOrValue [ "List" ] "map")
                    , composeFn outerFn innerFn
                    , xs
                    ]
                )

        -- Pattern: List.filter p (List.filter q xs) → List.filter (\x -> p x && q x) xs
        Application [ Node _ (FunctionOrValue [ "List" ] "filter"), outerP, Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "filter"), innerP, xs ]) ] ->
            Node range
                (Application
                    [ Node emptyRange (FunctionOrValue [ "List" ] "filter")
                    , conjoinPredicates outerP innerP
                    , xs
                    ]
                )

        -- Pattern: Dict.fromList (Dict.toList d) → d
        Application [ Node _ (FunctionOrValue [ "Dict" ] "fromList"), Node _ (Application [ Node _ (FunctionOrValue [ "Dict" ] "toList"), d ]) ] ->
            d

        -- Pattern: Set.fromList (Set.toList s) → s
        Application [ Node _ (FunctionOrValue [ "Set" ] "fromList"), Node _ (Application [ Node _ (FunctionOrValue [ "Set" ] "toList"), s ]) ] ->
            s

        -- Pattern: List.map Tuple.first (Dict.toList d) → Dict.keys d
        Application [ Node _ (FunctionOrValue [ "List" ] "map"), Node _ (FunctionOrValue [ "Tuple" ] "first"), Node _ (Application [ Node _ (FunctionOrValue [ "Dict" ] "toList"), d ]) ] ->
            Node range (Application [ Node emptyRange (FunctionOrValue [ "Dict" ] "keys"), d ])

        -- Pattern: List.map Tuple.second (Dict.toList d) → Dict.values d
        Application [ Node _ (FunctionOrValue [ "List" ] "map"), Node _ (FunctionOrValue [ "Tuple" ] "second"), Node _ (Application [ Node _ (FunctionOrValue [ "Dict" ] "toList"), d ]) ] ->
            Node range (Application [ Node emptyRange (FunctionOrValue [ "Dict" ] "values"), d ])

        -- Pattern: Dict.size (Dict.map f d) → Dict.size d  (map preserves size)
        Application [ Node _ (FunctionOrValue [ "Dict" ] "size"), Node _ (Application [ Node _ (FunctionOrValue [ "Dict" ] "map"), _, d ]) ] ->
            Node range (Application [ Node emptyRange (FunctionOrValue [ "Dict" ] "size"), d ])

        -- Pattern: Dict.isEmpty (Dict.map f d) → Dict.isEmpty d
        Application [ Node _ (FunctionOrValue [ "Dict" ] "isEmpty"), Node _ (Application [ Node _ (FunctionOrValue [ "Dict" ] "map"), _, d ]) ] ->
            Node range (Application [ Node emptyRange (FunctionOrValue [ "Dict" ] "isEmpty"), d ])

        -- Pattern: List.length (List.map f xs) → List.length xs  (map preserves length)
        Application [ Node _ (FunctionOrValue [ "List" ] "length"), Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "map"), _, xs ]) ] ->
            Node range (Application [ Node emptyRange (FunctionOrValue [ "List" ] "length"), xs ])

        -- Pattern: List.length (List.reverse xs) → List.length xs
        Application [ Node _ (FunctionOrValue [ "List" ] "length"), Node _ (Application [ Node _ (FunctionOrValue [ "List" ] "reverse"), xs ]) ] ->
            Node range (Application [ Node emptyRange (FunctionOrValue [ "List" ] "length"), xs ])

        _ ->
            node


{-| Build `\x -> outerFn (innerFn x)` from two function expressions.
-}
composeFn : Node Expression -> Node Expression -> Node Expression
composeFn outerFn innerFn =
    let
        varName =
            "__x_fused"

        varRef =
            Node emptyRange (FunctionOrValue [] varName)
    in
    Node emptyRange
        (LambdaExpression
            { args = [ Node emptyRange (VarPattern varName) ]
            , expression =
                Node emptyRange
                    (Application
                        [ outerFn
                        , Node emptyRange
                            (ParenthesizedExpression
                                (Node emptyRange (Application [ innerFn, varRef ]))
                            )
                        ]
                    )
            }
        )


{-| Build `\x -> p1 x && p2 x` from two predicate expressions.
-}
conjoinPredicates : Node Expression -> Node Expression -> Node Expression
conjoinPredicates p1 p2 =
    let
        varName =
            "__x_fused"

        varRef =
            Node emptyRange (FunctionOrValue [] varName)

        call p =
            Node emptyRange (Application [ p, varRef ])
    in
    Node emptyRange
        (LambdaExpression
            { args = [ Node emptyRange (VarPattern varName) ]
            , expression =
                Node emptyRange (OperatorApplication "&&" Elm.Syntax.Infix.Right (call p1) (call p2))
            }
        )
