module ListFusion exposing (FuseResult, FuseStats, canonicalizeWithStats, emptyFuseStats, fuse, fuseWithStats)

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
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (emptyRange)


type alias FuseStats =
    { pipelineNormalizations : Int
    , headFlattenRewrites : Int
    , ruleRewrites : Int
    }


type alias FuseResult =
    { expression : Node Expression
    , stats : FuseStats
    }


emptyFuseStats : FuseStats
emptyFuseStats =
    { pipelineNormalizations = 0
    , headFlattenRewrites = 0
    , ruleRewrites = 0
    }


mergeFuseStats : FuseStats -> FuseStats -> FuseStats
mergeFuseStats left right =
    { pipelineNormalizations = left.pipelineNormalizations + right.pipelineNormalizations
    , headFlattenRewrites = left.headFlattenRewrites + right.headFlattenRewrites
    , ruleRewrites = left.ruleRewrites + right.ruleRewrites
    }


mergeFuseResults : List FuseResult -> FuseStats
mergeFuseResults results =
    results
        |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats


fuse : Node Expression -> Node Expression
fuse node =
    (fuseWithStats node).expression


fuseWithStats : Node Expression -> FuseResult
fuseWithStats node =
    let
        pipelineResult =
            normalizePipelineWithStats node

        childrenResult =
            foldChildrenWithStats pipelineResult.expression

        localResult =
            fuseHelpWithStats childrenResult.expression
    in
    { expression = localResult.expression
    , stats =
        mergeFuseStats
            pipelineResult.stats
            (mergeFuseStats childrenResult.stats localResult.stats)
    }


canonicalizeWithStats : Node Expression -> FuseResult
canonicalizeWithStats node =
    let
        pipelineResult =
            normalizePipelineWithStats node

        childrenResult =
            canonicalizeChildrenWithStats pipelineResult.expression
    in
    { expression = childrenResult.expression
    , stats = mergeFuseStats pipelineResult.stats childrenResult.stats
    }


{-| Normalize `xs |> List.map f` → `List.map f xs` so fusion can match
against direct application form. Also handles `List.map f <| xs`.
Only touches the top-level `|>` / `<|` shape; recursive walking happens
in `foldChildren`.
-}
normalizePipeline : Node Expression -> Node Expression
normalizePipeline node =
    (normalizePipelineWithStats node).expression


normalizePipelineWithStats : Node Expression -> FuseResult
normalizePipelineWithStats ((Node range expr) as node) =
    let
        normalized =
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
    in
    { expression = normalized
    , stats =
        if normalized /= node then
            { emptyFuseStats | pipelineNormalizations = 1 }

        else
            emptyFuseStats
    }


{-| Normalize `xs |> List.map f` → `List.map f xs` so fusion can match
against direct application form. Also handles `List.map f <| xs`.
Only touches the top-level `|>` / `<|` shape; recursive walking happens
in `foldChildren`.
-}
normalizePipelineLegacyDoc : ()
normalizePipelineLegacyDoc =
    ()


{-| Flatten nested `Application` / `ParenthesizedExpression` wrappers in
the head position of a function call so the evaluator sees a single
flat call site. `((f a) b) c` parses as
`Application [Application [Application [f, a], b], c]`; after this
rewrite it becomes `Application [f, a, b, c]` and hits the saturated
fast paths / arity dispatch directly.

Runs at AST-normalization time, so the evaluator's over-application
bounce-back — which reconstructs `Application [Application [first,
used], leftover]` at eval time — is untouched and can't trigger a
re-entry loop.

-}
flattenApplicationHead : List (Node Expression) -> List (Node Expression)
flattenApplicationHead items =
    flattenApplicationHeadHelp items


flattenApplicationHeadHelp : List (Node Expression) -> List (Node Expression)
flattenApplicationHeadHelp items =
    case items of
        (Node _ (Application innerItems)) :: rest ->
            flattenApplicationHeadHelp (innerItems ++ rest)

        (Node _ (ParenthesizedExpression inner)) :: rest ->
            flattenApplicationHeadHelp (inner :: rest)

        _ ->
            items


{-| Walk children first (bottom-up), then try to fuse at this level.
-}
foldChildren : Node Expression -> Node Expression
foldChildren node =
    (foldChildrenWithStats node).expression


foldChildrenWithStats : Node Expression -> FuseResult
foldChildrenWithStats ((Node range expr) as node) =
    case expr of
        Application items ->
            let
                itemResults =
                    List.map fuseWithStats items

                fusedItems =
                    List.map .expression itemResults

                flattenedItems =
                    flattenApplicationHeadHelp fusedItems
            in
            { expression = Node range (Application flattenedItems)
            , stats =
                mergeFuseStats
                    (mergeFuseResults itemResults)
                    (if flattenedItems /= fusedItems then
                        { emptyFuseStats | headFlattenRewrites = 1 }

                     else
                        emptyFuseStats
                    )
            }

        OperatorApplication op dir l r ->
            let
                leftResult =
                    fuseWithStats l

                rightResult =
                    fuseWithStats r
            in
            { expression = Node range (OperatorApplication op dir leftResult.expression rightResult.expression)
            , stats = mergeFuseStats leftResult.stats rightResult.stats
            }

        IfBlock c t e ->
            let
                condResult =
                    fuseWithStats c

                thenResult =
                    fuseWithStats t

                elseResult =
                    fuseWithStats e
            in
            { expression = Node range (IfBlock condResult.expression thenResult.expression elseResult.expression)
            , stats =
                mergeFuseStats
                    condResult.stats
                    (mergeFuseStats thenResult.stats elseResult.stats)
            }

        CaseExpression { expression, cases } ->
            let
                expressionResult =
                    fuseWithStats expression

                caseResults =
                    List.map
                        (\( pat, body ) ->
                            let
                                bodyResult =
                                    fuseWithStats body
                            in
                            { caseBody = ( pat, bodyResult.expression )
                            , stats = bodyResult.stats
                            }
                        )
                        cases
            in
            { expression =
                Node range
                    (CaseExpression
                        { expression = expressionResult.expression
                        , cases = List.map .caseBody caseResults
                        }
                    )
            , stats =
                mergeFuseStats
                    expressionResult.stats
                    (caseResults |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats)
            }

        LetExpression { declarations, expression } ->
            let
                declarationResults =
                    List.map
                        (\(Node dRange decl) ->
                            case decl of
                                LetFunction f ->
                                    let
                                        (Node implRange impl) =
                                            f.declaration

                                        functionResult =
                                            fuseWithStats impl.expression
                                    in
                                    { declaration =
                                        Node dRange
                                            (LetFunction
                                                { f
                                                    | declaration =
                                                        Node implRange { impl | expression = functionResult.expression }
                                                }
                                            )
                                    , stats = functionResult.stats
                                    }

                                LetDestructuring pat val ->
                                    let
                                        valueResult =
                                            fuseWithStats val
                                    in
                                    { declaration = Node dRange (LetDestructuring pat valueResult.expression)
                                    , stats = valueResult.stats
                                    }
                        )
                        declarations

                expressionResult =
                    fuseWithStats expression
            in
            { expression =
                Node range
                    (LetExpression
                        { declarations = List.map .declaration declarationResults
                        , expression = expressionResult.expression
                        }
                    )
            , stats =
                mergeFuseStats
                    expressionResult.stats
                    (declarationResults |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats)
            }

        ListExpr items ->
            let
                itemResults =
                    List.map fuseWithStats items
            in
            { expression = Node range (ListExpr (List.map .expression itemResults))
            , stats = mergeFuseResults itemResults
            }

        TupledExpression items ->
            let
                itemResults =
                    List.map fuseWithStats items
            in
            { expression = Node range (TupledExpression (List.map .expression itemResults))
            , stats = mergeFuseResults itemResults
            }

        ParenthesizedExpression inner ->
            let
                innerResult =
                    fuseWithStats inner
            in
            { expression = Node range (ParenthesizedExpression innerResult.expression)
            , stats = innerResult.stats
            }

        LambdaExpression lambda ->
            let
                expressionResult =
                    fuseWithStats lambda.expression
            in
            { expression = Node range (LambdaExpression { lambda | expression = expressionResult.expression })
            , stats = expressionResult.stats
            }

        RecordExpr fields ->
            let
                fieldResults =
                    List.map
                        (\(Node fRange ( name, val )) ->
                            let
                                valueResult =
                                    fuseWithStats val
                            in
                            { field = Node fRange ( name, valueResult.expression )
                            , stats = valueResult.stats
                            }
                        )
                        fields
            in
            { expression = Node range (RecordExpr (List.map .field fieldResults))
            , stats = fieldResults |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats
            }

        RecordAccess inner field ->
            let
                innerResult =
                    fuseWithStats inner
            in
            { expression = Node range (RecordAccess innerResult.expression field)
            , stats = innerResult.stats
            }

        RecordUpdateExpression name setters ->
            let
                setterResults =
                    List.map
                        (\(Node fRange ( fname, val )) ->
                            let
                                valueResult =
                                    fuseWithStats val
                            in
                            { setter = Node fRange ( fname, valueResult.expression )
                            , stats = valueResult.stats
                            }
                        )
                        setters
            in
            { expression = Node range (RecordUpdateExpression name (List.map .setter setterResults))
            , stats = setterResults |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats
            }

        Negation inner ->
            let
                innerResult =
                    fuseWithStats inner
            in
            { expression = Node range (Negation innerResult.expression)
            , stats = innerResult.stats
            }

        _ ->
            { expression = node
            , stats = emptyFuseStats
            }


canonicalizeChildrenWithStats : Node Expression -> FuseResult
canonicalizeChildrenWithStats ((Node range expr) as node) =
    case expr of
        Application items ->
            let
                itemResults =
                    List.map canonicalizeWithStats items

                fusedItems =
                    List.map .expression itemResults

                flattenedItems =
                    flattenApplicationHeadHelp fusedItems
            in
            { expression = Node range (Application flattenedItems)
            , stats =
                mergeFuseStats
                    (mergeFuseResults itemResults)
                    (if flattenedItems /= fusedItems then
                        { emptyFuseStats | headFlattenRewrites = 1 }

                     else
                        emptyFuseStats
                    )
            }

        OperatorApplication op dir l r ->
            let
                leftResult =
                    canonicalizeWithStats l

                rightResult =
                    canonicalizeWithStats r
            in
            { expression = Node range (OperatorApplication op dir leftResult.expression rightResult.expression)
            , stats = mergeFuseStats leftResult.stats rightResult.stats
            }

        IfBlock c t e ->
            let
                condResult =
                    canonicalizeWithStats c

                thenResult =
                    canonicalizeWithStats t

                elseResult =
                    canonicalizeWithStats e
            in
            { expression = Node range (IfBlock condResult.expression thenResult.expression elseResult.expression)
            , stats =
                mergeFuseStats
                    condResult.stats
                    (mergeFuseStats thenResult.stats elseResult.stats)
            }

        CaseExpression { expression, cases } ->
            let
                expressionResult =
                    canonicalizeWithStats expression

                caseResults =
                    List.map
                        (\( pat, body ) ->
                            let
                                bodyResult =
                                    canonicalizeWithStats body
                            in
                            { caseBody = ( pat, bodyResult.expression )
                            , stats = bodyResult.stats
                            }
                        )
                        cases
            in
            { expression =
                Node range
                    (CaseExpression
                        { expression = expressionResult.expression
                        , cases = List.map .caseBody caseResults
                        }
                    )
            , stats =
                mergeFuseStats
                    expressionResult.stats
                    (caseResults |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats)
            }

        LetExpression { declarations, expression } ->
            let
                declarationResults =
                    List.map
                        (\(Node dRange decl) ->
                            case decl of
                                LetFunction f ->
                                    let
                                        (Node implRange impl) =
                                            f.declaration

                                        functionResult =
                                            canonicalizeWithStats impl.expression
                                    in
                                    { declaration =
                                        Node dRange
                                            (LetFunction
                                                { f
                                                    | declaration =
                                                        Node implRange { impl | expression = functionResult.expression }
                                                }
                                            )
                                    , stats = functionResult.stats
                                    }

                                LetDestructuring pat val ->
                                    let
                                        valueResult =
                                            canonicalizeWithStats val
                                    in
                                    { declaration = Node dRange (LetDestructuring pat valueResult.expression)
                                    , stats = valueResult.stats
                                    }
                        )
                        declarations

                expressionResult =
                    canonicalizeWithStats expression
            in
            { expression =
                Node range
                    (LetExpression
                        { declarations = List.map .declaration declarationResults
                        , expression = expressionResult.expression
                        }
                    )
            , stats =
                mergeFuseStats
                    expressionResult.stats
                    (declarationResults |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats)
            }

        ListExpr items ->
            let
                itemResults =
                    List.map canonicalizeWithStats items
            in
            { expression = Node range (ListExpr (List.map .expression itemResults))
            , stats = mergeFuseResults itemResults
            }

        TupledExpression items ->
            let
                itemResults =
                    List.map canonicalizeWithStats items
            in
            { expression = Node range (TupledExpression (List.map .expression itemResults))
            , stats = mergeFuseResults itemResults
            }

        ParenthesizedExpression inner ->
            let
                innerResult =
                    canonicalizeWithStats inner
            in
            { expression = Node range (ParenthesizedExpression innerResult.expression)
            , stats = innerResult.stats
            }

        LambdaExpression lambda ->
            let
                expressionResult =
                    canonicalizeWithStats lambda.expression
            in
            { expression = Node range (LambdaExpression { lambda | expression = expressionResult.expression })
            , stats = expressionResult.stats
            }

        RecordExpr fields ->
            let
                fieldResults =
                    List.map
                        (\(Node fRange ( name, val )) ->
                            let
                                valueResult =
                                    canonicalizeWithStats val
                            in
                            { field = Node fRange ( name, valueResult.expression )
                            , stats = valueResult.stats
                            }
                        )
                        fields
            in
            { expression = Node range (RecordExpr (List.map .field fieldResults))
            , stats = fieldResults |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats
            }

        RecordAccess inner field ->
            let
                innerResult =
                    canonicalizeWithStats inner
            in
            { expression = Node range (RecordAccess innerResult.expression field)
            , stats = innerResult.stats
            }

        RecordUpdateExpression name setters ->
            let
                setterResults =
                    List.map
                        (\(Node fRange ( fname, val )) ->
                            let
                                valueResult =
                                    canonicalizeWithStats val
                            in
                            { setter = Node fRange ( fname, valueResult.expression )
                            , stats = valueResult.stats
                            }
                        )
                        setters
            in
            { expression = Node range (RecordUpdateExpression name (List.map .setter setterResults))
            , stats = setterResults |> List.foldl (\result acc -> mergeFuseStats acc result.stats) emptyFuseStats
            }

        Negation inner ->
            let
                innerResult =
                    canonicalizeWithStats inner
            in
            { expression = Node range (Negation innerResult.expression)
            , stats = innerResult.stats
            }

        _ ->
            { expression = node
            , stats = emptyFuseStats
            }


{-| Try to fuse at the current expression level. Each pattern is an
independent rule; we try them in order and return the first that matches.
-}
fuseHelp : Node Expression -> Node Expression
fuseHelp node =
    (fuseHelpWithStats node).expression


fuseHelpWithStats : Node Expression -> FuseResult
fuseHelpWithStats ((Node range expr) as node) =
    let
        rewritten =
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

        -- Pattern: Maybe.map f (Maybe.map g x) → Maybe.map (\v -> f (g v)) x
                Application [ Node _ (FunctionOrValue [ "Maybe" ] "map"), outerFn, Node _ (Application [ Node _ (FunctionOrValue [ "Maybe" ] "map"), innerFn, x ]) ] ->
                    Node range
                        (Application
                            [ Node emptyRange (FunctionOrValue [ "Maybe" ] "map")
                            , composeFn outerFn innerFn
                            , x
                            ]
                        )

        -- Pattern: Maybe.map identity x → x
                Application [ Node _ (FunctionOrValue [ "Maybe" ] "map"), Node _ (FunctionOrValue [] "identity"), x ] ->
                    x

        -- Pattern: Result.map f (Result.map g r) → Result.map (\v -> f (g v)) r
                Application [ Node _ (FunctionOrValue [ "Result" ] "map"), outerFn, Node _ (Application [ Node _ (FunctionOrValue [ "Result" ] "map"), innerFn, r ]) ] ->
                    Node range
                        (Application
                            [ Node emptyRange (FunctionOrValue [ "Result" ] "map")
                            , composeFn outerFn innerFn
                            , r
                            ]
                        )

        -- Pattern: Result.map identity r → r
                Application [ Node _ (FunctionOrValue [ "Result" ] "map"), Node _ (FunctionOrValue [] "identity"), r ] ->
                    r

        -- Pattern: Maybe.withDefault x (Maybe.map f y) → stays (conditional, not always a win)
        -- Pattern: Maybe.andThen f (Maybe.map g x) → Maybe.andThen (\v -> f (g v)) x
                Application [ Node _ (FunctionOrValue [ "Maybe" ] "andThen"), andThenFn, Node _ (Application [ Node _ (FunctionOrValue [ "Maybe" ] "map"), mapFn, x ]) ] ->
                    Node range
                        (Application
                            [ Node emptyRange (FunctionOrValue [ "Maybe" ] "andThen")
                            , composeFn andThenFn mapFn
                            , x
                            ]
                        )

                _ ->
                    node
    in
    { expression = rewritten
    , stats =
        if rewritten /= node then
            { emptyFuseStats | ruleRewrites = 1 }

        else
            emptyFuseStats
    }


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
