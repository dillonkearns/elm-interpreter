module ResolvedIRTests exposing (suite)

import Eval.ResolvedIR as IR
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Eval.ResolvedIR"
        [ slotCountTests
        , constructionTests
        ]


slotCountTests : Test
slotCountTests =
    describe "slotCount"
        [ test "RPVar binds 1 slot" <|
            \_ ->
                IR.slotCount IR.RPVar |> Expect.equal 1
        , test "RPWildcard binds 0 slots" <|
            \_ ->
                IR.slotCount IR.RPWildcard |> Expect.equal 0
        , test "RPUnit binds 0 slots" <|
            \_ ->
                IR.slotCount IR.RPUnit |> Expect.equal 0
        , test "literal patterns bind 0 slots" <|
            \_ ->
                [ IR.RPInt 42, IR.RPFloat 1.5, IR.RPChar 'a', IR.RPString "x" ]
                    |> List.map IR.slotCount
                    |> Expect.equal [ 0, 0, 0, 0 ]
        , test "RPTuple2 sums children" <|
            \_ ->
                IR.slotCount (IR.RPTuple2 IR.RPVar IR.RPVar) |> Expect.equal 2
        , test "RPTuple2 with wildcard only counts vars" <|
            \_ ->
                IR.slotCount (IR.RPTuple2 IR.RPVar IR.RPWildcard) |> Expect.equal 1
        , test "RPTuple3 sums three children" <|
            \_ ->
                IR.slotCount (IR.RPTuple3 IR.RPVar IR.RPVar IR.RPVar) |> Expect.equal 3
        , test "RPRecord binds one slot per field" <|
            \_ ->
                IR.slotCount (IR.RPRecord [ "a", "b", "c" ]) |> Expect.equal 3
        , test "RPCons sums head and tail" <|
            \_ ->
                IR.slotCount (IR.RPCons IR.RPVar IR.RPVar) |> Expect.equal 2
        , test "RPCons with literal head binds only tail" <|
            \_ ->
                IR.slotCount (IR.RPCons (IR.RPInt 0) IR.RPVar) |> Expect.equal 1
        , test "RPList sums all elements" <|
            \_ ->
                IR.slotCount (IR.RPList [ IR.RPVar, IR.RPVar, IR.RPWildcard ])
                    |> Expect.equal 2
        , test "empty RPList binds 0 slots" <|
            \_ ->
                IR.slotCount (IR.RPList []) |> Expect.equal 0
        , test "RPCtor sums argument patterns" <|
            \_ ->
                IR.slotCount
                    (IR.RPCtor
                        { moduleName = [ "Maybe" ], name = "Just" }
                        [ IR.RPVar ]
                    )
                    |> Expect.equal 1
        , test "RPCtor with multiple args sums correctly" <|
            \_ ->
                IR.slotCount
                    (IR.RPCtor
                        { moduleName = [], name = "Pair" }
                        [ IR.RPVar, IR.RPVar, IR.RPWildcard ]
                    )
                    |> Expect.equal 2
        , test "RPAs adds one slot on top of inner pattern" <|
            \_ ->
                IR.slotCount (IR.RPAs (IR.RPTuple2 IR.RPVar IR.RPVar))
                    |> Expect.equal 3
        , test "RPAs on wildcard still binds 1 slot" <|
            \_ ->
                IR.slotCount (IR.RPAs IR.RPWildcard) |> Expect.equal 1
        , test "deeply nested pattern" <|
            \_ ->
                -- ((a, _) :: rest) as whole : binds a, rest, whole
                let
                    pat =
                        IR.RPAs
                            (IR.RPCons
                                (IR.RPTuple2 IR.RPVar IR.RPWildcard)
                                IR.RPVar
                            )
                in
                IR.slotCount pat |> Expect.equal 3
        ]


constructionTests : Test
constructionTests =
    describe "RExpr construction and pattern matching"
        [ test "literals round-trip through a case expression" <|
            \_ ->
                let
                    expr =
                        IR.RInt 42

                    kind =
                        case expr of
                            IR.RInt _ ->
                                "int"

                            _ ->
                                "other"
                in
                kind |> Expect.equal "int"
        , test "identity lambda has arity 1 and body RLocal 0" <|
            \_ ->
                let
                    identityLambda =
                        IR.RLambda { arity = 1, body = IR.RLocal 0 }
                in
                case identityLambda of
                    IR.RLambda { arity, body } ->
                        ( arity, body == IR.RLocal 0 )
                            |> Expect.equal ( 1, True )

                    _ ->
                        Expect.fail "expected RLambda"
        , test "alpha-equivalence: two identity lambdas produce identical RExpr" <|
            \_ ->
                -- This is the whole point of using De Bruijn indices: the IR
                -- carries no variable names, so `\x -> x` and `\y -> y` must
                -- be structurally identical.
                let
                    lamX =
                        IR.RLambda { arity = 1, body = IR.RLocal 0 }

                    lamY =
                        IR.RLambda { arity = 1, body = IR.RLocal 0 }
                in
                lamX |> Expect.equal lamY
        , test "const lambda (\\x y -> x) has arity 2 and body RLocal 1" <|
            \_ ->
                -- With indices, `x` is the outer binding (index 1) and `y` is
                -- the inner (index 0). `\x y -> x` returns the outer, so the
                -- body is `RLocal 1`.
                let
                    constLambda =
                        IR.RLambda
                            { arity = 2
                            , body = IR.RLocal 1
                            }
                in
                case constLambda of
                    IR.RLambda { arity, body } ->
                        ( arity, body )
                            |> Expect.equal ( 2, IR.RLocal 1 )

                    _ ->
                        Expect.fail "expected RLambda"
        , test "let binding construction (simple value)" <|
            \_ ->
                let
                    expr =
                        IR.RLet
                            [ { pattern = IR.RPVar
                              , arity = 0
                              , body = IR.RInt 1
                              , debugName = "x"
                              }
                            ]
                            (IR.RLocal 0)
                in
                case expr of
                    IR.RLet bindings body ->
                        ( List.length bindings, body == IR.RLocal 0 )
                            |> Expect.equal ( 1, True )

                    _ ->
                        Expect.fail "expected RLet"
        , test "let binding construction (destructuring)" <|
            \_ ->
                -- `let (a, b) = pair in a + b` — pattern binds 2 slots.
                -- The body's view of locals: [b, a, ...outer] (b innermost).
                let
                    binding =
                        { pattern = IR.RPTuple2 IR.RPVar IR.RPVar
                        , arity = 0
                        , body = IR.RLocal 0
                        , debugName = "$destructure"
                        }
                in
                IR.slotCount binding.pattern |> Expect.equal 2
        , test "case expression with two branches" <|
            \_ ->
                let
                    expr =
                        IR.RCase (IR.RLocal 0)
                            [ ( IR.RPCtor
                                    { moduleName = [ "Maybe" ], name = "Just" }
                                    [ IR.RPVar ]
                              , IR.RLocal 0
                              )
                            , ( IR.RPCtor
                                    { moduleName = [ "Maybe" ], name = "Nothing" }
                                    []
                              , IR.RInt 0
                              )
                            ]
                in
                case expr of
                    IR.RCase _ branches ->
                        List.length branches |> Expect.equal 2

                    _ ->
                        Expect.fail "expected RCase"
        , test "record fields preserved in given order (canonicalization is the resolver's job)" <|
            \_ ->
                let
                    expr =
                        IR.RRecord
                            [ ( "a", IR.RInt 1 )
                            , ( "b", IR.RInt 2 )
                            , ( "c", IR.RInt 3 )
                            ]
                in
                case expr of
                    IR.RRecord fields ->
                        List.map Tuple.first fields
                            |> Expect.equal [ "a", "b", "c" ]

                    _ ->
                        Expect.fail "expected RRecord"
        , test "nested apply" <|
            \_ ->
                -- (\f x -> f x) applied to some globals
                let
                    expr =
                        IR.RApply (IR.RLocal 1) [ IR.RLocal 0 ]
                in
                case expr of
                    IR.RApply head args ->
                        ( head, List.length args )
                            |> Expect.equal ( IR.RLocal 1, 1 )

                    _ ->
                        Expect.fail "expected RApply"
        ]
