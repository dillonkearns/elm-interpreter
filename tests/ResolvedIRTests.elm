module ResolvedIRTests exposing (suite)

import Eval.ResolvedIR as IR
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Eval.ResolvedIR"
        [ slotCountTests
        , constructionTests
        , freeVarsTests
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
                        IR.mkLambda 1 (IR.RLocal 0)
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
                        IR.mkLambda 1 (IR.RLocal 0)

                    lamY =
                        IR.mkLambda 1 (IR.RLocal 0)
                in
                lamX |> Expect.equal lamY
        , test "const lambda (\\x y -> x) has arity 2 and body RLocal 1" <|
            \_ ->
                -- With indices, `x` is the outer binding (index 1) and `y` is
                -- the inner (index 0). `\x y -> x` returns the outer, so the
                -- body is `RLocal 1`.
                let
                    constLambda =
                        IR.mkLambda 2 (IR.RLocal 1)
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


freeVarsTests : Test
freeVarsTests =
    describe "freeVars and mkLambda captureSlots"
        [ test "closed lambda: identity has no free vars" <|
            \_ ->
                -- \x -> x
                -- Body is RLocal 0; arity is 1. Body's slot 0 is the param,
                -- so no outer-scope references.
                case IR.mkLambda 1 (IR.RLocal 0) of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal []

                    _ ->
                        Expect.fail "expected RLambda"
        , test "closed lambda: const has no free vars" <|
            \_ ->
                -- \x y -> x
                -- Body is RLocal 1; arity is 2. Slot 1 is the outer param,
                -- still bound, so no captures.
                case IR.mkLambda 2 (IR.RLocal 1) of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal []

                    _ ->
                        Expect.fail "expected RLambda"
        , test "single capture: \\y -> outer" <|
            \_ ->
                -- In an outer scope with one local [outer], the lambda
                -- `\y -> outer` has body = RLocal 1. arity = 1. Slot 1 is
                -- beyond the lambda's params, so it's a capture of outer
                -- slot 0.
                case IR.mkLambda 1 (IR.RLocal 1) of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal [ 0 ]

                    _ ->
                        Expect.fail "expected RLambda"
        , test "sorted deduplicated captures" <|
            \_ ->
                -- \x -> (outer0, outer2, outer0, outer1)
                -- Body references outer slots 0, 2, 0, 1 (duplicates and
                -- out of order). After dedup + sort, captureSlots should
                -- be [0, 1, 2].
                let
                    body =
                        IR.RList
                            [ IR.RLocal 1 -- outer 0 (skipped by arity=1)
                            , IR.RLocal 3 -- outer 2
                            , IR.RLocal 1 -- outer 0 (dup)
                            , IR.RLocal 2 -- outer 1
                            ]
                in
                case IR.mkLambda 1 body of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal [ 0, 1, 2 ]

                    _ ->
                        Expect.fail "expected RLambda"
        , test "nested lambda: captures are in outermost-relative coordinates" <|
            \_ ->
                -- Outer = \a -> \b -> (a, b, outerCaptured)
                -- Inner body references RLocal 0 (= b), RLocal 1 (= a,
                -- still bound by the outer), and RLocal 2 (= outer
                -- captured slot 0). For the INNER lambda with arity 1,
                -- captureSlots = [ 0, 1 ] — it captures a from outer
                -- plus outerCaptured from outside the outer.
                --
                -- For the OUTER lambda with arity 1 (just param a), the
                -- inner lambda references outer slot 2 (= outerCaptured),
                -- which is outer-scope slot 1 from the outer lambda's POV
                -- (after a is bound). So outer captureSlots = [ 0 ].
                let
                    inner =
                        IR.mkLambda 1
                            (IR.RList
                                [ IR.RLocal 0 -- b (inner param)
                                , IR.RLocal 1 -- a (outer param, bound by outer)
                                , IR.RLocal 2 -- outer captured (slot 0 from inner view)
                                ]
                            )

                    outer =
                        IR.mkLambda 1 inner
                in
                case ( inner, outer ) of
                    ( IR.RLambda innerLambda, IR.RLambda outerLambda ) ->
                        Expect.all
                            [ \_ -> innerLambda.captureSlots |> Expect.equal [ 0, 1 ]
                            , \_ -> outerLambda.captureSlots |> Expect.equal [ 0 ]
                            ]
                            ()

                    _ ->
                        Expect.fail "expected two RLambdas"
        , test "let binding: body sees let slots as bound" <|
            \_ ->
                -- \x -> let y = x in y
                -- body = RLet [{pat=RPVar, arity=0, body=RLocal 0, ...}] (RLocal 0)
                -- The let binding body "RLocal 0" is x (outer) — bound within
                -- the lambda. The let result body "RLocal 0" is y (the let's
                -- own binding, also bound). So outer captureSlots = [].
                let
                    body =
                        IR.RLet
                            [ { pattern = IR.RPVar
                              , arity = 0
                              , body = IR.RLocal 0
                              , debugName = "y"
                              }
                            ]
                            (IR.RLocal 0)
                in
                case IR.mkLambda 1 body of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal []

                    _ ->
                        Expect.fail "expected RLambda"
        , test "let binding: captures from binding body propagate" <|
            \_ ->
                -- Lambda with arity 0 + let body references outer slot 1
                -- inside the let value binding.
                --
                -- mkLambda 0 (RLet [{pat=RPVar, arity=0, body=RLocal 1, ...}] (RLocal 0))
                -- arity = 0 → outer binderDepth starts at 0.
                -- binding body RLocal 1 at depth 0 (pre-binding) → captures
                -- outer slot 1. Let body RLocal 0 at depth 1 (post-binding)
                -- → refers to the let binding itself, no capture.
                -- Expected captureSlots = [ 1 ].
                let
                    body =
                        IR.RLet
                            [ { pattern = IR.RPVar
                              , arity = 0
                              , body = IR.RLocal 1
                              , debugName = "y"
                              }
                            ]
                            (IR.RLocal 0)
                in
                case IR.mkLambda 0 body of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal [ 1 ]

                    _ ->
                        Expect.fail "expected RLambda"
        , test "case pattern: branches see pattern slots as bound" <|
            \_ ->
                -- \x -> case x of (a, b) -> a
                -- The case scrutinee is RLocal 0 (x), bound. The branch
                -- body RLocal 1 refers to `a` which is the innermost
                -- pattern binding after (a, b) is destructured. The slot
                -- layout inside the branch: locals = [b, a, x, ...outer].
                -- RLocal 1 = a, bound. No captures.
                let
                    body =
                        IR.RCase (IR.RLocal 0)
                            [ ( IR.RPTuple2 IR.RPVar IR.RPVar, IR.RLocal 1 )
                            ]
                in
                case IR.mkLambda 1 body of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal []

                    _ ->
                        Expect.fail "expected RLambda"
        , test "case pattern: branch body capture propagates" <|
            \_ ->
                -- \x -> case x of (a, b) -> outerCaptured
                -- Lambda arity = 1. Scrutinee RLocal 0 is x (param, bound).
                -- Branch body is RLocal 3: inside the branch locals =
                -- [b, a, x, outerCaptured, ...], slot 3 = outerCaptured.
                -- From the lambda's outer scope perspective, that's slot 0.
                let
                    body =
                        IR.RCase (IR.RLocal 0)
                            [ ( IR.RPTuple2 IR.RPVar IR.RPVar, IR.RLocal 3 )
                            ]
                in
                case IR.mkLambda 1 body of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal [ 0 ]

                    _ ->
                        Expect.fail "expected RLambda"
        , test "let-bound recursive function: self-ref is not a capture" <|
            \_ ->
                -- Lambda arity = 0 containing:
                --   let f x = f in f
                --
                -- The let binding's body is an RLambda with arity = 1 and
                -- binding.arity = 1 (function binding → selfSlots = 1 at
                -- call time). Inside the inner lambda body, RLocal 1 is
                -- `f` (the self slot). That's NOT a capture from outside
                -- the outer lambda — it's the let-bound self-ref.
                --
                -- Expected: captureSlots = [].
                let
                    innerLambda =
                        IR.mkLambda 1 (IR.RLocal 1)

                    body =
                        IR.RLet
                            [ { pattern = IR.RPVar
                              , arity = 1
                              , body = innerLambda
                              , debugName = "f"
                              }
                            ]
                            (IR.RLocal 0)
                in
                case IR.mkLambda 0 body of
                    IR.RLambda lambda ->
                        lambda.captureSlots |> Expect.equal []

                    _ ->
                        Expect.fail "expected RLambda"
        , test "freeVars on closed expression returns []" <|
            \_ ->
                -- \x y -> x + y (where + is some RGlobal). Arity 2, body
                -- references slots 0 and 1 (both bound). No captures.
                let
                    body =
                        IR.RApply (IR.RGlobal 1) [ IR.RLocal 0, IR.RLocal 1 ]
                in
                IR.freeVars 2 body |> Expect.equal []
        , test "freeVars is sorted and deduped" <|
            \_ ->
                -- RList with out-of-order duplicate captures. freeVars
                -- returns sorted ascending with no duplicates.
                let
                    body =
                        IR.RList
                            [ IR.RLocal 5
                            , IR.RLocal 2
                            , IR.RLocal 5
                            , IR.RLocal 3
                            , IR.RLocal 2
                            ]
                in
                IR.freeVars 0 body |> Expect.equal [ 2, 3, 5 ]
        ]
