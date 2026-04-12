module ResolverTests exposing (suite)

import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Eval.ResolvedIR as IR exposing (RExpr(..), RPattern(..))
import Eval.Resolver as Resolver exposing (ResolveError(..), ResolverContext)
import Expect
import FastDict
import Test exposing (Test, describe, test)



-- TEST HELPERS


{-| Build a ResolverContext with a hand-picked set of global ids. Covers the
operators and helpers we reference in the test bodies below.
-}
commonContext : ResolverContext
commonContext =
    Resolver.initContext [ "Test" ]
        (FastDict.fromList
            -- Basics operators/functions the tests use
            [ ( ( [ "Basics" ], "add" ), 1 )
            , ( ( [ "Basics" ], "sub" ), 2 )
            , ( ( [ "Basics" ], "mul" ), 3 )
            , ( ( [ "Basics" ], "eq" ), 4 )
            , ( ( [ "Basics" ], "neq" ), 5 )
            , ( ( [ "Basics" ], "lt" ), 6 )
            , ( ( [ "Basics" ], "append" ), 7 )
            , ( ( [ "Basics" ], "apL" ), 8 )
            , ( ( [ "Basics" ], "apR" ), 9 )
            , ( ( [ "Basics" ], "and" ), 10 )
            , ( ( [ "Basics" ], "or" ), 11 )
            , ( ( [ "List" ], "cons" ), 20 )
            , ( ( [ "List" ], "map" ), 21 )
            -- User globals used in tests
            , ( ( [ "Test" ], "helper" ), 100 )
            , ( ( [ "Test" ], "value" ), 101 )
            , ( ( [ "Other" ], "func" ), 200 )
            ]
        )


{-| Parse a single Elm expression wrapped in a throwaway module. Extracts the
body of the first declaration. Fails the test if parsing fails or the module
doesn't have exactly one declaration.
-}
parseExpr : String -> Result String (Node Expression)
parseExpr exprSrc =
    let
        source : String
        source =
            "module Test exposing (..)\n\nvalue =\n    " ++ exprSrc
    in
    case Elm.Parser.parseToFile source of
        Err _ ->
            Err ("parse error for: " ++ exprSrc)

        Ok file ->
            case file.declarations of
                [ Node _ (FunctionDeclaration f) ] ->
                    Ok (Node.value f.declaration).expression

                _ ->
                    Err "expected exactly one function declaration"


{-| Parse and resolve in one shot. Use this for tests that don't need a
custom context.
-}
resolveStr : String -> Result String RExpr
resolveStr exprSrc =
    parseExpr exprSrc
        |> Result.andThen
            (\node ->
                case Resolver.resolveExpression commonContext node of
                    Ok e ->
                        Ok e

                    Err err ->
                        Err ("resolve error: " ++ errorToString err)
            )


errorToString : ResolveError -> String
errorToString err =
    case err of
        UnknownName { moduleName, name } ->
            let
                qualified : String
                qualified =
                    if List.isEmpty moduleName then
                        name

                    else
                        String.join "." moduleName ++ "." ++ name
            in
            "UnknownName " ++ qualified

        UnknownOperator op ->
            "UnknownOperator " ++ op

        UnsupportedExpression s ->
            "UnsupportedExpression " ++ s

        InvalidRecordUpdateTarget s ->
            "InvalidRecordUpdateTarget " ++ s

        UnexpectedTupleArity n ->
            "UnexpectedTupleArity " ++ String.fromInt n


{-| Convenience: assert that resolving `src` produces `expected`.
-}
expectResolvesTo : String -> RExpr -> Expect.Expectation
expectResolvesTo src expected =
    case resolveStr src of
        Ok actual ->
            actual |> Expect.equal expected

        Err msg ->
            Expect.fail msg


{-| Strip `debugName` fields from every `RLetBinding` in an `RExpr`. The IR
docstring explicitly says `debugName` is not part of the canonical form —
two identical programs that differ only in user-chosen variable names must
compare equal in the alpha-equivalence sense. Elm's derived equality is
structural and doesn't know that, so we normalize before comparing.
-}
stripDebugNames : RExpr -> RExpr
stripDebugNames expr =
    case expr of
        RInt _ ->
            expr

        RFloat _ ->
            expr

        RString _ ->
            expr

        RChar _ ->
            expr

        RUnit ->
            expr

        RLocal _ ->
            expr

        RGlobal _ ->
            expr

        RCtor _ ->
            expr

        RRecordAccessFunction _ ->
            expr

        RGLSL _ ->
            expr

        RIf c t f ->
            RIf (stripDebugNames c) (stripDebugNames t) (stripDebugNames f)

        RAnd l r ->
            RAnd (stripDebugNames l) (stripDebugNames r)

        ROr l r ->
            ROr (stripDebugNames l) (stripDebugNames r)

        RCase scrut branches ->
            RCase (stripDebugNames scrut)
                (List.map (\( p, b ) -> ( p, stripDebugNames b )) branches)

        RLambda { arity, body } ->
            RLambda { arity = arity, body = stripDebugNames body }

        RLet bindings body ->
            RLet
                (List.map
                    (\b ->
                        { pattern = b.pattern
                        , arity = b.arity
                        , body = stripDebugNames b.body
                        , debugName = ""
                        }
                    )
                    bindings
                )
                (stripDebugNames body)

        RApply head args ->
            RApply (stripDebugNames head) (List.map stripDebugNames args)

        RNegate inner ->
            RNegate (stripDebugNames inner)

        RList items ->
            RList (List.map stripDebugNames items)

        RTuple2 a b ->
            RTuple2 (stripDebugNames a) (stripDebugNames b)

        RTuple3 a b c ->
            RTuple3 (stripDebugNames a) (stripDebugNames b) (stripDebugNames c)

        RRecord fields ->
            RRecord (List.map (\( k, v ) -> ( k, stripDebugNames v )) fields)

        RRecordAccess rec field ->
            RRecordAccess (stripDebugNames rec) field

        RRecordUpdate targetSlot fields ->
            RRecordUpdate targetSlot
                (List.map (\( k, v ) -> ( k, stripDebugNames v )) fields)



-- TESTS


suite : Test
suite =
    describe "Eval.Resolver"
        [ literalTests
        , variableTests
        , lambdaTests
        , applicationTests
        , operatorTests
        , controlFlowTests
        , letTests
        , caseTests
        , recordTests
        , collectionTests
        , alphaEquivalenceTests
        , shadowingTests
        , errorTests
        ]


literalTests : Test
literalTests =
    describe "literals"
        [ test "integer" <|
            \_ -> expectResolvesTo "42" (RInt 42)
        , test "hex literal normalizes to RInt" <|
            \_ -> expectResolvesTo "0x10" (RInt 16)
        , test "negative integer uses RNegate" <|
            \_ -> expectResolvesTo "-5" (RNegate (RInt 5))
        , test "float" <|
            \_ -> expectResolvesTo "3.14" (RFloat 3.14)
        , test "string" <|
            \_ -> expectResolvesTo "\"hello\"" (RString "hello")
        , test "char" <|
            \_ -> expectResolvesTo "'a'" (RChar 'a')
        , test "unit" <|
            \_ -> expectResolvesTo "()" RUnit
        ]


variableTests : Test
variableTests =
    describe "variable resolution"
        [ test "unqualified global resolves to RGlobal via context" <|
            \_ -> expectResolvesTo "helper" (RGlobal 100)
        , test "qualified global" <|
            \_ -> expectResolvesTo "Other.func" (RGlobal 200)
        , test "uppercase unqualified → RCtor" <|
            \_ ->
                expectResolvesTo "Nothing"
                    (RCtor { moduleName = [], name = "Nothing" })
        , test "uppercase qualified → RCtor" <|
            \_ ->
                expectResolvesTo "Maybe.Just"
                    (RCtor { moduleName = [ "Maybe" ], name = "Just" })
        ]


lambdaTests : Test
lambdaTests =
    describe "lambdas"
        [ test "identity: \\x -> x" <|
            \_ ->
                expectResolvesTo "\\x -> x"
                    (RLambda { arity = 1, body = RLocal 0 })
        , test "const: \\x y -> x (outer var is RLocal 1)" <|
            \_ ->
                expectResolvesTo "\\x y -> x"
                    (RLambda { arity = 2, body = RLocal 1 })
        , test "flip: \\x y -> y (inner var is RLocal 0)" <|
            \_ ->
                expectResolvesTo "\\x y -> y"
                    (RLambda { arity = 2, body = RLocal 0 })
        , test "wildcard parameter" <|
            \_ ->
                expectResolvesTo "\\_ -> 42"
                    (RLambda { arity = 1, body = RInt 42 })
        , test "destructuring parameter desugars to let" <|
            \_ ->
                -- `\(a, b) -> a` desugars to `\$_arg0 -> let (a, b) = $_arg0 in a`.
                --
                -- With sequential scoping, the binding's RHS (`$_arg0`) is
                -- resolved against the outer context (`["$_arg0"]`), so
                -- `$_arg0` is at RLocal 0. After the pattern match, the
                -- let body sees locals = [b, a, $_arg0, ...outer], so
                -- `a` is at RLocal 1.
                --
                -- Phase 3's sequential evaluator will honor this by
                -- evaluating each binding's RHS and prepending the
                -- pattern's bindings to the locals stack in order.
                expectResolvesTo "\\( a, b ) -> a"
                    (RLambda
                        { arity = 1
                        , body =
                            RLet
                                [ { pattern = RPTuple2 RPVar RPVar
                                  , arity = 0
                                  , body = RLocal 0
                                  , debugName = "a"
                                  }
                                ]
                                (RLocal 1)
                        }
                    )
        , test "destructuring with reference to second var" <|
            \_ ->
                expectResolvesTo "\\( a, b ) -> b"
                    (RLambda
                        { arity = 1
                        , body =
                            RLet
                                [ { pattern = RPTuple2 RPVar RPVar
                                  , arity = 0
                                  , body = RLocal 0
                                  , debugName = "a"
                                  }
                                ]
                                (RLocal 0)
                        }
                    )
        ]


applicationTests : Test
applicationTests =
    describe "applications"
        [ test "simple application" <|
            \_ ->
                expectResolvesTo "helper 1"
                    (RApply (RGlobal 100) [ RInt 1 ])
        , test "curried application" <|
            \_ ->
                expectResolvesTo "helper 1 2"
                    (RApply (RGlobal 100) [ RInt 1, RInt 2 ])
        , test "applying a local to a global" <|
            \_ ->
                expectResolvesTo "\\f -> f helper"
                    (RLambda
                        { arity = 1
                        , body = RApply (RLocal 0) [ RGlobal 100 ]
                        }
                    )
        , test "lambda passed as an argument (List.map idiom)" <|
            \_ ->
                -- The classic `List.map (\x -> x * 2) xs` idiom. Verifies
                -- that nested scopes compose correctly through applications.
                expectResolvesTo "\\xs -> List.map (\\x -> x * 2) xs"
                    (RLambda
                        { arity = 1
                        , body =
                            RApply (RGlobal 21)
                                [ RLambda
                                    { arity = 1
                                    , body =
                                        RApply (RGlobal 3)
                                            [ RLocal 0
                                            , RInt 2
                                            ]
                                    }
                                , RLocal 0
                                ]
                        }
                    )
        ]


operatorTests : Test
operatorTests =
    describe "operators"
        [ test "+ resolves via Core.operators to Basics.add" <|
            \_ ->
                expectResolvesTo "1 + 2"
                    (RApply (RGlobal 1) [ RInt 1, RInt 2 ])
        , test "- resolves to Basics.sub" <|
            \_ ->
                expectResolvesTo "3 - 1"
                    (RApply (RGlobal 2) [ RInt 3, RInt 1 ])
        , test "* resolves to Basics.mul" <|
            \_ ->
                expectResolvesTo "2 * 3"
                    (RApply (RGlobal 3) [ RInt 2, RInt 3 ])
        , test "== resolves to Basics.eq" <|
            \_ ->
                expectResolvesTo "1 == 1"
                    (RApply (RGlobal 4) [ RInt 1, RInt 1 ])
        , test "&& stays as RAnd (short-circuit preserved)" <|
            \_ ->
                expectResolvesTo "\\a b -> a && b"
                    (RLambda
                        { arity = 2
                        , body = RAnd (RLocal 1) (RLocal 0)
                        }
                    )
        , test "|| stays as ROr" <|
            \_ ->
                expectResolvesTo "\\a b -> a || b"
                    (RLambda
                        { arity = 2
                        , body = ROr (RLocal 1) (RLocal 0)
                        }
                    )
        , test "|> desugars to direct RApply (no Basics.apR indirection)" <|
            \_ ->
                -- `1 |> helper` should become `helper 1`, i.e., RApply helper [1]
                expectResolvesTo "1 |> helper"
                    (RApply (RGlobal 100) [ RInt 1 ])
        , test "<| desugars to direct RApply" <|
            \_ ->
                expectResolvesTo "helper <| 1"
                    (RApply (RGlobal 100) [ RInt 1 ])
        , test "operator reference (+) desugars via Core.operators" <|
            \_ ->
                -- `(+)` is a reference to the operator's function — should
                -- resolve to the underlying function.
                expectResolvesTo "(+)" (RGlobal 1)
        ]


controlFlowTests : Test
controlFlowTests =
    describe "control flow"
        [ test "if-then-else" <|
            \_ ->
                expectResolvesTo "if helper then 1 else 2"
                    (RIf (RGlobal 100) (RInt 1) (RInt 2))
        , test "nested if in lambda" <|
            \_ ->
                expectResolvesTo "\\x -> if x then 1 else 2"
                    (RLambda
                        { arity = 1
                        , body = RIf (RLocal 0) (RInt 1) (RInt 2)
                        }
                    )
        ]


letTests : Test
letTests =
    describe "let bindings"
        [ test "simple value let" <|
            \_ ->
                expectResolvesTo "let x = 1 in x"
                    (RLet
                        [ { pattern = RPVar
                          , arity = 0
                          , body = RInt 1
                          , debugName = "x"
                          }
                        ]
                        (RLocal 0)
                    )
        , test "function let (arity > 0) wraps body in RLambda" <|
            \_ ->
                expectResolvesTo "let f x = x in f"
                    (RLet
                        [ { pattern = RPVar
                          , arity = 1
                          , body = RLambda { arity = 1, body = RLocal 0 }
                          , debugName = "f"
                          }
                        ]
                        (RLocal 0)
                    )
        , test "multiple bindings (later sees earlier)" <|
            \_ ->
                -- let x = 1; y = x in y
                --
                -- With sequential scoping:
                --   * x's RHS is resolved against []. Result: RInt 1.
                --     After: extendedLocals = ["x"].
                --   * y's RHS is resolved against ["x"]. `x` is at slot 0.
                --     After: extendedLocals = ["y", "x"].
                --   * The let body `y` is resolved against ["y", "x"].
                --     `y` is at slot 0.
                expectResolvesTo "let\n        x = 1\n        y = x\n    in\n    y"
                    (RLet
                        [ { pattern = RPVar
                          , arity = 0
                          , body = RInt 1
                          , debugName = "x"
                          }
                        , { pattern = RPVar
                          , arity = 0
                          , body = RLocal 0
                          , debugName = "y"
                          }
                        ]
                        (RLocal 0)
                    )
        , test "destructuring let" <|
            \_ ->
                -- let (a, b) = (1, 2) in a
                -- Pattern binds 2 slots. After the let, locals = [b, a, ...outer]
                -- So `a` resolves to RLocal 1.
                expectResolvesTo "let\n        ( a, b ) =\n            ( 1, 2 )\n    in\n    a"
                    (RLet
                        [ { pattern = RPTuple2 RPVar RPVar
                          , arity = 0
                          , body = RTuple2 (RInt 1) (RInt 2)
                          , debugName = "a"
                          }
                        ]
                        (RLocal 1)
                    )
        ]


caseTests : Test
caseTests =
    describe "case expressions"
        [ test "literal pattern" <|
            \_ ->
                expectResolvesTo
                    "case helper of\n        0 -> \"zero\"\n        _ -> \"other\""
                    (RCase (RGlobal 100)
                        [ ( RPInt 0, RString "zero" )
                        , ( RPWildcard, RString "other" )
                        ]
                    )
        , test "tuple pattern binds fields in order" <|
            \_ ->
                -- case helper of (a, b) -> a
                -- Inside the branch: locals = [b, a, ...outer]
                -- So `a` is RLocal 1.
                expectResolvesTo "case helper of\n        ( a, b ) ->\n            a"
                    (RCase (RGlobal 100)
                        [ ( RPTuple2 RPVar RPVar, RLocal 1 ) ]
                    )
        , test "constructor pattern with one arg" <|
            \_ ->
                expectResolvesTo
                    "case helper of\n        Just v -> v\n        Nothing -> 0"
                    (RCase (RGlobal 100)
                        [ ( RPCtor { moduleName = [], name = "Just" } [ RPVar ]
                          , RLocal 0
                          )
                        , ( RPCtor { moduleName = [], name = "Nothing" } []
                          , RInt 0
                          )
                        ]
                    )
        , test "cons pattern" <|
            \_ ->
                expectResolvesTo
                    "case helper of\n        first :: rest -> first\n        [] -> 0"
                    (RCase (RGlobal 100)
                        [ ( RPCons RPVar RPVar, RLocal 1 )
                        , ( RPList [], RInt 0 )
                        ]
                    )
        , test "as-pattern binds both the inner pattern and the whole value" <|
            \_ ->
                -- case helper of (a, b) as pair -> pair
                -- Bindings in walk order: ["a", "b", "pair"]
                -- After the match: locals = [pair, b, a, ...outer]
                -- So `pair` is RLocal 0.
                expectResolvesTo
                    "case helper of\n        (( a, b ) as pair) ->\n            pair"
                    (RCase (RGlobal 100)
                        [ ( RPAs (RPTuple2 RPVar RPVar), RLocal 0 ) ]
                    )
        ]


recordTests : Test
recordTests =
    describe "records"
        [ test "record construction sorts fields alphabetically" <|
            \_ ->
                -- Source order is b, a, c but we store sorted
                expectResolvesTo "{ b = 2, a = 1, c = 3 }"
                    (RRecord
                        [ ( "a", RInt 1 )
                        , ( "b", RInt 2 )
                        , ( "c", RInt 3 )
                        ]
                    )
        , test "record access" <|
            \_ ->
                expectResolvesTo "helper.field"
                    (RRecordAccess (RGlobal 100) "field")
        , test "record accessor function strips the leading dot" <|
            \_ ->
                expectResolvesTo ".field"
                    (RRecordAccessFunction "field")
        , test "record update targets a local slot" <|
            \_ ->
                -- \r -> { r | x = 1 }
                -- r is the only local (RLocal 0), update targets it
                expectResolvesTo "\\r -> { r | x = 1 }"
                    (RLambda
                        { arity = 1
                        , body = RRecordUpdate 0 [ ( "x", RInt 1 ) ]
                        }
                    )
        , test "record update sorts fields" <|
            \_ ->
                expectResolvesTo "\\r -> { r | b = 2, a = 1 }"
                    (RLambda
                        { arity = 1
                        , body =
                            RRecordUpdate 0
                                [ ( "a", RInt 1 )
                                , ( "b", RInt 2 )
                                ]
                        }
                    )
        ]


collectionTests : Test
collectionTests =
    describe "collections"
        [ test "empty list" <|
            \_ -> expectResolvesTo "[]" (RList [])
        , test "non-empty list" <|
            \_ ->
                expectResolvesTo "[ 1, 2, 3 ]"
                    (RList [ RInt 1, RInt 2, RInt 3 ])
        , test "2-tuple" <|
            \_ ->
                expectResolvesTo "( 1, 2 )"
                    (RTuple2 (RInt 1) (RInt 2))
        , test "3-tuple" <|
            \_ ->
                expectResolvesTo "( 1, 2, 3 )"
                    (RTuple3 (RInt 1) (RInt 2) (RInt 3))
        ]


alphaEquivalenceTests : Test
alphaEquivalenceTests =
    describe "alpha-equivalence (the whole point of De Bruijn indices)"
        [ test "\\x -> x and \\y -> y produce byte-identical RExpr" <|
            \_ ->
                Expect.equal
                    (resolveStr "\\x -> x")
                    (resolveStr "\\y -> y")
        , test "\\x -> x + 1 and \\y -> y + 1 produce identical RExpr" <|
            \_ ->
                Expect.equal
                    (resolveStr "\\x -> x + 1")
                    (resolveStr "\\y -> y + 1")
        , test "\\x y -> x + y and \\a b -> a + b produce identical RExpr" <|
            \_ ->
                Expect.equal
                    (resolveStr "\\x y -> x + y")
                    (resolveStr "\\a b -> a + b")
        , test "let x = 1 in x + 1 and let y = 1 in y + 1 produce identical RExpr (modulo debug names)" <|
            \_ ->
                Expect.equal
                    (Result.map stripDebugNames (resolveStr "let x = 1 in x + 1"))
                    (Result.map stripDebugNames (resolveStr "let y = 1 in y + 1"))
        ]


shadowingTests : Test
shadowingTests =
    describe "shadowing"
        [ test "inner binding shadows outer" <|
            \_ ->
                -- \x -> \x -> x
                -- Outer x is at slot 1, inner x is at slot 0.
                -- Reference resolves to the innermost (slot 0).
                expectResolvesTo "\\x -> \\x -> x"
                    (RLambda
                        { arity = 1
                        , body =
                            RLambda { arity = 1, body = RLocal 0 }
                        }
                    )
        , test "let binding shadows outer lambda parameter" <|
            \_ ->
                -- \x -> let x = 1 in x
                -- Outer x at slot 1, let x at slot 0.
                expectResolvesTo "\\x -> let y = 1 in y"
                    (RLambda
                        { arity = 1
                        , body =
                            RLet
                                [ { pattern = RPVar
                                  , arity = 0
                                  , body = RInt 1
                                  , debugName = "y"
                                  }
                                ]
                                (RLocal 0)
                        }
                    )
        ]


errorTests : Test
errorTests =
    describe "errors"
        [ test "unknown global name" <|
            \_ ->
                case resolveStr "nonexistent" of
                    Err msg ->
                        String.startsWith "resolve error: UnknownName" msg
                            |> Expect.equal True

                    Ok _ ->
                        Expect.fail "expected UnknownName error"
        , test "unknown qualified name" <|
            \_ ->
                case resolveStr "Zzz.nope" of
                    Err msg ->
                        String.startsWith "resolve error: UnknownName" msg
                            |> Expect.equal True

                    Ok _ ->
                        Expect.fail "expected UnknownName error"
        ]
