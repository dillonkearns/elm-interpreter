module Generate.Program exposing (programGenerator)

{-| Generates random Elm programs for property-based testing of the interpreter.

Each generated module has:

  - A `computeResult : String` function that exercises various language features
    and produces a deterministic string output
  - A `port output` and `main` using Platform.worker for the elm compiler path

The interpreter evaluates `computeResult` directly; the elm compiler compiles
the whole port module and captures the port output.

-}

import Elm
import Elm.Annotation as Type
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Let
import Elm.Op
import Gen.Basics
import Gen.Char
import Gen.List
import Gen.Maybe
import Gen.Platform
import Gen.Result
import Gen.Platform.Cmd
import Gen.Platform.Sub
import Gen.String
import Gen.Tuple
import Random


type alias ModuleParts =
    { declarations : List Elm.Declaration
    , testExprs : List Elm.Expression
    }


emptyParts : ModuleParts
emptyParts =
    { declarations = [], testExprs = [] }


combineParts : List ModuleParts -> ModuleParts
combineParts parts =
    { declarations = List.concatMap .declarations parts
    , testExprs = List.concatMap .testExprs parts
    }


programGenerator : Int -> Random.Generator Elm.File
programGenerator index =
    let
        moduleName =
            "Test" ++ String.fromInt index
    in
    allTestGenerators
        |> Random.map
            (\parts ->
                let
                    -- Wrap each expr in identity to force parenthesization,
                    -- avoiding bare case/if inside list literals
                    wrappedExprs =
                        List.map (\e -> Gen.Basics.call_.identity e) parts.testExprs
                in
                Elm.file [ moduleName ]
                    ([ Elm.portOutgoing "output" Type.string ]
                        ++ parts.declarations
                        ++ [ Elm.declaration "computeResult"
                                (Gen.String.call_.join (Elm.string "|")
                                    (Elm.list wrappedExprs)
                                )
                           , mainDeclaration
                           ]
                    )
            )


allTestGenerators : Random.Generator ModuleParts
allTestGenerators =
    Random.map5
        (\arithmetic strings lists advanced custom ->
            combineParts [ arithmetic, strings, lists, advanced, custom ]
        )
        (Random.map (\exprs -> { emptyParts | testExprs = exprs }) arithmeticTestGenerator)
        (Random.map (\exprs -> { emptyParts | testExprs = exprs }) stringTestGenerator)
        (Random.map (\exprs -> { emptyParts | testExprs = exprs }) listTestGenerator)
        advancedTestGenerator2
        patternAndEdgeCaseGenerator


{-| Combines custom type tests with pattern matching edge case tests.
-}
patternAndEdgeCaseGenerator : Random.Generator ModuleParts
patternAndEdgeCaseGenerator =
    Random.map2
        (\custom patterns ->
            combineParts [ custom, patterns ]
        )
        customTypeTestGenerator
        (Random.map (\exprs -> { emptyParts | testExprs = exprs }) patternMatchExprGenerator)


{-| Extended advanced tests: wraps original advancedTestGenerator + adds
recursive functions, List.foldl with string accumulators, record updates, etc.
-}
advancedTestGenerator2 : Random.Generator ModuleParts
advancedTestGenerator2 =
    Random.map2
        (\baseExprs extraParts ->
            { declarations = extraParts.declarations
            , testExprs = baseExprs ++ extraParts.testExprs
            }
        )
        advancedTestGenerator
        recursiveAndRecordGenerator


recursiveAndRecordGenerator : Random.Generator ModuleParts
recursiveAndRecordGenerator =
    Random.int 0 4
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- List.foldl with string accumulator
                        randomIntList 2 6
                            |> Random.map
                                (\items ->
                                    { emptyParts
                                        | testExprs =
                                            [ Gen.List.call_.foldl
                                                (Elm.fn2 (Elm.Arg.var "n") (Elm.Arg.var "acc")
                                                    (\n acc ->
                                                        Elm.Op.append acc
                                                            (Elm.Op.append (Elm.string ",")
                                                                (Gen.String.call_.fromInt n)
                                                            )
                                                    )
                                                    |> Elm.withType
                                                        (Type.function [ Type.int, Type.string ] Type.string)
                                                )
                                                (Elm.string "start")
                                                (Elm.list (List.map Elm.int items))
                                            ]
                                    }
                                )

                    1 ->
                        -- Record update syntax via let
                        Random.map3
                            (\a b c ->
                                { emptyParts
                                    | testExprs =
                                        [ Elm.Let.letIn
                                            (\rec ->
                                                let
                                                    updated =
                                                        Elm.updateRecord [ ( "x", Elm.int c ) ] rec
                                                in
                                                Gen.String.call_.fromInt
                                                    (Elm.Op.plus
                                                        (Elm.get "x" updated)
                                                        (Elm.get "y" updated)
                                                    )
                                            )
                                            |> Elm.Let.value "rec"
                                                (Elm.record
                                                    [ ( "x", Elm.int a )
                                                    , ( "y", Elm.int b )
                                                    ]
                                                )
                                            |> Elm.Let.toExpression
                                        ]
                                }
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)
                            (Random.int -20 20)

                    2 ->
                        -- Shadowing in let expressions
                        Random.map3
                            (\a b c ->
                                { emptyParts
                                    | testExprs =
                                        [ Elm.Let.letIn
                                            (\x ->
                                                Elm.Let.letIn
                                                    (\x2 ->
                                                        Gen.String.call_.fromInt (Elm.Op.plus x x2)
                                                    )
                                                    |> Elm.Let.value "x2" (Elm.Op.multiply x (Elm.int c))
                                                    |> Elm.Let.toExpression
                                            )
                                            |> Elm.Let.value "x" (Elm.Op.plus (Elm.int a) (Elm.int b))
                                            |> Elm.Let.toExpression
                                        ]
                                }
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)
                            (Random.int -10 10)

                    3 ->
                        -- List.map with complex lambda
                        randomIntList 3 6
                            |> Random.map
                                (\items ->
                                    { emptyParts
                                        | testExprs =
                                            [ Gen.String.call_.join (Elm.string ",")
                                                (Gen.List.call_.map
                                                    (Elm.fn (Elm.Arg.var "n")
                                                        (\n ->
                                                            Elm.ifThen (Elm.Op.gt n (Elm.int 0))
                                                                (Elm.Op.append (Elm.string "+") (Gen.String.call_.fromInt n))
                                                                (Gen.String.call_.fromInt n)
                                                        )
                                                        |> Elm.withType (Type.function [ Type.int ] Type.string)
                                                    )
                                                    (Elm.list (List.map Elm.int items))
                                                )
                                            ]
                                    }
                                )

                    _ ->
                        -- List.filterMap
                        randomIntList 4 8
                            |> Random.map
                                (\items ->
                                    { emptyParts
                                        | testExprs =
                                            [ Gen.String.call_.join (Elm.string ",")
                                                (Gen.List.call_.filterMap
                                                    (Elm.fn (Elm.Arg.var "n")
                                                        (\n ->
                                                            Elm.ifThen (Elm.Op.gt n (Elm.int 0))
                                                                (Gen.Maybe.make_.just (Gen.String.call_.fromInt n))
                                                                Gen.Maybe.make_.nothing
                                                        )
                                                        |> Elm.withType (Type.function [ Type.int ] (Type.maybe Type.string))
                                                    )
                                                    (Elm.list (List.map Elm.int items))
                                                )
                                            ]
                                    }
                                )
            )


{-| Generate `main` that outputs computeResult via the port.
-}
mainDeclaration : Elm.Declaration
mainDeclaration =
    Elm.declaration "main"
        (Elm.apply
            (Elm.value
                { importFrom = [ "Platform" ]
                , name = "worker"
                , annotation = Nothing
                }
            )
            [ Elm.record
                [ ( "init"
                  , Elm.fn (Elm.Arg.unit)
                        (\_ ->
                            Elm.tuple Elm.unit
                                (Elm.apply (Elm.val "output")
                                    [ Elm.val "computeResult" ]
                                )
                        )
                  )
                , ( "update"
                  , Elm.fn2 (Elm.Arg.var "msg") (Elm.Arg.var "model")
                        (\_ model ->
                            Elm.tuple model Gen.Platform.Cmd.none
                        )
                  )
                , ( "subscriptions"
                  , Elm.fn (Elm.Arg.var "model")
                        (\_ -> Gen.Platform.Sub.none)
                  )
                ]
            ]
        )



-- ARITHMETIC TEST GENERATOR


arithmeticTestGenerator : Random.Generator (List Elm.Expression)
arithmeticTestGenerator =
    Random.int 2 5
        |> Random.andThen (\count -> Random.list count singleArithmeticTest)


singleArithmeticTest : Random.Generator Elm.Expression
singleArithmeticTest =
    Random.int 0 9
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        Random.map2
                            (\a b -> Gen.String.call_.fromInt (Elm.Op.plus (Elm.int a) (Elm.int b)))
                            (Random.int -100 100)
                            (Random.int -100 100)

                    1 ->
                        Random.map2
                            (\a b -> Gen.String.call_.fromInt (Elm.Op.multiply (Elm.int a) (Elm.int b)))
                            (Random.int -50 50)
                            (Random.int -50 50)

                    2 ->
                        Random.map3
                            (\a b c ->
                                Gen.String.call_.fromInt
                                    (Elm.Op.plus (Elm.Op.multiply (Elm.int a) (Elm.int b)) (Elm.int c))
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)
                            (Random.int -20 20)

                    3 ->
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromInt
                                    (Elm.Op.intDivide (Elm.int a) (Elm.int (max 1 (abs b))))
                            )
                            (Random.int -100 100)
                            (Random.int 1 20)

                    4 ->
                        Random.map
                            (\a -> Gen.String.call_.fromInt (Gen.Basics.call_.negate (Elm.int a)))
                            (Random.int -100 100)

                    5 ->
                        Random.map
                            (\a -> Gen.String.call_.fromInt (Gen.Basics.call_.abs (Elm.int a)))
                            (Random.int -100 100)

                    6 ->
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromInt
                                    (Gen.Basics.call_.modBy (Elm.int (max 1 (abs b))) (Elm.int a))
                            )
                            (Random.int -100 100)
                            (Random.int 1 20)

                    7 ->
                        Random.map2
                            (\a b -> Gen.String.call_.fromInt (Gen.Basics.call_.min (Elm.int a) (Elm.int b)))
                            (Random.int -100 100)
                            (Random.int -100 100)

                    8 ->
                        Random.map2
                            (\a b -> Gen.String.call_.fromInt (Gen.Basics.call_.max (Elm.int a) (Elm.int b)))
                            (Random.int -100 100)
                            (Random.int -100 100)

                    _ ->
                        -- Subtraction
                        Random.map2
                            (\a b -> Gen.String.call_.fromInt (Elm.Op.minus (Elm.int a) (Elm.int b)))
                            (Random.int -100 100)
                            (Random.int -100 100)
            )



-- STRING TEST GENERATOR


stringTestGenerator : Random.Generator (List Elm.Expression)
stringTestGenerator =
    Random.int 2 4
        |> Random.andThen (\count -> Random.list count singleStringTest)


singleStringTest : Random.Generator Elm.Expression
singleStringTest =
    Random.int 0 7
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        Random.map2
                            (\a b -> Gen.String.call_.append (Elm.string (randomWord a)) (Elm.string (randomWord b)))
                            (Random.int 0 9)
                            (Random.int 0 9)

                    1 ->
                        Random.map
                            (\a -> Gen.String.call_.fromInt (Gen.String.call_.length (Elm.string (randomWord a))))
                            (Random.int 0 9)

                    2 ->
                        Random.map (\a -> Gen.String.call_.reverse (Elm.string (randomWord a))) (Random.int 0 9)

                    3 ->
                        Random.map (\a -> Gen.String.call_.toUpper (Elm.string (randomWord a))) (Random.int 0 9)

                    4 ->
                        Random.map (\a -> Gen.String.call_.toLower (Elm.string (randomWord a))) (Random.int 0 9)

                    5 ->
                        Random.map (\a -> Gen.String.call_.fromInt (Elm.int a)) (Random.int -1000 1000)

                    6 ->
                        Random.map2
                            (\a n -> Gen.String.call_.left (Elm.int n) (Elm.string (randomWord a)))
                            (Random.int 0 9)
                            (Random.int 0 5)

                    _ ->
                        Random.map2
                            (\a n -> Gen.String.call_.repeat (Elm.int n) (Elm.string (randomWord a)))
                            (Random.int 0 9)
                            (Random.int 0 4)
            )


randomWord : Int -> String
randomWord index =
    case modBy 10 index of
        0 -> "hello"
        1 -> "world"
        2 -> "foo"
        3 -> "bar"
        4 -> "elm"
        5 -> "test"
        6 -> "alpha"
        7 -> "beta"
        8 -> "gamma"
        _ -> "delta"



-- LIST TEST GENERATOR


listTestGenerator : Random.Generator (List Elm.Expression)
listTestGenerator =
    Random.int 2 4
        |> Random.andThen (\count -> Random.list count singleListTest)


singleListTest : Random.Generator Elm.Expression
singleListTest =
    Random.int 0 5
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        randomIntList 3 7
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.join (Elm.string ",")
                                        (Gen.List.call_.map Gen.String.values_.fromInt (Elm.list (List.map Elm.int items)))
                                )

                    1 ->
                        randomIntList 2 8
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.fromInt (Gen.List.call_.length (Elm.list (List.map Elm.int items)))
                                )

                    2 ->
                        randomIntList 3 6
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.join (Elm.string ",")
                                        (Gen.List.call_.map Gen.String.values_.fromInt
                                            (Gen.List.call_.reverse (Elm.list (List.map Elm.int items)))
                                        )
                                )

                    3 ->
                        randomIntList 3 8
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.fromInt (Gen.List.sum (List.map Elm.int items))
                                )

                    4 ->
                        Random.map2
                            (\items threshold ->
                                Gen.String.call_.join (Elm.string ",")
                                    (Gen.List.call_.map Gen.String.values_.fromInt
                                        (Gen.List.call_.filter
                                            (Elm.fn (Elm.Arg.var "x") (\x -> Elm.Op.gt x (Elm.int threshold)))
                                            (Elm.list (List.map Elm.int items))
                                        )
                                    )
                            )
                            (randomIntList 4 8)
                            (Random.int -10 10)

                    _ ->
                        randomIntList 1 5
                            |> Random.map
                                (\items ->
                                    Elm.Case.maybe
                                        (Gen.List.call_.head (Elm.list (List.map Elm.int items)))
                                        { nothing = Elm.string "Nothing"
                                        , just = ( "x", \x -> Gen.String.call_.fromInt x )
                                        }
                                )
            )



-- ADVANCED TEST GENERATOR (custom types, records, let, if/else, tuples, closures)


advancedTestGenerator : Random.Generator (List Elm.Expression)
advancedTestGenerator =
    Random.int 3 6
        |> Random.andThen (\count -> Random.list count singleAdvancedTest)


singleAdvancedTest : Random.Generator Elm.Expression
singleAdvancedTest =
    Random.int 0 15
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- Let expression with binding
                        Random.map2
                            (\a b ->
                                Elm.Let.letIn
                                    (\x ->
                                        Gen.String.call_.fromInt (Elm.Op.plus x (Elm.int b))
                                    )
                                    |> Elm.Let.value "x" (Elm.int a)
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -50 50)
                            (Random.int -50 50)

                    1 ->
                        -- Nested let expressions
                        Random.map3
                            (\a b c ->
                                Elm.Let.letIn
                                    (\x ->
                                        Elm.Let.letIn
                                            (\y ->
                                                Gen.String.call_.fromInt
                                                    (Elm.Op.plus x (Elm.Op.multiply y (Elm.int c)))
                                            )
                                            |> Elm.Let.value "y" (Elm.Op.plus (Elm.int b) x)
                                            |> Elm.Let.toExpression
                                    )
                                    |> Elm.Let.value "x" (Elm.int a)
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)
                            (Random.int -20 20)

                    2 ->
                        -- If/else
                        Random.map3
                            (\a b c ->
                                Elm.ifThen (Elm.Op.gt (Elm.int a) (Elm.int b))
                                    (Gen.String.call_.fromInt (Elm.int c))
                                    (Gen.String.call_.fromInt (Gen.Basics.call_.negate (Elm.int c)))
                            )
                            (Random.int -50 50)
                            (Random.int -50 50)
                            (Random.int 0 100)

                    3 ->
                        -- Nested if/else
                        Random.map3
                            (\a b c ->
                                Elm.ifThen (Elm.Op.gt (Elm.int a) (Elm.int 0))
                                    (Elm.ifThen (Elm.Op.gt (Elm.int b) (Elm.int 0))
                                        (Elm.string "both-pos")
                                        (Elm.string "a-pos")
                                    )
                                    (Elm.ifThen (Elm.Op.gt (Elm.int b) (Elm.int 0))
                                        (Elm.string "b-pos")
                                        (Elm.string "neither-pos")
                                    )
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)
                            (Random.int -10 10)

                    4 ->
                        -- Tuple access
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromInt
                                    (Elm.Op.plus
                                        (Gen.Tuple.call_.first (Elm.tuple (Elm.int a) (Elm.int b)))
                                        (Gen.Tuple.call_.second (Elm.tuple (Elm.int a) (Elm.int b)))
                                    )
                            )
                            (Random.int -50 50)
                            (Random.int -50 50)

                    5 ->
                        -- Record creation and field access
                        Random.map2
                            (\a s ->
                                Elm.Op.append
                                    (Elm.get "name" (Elm.record [ ( "name", Elm.string (randomWord s) ), ( "value", Elm.int a ) ]))
                                    (Gen.String.call_.fromInt (Elm.get "value" (Elm.record [ ( "name", Elm.string (randomWord s) ), ( "value", Elm.int a ) ])))
                            )
                            (Random.int -100 100)
                            (Random.int 0 9)

                    6 ->
                        -- Lambda application
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromInt
                                    (Elm.apply
                                        (Elm.fn (Elm.Arg.var "n")
                                            (\n -> Elm.Op.plus n (Elm.int b))
                                        )
                                        [ Elm.int a ]
                                    )
                            )
                            (Random.int -50 50)
                            (Random.int -50 50)

                    7 ->
                        -- Multi-arg lambda
                        Random.map3
                            (\a b c ->
                                Gen.String.call_.fromInt
                                    (Elm.apply
                                        (Elm.fn2 (Elm.Arg.var "x") (Elm.Arg.var "y")
                                            (\x y -> Elm.Op.plus (Elm.Op.multiply x y) (Elm.int c))
                                        )
                                        [ Elm.int a, Elm.int b ]
                                    )
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)
                            (Random.int -10 10)

                    8 ->
                        -- Let with function binding (closure)
                        Random.map2
                            (\a b ->
                                Elm.Let.letIn
                                    (\addN ->
                                        Gen.String.call_.fromInt
                                            (Elm.apply addN [ Elm.int b ])
                                    )
                                    |> Elm.Let.value "addN"
                                        (Elm.fn (Elm.Arg.var "x")
                                            (\x -> Elm.Op.plus x (Elm.int a))
                                        )
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -30 30)
                            (Random.int -30 30)

                    9 ->
                        -- Maybe.map
                        Random.map2
                            (\a useJust ->
                                let
                                    maybeVal =
                                        if useJust then
                                            Gen.Maybe.make_.just (Elm.int a)

                                        else
                                            Gen.Maybe.make_.nothing
                                in
                                Elm.Case.maybe
                                    (Gen.Maybe.call_.map
                                        (Elm.fn (Elm.Arg.var "n")
                                            (\n -> Elm.Op.multiply n (Elm.int 2))
                                        )
                                        maybeVal
                                    )
                                    { nothing = Elm.string "Nothing"
                                    , just = ( "v", \v -> Gen.String.call_.fromInt v )
                                    }
                            )
                            (Random.int -50 50)
                            (Random.map (\n -> n > 0) (Random.int 0 1))

                    10 ->
                        -- Result.map
                        Random.map2
                            (\a useOk ->
                                let
                                    resultVal =
                                        if useOk then
                                            Gen.Result.make_.ok (Elm.int a)

                                        else
                                            Gen.Result.make_.err (Elm.string "oops")
                                in
                                Elm.Case.result resultVal
                                    { err = ( "e", \e -> Elm.Op.append (Elm.string "Err:") e )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") (Gen.String.call_.fromInt v) )
                                    }
                            )
                            (Random.int -50 50)
                            (Random.map (\n -> n > 0) (Random.int 0 1))

                    11 ->
                        -- Piping (|>)
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromInt
                                    (Elm.Op.pipe
                                        (Elm.fn (Elm.Arg.var "n")
                                            (\n -> Elm.Op.plus n (Elm.int b))
                                        )
                                        (Elm.int a)
                                    )
                            )
                            (Random.int -50 50)
                            (Random.int -50 50)

                    12 ->
                        -- Record accessor with List.sortBy
                        Random.map3
                            (\a b c ->
                                let
                                    getName = Elm.fn (Elm.Arg.var "r") (\r -> Elm.get "name" r)
                                    getScore = Elm.fn (Elm.Arg.var "r") (\r -> Elm.get "score" r)
                                    records =
                                        Elm.list
                                            [ Elm.record [ ( "name", Elm.string (randomWord a) ), ( "score", Elm.int b ) ]
                                            , Elm.record [ ( "name", Elm.string (randomWord c) ), ( "score", Elm.int a ) ]
                                            , Elm.record [ ( "name", Elm.string (randomWord b) ), ( "score", Elm.int c ) ]
                                            ]
                                in
                                Gen.String.call_.join (Elm.string ",")
                                    (Gen.List.call_.map getName
                                        (Gen.List.call_.sortBy getScore records)
                                    )
                            )
                            (Random.int 0 9)
                            (Random.int -20 20)
                            (Random.int 0 9)

                    13 ->
                        -- Char classification functions
                        Random.map
                            (\idx ->
                                let
                                    c = Elm.char (randomChar idx)

                                    boolStr b =
                                        Elm.ifThen b (Elm.string "T") (Elm.string "F")
                                in
                                Gen.String.call_.join (Elm.string ",")
                                    (Elm.list
                                        [ boolStr (Gen.Char.call_.isAlpha c)
                                        , boolStr (Gen.Char.call_.isDigit c)
                                        , boolStr (Gen.Char.call_.isUpper c)
                                        , boolStr (Gen.Char.call_.isLower c)
                                        ]
                                    )
                            )
                            (Random.int 0 9)

                    14 ->
                        -- Composed functions applied
                        Random.map2
                            (\a b ->
                                let
                                    addB = Elm.fn (Elm.Arg.var "x") (\x -> Elm.Op.plus x (Elm.int b))
                                        |> Elm.withType (Type.function [ Type.int ] Type.int)
                                    doubleIt = Elm.fn (Elm.Arg.var "y") (\y -> Elm.Op.multiply y (Elm.int 2))
                                        |> Elm.withType (Type.function [ Type.int ] Type.int)
                                in
                                Gen.String.call_.fromInt
                                    (Elm.apply doubleIt [ Elm.apply addB [ Elm.int a ] ])
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)

                    _ ->
                        -- Record field access via lambda
                        Random.map2
                            (\a s ->
                                Elm.apply
                                    (Elm.fn (Elm.Arg.var "rec") (\rec -> Elm.get "name" rec))
                                    [ Elm.record [ ( "name", Elm.string (randomWord s) ), ( "value", Elm.int a ) ] ]
                            )
                            (Random.int -50 50)
                            (Random.int 0 9)
            )



-- CUSTOM TYPE + HELPER FUNCTION GENERATOR


customTypeTestGenerator : Random.Generator ModuleParts
customTypeTestGenerator =
    Random.map2
        (\helperParts miscExprs ->
            { declarations = helperParts.declarations
            , testExprs = helperParts.testExprs ++ miscExprs
            }
        )
        helperFunctionGenerator
        miscExprGenerator


{-| Generate top-level helper functions and test expressions that call them.
Exercises: function declarations, partial application, recursion, closures.
-}
helperFunctionGenerator : Random.Generator ModuleParts
helperFunctionGenerator =
    Random.int 0 7
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- Helper function with 2 args (typed as Int -> Int -> Int)
                        Random.map3
                            (\a b op ->
                                let
                                    fnName = "helper"
                                    body x y =
                                        case op of
                                            0 -> Elm.Op.plus x y
                                            1 -> Elm.Op.multiply x y
                                            _ -> Elm.Op.minus x y
                                in
                                { declarations =
                                    [ Elm.declaration fnName
                                        (Elm.fn2 (Elm.Arg.var "x") (Elm.Arg.var "y") (\x y -> body x y)
                                            |> Elm.withType (Type.function [ Type.int, Type.int ] Type.int)
                                        )
                                    ]
                                , testExprs =
                                    [ Gen.String.call_.fromInt
                                        (Elm.apply (Elm.val fnName) [ Elm.int a, Elm.int b ])
                                    ]
                                }
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)
                            (Random.int 0 2)

                    1 ->
                        -- Partial application of helper function
                        Random.map3
                            (\a b c ->
                                { declarations =
                                    [ Elm.declaration "add3"
                                        (Elm.fn2 (Elm.Arg.var "x") (Elm.Arg.var "y")
                                            (\x y -> Elm.Op.plus x y)
                                            |> Elm.withType (Type.function [ Type.int, Type.int ] Type.int)
                                        )
                                    , Elm.declaration "addTo"
                                        (Elm.apply (Elm.val "add3") [ Elm.int a ]
                                            |> Elm.withType (Type.function [ Type.int ] Type.int)
                                        )
                                    ]
                                , testExprs =
                                    [ Gen.String.call_.fromInt
                                        (Elm.apply (Elm.val "addTo") [ Elm.int b ])
                                    , Gen.String.call_.fromInt
                                        (Elm.apply (Elm.val "add3") [ Elm.int a, Elm.int c ])
                                    ]
                                }
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)
                            (Random.int -20 20)

                    2 ->
                        -- Function with conditional (typed as Int -> String)
                        Random.map2
                            (\a b ->
                                { declarations =
                                    [ Elm.declaration "describeSign"
                                        (Elm.fn (Elm.Arg.var "n")
                                            (\n ->
                                                Elm.ifThen (Elm.Op.gt n (Elm.int 0))
                                                    (Elm.string "positive")
                                                    (Elm.ifThen (Elm.Op.lt n (Elm.int 0))
                                                        (Elm.string "negative")
                                                        (Elm.string "zero")
                                                    )
                                            )
                                            |> Elm.withType (Type.function [ Type.int ] Type.string)
                                        )
                                    ]
                                , testExprs =
                                    [ Elm.apply (Elm.val "describeSign") [ Elm.int a ]
                                    , Elm.apply (Elm.val "describeSign") [ Elm.int b ]
                                    , Elm.apply (Elm.val "describeSign") [ Elm.int 0 ]
                                    ]
                                }
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)

                    3 ->
                        -- Record-returning function
                        Random.map2
                            (\a s ->
                                { declarations =
                                    [ Elm.declaration "makeRecord"
                                        (Elm.fn2 (Elm.Arg.var "name") (Elm.Arg.var "val")
                                            (\name val ->
                                                Elm.record
                                                    [ ( "label", name )
                                                    , ( "score", val )
                                                    ]
                                            )
                                            |> Elm.withType
                                                (Type.function [ Type.string, Type.int ]
                                                    (Type.record [ ( "label", Type.string ), ( "score", Type.int ) ])
                                                )
                                        )
                                    ]
                                , testExprs =
                                    [ Elm.Op.append
                                        (Elm.get "label"
                                            (Elm.apply (Elm.val "makeRecord")
                                                [ Elm.string (randomWord s), Elm.int a ]
                                            )
                                        )
                                        (Gen.String.call_.fromInt
                                            (Elm.get "score"
                                                (Elm.apply (Elm.val "makeRecord")
                                                    [ Elm.string (randomWord s), Elm.int a ]
                                                )
                                            )
                                        )
                                    ]
                                }
                            )
                            (Random.int -50 50)
                            (Random.int 0 9)

                    4 ->
                        -- Higher-order function (takes Int -> Int function)
                        Random.map2
                            (\a b ->
                                { declarations =
                                    [ Elm.declaration "applyTwice"
                                        (Elm.fn2 (Elm.Arg.var "f") (Elm.Arg.var "x")
                                            (\f x ->
                                                Elm.apply f [ Elm.apply f [ x ] ]
                                            )
                                            |> Elm.withType
                                                (Type.function
                                                    [ Type.function [ Type.int ] Type.int
                                                    , Type.int
                                                    ]
                                                    Type.int
                                                )
                                        )
                                    ]
                                , testExprs =
                                    [ Gen.String.call_.fromInt
                                        (Elm.apply (Elm.val "applyTwice")
                                            [ Elm.fn (Elm.Arg.var "n")
                                                (\n -> Elm.Op.plus n (Elm.int a))
                                            , Elm.int b
                                            ]
                                        )
                                    ]
                                }
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)

                    5 ->
                        -- 3-arg partial application chain
                        Random.map3
                            (\a b c ->
                                { declarations =
                                    [ Elm.declaration "add3args"
                                        (Elm.fn2 (Elm.Arg.var "x") (Elm.Arg.var "y")
                                            (\x y ->
                                                Elm.fn (Elm.Arg.var "z")
                                                    (\z -> Elm.Op.plus x (Elm.Op.plus y z))
                                            )
                                            |> Elm.withType (Type.function [ Type.int, Type.int ] (Type.function [ Type.int ] Type.int))
                                        )
                                    , Elm.declaration "partial1"
                                        (Elm.apply (Elm.val "add3args") [ Elm.int a ])
                                    , Elm.declaration "partial2"
                                        (Elm.apply (Elm.val "partial1") [ Elm.int b ])
                                    ]
                                , testExprs =
                                    [ Gen.String.call_.fromInt
                                        (Elm.apply (Elm.val "partial2") [ Elm.int c ])
                                    , Gen.String.call_.fromInt
                                        (Elm.apply (Elm.val "add3args") [ Elm.int a, Elm.int b, Elm.int c ])
                                    ]
                                }
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)
                            (Random.int -10 10)

                    6 ->
                        -- String.filter and String.map
                        Random.map
                            (\idx ->
                                let
                                    word = randomWord idx
                                in
                                { declarations =
                                    [ Elm.declaration "filterVowels"
                                        (Elm.fn (Elm.Arg.var "s")
                                            (\s ->
                                                Gen.String.call_.filter
                                                    (Elm.fn (Elm.Arg.var "c")
                                                        (\c ->
                                                            Gen.Basics.call_.not
                                                                (Gen.List.call_.member c
                                                                    (Elm.list [ Elm.char 'a', Elm.char 'e', Elm.char 'i', Elm.char 'o', Elm.char 'u' ])
                                                                )
                                                        )
                                                    )
                                                    s
                                            )
                                            |> Elm.withType (Type.function [ Type.string ] Type.string)
                                        )
                                    ]
                                , testExprs =
                                    [ Elm.apply (Elm.val "filterVowels") [ Elm.string word ]
                                    ]
                                }
                            )
                            (Random.int 0 9)

                    _ ->
                        -- Complex let with dependency chain
                        Random.map3
                            (\a b c ->
                                { emptyParts
                                    | testExprs =
                                        [ Elm.Let.letIn
                                            (\w ->
                                                Elm.Let.letIn
                                                    (\z ->
                                                        Elm.Let.letIn
                                                            (\y ->
                                                                Elm.Let.letIn
                                                                    (\x ->
                                                                        Gen.String.call_.fromInt x
                                                                    )
                                                                    |> Elm.Let.value "x" (Elm.Op.plus y (Elm.int 1))
                                                                    |> Elm.Let.toExpression
                                                            )
                                                            |> Elm.Let.value "y" (Elm.Op.plus z (Elm.int 2))
                                                            |> Elm.Let.toExpression
                                                    )
                                                    |> Elm.Let.value "z" (Elm.Op.plus w (Elm.int 3))
                                                    |> Elm.Let.toExpression
                                            )
                                            |> Elm.Let.value "w" (Elm.int a)
                                            |> Elm.Let.toExpression
                                        ]
                                }
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)
                            (Random.int -20 20)
            )


{-| Miscellaneous expressions (boolean, comparison, string ops, list ops).
-}
miscExprGenerator : Random.Generator (List Elm.Expression)
miscExprGenerator =
    Random.int 2 4
        |> Random.andThen (\count -> Random.list count singleMiscExpr)


singleMiscExpr : Random.Generator Elm.Expression
singleMiscExpr =
    Random.int 0 8
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- Boolean and/or
                        Random.map2
                            (\a b ->
                                let
                                    boolA = Elm.Op.gt (Elm.int a) (Elm.int 0)
                                    boolB = Elm.Op.gt (Elm.int b) (Elm.int 0)
                                in
                                Elm.ifThen (Elm.Op.and boolA boolB)
                                    (Elm.string "both")
                                    (Elm.ifThen (Elm.Op.or boolA boolB)
                                        (Elm.string "one")
                                        (Elm.string "none")
                                    )
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)

                    1 ->
                        -- String.contains
                        Random.map2
                            (\a b ->
                                Elm.ifThen
                                    (Gen.String.call_.contains
                                        (Elm.string (randomWord a))
                                        (Elm.string (randomWord a ++ randomWord b))
                                    )
                                    (Elm.string "yes")
                                    (Elm.string "no")
                            )
                            (Random.int 0 9)
                            (Random.int 0 9)

                    2 ->
                        -- Equality check
                        Random.map2
                            (\a b ->
                                Elm.ifThen (Elm.Op.equal (Elm.int a) (Elm.int b))
                                    (Elm.string "eq")
                                    (Elm.string "neq")
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)

                    3 ->
                        -- String.slice
                        Random.map3
                            (\s start end ->
                                Gen.String.call_.slice (Elm.int start) (Elm.int end) (Elm.string (randomWord s))
                            )
                            (Random.int 0 9)
                            (Random.int 0 3)
                            (Random.int 2 5)

                    4 ->
                        -- String.split and List.length
                        Random.map
                            (\s ->
                                Gen.String.call_.fromInt
                                    (Gen.List.call_.length
                                        (Gen.String.call_.split (Elm.string "l")
                                            (Elm.string (randomWord s))
                                        )
                                    )
                            )
                            (Random.int 0 9)

                    5 ->
                        -- List.concat
                        Random.map2
                            (\items1 items2 ->
                                Gen.String.call_.join (Elm.string ",")
                                    (Gen.List.call_.map Gen.String.values_.fromInt
                                        (Gen.List.call_.concat
                                            (Elm.list
                                                [ Elm.list (List.map Elm.int items1)
                                                , Elm.list (List.map Elm.int items2)
                                                ]
                                            )
                                        )
                                    )
                            )
                            (randomIntList 1 3)
                            (randomIntList 1 3)

                    6 ->
                        -- List.take
                        Random.map2
                            (\items n ->
                                Gen.String.call_.join (Elm.string ",")
                                    (Gen.List.call_.map Gen.String.values_.fromInt
                                        (Gen.List.call_.take (Elm.int n) (Elm.list (List.map Elm.int items)))
                                    )
                            )
                            (randomIntList 3 7)
                            (Random.int 0 5)

                    7 ->
                        -- List.drop
                        Random.map2
                            (\items n ->
                                Gen.String.call_.join (Elm.string ",")
                                    (Gen.List.call_.map Gen.String.values_.fromInt
                                        (Gen.List.call_.drop (Elm.int n) (Elm.list (List.map Elm.int items)))
                                    )
                            )
                            (randomIntList 3 7)
                            (Random.int 0 5)

                    _ ->
                        -- Comparison with compare
                        Random.map2
                            (\a b ->
                                Elm.Case.custom
                                    (Gen.Basics.call_.compare (Elm.int a) (Elm.int b))
                                    (Type.named [ "Basics" ] "Order")
                                    [ Elm.Case.branch (Elm.Arg.customType "LT" ()) (\_ -> Elm.string "LT")
                                    , Elm.Case.branch (Elm.Arg.customType "EQ" ()) (\_ -> Elm.string "EQ")
                                    , Elm.Case.branch (Elm.Arg.customType "GT" ()) (\_ -> Elm.string "GT")
                                    ]
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)
            )



-- PATTERN MATCHING EDGE CASE GENERATOR


patternMatchExprGenerator : Random.Generator (List Elm.Expression)
patternMatchExprGenerator =
    Random.int 3 6
        |> Random.andThen (\count -> Random.list count singlePatternTest)


singlePatternTest : Random.Generator Elm.Expression
singlePatternTest =
    Random.int 0 9
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- Case on integer literal patterns (non-negative only; negative literals aren't valid patterns in Elm)
                        Random.map3
                            (\a b target ->
                                Elm.Case.custom (Elm.int target)
                                    Type.int
                                    [ Elm.Case.branch (Elm.Arg.int a) (\_ -> Elm.string ("got" ++ String.fromInt a))
                                    , Elm.Case.branch (Elm.Arg.int b) (\_ -> Elm.string ("got" ++ String.fromInt b))
                                    , Elm.Case.branch (Elm.Arg.var "other") (\other -> Gen.String.call_.fromInt other)
                                    ]
                            )
                            (Random.int 0 20)
                            (Random.int 21 40)
                            (Random.int -20 40)

                    1 ->
                        -- Tuple destructuring in let
                        Random.map3
                            (\a b c ->
                                Elm.Let.letIn
                                    (\pair ->
                                        Gen.String.call_.fromInt
                                            (Elm.Op.plus
                                                (Gen.Tuple.call_.first pair)
                                                (Elm.Op.multiply (Gen.Tuple.call_.second pair) (Elm.int c))
                                            )
                                    )
                                    |> Elm.Let.value "pair" (Elm.tuple (Elm.int a) (Elm.int b))
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)
                            (Random.int -20 20)

                    2 ->
                        -- Case on string literal patterns
                        Random.map2
                            (\idx extra ->
                                let
                                    word = randomWord idx
                                in
                                Elm.Case.custom (Elm.string word)
                                    Type.string
                                    [ Elm.Case.branch (Elm.Arg.string "hello") (\_ -> Elm.string "greeting")
                                    , Elm.Case.branch (Elm.Arg.string "world") (\_ -> Elm.string "planet")
                                    , Elm.Case.branch (Elm.Arg.string "foo") (\_ -> Elm.string "metavar")
                                    , Elm.Case.branch (Elm.Arg.var "s") (\s -> Elm.Op.append (Elm.string "other:") s)
                                    ]
                            )
                            (Random.int 0 9)
                            (Random.int 0 9)

                    3 ->
                        -- Char literal
                        Random.map
                            (\idx ->
                                let
                                    c = randomChar idx
                                in
                                Gen.String.call_.fromChar (Elm.char c)
                            )
                            (Random.int 0 9)

                    4 ->
                        -- Float arithmetic
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromFloat
                                    (Elm.Op.plus (Elm.float (toFloat a / 10.0)) (Elm.float (toFloat b / 10.0)))
                            )
                            (Random.int -100 100)
                            (Random.int -100 100)

                    5 ->
                        -- Nested Maybe pattern matching (Just (Just x) vs Just Nothing vs Nothing)
                        Random.map2
                            (\a useInner ->
                                let
                                    innerMaybe =
                                        if useInner then
                                            Gen.Maybe.make_.just (Elm.int a)
                                        else
                                            Gen.Maybe.make_.nothing

                                    outerMaybe =
                                        if a > 0 then
                                            Gen.Maybe.make_.just innerMaybe
                                        else
                                            Gen.Maybe.make_.nothing
                                in
                                Elm.Case.maybe outerMaybe
                                    { nothing = Elm.string "none"
                                    , just =
                                        ( "inner"
                                        , \inner ->
                                            Elm.Case.maybe inner
                                                { nothing = Elm.string "just-nothing"
                                                , just = ( "v", \v -> Elm.Op.append (Elm.string "val:") (Gen.String.call_.fromInt v) )
                                                }
                                        )
                                    }
                            )
                            (Random.int -10 10)
                            (Random.map (\n -> n > 0) (Random.int 0 1))

                    6 ->
                        -- List.sort
                        randomIntList 3 6
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.join (Elm.string ",")
                                        (Gen.List.call_.map Gen.String.values_.fromInt
                                            (Gen.List.sort (List.map Elm.int items))
                                        )
                                )

                    7 ->
                        -- Multiple let bindings referencing each other
                        Random.map3
                            (\a b c ->
                                Elm.Let.letIn
                                    (\x ->
                                        Elm.Let.letIn
                                            (\y ->
                                                Elm.Let.letIn
                                                    (\z ->
                                                        Gen.String.call_.fromInt (Elm.Op.plus x (Elm.Op.plus y z))
                                                    )
                                                    |> Elm.Let.value "z" (Elm.Op.multiply x y)
                                                    |> Elm.Let.toExpression
                                            )
                                            |> Elm.Let.value "y" (Elm.Op.plus x (Elm.int b))
                                            |> Elm.Let.toExpression
                                    )
                                    |> Elm.Let.value "x" (Elm.int a)
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)
                            (Random.int -10 10)

                    8 ->
                        -- List.any / List.all
                        randomIntList 3 7
                            |> Random.map
                                (\items ->
                                    Elm.ifThen
                                        (Gen.List.call_.any
                                            (Elm.fn (Elm.Arg.var "n")
                                                (\n -> Elm.Op.gt n (Elm.int 0))
                                                |> Elm.withType (Type.function [ Type.int ] Type.bool)
                                            )
                                            (Elm.list (List.map Elm.int items))
                                        )
                                        (Elm.ifThen
                                            (Gen.List.call_.all
                                                (Elm.fn (Elm.Arg.var "n")
                                                    (\n -> Elm.Op.gt n (Elm.int 0))
                                                    |> Elm.withType (Type.function [ Type.int ] Type.bool)
                                                )
                                                (Elm.list (List.map Elm.int items))
                                            )
                                            (Elm.string "all-pos")
                                            (Elm.string "some-pos")
                                        )
                                        (Elm.string "none-pos")
                                )

                    _ ->
                        -- Not operator
                        Random.map
                            (\a ->
                                Elm.ifThen
                                    (Gen.Basics.call_.not (Elm.Op.gt (Elm.int a) (Elm.int 0)))
                                    (Elm.string "not-positive")
                                    (Elm.string "positive")
                            )
                            (Random.int -10 10)
            )


randomChar : Int -> Char
randomChar idx =
    case modBy 10 idx of
        0 -> 'a'
        1 -> 'z'
        2 -> 'A'
        3 -> 'Z'
        4 -> '0'
        5 -> '9'
        6 -> ' '
        7 -> '!'
        8 -> 'x'
        _ -> 'q'



-- HELPERS


randomIntList : Int -> Int -> Random.Generator (List Int)
randomIntList minLen maxLen =
    Random.int minLen maxLen
        |> Random.andThen (\len -> Random.list len (Random.int -50 50))
