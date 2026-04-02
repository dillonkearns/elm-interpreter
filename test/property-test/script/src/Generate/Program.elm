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
import Gen.Json.Decode
import Gen.Json.Encode
import Gen.List
import Gen.Maybe
import Gen.Platform
import Gen.Platform.Cmd
import Gen.Platform.Sub
import Gen.Result
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


{-| Combines custom type tests, pattern matching edge cases, JSON tests, and deep nesting.
-}
patternAndEdgeCaseGenerator : Random.Generator ModuleParts
patternAndEdgeCaseGenerator =
    Random.map3
        (\custom patterns jsonAndDeep ->
            combineParts [ custom, patterns, jsonAndDeep ]
        )
        customTypeTestGenerator
        (Random.map (\exprs -> { emptyParts | testExprs = exprs }) patternMatchExprGenerator)
        jsonAndDeepGenerator


{-| JSON tests combined with deep expression nesting tests.
-}
jsonAndDeepGenerator : Random.Generator ModuleParts
jsonAndDeepGenerator =
    Random.map2
        (\json deep ->
            { emptyParts | testExprs = json ++ deep }
        )
        jsonTestGenerator
        deepNestingGenerator


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
    Random.int 0 9
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
                                                (Elm.fn (Elm.Arg.var "n")
                                                    (\n ->
                                                        Elm.fn (Elm.Arg.var "acc")
                                                            (\acc ->
                                                                Elm.Op.append acc
                                                                    (Elm.Op.append (Elm.string ",")
                                                                        (Gen.String.call_.fromInt n)
                                                                    )
                                                            )
                                                    )
                                                    |> Elm.withType
                                                        (Type.function [ Type.int ] (Type.function [ Type.string ] Type.string))
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
                  , Elm.fn (Elm.Arg.var "msg")
                        (\_ ->
                            Elm.fn (Elm.Arg.var "model")
                                (\model -> Elm.tuple model Gen.Platform.Cmd.none)
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
    Random.int 0 9
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
                            (Random.int 0 9)

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
                            (Random.int 0 9)

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
    Random.int 0 9
        |> Random.andThen (\count -> Random.list count singleStringTest)


singleStringTest : Random.Generator Elm.Expression
singleStringTest =
    Random.int 0 9
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
                            (Random.int 0 9)

                    _ ->
                        Random.map2
                            (\a n -> Gen.String.call_.repeat (Elm.int n) (Elm.string (randomWord a)))
                            (Random.int 0 9)
                            (Random.int 0 9)
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
    Random.int 0 9
        |> Random.andThen (\count -> Random.list count singleListTest)


singleListTest : Random.Generator Elm.Expression
singleListTest =
    Random.int 0 9
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
    Random.int 0 9
        |> Random.andThen (\count -> Random.list count singleAdvancedTest)


singleAdvancedTest : Random.Generator Elm.Expression
singleAdvancedTest =
    Random.int 0 9
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
                            (Random.int 0 9)

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
                                        (Elm.fn (Elm.Arg.var "x")
                                            (\x ->
                                                Elm.fn (Elm.Arg.var "y")
                                                    (\y -> Elm.Op.plus (Elm.Op.multiply x y) (Elm.int c))
                                            )
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
                            (Random.map (\n -> n > 0) (Random.int 0 9))

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
                            (Random.map (\n -> n > 0) (Random.int 0 9))

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
    Random.int 0 9
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
                                        (Elm.fn (Elm.Arg.var "x") (\x -> Elm.fn (Elm.Arg.var "y") (\y -> body x y))
                                            |> Elm.withType (Type.function [ Type.int ] (Type.function [ Type.int ] Type.int))
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
                            (Random.int 0 9)

                    1 ->
                        -- Partial application of helper function
                        Random.map3
                            (\a b c ->
                                { declarations =
                                    [ Elm.declaration "add3"
                                        (Elm.fn (Elm.Arg.var "x") (\x -> Elm.fn (Elm.Arg.var "y") (\y -> Elm.Op.plus x y))
                                            |> Elm.withType (Type.function [ Type.int ] (Type.function [ Type.int ] Type.int))
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
                                        (Elm.fn (Elm.Arg.var "name")
                                            (\name ->
                                                Elm.fn (Elm.Arg.var "val")
                                                    (\val ->
                                                        Elm.record
                                                            [ ( "label", name )
                                                            , ( "score", val )
                                                            ]
                                                    )
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
                                        (Elm.fn (Elm.Arg.var "f")
                                            (\f ->
                                                Elm.fn (Elm.Arg.var "x")
                                                    (\x -> Elm.apply f [ Elm.apply f [ x ] ])
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
                                        (Elm.fn (Elm.Arg.var "x")
                                            (\x ->
                                                Elm.fn (Elm.Arg.var "y")
                                                    (\y ->
                                                        Elm.fn (Elm.Arg.var "z")
                                                            (\z -> Elm.Op.plus x (Elm.Op.plus y z))
                                                    )
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
    Random.int 0 9
        |> Random.andThen (\count -> Random.list count singleMiscExpr)


singleMiscExpr : Random.Generator Elm.Expression
singleMiscExpr =
    Random.int 0 9
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
                            (Random.int 0 9)
                            (Random.int 0 9)

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
                            (Random.int 0 9)

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
                            (Random.int 0 9)

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
    Random.int 0 9
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
                            (Random.map (\n -> n > 0) (Random.int 0 9))

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



-- DEEP NESTING GENERATOR
-- Generates expressions where multiple features interact deeply:
-- closures + partial application, case + records, lambdas + let + pipes


deepNestingGenerator : Random.Generator (List Elm.Expression)
deepNestingGenerator =
    Random.int 2 4
        |> Random.andThen (\count -> Random.list count singleDeepTest)


singleDeepTest : Random.Generator Elm.Expression
singleDeepTest =
    Random.int 0 9
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- Closure captures a partially-applied function, used in List.map
                        Random.map2
                            (\a b ->
                                Elm.Let.letIn
                                    (\addN ->
                                        Gen.String.call_.join (Elm.string ",")
                                            (Gen.List.call_.map addN
                                                (Elm.list [ Elm.int 1, Elm.int 2, Elm.int 3 ])
                                            )
                                    )
                                    |> Elm.Let.value "addN"
                                        (Elm.Let.letIn
                                            (\add ->
                                                Elm.apply add [ Elm.int a ]
                                            )
                                            |> Elm.Let.value "add"
                                                (Elm.fn (Elm.Arg.var "x")
                                                    (\x ->
                                                        Elm.fn (Elm.Arg.var "y")
                                                            (\y -> Gen.String.call_.fromInt (Elm.Op.plus x y))
                                                    )
                                                    |> Elm.withType (Type.function [ Type.int ] (Type.function [ Type.int ] Type.string))
                                                )
                                            |> Elm.Let.toExpression
                                        )
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)

                    1 ->
                        -- Case expression returns record, field accessed through pipe
                        Random.map2
                            (\a b ->
                                Elm.Let.letIn
                                    (\result ->
                                        Elm.Op.append
                                            (Elm.get "label" result)
                                            (Gen.String.call_.fromInt (Elm.get "value" result))
                                    )
                                    |> Elm.Let.value "result"
                                        (Elm.Case.maybe
                                            (Elm.ifThen (Elm.Op.gt (Elm.int a) (Elm.int 0))
                                                (Gen.Maybe.make_.just (Elm.int a))
                                                Gen.Maybe.make_.nothing
                                            )
                                            { nothing =
                                                Elm.record
                                                    [ ( "label", Elm.string "none" )
                                                    , ( "value", Elm.int 0 )
                                                    ]
                                            , just =
                                                ( "n"
                                                , \n ->
                                                    Elm.record
                                                        [ ( "label", Elm.string "some" )
                                                        , ( "value", Elm.Op.multiply n (Elm.int b) )
                                                        ]
                                                )
                                            }
                                        )
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)

                    2 ->
                        -- Lambda inside List.map that does case matching on Maybe
                        randomIntList 3 6
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.join (Elm.string ",")
                                        (Gen.List.call_.map
                                            (Elm.fn (Elm.Arg.var "n")
                                                (\n ->
                                                    Elm.Case.maybe
                                                        (Elm.ifThen (Elm.Op.gt n (Elm.int 0))
                                                            (Gen.Maybe.make_.just n)
                                                            Gen.Maybe.make_.nothing
                                                        )
                                                        { nothing = Elm.string "neg"
                                                        , just =
                                                            ( "v"
                                                            , \v -> Gen.String.call_.fromInt (Elm.Op.multiply v (Elm.int 2))
                                                            )
                                                        }
                                                )
                                                |> Elm.withType (Type.function [ Type.int ] Type.string)
                                            )
                                            (Elm.list (List.map Elm.int items))
                                        )
                                )

                    3 ->
                        -- Nested closures: outer captures value, inner captures outer's function
                        Random.map3
                            (\a b c ->
                                Elm.Let.letIn
                                    (\multiplier ->
                                        Elm.Let.letIn
                                            (\scale ->
                                                Elm.Let.letIn
                                                    (\scaleAndAdd ->
                                                        Gen.String.call_.fromInt
                                                            (Elm.apply scaleAndAdd [ Elm.int c ])
                                                    )
                                                    |> Elm.Let.value "scaleAndAdd"
                                                        (Elm.fn (Elm.Arg.var "x")
                                                            (\x ->
                                                                Elm.Op.plus
                                                                    (Elm.apply scale [ x ])
                                                                    (Elm.int b)
                                                            )
                                                            |> Elm.withType (Type.function [ Type.int ] Type.int)
                                                        )
                                                    |> Elm.Let.toExpression
                                            )
                                            |> Elm.Let.value "scale"
                                                (Elm.fn (Elm.Arg.var "n")
                                                    (\n -> Elm.Op.multiply n multiplier)
                                                    |> Elm.withType (Type.function [ Type.int ] Type.int)
                                                )
                                            |> Elm.Let.toExpression
                                    )
                                    |> Elm.Let.value "multiplier" (Elm.int a)
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -5 5)
                            (Random.int -5 5)
                            (Random.int -5 5)

                    4 ->
                        -- Closure captures value, used in nested if/else chain
                        Random.map3
                            (\a b c ->
                                Elm.Let.letIn
                                    (\threshold ->
                                        Elm.Let.letIn
                                            (\classify ->
                                                Gen.String.call_.join (Elm.string ",")
                                                    (Elm.list
                                                        [ Elm.apply classify [ Elm.int a ]
                                                        , Elm.apply classify [ Elm.int b ]
                                                        , Elm.apply classify [ Elm.int c ]
                                                        ]
                                                    )
                                            )
                                            |> Elm.Let.value "classify"
                                                (Elm.fn (Elm.Arg.var "x")
                                                    (\x ->
                                                        Elm.ifThen (Elm.Op.gt x threshold)
                                                            (Elm.string "high")
                                                            (Elm.ifThen (Elm.Op.lt x (Gen.Basics.call_.negate threshold))
                                                                (Elm.string "low")
                                                                (Elm.string "mid")
                                                            )
                                                    )
                                                    |> Elm.withType (Type.function [ Type.int ] Type.string)
                                                )
                                            |> Elm.Let.toExpression
                                    )
                                    |> Elm.Let.value "threshold" (Elm.int (abs a + 1))
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)
                            (Random.int -10 10)

                    5 ->
                        -- Tuple destructuring inside List.map with closure
                        Random.map3
                            (\a b c ->
                                let
                                    pairs =
                                        Elm.list
                                            [ Elm.tuple (Elm.int a) (Elm.string (randomWord (abs a)))
                                            , Elm.tuple (Elm.int b) (Elm.string (randomWord (abs b)))
                                            , Elm.tuple (Elm.int c) (Elm.string (randomWord (abs c)))
                                            ]
                                in
                                Gen.String.call_.join (Elm.string ",")
                                    (Gen.List.call_.map
                                        (Elm.fn (Elm.Arg.var "pair")
                                            (\pair ->
                                                Elm.Op.append
                                                    (Gen.Tuple.call_.second pair)
                                                    (Gen.String.call_.fromInt (Gen.Tuple.call_.first pair))
                                            )
                                        )
                                        pairs
                                    )
                            )
                            (Random.int 0 9)
                            (Random.int 0 9)
                            (Random.int 0 9)

                    6 ->
                        -- Recursive function passed as argument to higher-order function
                        Random.map2
                            (\a b ->
                                Elm.Let.letIn
                                    (\factorial ->
                                        Gen.String.call_.join (Elm.string ",")
                                            (Gen.List.call_.map
                                                (Elm.fn (Elm.Arg.var "n")
                                                    (\n -> Gen.String.call_.fromInt (Elm.apply factorial [ n ]))
                                                    |> Elm.withType (Type.function [ Type.int ] Type.string)
                                                )
                                                (Elm.list (List.map Elm.int (List.range 1 (min 6 (abs a + 1)))))
                                            )
                                    )
                                    |> Elm.Let.value "factorial"
                                        (Elm.fn (Elm.Arg.var "n")
                                            (\n ->
                                                Elm.ifThen (Elm.Op.lte n (Elm.int 1))
                                                    (Elm.int 1)
                                                    (Elm.Op.multiply n
                                                        (Elm.apply (Elm.val "factorial")
                                                            [ Elm.Op.minus n (Elm.int 1) ]
                                                        )
                                                    )
                                            )
                                            |> Elm.withType (Type.function [ Type.int ] Type.int)
                                        )
                                    |> Elm.Let.toExpression
                            )
                            (Random.int 0 9)
                            (Random.int 0 9)

                    7 ->
                        -- Pipeline through multiple transformations with closures
                        Random.map2
                            (\a items ->
                                Elm.Let.letIn
                                    (\threshold ->
                                        Elm.Op.pipe
                                            (Elm.fn (Elm.Arg.var "xs3")
                                                (\xs3 ->
                                                    Gen.String.call_.join (Elm.string ",")
                                                        (Gen.List.call_.map Gen.String.values_.fromInt xs3)
                                                )
                                            )
                                            (Elm.Op.pipe
                                                (Elm.fn (Elm.Arg.var "xs2")
                                                    (\xs2 ->
                                                        Gen.List.call_.filter
                                                            (Elm.fn (Elm.Arg.var "x2")
                                                                (\x2 -> Elm.Op.gt x2 threshold)
                                                                |> Elm.withType (Type.function [ Type.int ] Type.bool)
                                                            )
                                                            xs2
                                                    )
                                                )
                                                (Elm.Op.pipe
                                                    (Elm.fn (Elm.Arg.var "xs1")
                                                        (\xs1 ->
                                                            Gen.List.call_.map
                                                                (Elm.fn (Elm.Arg.var "x1")
                                                                    (\x1 -> Elm.Op.multiply x1 (Elm.int 2))
                                                                    |> Elm.withType (Type.function [ Type.int ] Type.int)
                                                                )
                                                                xs1
                                                        )
                                                    )
                                                    (Elm.list (List.map Elm.int items))
                                                )
                                            )
                                    )
                                    |> Elm.Let.value "threshold" (Elm.int a)
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -5 15)
                            (randomIntList 4 7)

                    8 ->
                        -- JSON encode a record computed from case + closure, then decode a field
                        Random.map2
                            (\a b ->
                                Elm.Let.letIn
                                    (\encoded ->
                                        Elm.Case.result
                                            (Gen.Json.Decode.call_.decodeString
                                                (Gen.Json.Decode.call_.field (Elm.string "result") Gen.Json.Decode.string)
                                                encoded
                                            )
                                            { err = ( "e", \_ -> Elm.string "Err" )
                                            , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") v )
                                            }
                                    )
                                    |> Elm.Let.value "encoded"
                                        (Gen.Json.Encode.call_.encode (Elm.int 0)
                                            (Gen.Json.Encode.call_.object
                                                (Elm.list
                                                    [ Elm.tuple (Elm.string "result")
                                                        (Gen.Json.Encode.call_.string
                                                            (Elm.ifThen (Elm.Op.gt (Elm.int a) (Elm.int b))
                                                                (Elm.string "greater")
                                                                (Elm.string "not-greater")
                                                            )
                                                        )
                                                    ]
                                                )
                                            )
                                        )
                                    |> Elm.Let.toExpression
                            )
                            (Random.int -10 10)
                            (Random.int -10 10)

                    _ ->
                        -- Result.andThen chain with closures
                        Random.map3
                            (\a b c ->
                                Elm.Let.letIn
                                    (\safeDivide ->
                                        Elm.Case.result
                                            (Elm.apply
                                                (Elm.value { importFrom = [ "Result" ], name = "andThen", annotation = Nothing })
                                                [ Elm.fn (Elm.Arg.var "v")
                                                    (\v -> Elm.apply safeDivide [ v, Elm.int c ])
                                                , Elm.apply safeDivide [ Elm.int a, Elm.int b ]
                                                ]
                                            )
                                            { err = ( "e", \e -> Elm.Op.append (Elm.string "Err:") e )
                                            , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") (Gen.String.call_.fromInt v) )
                                            }
                                    )
                                    |> Elm.Let.value "safeDivide"
                                        (Elm.fn (Elm.Arg.var "x")
                                            (\x ->
                                                Elm.fn (Elm.Arg.var "y")
                                                    (\y ->
                                                        Elm.ifThen (Elm.Op.equal y (Elm.int 0))
                                                            (Gen.Result.make_.err (Elm.string "div0"))
                                                            (Gen.Result.make_.ok (Elm.Op.intDivide x y))
                                                    )
                                            )
                                            |> Elm.withType (Type.function [ Type.int ] (Type.function [ Type.int ] (Type.result Type.string Type.int)))
                                        )
                                    |> Elm.Let.toExpression
                            )
                            (Random.int 0 9)
                            (Random.int 0 9)
                            (Random.int 0 9)
            )



-- JSON ENCODE/DECODE ROUNDTRIP GENERATOR


jsonTestGenerator : Random.Generator (List Elm.Expression)
jsonTestGenerator =
    Random.int 0 9
        |> Random.andThen (\count -> Random.list count singleJsonTest)


singleJsonTest : Random.Generator Elm.Expression
singleJsonTest =
    Random.int 0 9
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- Int encode/decode roundtrip
                        Random.map
                            (\n ->
                                let
                                    encoded = Gen.Json.Encode.call_.encode (Elm.int 0) (Gen.Json.Encode.call_.int (Elm.int n))
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString Gen.Json.Decode.int encoded)
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") (Gen.String.call_.fromInt v) )
                                    }
                            )
                            (Random.int -1000 1000)

                    1 ->
                        -- String encode/decode roundtrip
                        Random.map
                            (\idx ->
                                let
                                    word = randomWord idx
                                    encoded = Gen.Json.Encode.call_.encode (Elm.int 0) (Gen.Json.Encode.call_.string (Elm.string word))
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString Gen.Json.Decode.string encoded)
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") v )
                                    }
                            )
                            (Random.int 0 9)

                    2 ->
                        -- Bool encode/decode roundtrip
                        Random.map
                            (\b ->
                                let
                                    boolVal = b > 0
                                    encoded = Gen.Json.Encode.call_.encode (Elm.int 0) (Gen.Json.Encode.call_.bool (Elm.bool boolVal))
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString Gen.Json.Decode.bool encoded)
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.ifThen v (Elm.string "Ok:T") (Elm.string "Ok:F") )
                                    }
                            )
                            (Random.int 0 9)

                    3 ->
                        -- List of ints encode/decode roundtrip
                        randomIntList 2 5
                            |> Random.map
                                (\items ->
                                    let
                                        encoded =
                                            Gen.Json.Encode.call_.encode (Elm.int 0)
                                                (Gen.Json.Encode.call_.list Gen.Json.Encode.values_.int
                                                    (Elm.list (List.map Elm.int items))
                                                )
                                    in
                                    Elm.Case.result
                                        (Gen.Json.Decode.call_.decodeString
                                            (Gen.Json.Decode.call_.list Gen.Json.Decode.int)
                                            encoded
                                        )
                                        { err = ( "e", \_ -> Elm.string "Err" )
                                        , ok = ( "vs", \vs ->
                                            Elm.Op.append (Elm.string "Ok:")
                                                (Gen.String.call_.join (Elm.string ",")
                                                    (Gen.List.call_.map Gen.String.values_.fromInt vs)
                                                )
                                            )
                                        }
                                )

                    4 ->
                        -- Object with field decode
                        Random.map2
                            (\idx n ->
                                let
                                    name = randomWord idx
                                    encoded =
                                        Gen.Json.Encode.call_.encode (Elm.int 0)
                                            (Gen.Json.Encode.call_.object
                                                (Elm.list
                                                    [ Elm.tuple (Elm.string "name") (Gen.Json.Encode.call_.string (Elm.string name))
                                                    , Elm.tuple (Elm.string "value") (Gen.Json.Encode.call_.int (Elm.int n))
                                                    ]
                                                )
                                            )
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString
                                        (Gen.Json.Decode.call_.field (Elm.string "name") Gen.Json.Decode.string)
                                        encoded
                                    )
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") v )
                                    }
                            )
                            (Random.int 0 9)
                            (Random.int -100 100)

                    5 ->
                        -- Decode.map
                        Random.map
                            (\n ->
                                let
                                    encoded = Gen.Json.Encode.call_.encode (Elm.int 0) (Gen.Json.Encode.call_.int (Elm.int n))
                                    decoder =
                                        Gen.Json.Decode.call_.map
                                            (Elm.fn (Elm.Arg.var "x")
                                                (\x -> Elm.Op.multiply x (Elm.int 2))
                                                |> Elm.withType (Type.function [ Type.int ] Type.int)
                                            )
                                            Gen.Json.Decode.int
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString decoder encoded)
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") (Gen.String.call_.fromInt v) )
                                    }
                            )
                            (Random.int -50 50)

                    6 ->
                        -- Decode.null
                        Random.map
                            (\n ->
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString
                                        (Gen.Json.Decode.call_.null (Elm.int n))
                                        (Elm.string "null")
                                    )
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") (Gen.String.call_.fromInt v) )
                                    }
                            )
                            (Random.int -50 50)

                    7 ->
                        -- Decode.succeed always succeeds
                        Random.map
                            (\idx ->
                                let
                                    word = randomWord idx
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString
                                        (Gen.Json.Decode.call_.succeed (Elm.string word))
                                        (Elm.string "42")
                                    )
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") v )
                                    }
                            )
                            (Random.int 0 9)

                    8 ->
                        -- Decode.index
                        Random.map
                            (\n ->
                                let
                                    encoded =
                                        Gen.Json.Encode.call_.encode (Elm.int 0)
                                            (Gen.Json.Encode.call_.list Gen.Json.Encode.values_.int
                                                (Elm.list [ Elm.int 10, Elm.int 20, Elm.int 30 ])
                                            )
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString
                                        (Gen.Json.Decode.call_.index (Elm.int 1) Gen.Json.Decode.int)
                                        encoded
                                    )
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") (Gen.String.call_.fromInt v) )
                                    }
                            )
                            (Random.int 0 9)

                    9 ->
                        -- Decode.andThen (version-dispatch pattern)
                        Random.map
                            (\n ->
                                let
                                    json =
                                        Gen.Json.Encode.call_.encode (Elm.int 0)
                                            (Gen.Json.Encode.call_.object
                                                (Elm.list
                                                    [ Elm.tuple (Elm.string "version") (Gen.Json.Encode.call_.int (Elm.int n))
                                                    , Elm.tuple (Elm.string "data") (Gen.Json.Encode.call_.string (Elm.string "payload"))
                                                    ]
                                                )
                                            )

                                    decoder =
                                        Gen.Json.Decode.call_.andThen
                                            (Elm.fn (Elm.Arg.var "v")
                                                (\v ->
                                                    Elm.ifThen (Elm.Op.equal v (Elm.int 1))
                                                        (Gen.Json.Decode.call_.field (Elm.string "data") Gen.Json.Decode.string)
                                                        (Gen.Json.Decode.call_.fail (Elm.string "bad version"))
                                                )
                                                |> Elm.withType (Type.function [ Type.int ]
                                                    (Type.namedWith [ "Json", "Decode" ] "Decoder" [ Type.string ]))
                                            )
                                            (Gen.Json.Decode.call_.field (Elm.string "version") Gen.Json.Decode.int)
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString decoder json)
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") v )
                                    }
                            )
                            (Random.int 0 9)

                    10 ->
                        -- Decode.map2 (decode two fields and combine)
                        Random.map2
                            (\a s ->
                                let
                                    json =
                                        Gen.Json.Encode.call_.encode (Elm.int 0)
                                            (Gen.Json.Encode.call_.object
                                                (Elm.list
                                                    [ Elm.tuple (Elm.string "x") (Gen.Json.Encode.call_.int (Elm.int a))
                                                    , Elm.tuple (Elm.string "y") (Gen.Json.Encode.call_.string (Elm.string (randomWord s)))
                                                    ]
                                                )
                                            )

                                    decoder =
                                        Gen.Json.Decode.call_.map2
                                            (Elm.fn (Elm.Arg.var "n")
                                                (\n ->
                                                    Elm.fn (Elm.Arg.var "s")
                                                        (\str -> Elm.Op.append str (Gen.String.call_.fromInt n))
                                                )
                                                |> Elm.withType (Type.function [ Type.int ] (Type.function [ Type.string ] Type.string))
                                            )
                                            (Gen.Json.Decode.call_.field (Elm.string "x") Gen.Json.Decode.int)
                                            (Gen.Json.Decode.call_.field (Elm.string "y") Gen.Json.Decode.string)
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString decoder json)
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") v )
                                    }
                            )
                            (Random.int -50 50)
                            (Random.int 0 9)

                    11 ->
                        -- Decode.oneOf (try int first, then string)
                        Random.map
                            (\useInt ->
                                let
                                    json =
                                        if useInt then
                                            Elm.string "42"
                                        else
                                            Elm.string "\"hello\""

                                    decoder =
                                        Gen.Json.Decode.call_.oneOf
                                            (Elm.list
                                                [ Gen.Json.Decode.call_.map
                                                    (Gen.String.values_.fromInt)
                                                    Gen.Json.Decode.int
                                                , Gen.Json.Decode.string
                                                ]
                                            )
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString decoder json)
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok = ( "v", \v -> Elm.Op.append (Elm.string "Ok:") v )
                                    }
                            )
                            (Random.map (\n -> n > 0) (Random.int 0 9))

                    _ ->
                        -- Decode.nullable roundtrip
                        Random.map
                            (\useNull ->
                                let
                                    json =
                                        if useNull then
                                            Elm.string "null"
                                        else
                                            Elm.string "42"

                                    decoder =
                                        Gen.Json.Decode.call_.nullable Gen.Json.Decode.int
                                in
                                Elm.Case.result
                                    (Gen.Json.Decode.call_.decodeString decoder json)
                                    { err = ( "e", \_ -> Elm.string "Err" )
                                    , ok =
                                        ( "v"
                                        , \v ->
                                            Elm.Case.maybe v
                                                { nothing = Elm.string "Ok:null"
                                                , just = ( "n", \n -> Elm.Op.append (Elm.string "Ok:") (Gen.String.call_.fromInt n) )
                                                }
                                        )
                                    }
                            )
                            (Random.map (\n -> n > 0) (Random.int 0 9))
            )



-- HELPERS


randomIntList : Int -> Int -> Random.Generator (List Int)
randomIntList minLen maxLen =
    Random.int minLen maxLen
        |> Random.andThen (\len -> Random.list len (Random.int -50 50))
