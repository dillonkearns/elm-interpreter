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
import Gen.List
import Gen.Platform
import Gen.Platform.Cmd
import Gen.Platform.Sub
import Gen.String
import Random


programGenerator : Int -> Random.Generator Elm.File
programGenerator index =
    let
        moduleName =
            "Test" ++ String.fromInt index
    in
    Random.map3
        (\arithmeticTests stringTests listTests ->
            let
                allTestExprs =
                    arithmeticTests ++ stringTests ++ listTests
            in
            Elm.file [ moduleName ]
                [ Elm.portOutgoing "output" Type.string
                , Elm.declaration "computeResult"
                    (Gen.String.call_.join (Elm.string "|")
                        (Elm.list allTestExprs)
                    )
                , mainDeclaration
                ]
        )
        arithmeticTestGenerator
        stringTestGenerator
        listTestGenerator


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
    Random.int 3 8
        |> Random.andThen
            (\count ->
                Random.list count singleArithmeticTest
            )


singleArithmeticTest : Random.Generator Elm.Expression
singleArithmeticTest =
    Random.int 0 7
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- Simple addition
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromInt
                                    (Elm.Op.plus (Elm.int a) (Elm.int b))
                            )
                            (Random.int -100 100)
                            (Random.int -100 100)

                    1 ->
                        -- Multiplication
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromInt
                                    (Elm.Op.multiply (Elm.int a) (Elm.int b))
                            )
                            (Random.int -50 50)
                            (Random.int -50 50)

                    2 ->
                        -- Nested arithmetic
                        Random.map3
                            (\a b c ->
                                Gen.String.call_.fromInt
                                    (Elm.Op.plus
                                        (Elm.Op.multiply (Elm.int a) (Elm.int b))
                                        (Elm.int c)
                                    )
                            )
                            (Random.int -20 20)
                            (Random.int -20 20)
                            (Random.int -20 20)

                    3 ->
                        -- Integer division
                        Random.map2
                            (\a b ->
                                let
                                    divisor =
                                        if b == 0 then
                                            1

                                        else
                                            b
                                in
                                Gen.String.call_.fromInt
                                    (Elm.Op.intDivide (Elm.int a) (Elm.int divisor))
                            )
                            (Random.int -100 100)
                            (Random.int 1 20)

                    4 ->
                        -- Negate
                        Random.map
                            (\a ->
                                Gen.String.call_.fromInt
                                    (Gen.Basics.call_.negate (Elm.int a))
                            )
                            (Random.int -100 100)

                    5 ->
                        -- abs
                        Random.map
                            (\a ->
                                Gen.String.call_.fromInt
                                    (Gen.Basics.call_.abs (Elm.int a))
                            )
                            (Random.int -100 100)

                    6 ->
                        -- modBy
                        Random.map2
                            (\a b ->
                                let
                                    divisor =
                                        if b == 0 then
                                            1

                                        else
                                            abs b
                                in
                                Gen.String.call_.fromInt
                                    (Gen.Basics.call_.modBy (Elm.int divisor) (Elm.int a))
                            )
                            (Random.int -100 100)
                            (Random.int 1 20)

                    _ ->
                        -- min/max
                        Random.map2
                            (\a b ->
                                Gen.String.call_.fromInt
                                    (Gen.Basics.call_.min (Elm.int a) (Elm.int b))
                            )
                            (Random.int -100 100)
                            (Random.int -100 100)
            )



-- STRING TEST GENERATOR


stringTestGenerator : Random.Generator (List Elm.Expression)
stringTestGenerator =
    Random.int 2 6
        |> Random.andThen
            (\count ->
                Random.list count singleStringTest
            )


singleStringTest : Random.Generator Elm.Expression
singleStringTest =
    Random.int 0 7
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- String.append
                        Random.map2
                            (\a b ->
                                Gen.String.call_.append
                                    (Elm.string (randomWord a))
                                    (Elm.string (randomWord b))
                            )
                            (Random.int 0 9)
                            (Random.int 0 9)

                    1 ->
                        -- String.length
                        Random.map
                            (\a ->
                                Gen.String.call_.fromInt
                                    (Gen.String.call_.length (Elm.string (randomWord a)))
                            )
                            (Random.int 0 9)

                    2 ->
                        -- String.reverse
                        Random.map
                            (\a ->
                                Gen.String.call_.reverse (Elm.string (randomWord a))
                            )
                            (Random.int 0 9)

                    3 ->
                        -- String.toUpper
                        Random.map
                            (\a ->
                                Gen.String.call_.toUpper (Elm.string (randomWord a))
                            )
                            (Random.int 0 9)

                    4 ->
                        -- String.toLower
                        Random.map
                            (\a ->
                                Gen.String.call_.toLower (Elm.string (randomWord a))
                            )
                            (Random.int 0 9)

                    5 ->
                        -- String.fromInt
                        Random.map
                            (\a ->
                                Gen.String.call_.fromInt (Elm.int a)
                            )
                            (Random.int -1000 1000)

                    6 ->
                        -- String.left
                        Random.map2
                            (\a n ->
                                Gen.String.call_.left (Elm.int n) (Elm.string (randomWord a))
                            )
                            (Random.int 0 9)
                            (Random.int 0 5)

                    _ ->
                        -- String.repeat
                        Random.map2
                            (\a n ->
                                Gen.String.call_.repeat (Elm.int n) (Elm.string (randomWord a))
                            )
                            (Random.int 0 9)
                            (Random.int 0 4)
            )


randomWord : Int -> String
randomWord index =
    case modBy 10 index of
        0 ->
            "hello"

        1 ->
            "world"

        2 ->
            "foo"

        3 ->
            "bar"

        4 ->
            "elm"

        5 ->
            "test"

        6 ->
            "alpha"

        7 ->
            "beta"

        8 ->
            "gamma"

        _ ->
            "delta"



-- LIST TEST GENERATOR


listTestGenerator : Random.Generator (List Elm.Expression)
listTestGenerator =
    Random.int 2 5
        |> Random.andThen
            (\count ->
                Random.list count singleListTest
            )


singleListTest : Random.Generator Elm.Expression
singleListTest =
    Random.int 0 5
        |> Random.andThen
            (\tag ->
                case tag of
                    0 ->
                        -- List.map with String.fromInt
                        randomIntList 3 7
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.join (Elm.string ",")
                                        (Gen.List.call_.map
                                            (Gen.String.values_.fromInt)
                                            (Elm.list (List.map Elm.int items))
                                        )
                                )

                    1 ->
                        -- List.length
                        randomIntList 2 8
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.fromInt
                                        (Gen.List.call_.length
                                            (Elm.list (List.map Elm.int items))
                                        )
                                )

                    2 ->
                        -- List.reverse then show
                        randomIntList 3 6
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.join (Elm.string ",")
                                        (Gen.List.call_.map
                                            (Gen.String.values_.fromInt)
                                            (Gen.List.call_.reverse
                                                (Elm.list (List.map Elm.int items))
                                            )
                                        )
                                )

                    3 ->
                        -- List.sum
                        randomIntList 3 8
                            |> Random.map
                                (\items ->
                                    Gen.String.call_.fromInt
                                        (Gen.List.sum
                                            (List.map Elm.int items)
                                        )
                                )

                    4 ->
                        -- List.filter (> threshold)
                        Random.map2
                            (\items threshold ->
                                Gen.String.call_.join (Elm.string ",")
                                    (Gen.List.call_.map
                                        (Gen.String.values_.fromInt)
                                        (Gen.List.call_.filter
                                            (Elm.fn (Elm.Arg.var "x")
                                                (\x ->
                                                    Elm.Op.gt x (Elm.int threshold)
                                                )
                                            )
                                            (Elm.list (List.map Elm.int items))
                                        )
                                    )
                            )
                            (randomIntList 4 8)
                            (Random.int -10 10)

                    _ ->
                        -- List.head
                        randomIntList 1 5
                            |> Random.map
                                (\items ->
                                    Elm.Case.maybe
                                        (Gen.List.call_.head (Elm.list (List.map Elm.int items)))
                                        { nothing = Elm.string "Nothing"
                                        , just =
                                            ( "x"
                                            , \x -> Gen.String.call_.fromInt x
                                            )
                                        }
                                )
            )


randomIntList : Int -> Int -> Random.Generator (List Int)
randomIntList minLen maxLen =
    Random.int minLen maxLen
        |> Random.andThen
            (\len ->
                Random.list len (Random.int -50 50)
            )
