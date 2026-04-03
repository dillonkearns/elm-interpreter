module CoreTests.Equality exposing (suite)

import Fuzz
import Test exposing (Test, describe)
import TestUtils exposing (evalExpect, evalTest, withInt)
import Types exposing (Value(..))


type Different
    = A String
    | B (List Int)


suite : Test
suite =
    describe "Equality Tests"
        [ diffTests
        , recordTests
        , listTests
        , boolComparisonTests
        , dictSetEqualityTests
        ]


{-| Tests for Dict/Set equality.
Elm's runtime converts Dict/Set to sorted lists before comparing,
so two trees with the same elements but different internal structure
(e.g., different red-black node colors) should still be equal.
These tests exercise operations that produce structurally different
red-black trees compared to Set.fromList/Dict.fromList.
-}
dictSetEqualityTests : Test
dictSetEqualityTests =
    describe "Dict/Set structural equality"
        [ -- These two tests exercise the fix: Set.union and Set.remove produce
          -- red-black trees with different internal structure (node colors/rotations)
          -- than Set.fromList. Without converting to sorted lists before comparing,
          -- structural equality would incorrectly return False.
          evalTest "set union equality"
            "Set.union (Set.fromList [1, 2]) (Set.fromList [3, 4]) == Set.fromList [1, 2, 3, 4]"
            Bool
            True
        , evalTest "set remove equality"
            "Set.remove 3 (Set.fromList [1, 2, 3, 4, 5]) == Set.fromList [1, 2, 4, 5]"
            Bool
            True

        -- Additional coverage for other Set/Dict operations
        , evalTest "set diff equality"
            "Set.diff (Set.fromList [1, 2, 3, 4]) (Set.fromList [2, 4]) == Set.fromList [1, 3]"
            Bool
            True
        , evalTest "set intersect equality"
            "Set.intersect (Set.fromList [1, 2, 3, 4]) (Set.fromList [2, 3, 5]) == Set.fromList [2, 3]"
            Bool
            True
        , evalTest "dict union equality"
            "Dict.union (Dict.fromList [(1, \"a\")]) (Dict.fromList [(2, \"b\")]) == Dict.fromList [(1, \"a\"), (2, \"b\")]"
            Bool
            True
        , evalTest "dict remove equality"
            "Dict.remove 2 (Dict.fromList [(1, \"a\"), (2, \"b\"), (3, \"c\")]) == Dict.fromList [(1, \"a\"), (3, \"c\")]"
            Bool
            True

        -- Negative cases: sets/dicts with different elements should not be equal
        , evalTest "set not equal"
            "Set.fromList [1, 2, 3] == Set.fromList [1, 2, 4]"
            Bool
            False
        , evalTest "dict not equal"
            "Dict.fromList [(1, \"a\")] == Dict.fromList [(1, \"b\")]"
            Bool
            False
        ]


listTests : Test
listTests =
    describe "List equality"
        [ Test.fuzz2
            (Fuzz.intRange 100 2000)
            (Fuzz.intRange 100 2000)
            "Simple comparison"
          <|
            \size1 size2 ->
                evalExpect
                    (withInt "size1" size1 <|
                        withInt "size2" size2 <|
                            "List.range 0 size1 == List.range 0 size2"
                    )
                    Bool
                    (List.range 0 size1 == List.range 0 size2)
        ]


diffTests : Test
diffTests =
    describe "ADT equality"
        [ evalTest "As eq"
            """(A "a" == A "a")"""
            Bool
            (A "a" == A "a")
        , evalTest "Bs eq"
            """(B [ 1 ] == B [ 1 ])"""
            Bool
            (B [ 1 ] == B [ 1 ])
        , evalTest "A left neq"
            """(A "a" /= B [ 1 ])"""
            Bool
            (A "a" /= B [ 1 ])
        , evalTest "A right neq"
            """(B [ 1 ] /= A "a")"""
            Bool
            (B [ 1 ] /= A "a")
        ]


recordTests : Test
recordTests =
    describe "Record equality"
        [ evalTest "empty same"
            """({} == {})"""
            Bool
            ({} == {})
        , evalTest "ctor same"
            """({ field = Just 3 } == { field = Just 3 })"""
            Bool
            ({ field = Just 3 } == { field = Just 3 })
        , evalTest "ctor same, special case"
            """({ ctor = Just 3 } == { ctor = Just 3 })"""
            Bool
            ({ ctor = Just 3 } == { ctor = Just 3 })
        , evalTest "ctor diff"
            """({ field = Just 3 } /= { field = Nothing })"""
            Bool
            ({ field = Just 3 } /= { field = Nothing })
        , evalTest "ctor diff, special case"
            """({ ctor = Just 3 } /= { ctor = Nothing })"""
            Bool
            ({ ctor = Just 3 } /= { ctor = Nothing })
        ]


boolComparisonTests : Test
boolComparisonTests =
    describe "Bool comparison"
        [ evalTest "compare True False"
            "compare True False"
            identity
        <|
            Custom { moduleName = [ "Basics" ], name = "GT" } []
        , evalTest "compare False True"
            "compare False True"
            identity
        <|
            Custom { moduleName = [ "Basics" ], name = "LT" } []
        , evalTest "compare True True"
            "compare True True"
            identity
        <|
            Custom { moduleName = [ "Basics" ], name = "EQ" } []
        ]
