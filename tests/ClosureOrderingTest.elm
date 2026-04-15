module ClosureOrderingTest exposing (suite)

{-| Regression tests targeting `Test.fuzz2`'s exact shape. The real
test-runner path: `extendResolvedWithFiles` + `evalWithResolvedIRFromFilesAndIntercepts`
with a `>>` (composeR) chain that partially applies a lambda with a
non-trivial (tuple-destructure) parameter.

**Known failing (task #57)**: `actualFuzz2Reproducer` and
`inlineCompositionWithBasicsComposeR` exercise the `Basics.composeR`
path — `fuzz2Like = (\f (a, b) -> f a b) >> identity`. The same shape
using a **user-defined** `composeR` (see `explicitComposeR`) passes,
which narrows the bug to how the resolved-IR evaluator falls back to
the old evaluator for `Basics.composeR` (a package function whose
body isn't in `env.resolvedBodies`). The fallback synthesizes an AST
application calling `Basics.composeR` through `delegateByName` →
`Eval.Expression.evalExpression`, which then hits "Could not match
lambda patterns" somewhere inside composeR's body evaluation.
Skipping until the fallback path is fixed.

-}

import Elm.Parser
import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import Expect
import FastDict
import Test exposing (Test, describe, test)
import Types exposing (EvalResult(..), Value(..))


suite : Test
suite =
    describe "Test.fuzz2-shape partial-then-exact tuple destructure"
        [ composeRWithKernelCallback
        , actualFuzz2Reproducer
        , explicitComposeR
        , inlineCompositionWithBasicsComposeR
        , orderExtraExplicitSuiteShape
        , orderExtraExplicitViaExtendResolved
        , orderExtraExplicitViaOldEvalPath
        , orderExtraExplicitViaBridgeNoWrapper
        , orderExtraExplicitViaBridgeExtendResolvedNoWrapper
        , orderExtraExplicitViaBridgeWrapperOnly
        , coreExtraStyleOrderTestsRepro
        ]


{-| Pinned regression for the remaining `OrderTests` failures (task
#57): `Order.Extra.explicit [ Clubs, Hearts, Diamonds, Spades ] Hearts
Spades` should produce `LT`. Via the OLD evaluator path (without
`extendResolvedWithFiles`) it does; via the resolved-IR path it returns
something that doesn't equal `LT`, causing 5 `OrderTests` failures on
the core-extra suite even though all 4 previously-broken test files
(SetTests, FloatTests, ListTests, RemoveAccentsTest) now pass.

Likely a let-rec slot-assignment divergence: `explicit` uses mutually
recursive `scanForEither` / `scanForX` / `scanForY` helper functions
defined inside a `let` block, and the resolver's slot indexing may
diverge from the runtime's locals stack when all three helpers are
bound together.

Skipping until the root cause is identified.
-}
orderExtraExplicitSuiteShape : Test
orderExtraExplicitSuiteShape =
    test "Order.Extra.explicit with Custom enum values returns correct Order" <|
        \_ ->
            case
                Eval.Module.buildProjectEnv
                    [ """module Foo exposing (result)


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


explicit : List a -> a -> a -> Order
explicit elements x y =
    let
        scanForEither items =
            case items of
                z :: zs ->
                    if z == x then
                        scanForY zs

                    else if z == y then
                        scanForX zs

                    else
                        scanForEither zs

                [] ->
                    EQ

        scanForX items =
            case items of
                z :: zs ->
                    if z == x then
                        GT

                    else
                        scanForX zs

                [] ->
                    LT

        scanForY items =
            case items of
                z :: zs ->
                    if z == y then
                        LT

                    else
                        scanForY zs

                [] ->
                    GT
    in
    if x == y then
        EQ

    else
        scanForEither elements


suiteOrdering : Suite -> Suite -> Order
suiteOrdering =
    explicit [ Clubs, Hearts, Diamonds, Spades ]


result : Order
result =
    suiteOrdering Hearts Spades
"""
                    ]
            of
                Ok projectEnv ->
                    case Eval.Module.evalWithResolvedIR projectEnv "Foo.result" of
                        Ok v ->
                            -- LT is `Custom { moduleName = ["Basics"], name = "LT" } []`
                            v
                                |> Expect.equal (Custom { moduleName = [ "Basics" ], name = "LT" } [])

                        Err e ->
                            Expect.fail ("Eval error: " ++ Debug.toString e)

                Err e ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)


{-| Same let-rec shape as `orderExtraExplicitSuiteShape`, but driven through
`extendResolvedWithFiles` + `evalWithResolvedIRFromFilesAndIntercepts` —
the test-runner's actual evaluation path. This is the minimal repro for
the 5 `OrderTests` failures in the core-extra bench.

Fixed by `Eval.Resolver.resolveFunctionOrValue`'s unqualified-ctor
fallback now producing `RCtor { moduleName = ctx.currentModule, ... }`
instead of `RCtor { moduleName = [], ... }`. Before the fix, the
`elements` list inside `Foo.suiteOrdering`'s body (unqualified `Hearts`,
`Spades`, etc.) produced `Custom { moduleName = [], ... }` while the
wrapper-supplied entry args (`Foo.Hearts`, qualified) produced
`Custom { moduleName = [ "Foo" ], ... }`. The `z == x` check inside
`scanForEither`'s let-rec body compared values with mismatched
`moduleName` fields and returned `False` every time, so the scan fell
through to the `[] -> EQ` base case instead of finding the match.
-}
orderExtraExplicitViaExtendResolved : Test
orderExtraExplicitViaExtendResolved =
    test "Order.Extra.explicit with mutual let-rec via extendResolvedWithFiles returns LT" <|
        run
            """module Foo exposing (..)


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


explicit : List a -> a -> a -> Order
explicit elements x y =
    let
        scanForEither items =
            case items of
                z :: zs ->
                    if z == x then
                        scanForY zs

                    else if z == y then
                        scanForX zs

                    else
                        scanForEither zs

                [] ->
                    EQ

        scanForX items =
            case items of
                z :: zs ->
                    if z == x then
                        GT

                    else
                        scanForX zs

                [] ->
                    LT

        scanForY items =
            case items of
                z :: zs ->
                    if z == y then
                        LT

                    else
                        scanForY zs

                [] ->
                    GT
    in
    if x == y then
        EQ

    else
        scanForEither elements


suiteOrdering : Suite -> Suite -> Order
suiteOrdering =
    explicit [ Clubs, Hearts, Diamonds, Spades ]
"""
            """module Wrapper exposing (result)

import Foo


result : Order
result =
    Foo.suiteOrdering Foo.Hearts Foo.Spades
"""
            (Custom { moduleName = [ "Basics" ], name = "LT" } [])


{-| Closest-to-real-test-runner repro of the 5 core-extra `OrderTests`
failures. Mirrors the shape where `Order.Extra` lives in a separate
module from the test file, and the test file defines its own custom
type `Suite` then calls `Order.Extra.explicit [ Clubs, ... ]` with
unqualified ctor references. Uses the `run` helper which drives
`extendResolvedWithFiles` + `evalWithResolvedIRFromFilesAndIntercepts`.
-}
coreExtraStyleOrderTestsRepro : Test
coreExtraStyleOrderTestsRepro =
    test "core-extra OrderTests repro: Order.Extra.explicit with separate Suite module returns LT" <|
        run
            """module Order.Extra exposing (explicit)


explicit : List a -> a -> a -> Order
explicit elements x y =
    let
        scanForEither items =
            case items of
                z :: zs ->
                    if z == x then
                        scanForY zs

                    else if z == y then
                        scanForX zs

                    else
                        scanForEither zs

                [] ->
                    EQ

        scanForX items =
            case items of
                z :: zs ->
                    if z == x then
                        GT

                    else
                        scanForX zs

                [] ->
                    LT

        scanForY items =
            case items of
                z :: zs ->
                    if z == y then
                        LT

                    else
                        scanForY zs

                [] ->
                    GT
    in
    if x == y then
        EQ

    else
        scanForEither elements
"""
            """module Wrapper exposing (result)

import Order.Extra


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


suiteOrdering : Suite -> Suite -> Order
suiteOrdering =
    Order.Extra.explicit [ Clubs, Hearts, Diamonds, Spades ]


result : Order
result =
    suiteOrdering Hearts Spades
"""
            (Custom { moduleName = [ "Basics" ], name = "LT" } [])


{-| Isolation test C: include `result` in Foo, re-register Foo via
`extendResolvedWithFiles`, call bridge entry with qualified `Foo.result`.
If this passes, `extendResolvedWithFiles` is not the culprit.
-}
orderExtraExplicitViaBridgeExtendResolvedNoWrapper : Test
orderExtraExplicitViaBridgeExtendResolvedNoWrapper =
    test "Foo.result via extendResolvedWithFiles + bridge, no wrapper, returns LT" <|
        \_ ->
            let
                userSource : String
                userSource =
                    """module Foo exposing (..)


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


explicit : List a -> a -> a -> Order
explicit elements x y =
    let
        scanForEither items =
            case items of
                z :: zs ->
                    if z == x then
                        scanForY zs

                    else if z == y then
                        scanForX zs

                    else
                        scanForEither zs

                [] ->
                    EQ

        scanForX items =
            case items of
                z :: zs ->
                    if z == x then
                        GT

                    else
                        scanForX zs

                [] ->
                    LT

        scanForY items =
            case items of
                z :: zs ->
                    if z == y then
                        LT

                    else
                        scanForY zs

                [] ->
                    GT
    in
    if x == y then
        EQ

    else
        scanForEither elements


suiteOrdering : Suite -> Suite -> Order
suiteOrdering =
    explicit [ Clubs, Hearts, Diamonds, Spades ]


result : Order
result =
    suiteOrdering Hearts Spades
"""
            in
            case Eval.Module.buildProjectEnv [ userSource ] of
                Err e ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                Ok projectEnv ->
                    case Elm.Parser.parseToFile userSource of
                        Err _ ->
                            Expect.fail "parse failed"

                        Ok userFile ->
                            let
                                extendedEnv : Eval.Module.ProjectEnv
                                extendedEnv =
                                    Eval.Module.extendResolvedWithFiles [ userFile ] projectEnv
                            in
                            case
                                Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                                    extendedEnv
                                    []
                                    FastDict.empty
                                    FastDict.empty
                                    (FunctionOrValue [ "Foo" ] "result")
                            of
                                EvOk value ->
                                    Expect.equal
                                        (Custom { moduleName = [ "Basics" ], name = "LT" } [])
                                        value

                                EvErr e ->
                                    Expect.fail ("eval error: " ++ Debug.toString e)

                                _ ->
                                    Expect.fail "unexpected EvalResult variant"


{-| Isolation test D: pass Foo (with result) as initial source, NO
extendResolvedWithFiles call, evaluate via bridge with wrapper file
added. Probes whether the wrapper file alone triggers the bug.
-}
orderExtraExplicitViaBridgeWrapperOnly : Test
orderExtraExplicitViaBridgeWrapperOnly =
    test "Wrapper.result via bridge with wrapper as additional file returns LT" <|
        \_ ->
            let
                userSource : String
                userSource =
                    """module Foo exposing (..)


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


explicit : List a -> a -> a -> Order
explicit elements x y =
    let
        scanForEither items =
            case items of
                z :: zs ->
                    if z == x then
                        scanForY zs

                    else if z == y then
                        scanForX zs

                    else
                        scanForEither zs

                [] ->
                    EQ

        scanForX items =
            case items of
                z :: zs ->
                    if z == x then
                        GT

                    else
                        scanForX zs

                [] ->
                    LT

        scanForY items =
            case items of
                z :: zs ->
                    if z == y then
                        LT

                    else
                        scanForY zs

                [] ->
                    GT
    in
    if x == y then
        EQ

    else
        scanForEither elements


suiteOrdering : Suite -> Suite -> Order
suiteOrdering =
    explicit [ Clubs, Hearts, Diamonds, Spades ]
"""

                wrapperSource : String
                wrapperSource =
                    """module Wrapper exposing (result)

import Foo


result : Order
result =
    Foo.suiteOrdering Foo.Hearts Foo.Spades
"""
            in
            case Eval.Module.buildProjectEnv [ userSource ] of
                Err e ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                Ok projectEnv ->
                    case Elm.Parser.parseToFile wrapperSource of
                        Err _ ->
                            Expect.fail "parse failed"

                        Ok wrapperFile ->
                            case
                                Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                                    projectEnv
                                    [ wrapperFile ]
                                    FastDict.empty
                                    FastDict.empty
                                    (FunctionOrValue [] "result")
                            of
                                EvOk value ->
                                    Expect.equal
                                        (Custom { moduleName = [ "Basics" ], name = "LT" } [])
                                        value

                                EvErr e ->
                                    Expect.fail ("eval error: " ++ Debug.toString e)

                                _ ->
                                    Expect.fail "unexpected EvalResult variant"


{-| Diagnostic: same shape as `orderExtraExplicitViaExtendResolved`, but
we define `result` inside `Foo` (no wrapper module) and call
`evalWithResolvedIRFromFilesAndIntercepts` with no additional files and
the entry expression `FunctionOrValue ["Foo"] "result"`.

This isolates whether the bug is in:

  - The bridge entry point itself (`evalWithResolvedIRFromFilesAndIntercepts`)
    — in which case this test will also fail; OR
  - Something specific about the wrapper-module indirection that
    `orderExtraExplicitViaExtendResolved` introduces — in which case
    this test will pass.

-}
orderExtraExplicitViaBridgeNoWrapper : Test
orderExtraExplicitViaBridgeNoWrapper =
    test "Foo.result via evalWithResolvedIRFromFilesAndIntercepts (no wrapper) returns LT" <|
        \_ ->
            let
                userSource : String
                userSource =
                    """module Foo exposing (..)


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


explicit : List a -> a -> a -> Order
explicit elements x y =
    let
        scanForEither items =
            case items of
                z :: zs ->
                    if z == x then
                        scanForY zs

                    else if z == y then
                        scanForX zs

                    else
                        scanForEither zs

                [] ->
                    EQ

        scanForX items =
            case items of
                z :: zs ->
                    if z == x then
                        GT

                    else
                        scanForX zs

                [] ->
                    LT

        scanForY items =
            case items of
                z :: zs ->
                    if z == y then
                        LT

                    else
                        scanForY zs

                [] ->
                    GT
    in
    if x == y then
        EQ

    else
        scanForEither elements


suiteOrdering : Suite -> Suite -> Order
suiteOrdering =
    explicit [ Clubs, Hearts, Diamonds, Spades ]


result : Order
result =
    suiteOrdering Hearts Spades
"""
            in
            case Eval.Module.buildProjectEnv [ userSource ] of
                Err e ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                Ok projectEnv ->
                    case
                        Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                            projectEnv
                            []
                            FastDict.empty
                            FastDict.empty
                            (FunctionOrValue [ "Foo" ] "result")
                    of
                        EvOk value ->
                            Expect.equal
                                (Custom { moduleName = [ "Basics" ], name = "LT" } [])
                                value

                        EvErr e ->
                            Expect.fail ("eval error: " ++ Debug.toString e)

                        _ ->
                            Expect.fail "unexpected EvalResult variant"


{-| Diagnostic companion to `orderExtraExplicitViaExtendResolved`: runs
the exact same shape through the OLD-eval path (`evalWithEnvFromFiles`)
instead of the resolved-IR bridge. If this passes, OLD eval handles
mutual let-rec correctly and the bug is isolated to the resolved-IR
path (`extendResolvedWithFiles` + `evalWithResolvedIRFromFilesAndIntercepts`).
If it also fails, the bug predates the resolved-IR work entirely.
-}
orderExtraExplicitViaOldEvalPath : Test
orderExtraExplicitViaOldEvalPath =
    test "Order.Extra.explicit shape via evalWithEnvFromFiles (OLD eval) returns LT" <|
        \_ ->
            let
                userSource : String
                userSource =
                    """module Foo exposing (..)


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


explicit : List a -> a -> a -> Order
explicit elements x y =
    let
        scanForEither items =
            case items of
                z :: zs ->
                    if z == x then
                        scanForY zs

                    else if z == y then
                        scanForX zs

                    else
                        scanForEither zs

                [] ->
                    EQ

        scanForX items =
            case items of
                z :: zs ->
                    if z == x then
                        GT

                    else
                        scanForX zs

                [] ->
                    LT

        scanForY items =
            case items of
                z :: zs ->
                    if z == y then
                        LT

                    else
                        scanForY zs

                [] ->
                    GT
    in
    if x == y then
        EQ

    else
        scanForEither elements


suiteOrdering : Suite -> Suite -> Order
suiteOrdering =
    explicit [ Clubs, Hearts, Diamonds, Spades ]
"""

                wrapperSource : String
                wrapperSource =
                    """module Wrapper exposing (result)

import Foo


result : Order
result =
    Foo.suiteOrdering Foo.Hearts Foo.Spades
"""
            in
            case Eval.Module.buildProjectEnv [ userSource ] of
                Err e ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                Ok projectEnv ->
                    case Elm.Parser.parseToFile wrapperSource of
                        Err _ ->
                            Expect.fail "parse failed"

                        Ok wrapperFile ->
                            case
                                Eval.Module.evalWithEnvFromFiles
                                    projectEnv
                                    [ wrapperFile ]
                                    (FunctionOrValue [] "result")
                            of
                                Ok value ->
                                    Expect.equal
                                        (Custom { moduleName = [ "Basics" ], name = "LT" } [])
                                        value

                                Err e ->
                                    Expect.fail ("eval error: " ++ Debug.toString e)


inlineCompositionWithBasicsComposeR : Test
inlineCompositionWithBasicsComposeR =
    test "(\\f ( a, b ) -> f a b) >> identity applied to plus (3,4) — inline, no intermediate binding" <|
        run
            """module Foo exposing (plus)


plus : Int -> Int -> Int
plus x y =
    x + y
"""
            """module Wrapper exposing (result)

import Foo


result : Int
result =
    ((\\f ( a, b ) -> f a b) >> identity) Foo.plus ( 3, 4 )
"""
            (Int 7)


run :
    String
    -> String
    -> Value
    -> (() -> Expect.Expectation)
run userSource wrapperSource expectedValue =
    \_ ->
        case Eval.Module.buildProjectEnv [ userSource ] of
            Err e ->
                Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

            Ok projectEnv ->
                case
                    ( Elm.Parser.parseToFile userSource
                    , Elm.Parser.parseToFile wrapperSource
                    )
                of
                    ( Ok userFile, Ok wrapperFile ) ->
                        let
                            extendedEnv : Eval.Module.ProjectEnv
                            extendedEnv =
                                Eval.Module.extendResolvedWithFiles [ userFile ] projectEnv
                        in
                        case
                            Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                                extendedEnv
                                [ wrapperFile ]
                                FastDict.empty
                                FastDict.empty
                                (FunctionOrValue [] "result")
                        of
                            EvOk value ->
                                Expect.equal expectedValue value

                            EvErr e ->
                                Expect.fail
                                    ("Eval error: "
                                        ++ Debug.toString e.error
                                        ++ " [currentModule: "
                                        ++ String.join "." e.currentModule
                                        ++ "]"
                                    )

                            _ ->
                                Expect.fail "Unexpected EvalResult variant"

                    _ ->
                        Expect.fail "Failed to parse sources"


composeRWithKernelCallback : Test
composeRWithKernelCallback =
    test "List.map (pair plus) [...] with composeR above" <|
        run
            """module Foo exposing (pair, plus)

pair : (Int -> Int -> Int) -> ( Int, Int ) -> Int
pair =
    \\f ( a, b ) -> f a b


plus : Int -> Int -> Int
plus x y =
    x + y
"""
            """module Wrapper exposing (result)

import Foo


identity2 : a -> a
identity2 x =
    x


result : List Int
result =
    List.map (Foo.pair Foo.plus >> identity2) [ ( 3, 4 ), ( 5, 6 ) ]
"""
            (List [ Int 7, Int 11 ])


actualFuzz2Reproducer : Test
actualFuzz2Reproducer =
    test "minimal Test.fuzz2 shape: (\\f ( a, b ) -> f a b) >> wrap, applied to plus and (3,4)" <|
        run
            """module Foo exposing (fuzz2Like, plus)

fuzz2Like : ((Int -> Int -> Int) -> ( Int, Int ) -> Int)
fuzz2Like =
    (\\f ( a, b ) -> f a b) >> identity


identity : a -> a
identity x =
    x


plus : Int -> Int -> Int
plus x y =
    x + y
"""
            """module Wrapper exposing (result)

import Foo


result : Int
result =
    Foo.fuzz2Like Foo.plus ( 3, 4 )
"""
            (Int 7)


explicitComposeR : Test
explicitComposeR =
    test "explicit composeR (no >>) with tuple destructure" <|
        run
            """module Foo exposing (fuzz2Like, plus)

composeR : (a -> b) -> (b -> c) -> a -> c
composeR f g x =
    g (f x)


innerLambda : (Int -> Int -> Int) -> ( Int, Int ) -> Int
innerLambda =
    \\f ( a, b ) -> f a b


identity : a -> a
identity x =
    x


fuzz2Like : (Int -> Int -> Int) -> ( Int, Int ) -> Int
fuzz2Like =
    composeR innerLambda identity


plus : Int -> Int -> Int
plus x y =
    x + y
"""
            """module Wrapper exposing (result)

import Foo


result : Int
result =
    Foo.fuzz2Like Foo.plus ( 3, 4 )
"""
            (Int 7)


