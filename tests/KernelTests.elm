module KernelTests exposing (suite)

import Array
import Core
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Eval.Expression
import Expect
import FastDict as Dict
import Kernel
import Rope exposing (Rope)
import Syntax
import Test exposing (Test, describe, test)
import Types exposing (Eval, Value)


suite : Test
suite =
    describe "Kernel"
        [ kernelFunctions
            |> List.map testDefined
            |> describe "Check that all Kernel functions have been defined"
        , kernelIdRegistry
        ]


{-| Step 1 of KernelImpl defunctionalization (see Kernel.kernelIdsByName).
The id-by-name registry is the substrate later steps build on, so guard
its invariants:

  - every kernel in `Kernel.functions` has an ID via `lookupKernelId`
  - IDs are unique
  - IDs are dense (`[0, kernelCount)`) so an `Array` indexed by ID covers
    every entry without holes

If any of these break, defunctionalized dispatch will silently miss
or alias kernels.

-}
kernelIdRegistry : Test
kernelIdRegistry =
    let
        registry : Dict.Dict ModuleName (Dict.Dict String ( Int, List Value -> Eval Value ))
        registry =
            Kernel.functions Eval.Expression.evalFunction

        allKernels : List ( ModuleName, String )
        allKernels =
            registry
                |> Dict.toList
                |> List.concatMap
                    (\( moduleName, moduleKernels ) ->
                        Dict.keys moduleKernels |> List.map (\name -> ( moduleName, name ))
                    )

        allIds : List Kernel.KernelId
        allIds =
            allKernels |> List.filterMap (\( m, n ) -> Kernel.lookupKernelId m n)
    in
    describe "kernelIdsByName"
        [ test "every kernel has an assigned ID" <|
            \_ ->
                allKernels
                    |> List.filter (\( m, n ) -> Kernel.lookupKernelId m n == Nothing)
                    |> Expect.equal []
        , test "ID count matches registry count" <|
            \_ ->
                Expect.equal Kernel.kernelCount (List.length allKernels)
        , test "IDs are unique" <|
            \_ ->
                Expect.equal (List.length (uniq allIds)) (List.length allIds)
        , test "IDs are dense in [0, kernelCount)" <|
            \_ ->
                Expect.equal (List.sort allIds) (List.range 0 (Kernel.kernelCount - 1))
        , test "kernelArray length matches kernelCount" <|
            \_ ->
                Expect.equal Kernel.kernelCount
                    (Array.length (Kernel.kernelArray Eval.Expression.evalFunction))
        , test "kernelArray slot count matches Dict-derived registry" <|
            \_ ->
                -- Stronger guard than IDs-are-dense: actually verify the
                -- runtime array slot count, since `kernelArray` uses
                -- the same Dict iteration as `kernelIdsByName`. If the
                -- two ever desync (e.g. a future refactor changes one
                -- iteration order), `Array.get id` would silently
                -- return the wrong function.
                Expect.equal (List.length allKernels)
                    (Array.length (Kernel.kernelArray Eval.Expression.evalFunction))
        ]


{-| Local de-dup helper (Set is overkill; the lists are small).
-}
uniq : List Int -> List Int
uniq xs =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []
        xs


kernelFunctions : List ( ( ModuleName, String ), List String )
kernelFunctions =
    Core.functions
        |> Dict.values
        |> Rope.fromList
        |> Rope.concatMap (\module_ -> Dict.values module_ |> Rope.fromList)
        |> Rope.concatMap
            (\function ->
                let
                    (Node _ name) =
                        function.name
                in
                visitFunctionImplementation function
                    |> Rope.map (\required -> ( required, name ))
            )
        |> Rope.toList
        |> List.foldl
            (\( required, by ) acc ->
                Dict.insert
                    required
                    (by :: Maybe.withDefault [] (Dict.get required acc))
                    acc
            )
            Dict.empty
        |> Dict.toList


visitFunctionImplementation : FunctionImplementation -> Rope ( ModuleName, String )
visitFunctionImplementation { expression } =
    visitExpression expression


visitExpression : Node Expression -> Rope ( ModuleName, String )
visitExpression (Node _ expression) =
    case expression of
        Application children ->
            Rope.fromList children
                |> Rope.concatMap visitExpression

        OperatorApplication _ _ l r ->
            Rope.fromList [ l, r ]
                |> Rope.concatMap visitExpression

        FunctionOrValue ("Elm" :: "Kernel" :: "Scheduler" :: _) _ ->
            Rope.empty

        FunctionOrValue ("Elm" :: "Kernel" :: "Platform" :: _) _ ->
            Rope.empty

        FunctionOrValue ("Elm" :: "Kernel" :: "Process" :: _) _ ->
            Rope.empty

        FunctionOrValue (("Elm" :: "Kernel" :: _) as moduleName) name ->
            Rope.singleton ( moduleName, name )

        IfBlock cond true false ->
            Rope.fromList [ cond, true, false ]
                |> Rope.concatMap visitExpression

        Negation child ->
            visitExpression child

        TupledExpression children ->
            Rope.fromList children
                |> Rope.concatMap visitExpression

        ParenthesizedExpression child ->
            visitExpression child

        LetExpression letBlock ->
            visitLetBlock letBlock

        CaseExpression caseBlock ->
            visitCaseBlock caseBlock

        LambdaExpression lambda ->
            visitLambda lambda

        RecordExpr setters ->
            setters
                |> Rope.fromList
                |> Rope.concatMap visitRecordSetter

        ListExpr children ->
            Rope.fromList children
                |> Rope.concatMap visitExpression

        RecordAccess child _ ->
            visitExpression child

        RecordUpdateExpression _ setters ->
            setters
                |> Rope.fromList
                |> Rope.concatMap visitRecordSetter

        _ ->
            Rope.empty


visitLambda : Lambda -> Rope ( ModuleName, String )
visitLambda { expression } =
    visitExpression expression


visitRecordSetter : Node RecordSetter -> Rope ( ModuleName, String )
visitRecordSetter (Node _ ( _, expression )) =
    visitExpression expression


visitCaseBlock : CaseBlock -> Rope ( ModuleName, String )
visitCaseBlock { expression, cases } =
    cases
        |> Rope.fromList
        |> Rope.concatMap visitCase
        |> Rope.appendTo (visitExpression expression)


visitCase : Case -> Rope ( ModuleName, String )
visitCase ( _, expression ) =
    visitExpression expression


visitLetBlock : LetBlock -> Rope ( ModuleName, String )
visitLetBlock { declarations, expression } =
    declarations
        |> Rope.fromList
        |> Rope.concatMap visitDeclaration
        |> Rope.appendTo (visitExpression expression)


visitDeclaration : Node LetDeclaration -> Rope ( ModuleName, String )
visitDeclaration (Node _ letDeclaration) =
    case letDeclaration of
        LetFunction function ->
            visitFunction function

        LetDestructuring _ child ->
            visitExpression child


visitFunction : Function -> Rope ( ModuleName, String )
visitFunction { declaration } =
    visitFunctionImplementation (Node.value declaration)


testDefined : ( ( ModuleName, String ), List String ) -> Test
testDefined ( ( moduleName, name ), requiredBy ) =
    case Dict.get moduleName (Kernel.functions Eval.Expression.evalFunction) of
        Just kernelModule ->
            if Dict.member name kernelModule then
                let
                    fullName : String
                    fullName =
                        Syntax.qualifiedNameToString
                            { moduleName = moduleName
                            , name = name
                            }
                in
                test fullName <|
                    \_ -> Expect.pass

            else
                trySearchingElmCoded moduleName requiredBy name

        Nothing ->
            trySearchingElmCoded moduleName requiredBy name


trySearchingElmCoded : ModuleName -> List String -> String -> Test
trySearchingElmCoded moduleName requiredBy name =
    let
        fullName : String
        fullName =
            Syntax.qualifiedNameToString
                { moduleName = moduleName
                , name = name
                }
    in
    case Dict.get moduleName Core.functions of
        Nothing ->
            error fullName requiredBy

        Just kernelModule ->
            if Dict.member name kernelModule then
                test fullName <|
                    \_ -> Expect.pass

            else
                error fullName requiredBy


error : String -> List String -> Test
error fullName requiredBy =
    Test.todo
        (fullName
            ++ " is not defined, but it's required by "
            ++ String.join ", " requiredBy
        )
