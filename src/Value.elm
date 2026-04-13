module Value exposing (eqValue, fromOrder, gtValue, ltValue, mkPartiallyApplied, nameError, nothingValue, toArray, toExpression, toOrder, toString, todo, typeError, unsupported)

import Array exposing (Array)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import FastDict as Dict
import Syntax exposing (fakeNode)
import Types exposing (Env, EvalErrorData, EvalErrorKind(..), Implementation(..), JsonVal(..), Value(..))


typeError : Env -> String -> EvalErrorData
typeError env msg =
    error env (TypeError msg)


nameError : Env -> String -> EvalErrorData
nameError env msg =
    error env (NameError msg)


unsupported : Env -> String -> EvalErrorData
unsupported env msg =
    error env (Unsupported msg)


todo : Env -> String -> EvalErrorData
todo env msg =
    error env (Todo msg)


error : Env -> EvalErrorKind -> EvalErrorData
error env msg =
    { currentModule = env.currentModule
    , callStack = env.callStack
    , error = msg
    }


{-| Construct a PartiallyApplied with cached arity.
-}
mkPartiallyApplied : Env -> List Value -> List (Node Pattern) -> Maybe QualifiedNameRef -> Implementation -> Value
mkPartiallyApplied env args patterns maybeName impl =
    PartiallyApplied env args patterns maybeName impl (List.length patterns)


toExpression : Value -> Node Expression
toExpression value =
    fakeNode <|
        case value of
            String s ->
                Expression.Literal s

            Int i ->
                Expression.Integer i

            Float f ->
                Expression.Floatable f

            Char c ->
                Expression.CharLiteral c

            Bool b ->
                Expression.FunctionOrValue [] (boolToString b)

            Unit ->
                Expression.UnitExpr

            Tuple l r ->
                Expression.TupledExpression
                    [ toExpression l
                    , toExpression r
                    ]

            Triple l m r ->
                Expression.TupledExpression
                    [ toExpression l
                    , toExpression m
                    , toExpression r
                    ]

            Record fields ->
                fields
                    |> Dict.toList
                    |> List.map
                        (\( fieldName, fieldValue ) ->
                            fakeNode ( fakeNode fieldName, toExpression fieldValue )
                        )
                    |> Expression.RecordExpr

            List list ->
                list
                    |> List.map toExpression
                    |> Expression.ListExpr

            Custom name args ->
                case toArray value of
                    Just array ->
                        arrayToExpression "Array" array

                    Nothing ->
                        (fakeNode (Expression.FunctionOrValue name.moduleName name.name)
                            :: List.map toExpression args
                        )
                            |> Expression.Application

            JsArray array ->
                arrayToExpression "JsArray" (Array.toList array)

            JsonValue _ ->
                -- Json.Encode.Value is opaque; represent as a placeholder
                Expression.FunctionOrValue [ "Json", "Encode" ] "value"

            JsonDecoderValue _ ->
                Expression.FunctionOrValue [ "Json", "Decode" ] "decoder"

            RegexValue _ ->
                Expression.FunctionOrValue [ "Regex" ] "regex"

            BytesValue _ ->
                Expression.FunctionOrValue [ "Bytes" ] "bytes"

            PartiallyApplied _ [] _ (Just qualifiedName) _ _ ->
                Expression.FunctionOrValue qualifiedName.moduleName qualifiedName.name

            PartiallyApplied _ args _ (Just qualifiedName) _ _ ->
                (fakeNode
                    (Expression.FunctionOrValue
                        qualifiedName.moduleName
                        qualifiedName.name
                    )
                    :: List.map toExpression args
                )
                    |> Expression.Application

            PartiallyApplied _ [] patterns Nothing implementation _ ->
                case implementation of
                    AstImpl expr ->
                        Expression.LambdaExpression
                            { args = patterns
                            , expression = expr
                            }

                    KernelImpl moduleName name _ ->
                        Expression.FunctionOrValue moduleName name

                    RExprImpl _ ->
                        -- Resolved-IR closures can't be round-tripped to
                        -- elm-syntax — they carry De Bruijn indexed bodies
                        -- with no parameter names. Display as an opaque
                        -- placeholder. Only reachable via debug-print code
                        -- paths that shouldn't mix with the new evaluator.
                        Expression.FunctionOrValue [] "<resolved-closure>"

            PartiallyApplied _ args patterns Nothing implementation _ ->
                case implementation of
                    AstImpl expr ->
                        (fakeNode
                            (Expression.LambdaExpression
                                { args = patterns
                                , expression = expr
                                }
                            )
                            :: List.map toExpression args
                        )
                            |> Expression.Application

                    KernelImpl moduleName name _ ->
                        (fakeNode (Expression.FunctionOrValue moduleName name)
                            :: List.map toExpression args
                        )
                            |> Expression.Application

                    RExprImpl _ ->
                        Expression.FunctionOrValue [] "<resolved-closure>"


arrayToExpression : String -> List Value -> Expression
arrayToExpression name array =
    Expression.Application
        [ Expression.FunctionOrValue
            [ name ]
            "fromList"
            |> fakeNode
        , array
            |> List
            |> toExpression
        ]


toArray : Value -> Maybe (List Value)
toArray value =
    case value of
        Custom name [ _, _, JsArray tree, JsArray tailArray ] ->
            case ( name.moduleName, name.name ) of
                ( [ "Array" ], "Array_elm_builtin" ) ->
                    let
                        treeToArray : Array Value -> List Value
                        treeToArray arr =
                            List.concatMap nodeToList (Array.toList arr)

                        nodeToList : Value -> List Value
                        nodeToList node =
                            case node of
                                Custom qualifiedName [ JsArray arr ] ->
                                    case qualifiedName.name of
                                        "SubTree" ->
                                            treeToArray arr

                                        "Leaf" ->
                                            Array.toList arr

                                        _ ->
                                            []

                                _ ->
                                    []
                    in
                    Just (treeToArray tree ++ Array.toList tailArray)

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Try to interpret a Value as a Dict (RBNode\_elm\_builtin/RBEmpty\_elm\_builtin tree).
Returns Just a sorted list of (key, value) pairs, or Nothing if not a Dict.
-}
toDict : Value -> Maybe (List ( Value, Value ))
toDict value =
    case value of
        Custom { name } _ ->
            if name == "RBNode_elm_builtin" || name == "RBEmpty_elm_builtin" then
                Just (toDictHelp value [])

            else
                Nothing

        _ ->
            Nothing


toDictHelp : Value -> List ( Value, Value ) -> List ( Value, Value )
toDictHelp value acc =
    case value of
        Custom { name } args ->
            case name of
                "RBNode_elm_builtin" ->
                    case args of
                        [ _, key, val, left, right ] ->
                            toDictHelp left (( key, val ) :: toDictHelp right acc)

                        _ ->
                            acc

                "RBEmpty_elm_builtin" ->
                    acc

                _ ->
                    acc

        _ ->
            acc


{-| Try to interpret a Value as a Set (Set\_elm\_builtin wrapping a Dict).
Returns Just a sorted list of elements, or Nothing if not a Set.
-}
toSet : Value -> Maybe (List Value)
toSet value =
    case value of
        Custom { name } [ innerDict ] ->
            if name == "Set_elm_builtin" then
                toDict innerDict
                    |> Maybe.map (List.map Tuple.first)

            else
                Nothing

        _ ->
            Nothing


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


toString : Value -> String
toString value =
    case value of
        String s ->
            "\"" ++ escapeString s ++ "\""

        Int i ->
            String.fromInt i

        Float f ->
            String.fromFloat f

        Char c ->
            "'" ++ escapeChar c ++ "'"

        Bool True ->
            "True"

        Bool False ->
            "False"

        Unit ->
            "()"

        Tuple l r ->
            "(" ++ toString l ++ "," ++ toString r ++ ")"

        Triple l m r ->
            "(" ++ toString l ++ "," ++ toString m ++ "," ++ toString r ++ ")"

        Record fields ->
            case Dict.toList fields of
                [] ->
                    "{}"

                pairs ->
                    "{ " ++ String.join ", " (List.map (\( k, v ) -> k ++ " = " ++ toString v) pairs) ++ " }"

        List items ->
            "[" ++ String.join "," (List.map toString items) ++ "]"

        Custom name args ->
            case toArray value of
                Just array ->
                    "Array.fromList [" ++ String.join "," (List.map toString array) ++ "]"

                Nothing ->
                    case toDict value of
                        Just pairs ->
                            "Dict.fromList [" ++ String.join "," (List.map (\( k, v ) -> "(" ++ toString k ++ "," ++ toString v ++ ")") pairs) ++ "]"

                        Nothing ->
                            case toSet value of
                                Just items ->
                                    "Set.fromList [" ++ String.join "," (List.map toString items) ++ "]"

                                Nothing ->
                                    case args of
                                        [] ->
                                            name.name

                                        _ ->
                                            name.name ++ " " ++ String.join " " (List.map toStringParens args)

        JsArray arr ->
            "[" ++ String.join "," (List.map toString (Array.toList arr)) ++ "]"

        JsonValue json ->
            jsonValToString json

        JsonDecoderValue _ ->
            "<decoder>"

        RegexValue _ ->
            "<regex>"

        BytesValue arr ->
            "<" ++ String.fromInt (Array.length arr) ++ " bytes>"

        PartiallyApplied _ _ _ _ _ _ ->
            "<function>"


toStringParens : Value -> String
toStringParens value =
    case value of
        Custom _ (_ :: _) ->
            "(" ++ toString value ++ ")"

        Int i ->
            if i < 0 then
                "(" ++ String.fromInt i ++ ")"

            else
                String.fromInt i

        Float f ->
            if f < 0 then
                "(" ++ String.fromFloat f ++ ")"

            else
                String.fromFloat f

        _ ->
            toString value


escapeString : String -> String
escapeString s =
    s
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\t" "\\t"


escapeChar : Char -> String
escapeChar c =
    case c of
        '\\' ->
            "\\\\"

        '\'' ->
            "\\'"

        _ ->
            String.fromChar c


{-| Pre-computed Order values. Avoids allocating QualifiedNameRef + Custom
on every comparison result (millions of times in fuzz tests).
-}
ltValue : Value
ltValue =
    Custom { moduleName = [ "Basics" ], name = "LT" } []


eqValue : Value
eqValue =
    Custom { moduleName = [ "Basics" ], name = "EQ" } []


gtValue : Value
gtValue =
    Custom { moduleName = [ "Basics" ], name = "GT" } []


{-| Pre-computed Nothing value.
-}
nothingValue : Value
nothingValue =
    Custom { moduleName = [ "Maybe" ], name = "Nothing" } []


fromOrder : Order -> Value
fromOrder order =
    case order of
        LT ->
            ltValue

        EQ ->
            eqValue

        GT ->
            gtValue


jsonValToString : JsonVal -> String
jsonValToString json =
    case json of
        JsonNull ->
            "<null>"

        JsonBool b ->
            if b then
                "<true>"

            else
                "<false>"

        JsonInt i ->
            "<" ++ String.fromInt i ++ ">"

        JsonFloat f ->
            "<" ++ String.fromFloat f ++ ">"

        JsonString s ->
            "<\"" ++ s ++ "\">"

        JsonArray items ->
            "<[" ++ String.join "," (List.map jsonValToString items) ++ "]>"

        JsonObject pairs ->
            "<{" ++ String.join "," (List.map (\( k, v ) -> k ++ ":" ++ jsonValToString v) pairs) ++ "}>"


toOrder : Value -> Maybe Order
toOrder value =
    case value of
        Custom { moduleName, name } [] ->
            case ( moduleName, name ) of
                ( [ "Basics" ], "LT" ) ->
                    Just LT

                ( [ "Basics" ], "EQ" ) ->
                    Just EQ

                ( [ "Basics" ], "GT" ) ->
                    Just GT

                _ ->
                    Nothing

        _ ->
            Nothing
