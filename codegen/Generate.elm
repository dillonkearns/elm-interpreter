module Generate exposing (main)

{-| -}

import Dict
import Elm
import Elm.Annotation as Type
import Elm.Dependency exposing (Dependency)
import Elm.Interface exposing (Exposed, Interface)
import Elm.Parser
import Elm.Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Elm.Syntax.Pattern as Pattern
import Gen.CodeGen.Generate as Generate exposing (Directory(..))
import Gen.Dict
import Gen.Elm.Dependency
import Gen.Elm.Interface
import Gen.Elm.Syntax.Expression
import Gen.Elm.Syntax.Infix
import Gen.Elm.Syntax.ModuleName
import Gen.Elm.Syntax.Pattern
import Gen.FastDict
import Gen.H
import Gen.List
import Gen.Maybe
import Json.Decode exposing (Value)
import List.Extra
import Result.Extra


main : Program Value () ()
main =
    Generate.fromDirectory toFiles


toFiles : Directory -> List Elm.File
toFiles modulesSource =
    let
        allFiles : List String
        allFiles =
            traverseDirectoryForFiles modulesSource

        maybeFiles :
            Result
                String
                (List
                    { moduleName : ModuleName
                    , file : Elm.File
                    , hasOperators : Bool
                    , interface : Interface
                    }
                )
        maybeFiles =
            allFiles
                |> List.filterMap
                    (\file ->
                        case Elm.Parser.parse file of
                            Err _ ->
                                case String.split "\n" file of
                                    [] ->
                                        Just (Err "Empty")

                                    head :: _ ->
                                        Just (Err head)

                            Ok rawFile ->
                                let
                                    selfDependencies : List Dependency
                                    selfDependencies =
                                        []
                                in
                                toFile selfDependencies rawFile
                                    |> Maybe.map Ok
                    )
                |> Result.Extra.combine
                |> Result.map
                    (\files ->
                        files
                            |> List.Extra.gatherEqualsBy .moduleName
                            |> List.map
                                (\( { moduleName } as first, rest ) ->
                                    let
                                        all :
                                            List
                                                { moduleName : ModuleName
                                                , declarations : List Elm.Declaration
                                                , hasOperators : Bool
                                                , interface : Interface
                                                }
                                        all =
                                            first :: rest
                                    in
                                    { moduleName = moduleName
                                    , file =
                                        all
                                            |> List.concatMap .declarations
                                            |> Elm.file moduleName
                                    , hasOperators = List.any .hasOperators all
                                    , interface = List.concatMap .interface all
                                    }
                                )
                    )
    in
    case maybeFiles of
        Err e ->
            [ Elm.file [ "Core" ]
                [ Elm.declaration "somethingWentWrong" (Elm.string e) ]
            ]

        Ok files ->
            let
                functions : Elm.Declaration
                functions =
                    files
                        |> List.map
                            (\{ moduleName } ->
                                Elm.tuple
                                    (Elm.list <| List.map Elm.string <| List.drop 1 moduleName)
                                    (Elm.value
                                        { importFrom = moduleName
                                        , name = "functions"
                                        , annotation = Nothing
                                        }
                                    )
                            )
                        |> Gen.FastDict.fromList
                        |> Elm.withType
                            (Gen.FastDict.annotation_.dict
                                Gen.Elm.Syntax.ModuleName.annotation_.moduleName
                                (Gen.FastDict.annotation_.dict
                                    Type.string
                                    Gen.Elm.Syntax.Expression.annotation_.functionImplementation
                                )
                            )
                        |> Elm.declaration "functions"
                        |> Elm.expose

                operators : Elm.Declaration
                operators =
                    files
                        |> List.filter .hasOperators
                        |> List.map
                            (\{ moduleName } ->
                                Elm.value
                                    { importFrom = moduleName
                                    , name = "operators"
                                    , annotation = Nothing
                                    }
                            )
                        |> Elm.list
                        |> Gen.List.call_.concat
                        |> Gen.FastDict.call_.fromList
                        |> Elm.withType
                            (Gen.FastDict.annotation_.dict
                                Type.string
                                Gen.Elm.Syntax.Pattern.annotation_.qualifiedNameRef
                            )
                        |> Elm.declaration "operators"
                        |> Elm.expose

                dependency : Elm.Declaration
                dependency =
                    Gen.Elm.Dependency.make_.dependency
                        { name = Elm.string "elm/core"
                        , version = Elm.string "1.0.0"
                        , interfaces =
                            files
                                |> List.map
                                    (\{ moduleName, interface } ->
                                        Elm.tuple
                                            (Elm.list <| List.map Elm.string <| List.drop 1 moduleName)
                                            (interfaceToGen interface)
                                    )
                                |> Gen.Dict.fromList
                        }
                        |> Elm.declaration "dependency"
                        |> Elm.expose

                core : Elm.File
                core =
                    [ functions
                    , operators
                    , dependency
                    ]
                        |> Elm.file [ "Core" ]
            in
            core :: List.map .file files


interfaceToGen : Interface -> Elm.Expression
interfaceToGen interface =
    Elm.list (List.map exposedToGen interface)


exposedToGen : Exposed -> Elm.Expression
exposedToGen exposed =
    case exposed of
        Elm.Interface.Function name ->
            Gen.Elm.Interface.make_.function (Elm.string name)

        Elm.Interface.CustomType ( name, ctors ) ->
            Gen.Elm.Interface.make_.customType
                (Elm.tuple (Elm.string name)
                    (Elm.list <| List.map Elm.string ctors)
                )

        Elm.Interface.Alias name ->
            Gen.Elm.Interface.make_.alias (Elm.string name)

        Elm.Interface.Operator fixity ->
            Gen.Elm.Interface.make_.operator
                (Gen.Elm.Syntax.Infix.make_.infix
                    { direction = renode directionToGen fixity.direction
                    , function = renode Elm.string fixity.function
                    , operator = renode Elm.string fixity.operator
                    , precedence = renode Elm.int fixity.precedence
                    }
                )


directionToGen : Infix.InfixDirection -> Elm.Expression
directionToGen direction =
    case direction of
        Infix.Left ->
            Gen.Elm.Syntax.Infix.make_.left

        Infix.Right ->
            Gen.Elm.Syntax.Infix.make_.right

        Infix.Non ->
            Gen.Elm.Syntax.Infix.make_.non


traverseDirectoryForFiles : Directory -> List String
traverseDirectoryForFiles d =
    let
        go : Bool -> Directory -> List String -> List String
        go inSrc (Directory directory) acc =
            case Dict.get "src" directory.directories of
                Just src ->
                    go True src acc

                Nothing ->
                    Dict.foldl
                        (\_ subdir iacc ->
                            go inSrc subdir iacc
                        )
                        (if inSrc then
                            Dict.foldl
                                (\name content iacc ->
                                    if String.endsWith ".elm" name then
                                        content :: iacc

                                    else
                                        iacc
                                )
                                acc
                                directory.files

                         else
                            acc
                        )
                        directory.directories
    in
    go False d []


type alias FileResult a =
    { a
        | moduleName : ModuleName
        , declarations : List Elm.Declaration
        , hasOperators : Bool
    }


toFile : List Dependency -> RawFile -> Maybe (FileResult { interface : Interface })
toFile selfDependencies rawFile =
    let
        context : Elm.Processing.ProcessContext
        context =
            List.foldl Elm.Processing.addDependency Elm.Processing.init selfDependencies

        file : File.File
        file =
            Elm.Processing.process context rawFile
    in
    case Node.value file.moduleDefinition of
        Module.EffectModule _ ->
            -- Effect modules are not supported
            Nothing

        Module.PortModule _ ->
            -- Port modules are not supported
            Nothing

        Module.NormalModule { moduleName } ->
            let
                normal : FileResult {}
                normal =
                    normalModuleToFile moduleName file
            in
            { moduleName = normal.moduleName
            , declarations = normal.declarations
            , hasOperators = normal.hasOperators
            , interface = Elm.Interface.build rawFile
            }
                |> Just


normalModuleToFile : Node ModuleName -> File.File -> FileResult {}
normalModuleToFile (Node _ moduleName) file =
    let
        generatedModuleName : ModuleName
        generatedModuleName =
            "Core" :: moduleName

        aliasMap : Dict.Dict ModuleName ModuleName
        aliasMap =
            file.imports
                |> List.filterMap
                    (\(Node _ imp) ->
                        imp.moduleAlias
                            |> Maybe.map
                                (\(Node _ alias_) ->
                                    ( alias_, Node.value imp.moduleName )
                                )
                    )
                |> Dict.fromList

        namesAndDeclarations : List ( String, Elm.Declaration )
        namesAndDeclarations =
            file.declarations
                |> List.filterMap (declarationToGen aliasMap moduleName)

        names : List String
        names =
            List.map Tuple.first namesAndDeclarations

        declarations : List Elm.Declaration
        declarations =
            List.map Tuple.second namesAndDeclarations

        aliasConstructors : List ( String, Elm.Expression )
        aliasConstructors =
            recordAliasConstructorsToGen aliasMap moduleName file.declarations

        typeConstructors : List ( String, Elm.Expression )
        typeConstructors =
            customTypeConstructorsToGen moduleName file.declarations

        functions : Elm.Declaration
        functions =
            let
                functionEntries : List Elm.Expression
                functionEntries =
                    names
                        |> List.map
                            (\name ->
                                Elm.tuple
                                    (Elm.string name)
                                    (Elm.value
                                        { importFrom = []
                                        , name = name
                                        , annotation =
                                            Just
                                                Gen.Elm.Syntax.Expression.annotation_.functionImplementation
                                        }
                                    )
                            )

                aliasEntries : List Elm.Expression
                aliasEntries =
                    aliasConstructors
                        |> List.map
                            (\( name, impl ) ->
                                Elm.tuple (Elm.string name) impl
                            )
            in
            let
                ctorEntries : List Elm.Expression
                ctorEntries =
                    typeConstructors
                        |> List.map (\( name, impl ) -> Elm.tuple (Elm.string name) impl)
            in
            (functionEntries ++ aliasEntries ++ ctorEntries)
                |> Gen.FastDict.fromList
                |> Elm.declaration "functions"
                |> Elm.expose

        operators : List Elm.Expression
        operators =
            List.filterMap
                (\(Node _ declaration) ->
                    case declaration of
                        Declaration.InfixDeclaration { operator, function } ->
                            let
                                functionName : String
                                functionName =
                                    Node.value function
                            in
                            case List.reverse <| List.map Elm.string <| String.split "." functionName of
                                name :: reverseModule ->
                                    let
                                        fixedModule : List Elm.Expression
                                        fixedModule =
                                            if List.isEmpty reverseModule then
                                                List.map Elm.string moduleName

                                            else
                                                List.reverse reverseModule
                                    in
                                    Just
                                        (Elm.tuple
                                            (Elm.string <| Node.value operator)
                                            (Gen.Elm.Syntax.Pattern.make_.qualifiedNameRef
                                                { moduleName = Elm.list fixedModule
                                                , name = name
                                                }
                                            )
                                        )

                                [] ->
                                    Nothing

                        _ ->
                            Nothing
                )
                file.declarations

        outputDeclarations : List Elm.Declaration
        outputDeclarations =
            if List.isEmpty operators then
                functions :: declarations

            else
                let
                    operatorsDeclaration : Elm.Declaration
                    operatorsDeclaration =
                        operators
                            |> Elm.list
                            |> Elm.declaration "operators"
                            |> Elm.expose
                in
                functions :: operatorsDeclaration :: declarations
    in
    { moduleName = generatedModuleName
    , declarations = outputDeclarations
    , hasOperators = not (List.isEmpty operators)
    }


declarationToGen : Dict.Dict ModuleName ModuleName -> ModuleName -> Node Declaration.Declaration -> Maybe ( String, Elm.Declaration )
declarationToGen aliasMap moduleName (Node _ declaration) =
    case declaration of
        Declaration.FunctionDeclaration function ->
            let
                implementation : Expression.FunctionImplementation
                implementation =
                    Node.value function.declaration

                name : String
                name =
                    Node.value implementation.name
            in
            Just
                ( name
                , functionImplementationToGen aliasMap
                    { implementation
                        | name =
                            Node
                                (Node.range implementation.name)
                                (String.join "." (moduleName ++ [ name ]))
                    }
                    |> Elm.declaration name
                    |> Elm.expose
                )

        _ ->
            Nothing


{-| Generate record alias constructor functions from type alias declarations.
These are separate from regular declarations because they can't use normal
Elm declaration syntax (constructor names are capitalized).
-}
recordAliasConstructorsToGen : Dict.Dict ModuleName ModuleName -> ModuleName -> List (Node Declaration.Declaration) -> List ( String, Elm.Expression )
recordAliasConstructorsToGen aliasMap moduleName declarations =
    List.filterMap
        (\(Node _ declaration) ->
            case declaration of
                Declaration.AliasDeclaration alias_ ->
                    case Node.value alias_.typeAnnotation of
                        Elm.Syntax.TypeAnnotation.Record fields ->
                            let
                                aliasName : String
                                aliasName =
                                    Node.value alias_.name

                                fieldNames : List String
                                fieldNames =
                                    List.map (\(Node _ ( Node _ fieldName, _ )) -> fieldName) fields

                                argNames : List String
                                argNames =
                                    List.indexedMap (\i _ -> "$alias_arg" ++ String.fromInt i) fieldNames

                                implementation : Expression.FunctionImplementation
                                implementation =
                                    { name = Node (Node.range alias_.name) (String.join "." (moduleName ++ [ aliasName ]))
                                    , arguments =
                                        argNames
                                            |> List.map (\n -> Node Elm.Syntax.Range.emptyRange (Pattern.VarPattern n))
                                    , expression =
                                        Node Elm.Syntax.Range.emptyRange
                                            (Expression.RecordExpr
                                                (List.map2
                                                    (\fieldName argName ->
                                                        Node Elm.Syntax.Range.emptyRange
                                                            ( Node Elm.Syntax.Range.emptyRange fieldName
                                                            , Node Elm.Syntax.Range.emptyRange (Expression.FunctionOrValue [] argName)
                                                            )
                                                    )
                                                    fieldNames
                                                    argNames
                                                )
                                            )
                                    }
                            in
                            Just ( aliasName, functionImplementationToGen aliasMap implementation )

                        _ ->
                            Nothing

                _ ->
                    Nothing
        )
        declarations


{-| Generate custom type constructor functions.
Each constructor becomes a FunctionImplementation that creates the appropriate Custom value.
-}
customTypeConstructorsToGen : ModuleName -> List (Node Declaration.Declaration) -> List ( String, Elm.Expression )
customTypeConstructorsToGen moduleName declarations =
    List.concatMap
        (\(Node _ declaration) ->
            case declaration of
                Declaration.CustomTypeDeclaration customType ->
                    List.filterMap
                        (\(Node _ ctor) ->
                            let
                                ctorName : String
                                ctorName =
                                    Node.value ctor.name

                                arity : Int
                                arity =
                                    List.length ctor.arguments

                                argNames : List String
                                argNames =
                                    List.indexedMap (\i _ -> "$ctor_arg" ++ String.fromInt i) ctor.arguments

                                implementation : Expression.FunctionImplementation
                                implementation =
                                    { name = Node Elm.Syntax.Range.emptyRange (String.join "." (moduleName ++ [ ctorName ]))
                                    , arguments =
                                        argNames
                                            |> List.map (\n -> Node Elm.Syntax.Range.emptyRange (Pattern.VarPattern n))
                                    , expression =
                                        if arity == 0 then
                                            Node Elm.Syntax.Range.emptyRange
                                                (Expression.FunctionOrValue moduleName ctorName)

                                        else
                                            Node Elm.Syntax.Range.emptyRange
                                                (Expression.Application
                                                    (Node Elm.Syntax.Range.emptyRange (Expression.FunctionOrValue moduleName ctorName)
                                                        :: List.map
                                                            (\n -> Node Elm.Syntax.Range.emptyRange (Expression.FunctionOrValue [] n))
                                                            argNames
                                                    )
                                                )
                                    }
                            in
                            Just ( ctorName, functionImplementationToGen Dict.empty implementation )
                        )
                        customType.constructors

                _ ->
                    []
        )
        declarations


functionImplementationToGen : Dict.Dict ModuleName ModuleName -> Expression.FunctionImplementation -> Elm.Expression
functionImplementationToGen aliasMap { name, arguments, expression } =
    Gen.Elm.Syntax.Expression.make_.functionImplementation
        { name = renode Elm.string name
        , arguments = arguments |> List.map (\pattern -> renode (patternToGen aliasMap) pattern) |> Elm.list
        , expression = renode (expressionToGen aliasMap) expression
        }


patternToGen : Dict.Dict ModuleName ModuleName -> Pattern.Pattern -> Elm.Expression
patternToGen aliasMap pattern =
    case pattern of
        Pattern.AllPattern ->
            Gen.Elm.Syntax.Pattern.make_.allPattern

        Pattern.UnitPattern ->
            Gen.Elm.Syntax.Pattern.make_.unitPattern

        Pattern.CharPattern c ->
            Gen.Elm.Syntax.Pattern.make_.charPattern (Elm.char c)

        Pattern.StringPattern s ->
            Gen.Elm.Syntax.Pattern.make_.stringPattern (Elm.string s)

        Pattern.IntPattern i ->
            Gen.Elm.Syntax.Pattern.make_.intPattern (Elm.int i)

        Pattern.HexPattern x ->
            Gen.Elm.Syntax.Pattern.make_.hexPattern (Elm.hex x)

        Pattern.FloatPattern f ->
            Gen.Elm.Syntax.Pattern.make_.floatPattern (Elm.float f)

        Pattern.TuplePattern children ->
            Gen.Elm.Syntax.Pattern.make_.tuplePattern (renodeList (patternToGen aliasMap) children)

        Pattern.RecordPattern fields ->
            Gen.Elm.Syntax.Pattern.make_.recordPattern (renodeList Elm.string fields)

        Pattern.VarPattern name ->
            Gen.Elm.Syntax.Pattern.make_.varPattern (Elm.string name)

        Pattern.ParenthesizedPattern child ->
            Gen.Elm.Syntax.Pattern.make_.parenthesizedPattern (renode (patternToGen aliasMap) child)

        Pattern.AsPattern child name ->
            Gen.Elm.Syntax.Pattern.make_.asPattern (renode (patternToGen aliasMap) child) (renode Elm.string name)

        Pattern.UnConsPattern head tail ->
            Gen.Elm.Syntax.Pattern.make_.unConsPattern (renode (patternToGen aliasMap) head) (renode (patternToGen aliasMap) tail)

        Pattern.ListPattern children ->
            Gen.Elm.Syntax.Pattern.make_.listPattern (renodeList (patternToGen aliasMap) children)

        Pattern.NamedPattern qualifiedNameRef children ->
            let
                resolvedRef =
                    { qualifiedNameRef
                        | moduleName = resolveAlias aliasMap qualifiedNameRef.moduleName
                    }
            in
            Gen.Elm.Syntax.Pattern.make_.namedPattern (qualifiedNameRefToGen resolvedRef) (renodeList (patternToGen aliasMap) children)


qualifiedNameRefToGen : Pattern.QualifiedNameRef -> Elm.Expression
qualifiedNameRefToGen { name, moduleName } =
    Gen.Elm.Syntax.Pattern.make_.qualifiedNameRef
        { name = Elm.string name
        , moduleName = Elm.list (List.map Elm.string moduleName)
        }


renode : (a -> Elm.Expression) -> Node a -> Elm.Expression
renode toGen (Node range value) =
    if range.start.row == range.end.row then
        Gen.H.node1 range.start.row range.start.column range.end.column (toGen value)

    else
        Gen.H.node range.start.row range.start.column range.end.row range.end.column (toGen value)


renodeList : (a -> Elm.Expression) -> List (Node a) -> Elm.Expression
renodeList f list =
    Elm.list (List.map (renode f) list)


resolveAlias : Dict.Dict ModuleName ModuleName -> ModuleName -> ModuleName
resolveAlias aliasMap moduleName =
    Dict.get moduleName aliasMap
        |> Maybe.withDefault moduleName


expressionToGen : Dict.Dict ModuleName ModuleName -> Expression.Expression -> Elm.Expression
expressionToGen aliasMap expression =
    case expression of
        Expression.UnitExpr ->
            Gen.Elm.Syntax.Expression.make_.unitExpr

        Expression.Application children ->
            Gen.Elm.Syntax.Expression.make_.application (renodeList (expressionToGen aliasMap) children)

        Expression.OperatorApplication opName infix_ l r ->
            Gen.Elm.Syntax.Expression.make_.operatorApplication
                (Elm.string opName)
                (infixToGen infix_)
                (renode (expressionToGen aliasMap) l)
                (renode (expressionToGen aliasMap) r)

        Expression.FunctionOrValue [] name ->
            Gen.H.val name

        Expression.FunctionOrValue moduleName name ->
            let
                resolvedModuleName =
                    resolveAlias aliasMap moduleName
            in
            Gen.Elm.Syntax.Expression.make_.functionOrValue (Elm.list <| List.map Elm.string resolvedModuleName) (Elm.string name)

        Expression.IfBlock cond true false ->
            Gen.Elm.Syntax.Expression.make_.ifBlock
                (renode (expressionToGen aliasMap) cond)
                (renode (expressionToGen aliasMap) true)
                (renode (expressionToGen aliasMap) false)

        Expression.PrefixOperator opName ->
            Gen.Elm.Syntax.Expression.make_.prefixOperator (Elm.string opName)

        Expression.Operator opName ->
            Gen.Elm.Syntax.Expression.make_.operator (Elm.string opName)

        Expression.Integer i ->
            Gen.Elm.Syntax.Expression.make_.integer (Elm.int i)

        Expression.Hex x ->
            Gen.Elm.Syntax.Expression.make_.hex (Elm.hex x)

        Expression.Floatable f ->
            Gen.Elm.Syntax.Expression.make_.floatable (Elm.float f)

        Expression.Negation child ->
            Gen.Elm.Syntax.Expression.make_.negation (renode (expressionToGen aliasMap) child)

        Expression.Literal s ->
            Gen.Elm.Syntax.Expression.make_.literal (Elm.string s)

        Expression.CharLiteral c ->
            Gen.Elm.Syntax.Expression.make_.charLiteral
                (if Char.toCode c < 32 || Char.toCode c > 126 then
                    Elm.apply (Elm.value { importFrom = [ "Char" ], name = "fromCode", annotation = Nothing }) [ Elm.int (Char.toCode c) ]

                 else
                    Elm.char c
                )

        Expression.TupledExpression children ->
            Gen.Elm.Syntax.Expression.make_.tupledExpression (renodeList (expressionToGen aliasMap) children)

        Expression.ParenthesizedExpression child ->
            Gen.Elm.Syntax.Expression.make_.parenthesizedExpression (renode (expressionToGen aliasMap) child)

        Expression.LetExpression letBlock ->
            Gen.Elm.Syntax.Expression.make_.letExpression (letBlockToGen aliasMap letBlock)

        Expression.CaseExpression caseBlock ->
            Gen.Elm.Syntax.Expression.make_.caseExpression (caseBlockToGen aliasMap caseBlock)

        Expression.LambdaExpression lambda ->
            Gen.Elm.Syntax.Expression.make_.lambdaExpression (lambdaToGen aliasMap lambda)

        Expression.RecordExpr setters ->
            Gen.Elm.Syntax.Expression.make_.recordExpr (renodeList (recordSetterToGen aliasMap) setters)

        Expression.ListExpr children ->
            Gen.Elm.Syntax.Expression.make_.listExpr (renodeList (expressionToGen aliasMap) children)

        Expression.RecordAccess child field ->
            Gen.Elm.Syntax.Expression.make_.recordAccess (renode (expressionToGen aliasMap) child) (renode Elm.string field)

        Expression.RecordAccessFunction name ->
            Gen.Elm.Syntax.Expression.make_.recordAccessFunction (Elm.string name)

        Expression.RecordUpdateExpression name setters ->
            Gen.Elm.Syntax.Expression.make_.recordUpdateExpression (renode Elm.string name) (renodeList (recordSetterToGen aliasMap) setters)

        Expression.GLSLExpression s ->
            Gen.Elm.Syntax.Expression.make_.gLSLExpression (Elm.string s)


caseBlockToGen : Dict.Dict ModuleName ModuleName -> Expression.CaseBlock -> Elm.Expression
caseBlockToGen aliasMap { expression, cases } =
    Gen.Elm.Syntax.Expression.make_.caseBlock
        { expression = renode (expressionToGen aliasMap) expression
        , cases = Elm.list <| List.map (caseToGen aliasMap) cases
        }


caseToGen : Dict.Dict ModuleName ModuleName -> Expression.Case -> Elm.Expression
caseToGen aliasMap ( pattern, expression ) =
    Elm.tuple
        (renode (patternToGen aliasMap) pattern)
        (renode (expressionToGen aliasMap) expression)


lambdaToGen : Dict.Dict ModuleName ModuleName -> Expression.Lambda -> Elm.Expression
lambdaToGen aliasMap { args, expression } =
    Gen.Elm.Syntax.Expression.make_.lambda
        { args = renodeList (patternToGen aliasMap) args
        , expression = renode (expressionToGen aliasMap) expression
        }


letBlockToGen : Dict.Dict ModuleName ModuleName -> Expression.LetBlock -> Elm.Expression
letBlockToGen aliasMap { declarations, expression } =
    Gen.Elm.Syntax.Expression.make_.letBlock
        { declarations = renodeList (letDeclarationToGen aliasMap) declarations
        , expression = renode (expressionToGen aliasMap) expression
        }


letDeclarationToGen : Dict.Dict ModuleName ModuleName -> Expression.LetDeclaration -> Elm.Expression
letDeclarationToGen aliasMap declaration =
    case declaration of
        Expression.LetFunction function ->
            Gen.Elm.Syntax.Expression.make_.letFunction (functionToGen aliasMap function)

        Expression.LetDestructuring pattern expression ->
            Gen.Elm.Syntax.Expression.make_.letDestructuring (renode (patternToGen aliasMap) pattern) (renode (expressionToGen aliasMap) expression)


functionToGen : Dict.Dict ModuleName ModuleName -> Expression.Function -> Elm.Expression
functionToGen aliasMap { declaration } =
    Gen.Elm.Syntax.Expression.make_.function
        { documentation = Gen.Maybe.make_.nothing
        , signature = Gen.Maybe.make_.nothing
        , declaration = renode (functionImplementationToGen aliasMap) declaration
        }


recordSetterToGen : Dict.Dict ModuleName ModuleName -> Expression.RecordSetter -> Elm.Expression
recordSetterToGen aliasMap ( name, value ) =
    Elm.tuple (renode Elm.string name) (renode (expressionToGen aliasMap) value)


infixToGen : Infix.InfixDirection -> Elm.Expression
infixToGen direction =
    case direction of
        Infix.Left ->
            Gen.Elm.Syntax.Infix.make_.left

        Infix.Right ->
            Gen.Elm.Syntax.Infix.make_.right

        Infix.Non ->
            Gen.Elm.Syntax.Infix.make_.non
