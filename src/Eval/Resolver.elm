module Eval.Resolver exposing
    ( ResolveError(..)
    , ResolverContext
    , initContext
    , resolveExpression
    , resolvePattern
    )

{-| Parse-time resolution pass: turn an `Elm.Syntax.Expression.Expression`
into the `Eval.ResolvedIR.RExpr` that the new evaluator (Phase 3) consumes.

The resolver's only job is to rewrite variable references to one of:

  - `RLocal Int` — a De Bruijn index into the current locals list, where `0`
    is the innermost binding. The resolver maintains a stack of in-scope
    local names (`ResolverContext.localNames`) and searches it on every
    `FunctionOrValue [] name` reference.

  - `RGlobal GlobalId` — a counter-interned top-level reference. The caller
    is responsible for pre-populating `ResolverContext.globalIds` with every
    top-level the resolver might encounter; unknown names produce
    `UnknownName` errors.

  - `RCtor` — a constructor reference, identified by the uppercase-first-letter
    convention that Elm already enforces at the lexer level. Constructors
    carry the source-level `(moduleName, name)` unchanged. Final resolution
    against imports happens in the evaluator at match / apply time.

Short-circuit operators (`&&`, `||`) get dedicated IR constructors so the
evaluator can preserve their semantics without the resolver fabricating
`True`/`False` constructor references. Pipeline operators (`<|`, `|>`) are
desugared to direct `RApply` — they're just function application with
reversed arg order, and the IR form is both simpler and faster.

All other operators are looked up in `Core.operators` (the same table the
existing evaluator uses in `Eval.Expression`), so `a + b` resolves to
`RApply (RGlobal <Basics.add>) [a, b]` exactly as the evaluator would execute
it today.

Destructuring `let` bindings (`let (a, b) = pair in body`) are handled by
storing the pattern on `RLetBinding`; the evaluator does the pattern match
at binding time. Non-variable patterns can only destructure — Elm's type
checker already rejected any attempt at recursive destructuring, so the
resolver treats all let groups as mutually-visible without further analysis.

-}

import Core
import Elm.Syntax.Expression as Expression exposing (Expression, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Eval.ResolvedIR as IR exposing (GlobalId, RExpr(..), RPattern(..))
import FastDict
import Syntax exposing (fakeNode)


{-| Context threaded through the resolver.

`localNames` is the stack of in-scope lexical bindings, innermost at the head.
`RLocal i` in the emitted IR corresponds to `List.drop i ctx.localNames |> List.head`
at eval time — no arithmetic, just cons-list traversal.

`globalIds` is a pre-built dictionary mapping `(ModuleName, name)` to the
caller-assigned counter id. Populating this is the caller's responsibility
(Phase 2 iteration 3 — the `Eval.Module.buildProjectEnvFromParsed` hook).

`currentModule` is used for one thing only: resolving bare references to
constructors defined in the module being resolved. Everything else is
looked up via `globalIds` keyed by the explicit source module.

-}
type alias ResolverContext =
    { localNames : List String
    , globalIds : FastDict.Dict ( List String, String ) GlobalId
    , currentModule : List String
    }


type ResolveError
    = UnknownName { moduleName : List String, name : String }
    | UnknownOperator String
    | UnsupportedExpression String
    | InvalidRecordUpdateTarget String
    | UnexpectedTupleArity Int


initContext :
    List String
    -> FastDict.Dict ( List String, String ) GlobalId
    -> ResolverContext
initContext currentModule globalIds =
    { localNames = []
    , globalIds = globalIds
    , currentModule = currentModule
    }



-- EXPRESSIONS


{-| Resolve a single expression against the current context. The context is
immutable here — binder forms (lambda, let, case) construct extended contexts
for their children but never mutate the caller's context.
-}
resolveExpression : ResolverContext -> Node Expression -> Result ResolveError RExpr
resolveExpression ctx (Node _ expr) =
    case expr of
        Expression.UnitExpr ->
            Ok RUnit

        Expression.Integer i ->
            Ok (RInt i)

        Expression.Hex i ->
            Ok (RInt i)

        Expression.Floatable f ->
            Ok (RFloat f)

        Expression.Literal s ->
            Ok (RString s)

        Expression.CharLiteral c ->
            Ok (RChar c)

        Expression.ParenthesizedExpression inner ->
            resolveExpression ctx inner

        Expression.Negation inner ->
            resolveExpression ctx inner |> Result.map RNegate

        Expression.FunctionOrValue moduleName name ->
            resolveFunctionOrValue ctx moduleName name

        Expression.Application exprs ->
            case exprs of
                [] ->
                    Err (UnsupportedExpression "empty Application")

                [ single ] ->
                    -- elm-syntax sometimes wraps a single expression in
                    -- Application; unwrap rather than emitting an empty-arg
                    -- RApply.
                    resolveExpression ctx single

                head :: rest ->
                    Result.map2 RApply
                        (resolveExpression ctx head)
                        (resolveAll ctx rest)

        Expression.OperatorApplication opName _ left right ->
            resolveOperatorApplication ctx opName left right

        Expression.IfBlock cond t f ->
            Result.map3 RIf
                (resolveExpression ctx cond)
                (resolveExpression ctx t)
                (resolveExpression ctx f)

        Expression.PrefixOperator opName ->
            resolveOperatorReference ctx opName

        Expression.Operator opName ->
            resolveOperatorReference ctx opName

        Expression.TupledExpression items ->
            case items of
                [ a, b ] ->
                    Result.map2 RTuple2
                        (resolveExpression ctx a)
                        (resolveExpression ctx b)

                [ a, b, c ] ->
                    Result.map3 RTuple3
                        (resolveExpression ctx a)
                        (resolveExpression ctx b)
                        (resolveExpression ctx c)

                _ ->
                    Err (UnexpectedTupleArity (List.length items))

        Expression.ListExpr items ->
            resolveAll ctx items |> Result.map RList

        Expression.LambdaExpression lambda ->
            resolveLambda ctx lambda

        Expression.LetExpression letBlock ->
            resolveLet ctx letBlock

        Expression.CaseExpression caseBlock ->
            resolveCase ctx caseBlock

        Expression.RecordExpr fields ->
            resolveRecordFields ctx fields
                |> Result.map (RRecord << sortFields)

        Expression.RecordUpdateExpression (Node _ targetName) setters ->
            resolveRecordUpdate ctx targetName setters

        Expression.RecordAccess recordExpr (Node _ field) ->
            resolveExpression ctx recordExpr
                |> Result.map (\rec -> RRecordAccess rec field)

        Expression.RecordAccessFunction field ->
            -- elm-syntax prefixes the dot; strip it.
            Ok (RRecordAccessFunction (String.dropLeft 1 field))

        Expression.GLSLExpression raw ->
            Ok (RGLSL raw)


resolveAll :
    ResolverContext
    -> List (Node Expression)
    -> Result ResolveError (List RExpr)
resolveAll ctx nodes =
    List.foldr
        (\node ->
            Result.map2 (::) (resolveExpression ctx node)
        )
        (Ok [])
        nodes


resolveFunctionOrValue :
    ResolverContext
    -> List String
    -> String
    -> Result ResolveError RExpr
resolveFunctionOrValue ctx moduleName name =
    if List.isEmpty moduleName then
        case findLocal name 0 ctx.localNames of
            Just idx ->
                Ok (RLocal idx)

            Nothing ->
                if isConstructorName name then
                    Ok (RCtor { moduleName = [], name = name })

                else
                    -- Unqualified value: try the current module first
                    -- (matches Elm's semantics for module-local references),
                    -- then fall back to the explicit-empty key for imports
                    -- that the caller may have pre-populated.
                    case FastDict.get ( ctx.currentModule, name ) ctx.globalIds of
                        Just id ->
                            Ok (RGlobal id)

                        Nothing ->
                            resolveGlobal ctx [] name

    else if isConstructorName name then
        Ok (RCtor { moduleName = moduleName, name = name })

    else
        resolveGlobal ctx moduleName name


findLocal : String -> Int -> List String -> Maybe Int
findLocal target idx names =
    case names of
        [] ->
            Nothing

        first :: rest ->
            if first == target then
                Just idx

            else
                findLocal target (idx + 1) rest


resolveGlobal :
    ResolverContext
    -> List String
    -> String
    -> Result ResolveError RExpr
resolveGlobal ctx moduleName name =
    case FastDict.get ( moduleName, name ) ctx.globalIds of
        Just id ->
            Ok (RGlobal id)

        Nothing ->
            Err (UnknownName { moduleName = moduleName, name = name })


isConstructorName : String -> Bool
isConstructorName name =
    case String.uncons name of
        Just ( first, _ ) ->
            Char.isUpper first

        Nothing ->
            False


resolveOperatorApplication :
    ResolverContext
    -> String
    -> Node Expression
    -> Node Expression
    -> Result ResolveError RExpr
resolveOperatorApplication ctx opName left right =
    case opName of
        "&&" ->
            Result.map2 RAnd
                (resolveExpression ctx left)
                (resolveExpression ctx right)

        "||" ->
            Result.map2 ROr
                (resolveExpression ctx left)
                (resolveExpression ctx right)

        "|>" ->
            -- `a |> f` → `f a`. Keep the IR direct — no `Basics.apR` indirection.
            Result.map2 (\a f -> RApply f [ a ])
                (resolveExpression ctx left)
                (resolveExpression ctx right)

        "<|" ->
            -- `f <| a` → `f a`.
            Result.map2 (\f a -> RApply f [ a ])
                (resolveExpression ctx left)
                (resolveExpression ctx right)

        _ ->
            case FastDict.get opName Core.operators of
                Just ref ->
                    resolveGlobal ctx ref.moduleName ref.name
                        |> Result.andThen
                            (\opExpr ->
                                Result.map2 (\l r -> RApply opExpr [ l, r ])
                                    (resolveExpression ctx left)
                                    (resolveExpression ctx right)
                            )

                Nothing ->
                    Err (UnknownOperator opName)


resolveOperatorReference : ResolverContext -> String -> Result ResolveError RExpr
resolveOperatorReference ctx opName =
    case FastDict.get opName Core.operators of
        Just ref ->
            resolveGlobal ctx ref.moduleName ref.name

        Nothing ->
            Err (UnknownOperator opName)


resolveLambda :
    ResolverContext
    -> Expression.Lambda
    -> Result ResolveError RExpr
resolveLambda ctx lambda =
    -- Non-trivial lambda parameters (e.g. `\(a, b) -> ...`) are desugared to
    -- trivial parameters plus `let` destructurings in the body. This keeps
    -- both the resolver's slot-assignment logic and the evaluator's hot path
    -- simple: lambdas only ever have RPVar/RPWildcard parameters in the IR,
    -- and the existing RLet pattern-matching machinery handles destructuring.
    let
        ( rewrittenArgs, rewrittenBody ) =
            desugarLambdaParams lambda.args lambda.expression
    in
    resolvePatterns ctx rewrittenArgs
        |> Result.andThen
            (\{ bindings } ->
                let
                    innerCtx : ResolverContext
                    innerCtx =
                        { ctx | localNames = prependBindings bindings ctx.localNames }
                in
                resolveExpression innerCtx rewrittenBody
                    |> Result.map
                        (\body ->
                            RLambda
                                { arity = List.length rewrittenArgs
                                , body = body
                                }
                        )
            )


{-| Rewrite a lambda's parameter list so every parameter is trivially bindable
(`VarPattern` or `AllPattern`). Non-trivial patterns become a fresh
placeholder variable plus a `let (originalPattern) = placeholder in ...`
wrapping the body.

    desugarLambdaParams [ x, (a, b), y ] body
    --> ( [ x, $_arg1, y ], `let (a, b) = $_arg1 in body` )

Using placeholders that start with `$` keeps them safely outside Elm's
identifier namespace, so they cannot shadow a user-written variable.

The synthetic `let` destructurings are prepended to the body in parameter
order, so references inside the body see the destructured names at the
expected depth. See the long comment inside `resolveLet` for how the let
group's slot assignment interacts with surrounding lambda parameters.

-}
desugarLambdaParams :
    List (Node Pattern)
    -> Node Expression
    -> ( List (Node Pattern), Node Expression )
desugarLambdaParams args body =
    let
        step :
            ( Int, Node Pattern )
            -> ( List (Node Pattern), Node Expression )
            -> ( List (Node Pattern), Node Expression )
        step ( idx, argNode ) ( accArgs, accBody ) =
            if isTriviallyBindable argNode then
                ( argNode :: accArgs, accBody )

            else
                let
                    placeholder : String
                    placeholder =
                        "$_arg" ++ String.fromInt idx

                    newParam : Node Pattern
                    newParam =
                        fakeNode (Pattern.VarPattern placeholder)

                    placeholderRef : Node Expression
                    placeholderRef =
                        fakeNode (Expression.FunctionOrValue [] placeholder)

                    letDecl : Node LetDeclaration
                    letDecl =
                        fakeNode
                            (LetDestructuring argNode placeholderRef)

                    newBody : Node Expression
                    newBody =
                        fakeNode
                            (Expression.LetExpression
                                { declarations = [ letDecl ]
                                , expression = accBody
                                }
                            )
                in
                ( newParam :: accArgs, newBody )
    in
    args
        |> List.indexedMap (\i a -> ( i, a ))
        |> List.foldr step ( [], body )


isTriviallyBindable : Node Pattern -> Bool
isTriviallyBindable (Node _ pat) =
    case pat of
        Pattern.VarPattern _ ->
            True

        Pattern.AllPattern ->
            True

        Pattern.ParenthesizedPattern inner ->
            isTriviallyBindable inner

        _ ->
            False


resolveLet :
    ResolverContext
    -> Expression.LetBlock
    -> Result ResolveError RExpr
resolveLet ctx letBlock =
    -- Let bindings are treated as a mutually-visible group: all siblings are
    -- in scope for all bodies AND for the final expression. This matches
    -- Elm's semantics for mutually-recursive function definitions; value
    -- bindings cannot reference themselves by construction (the type checker
    -- already enforced that).
    let
        resolved =
            collectLetBindings ctx letBlock.declarations
    in
    resolved
        |> Result.andThen
            (\{ bindings, extendedLocals } ->
                let
                    innerCtx : ResolverContext
                    innerCtx =
                        { ctx | localNames = extendedLocals }
                in
                resolveExpression innerCtx letBlock.expression
                    |> Result.map (\body -> RLet bindings body)
            )


collectLetBindings :
    ResolverContext
    -> List (Node LetDeclaration)
    -> Result ResolveError { bindings : List IR.RLetBinding, extendedLocals : List String }
collectLetBindings ctx declarations =
    -- Two-pass approach:
    --
    -- Pass 1: walk declarations in order, resolve each pattern to determine
    --         the slot shape, and prepend bindings to an accumulating
    --         localNames. This produces the "extended" context that every
    --         binding body (and the final expression) will use.
    --
    -- Pass 2: resolve each binding's body against the extended context.
    --         Function bodies are wrapped in RLambda here — the resolver
    --         treats `let f x = ...` as sugar for `let f = \x -> ...`.
    declarations
        |> List.foldl
            (\(Node _ decl) acc ->
                case acc of
                    Err e ->
                        Err e

                    Ok state ->
                        case decl of
                            LetFunction { declaration } ->
                                let
                                    impl =
                                        Node.value declaration

                                    name =
                                        Node.value impl.name
                                in
                                Ok
                                    { state
                                        | skeletons =
                                            { pattern = RPVar
                                            , patternBindings = [ name ]
                                            , arguments = impl.arguments
                                            , body = impl.expression
                                            , debugName = name
                                            }
                                                :: state.skeletons
                                        , extendedLocals = name :: state.extendedLocals
                                    }

                            LetDestructuring patternNode bodyNode ->
                                resolvePattern ctx patternNode
                                    |> Result.map
                                        (\{ resolved, bindings } ->
                                            { state
                                                | skeletons =
                                                    { pattern = resolved
                                                    , patternBindings = bindings
                                                    , arguments = []
                                                    , body = bodyNode
                                                    , debugName = firstBindingName bindings
                                                    }
                                                        :: state.skeletons
                                                , extendedLocals = prependBindings bindings state.extendedLocals
                                            }
                                        )
            )
            (Ok { skeletons = [], extendedLocals = ctx.localNames })
        |> Result.andThen
            (\{ skeletons, extendedLocals } ->
                let
                    innerCtx : ResolverContext
                    innerCtx =
                        { ctx | localNames = extendedLocals }
                in
                skeletons
                    -- skeletons was built with foldl which reverses, so
                    -- reverse back to source order before resolving bodies.
                    |> List.reverse
                    |> List.foldr
                        (\skeleton bindingsAcc ->
                            Result.map2 (::)
                                (resolveLetBody innerCtx skeleton)
                                bindingsAcc
                        )
                        (Ok [])
                    |> Result.map
                        (\bindings ->
                            { bindings = bindings, extendedLocals = extendedLocals }
                        )
            )


type alias LetSkeleton =
    { pattern : RPattern
    , patternBindings : List String
    , arguments : List (Node Pattern)
    , body : Node Expression
    , debugName : String
    }


resolveLetBody : ResolverContext -> LetSkeleton -> Result ResolveError IR.RLetBinding
resolveLetBody ctx skeleton =
    case skeleton.arguments of
        [] ->
            -- Plain value binding: body is resolved as-is, arity = 0.
            resolveExpression ctx skeleton.body
                |> Result.map
                    (\body ->
                        { pattern = skeleton.pattern
                        , arity = 0
                        , body = body
                        , debugName = skeleton.debugName
                        }
                    )

        args ->
            -- Function binding: the LetFunction `let f p1 p2 = body in ...`
            -- is semantically `let f = \p1 p2 -> body in ...`. Reuse the
            -- lambda path so non-trivial patterns get the same desugaring.
            let
                ( rewrittenArgs, rewrittenBody ) =
                    desugarLambdaParams args skeleton.body
            in
            resolvePatterns ctx rewrittenArgs
                |> Result.andThen
                    (\{ bindings } ->
                        let
                            lambdaCtx : ResolverContext
                            lambdaCtx =
                                { ctx | localNames = prependBindings bindings ctx.localNames }
                        in
                        resolveExpression lambdaCtx rewrittenBody
                            |> Result.map
                                (\body ->
                                    { pattern = skeleton.pattern
                                    , arity = List.length rewrittenArgs
                                    , body =
                                        RLambda
                                            { arity = List.length rewrittenArgs
                                            , body = body
                                            }
                                    , debugName = skeleton.debugName
                                    }
                                )
                    )


firstBindingName : List String -> String
firstBindingName bindings =
    case bindings of
        first :: _ ->
            first

        [] ->
            "$destructure"


resolveCase :
    ResolverContext
    -> Expression.CaseBlock
    -> Result ResolveError RExpr
resolveCase ctx caseBlock =
    Result.map2 RCase
        (resolveExpression ctx caseBlock.expression)
        (resolveCaseBranches ctx caseBlock.cases)


resolveCaseBranches :
    ResolverContext
    -> List Expression.Case
    -> Result ResolveError (List ( RPattern, RExpr ))
resolveCaseBranches ctx branches =
    List.foldr
        (\( patNode, bodyNode ) acc ->
            case acc of
                Err e ->
                    Err e

                Ok rest ->
                    resolvePattern ctx patNode
                        |> Result.andThen
                            (\{ resolved, bindings } ->
                                let
                                    branchCtx : ResolverContext
                                    branchCtx =
                                        { ctx | localNames = prependBindings bindings ctx.localNames }
                                in
                                resolveExpression branchCtx bodyNode
                                    |> Result.map (\body -> ( resolved, body ) :: rest)
                            )
        )
        (Ok [])
        branches


resolveRecordFields :
    ResolverContext
    -> List (Node Expression.RecordSetter)
    -> Result ResolveError (List ( String, RExpr ))
resolveRecordFields ctx fields =
    List.foldr
        (\(Node _ ( Node _ name, valueNode )) acc ->
            Result.map2 (\value rest -> ( name, value ) :: rest)
                (resolveExpression ctx valueNode)
                acc
        )
        (Ok [])
        fields


resolveRecordUpdate :
    ResolverContext
    -> String
    -> List (Node Expression.RecordSetter)
    -> Result ResolveError RExpr
resolveRecordUpdate ctx targetName setters =
    -- The target of a record update must be a local variable; Elm's grammar
    -- enforces this. Look up its De Bruijn index and emit RRecordUpdate with
    -- the index directly (skipping a second lookup at eval time).
    case findLocal targetName 0 ctx.localNames of
        Nothing ->
            Err (InvalidRecordUpdateTarget targetName)

        Just idx ->
            resolveRecordFields ctx setters
                |> Result.map (\fields -> RRecordUpdate idx (sortFields fields))


sortFields : List ( String, a ) -> List ( String, a )
sortFields fields =
    List.sortBy Tuple.first fields



-- PATTERNS


{-| Resolve a pattern. Returns the resolved `RPattern` plus the list of bound
names in prepend order (source walk order). Callers extend their
`ResolverContext.localNames` via `prependBindings`.

The resolver does NOT lower constructor module qualification — `Maybe.Just`
patterns keep their original `moduleName` and the evaluator handles the
match. This matches the existing evaluator's behavior and leaves room for a
future constructor-resolution pass if type information becomes available.

-}
resolvePattern :
    ResolverContext
    -> Node Pattern
    -> Result ResolveError { resolved : RPattern, bindings : List String }
resolvePattern ctx (Node _ pat) =
    case pat of
        Pattern.AllPattern ->
            Ok { resolved = RPWildcard, bindings = [] }

        Pattern.UnitPattern ->
            Ok { resolved = RPUnit, bindings = [] }

        Pattern.CharPattern c ->
            Ok { resolved = RPChar c, bindings = [] }

        Pattern.StringPattern s ->
            Ok { resolved = RPString s, bindings = [] }

        Pattern.IntPattern i ->
            Ok { resolved = RPInt i, bindings = [] }

        Pattern.HexPattern i ->
            Ok { resolved = RPInt i, bindings = [] }

        Pattern.FloatPattern f ->
            Ok { resolved = RPFloat f, bindings = [] }

        Pattern.VarPattern name ->
            Ok { resolved = RPVar, bindings = [ name ] }

        Pattern.ParenthesizedPattern inner ->
            resolvePattern ctx inner

        Pattern.TuplePattern items ->
            case items of
                [ a, b ] ->
                    Result.map2
                        (\ra rb ->
                            { resolved = RPTuple2 ra.resolved rb.resolved
                            , bindings = ra.bindings ++ rb.bindings
                            }
                        )
                        (resolvePattern ctx a)
                        (resolvePattern ctx b)

                [ a, b, c ] ->
                    Result.map3
                        (\ra rb rc ->
                            { resolved = RPTuple3 ra.resolved rb.resolved rc.resolved
                            , bindings = ra.bindings ++ rb.bindings ++ rc.bindings
                            }
                        )
                        (resolvePattern ctx a)
                        (resolvePattern ctx b)
                        (resolvePattern ctx c)

                _ ->
                    Err (UnexpectedTupleArity (List.length items))

        Pattern.RecordPattern fields ->
            let
                names : List String
                names =
                    List.map Node.value fields
                        |> List.sort
            in
            Ok { resolved = RPRecord names, bindings = names }

        Pattern.UnConsPattern head tail ->
            Result.map2
                (\rh rt ->
                    { resolved = RPCons rh.resolved rt.resolved
                    , bindings = rh.bindings ++ rt.bindings
                    }
                )
                (resolvePattern ctx head)
                (resolvePattern ctx tail)

        Pattern.ListPattern items ->
            resolvePatternList ctx items
                |> Result.map
                    (\{ patterns, bindings } ->
                        { resolved = RPList patterns
                        , bindings = bindings
                        }
                    )

        Pattern.NamedPattern qualRef args ->
            resolvePatternList ctx args
                |> Result.map
                    (\{ patterns, bindings } ->
                        { resolved =
                            RPCtor
                                { moduleName = qualRef.moduleName
                                , name = qualRef.name
                                }
                                patterns
                        , bindings = bindings
                        }
                    )

        Pattern.AsPattern inner (Node _ asName) ->
            resolvePattern ctx inner
                |> Result.map
                    (\{ resolved, bindings } ->
                        { resolved = RPAs resolved
                        , bindings = bindings ++ [ asName ]
                        }
                    )


resolvePatterns :
    ResolverContext
    -> List (Node Pattern)
    -> Result ResolveError { patterns : List RPattern, bindings : List String }
resolvePatterns ctx nodes =
    List.foldr
        (\node acc ->
            case acc of
                Err e ->
                    Err e

                Ok state ->
                    resolvePattern ctx node
                        |> Result.map
                            (\{ resolved, bindings } ->
                                { patterns = resolved :: state.patterns
                                , bindings = bindings ++ state.bindings
                                }
                            )
        )
        (Ok { patterns = [], bindings = [] })
        nodes


resolvePatternList :
    ResolverContext
    -> List (Node Pattern)
    -> Result ResolveError { patterns : List RPattern, bindings : List String }
resolvePatternList =
    resolvePatterns


{-| Prepend a list of bound names to an existing locals stack. `bindings` is
in walk order (first-bound first); fold-left with cons produces the correct
innermost-at-head order.

    prependBindings [ "x", "y" ] [ "outer" ]
    --> [ "y", "x", "outer" ]

So in the body, `y` is `RLocal 0`, `x` is `RLocal 1`, and `outer` is
`RLocal 2`.

-}
prependBindings : List String -> List String -> List String
prependBindings bindings locals =
    List.foldl (::) locals bindings
