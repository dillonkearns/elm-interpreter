module Eval.Resolver exposing
    ( ResolveError(..)
    , ResolverContext
    , initContext
    , initContextWithImports
    , resolveDeclaration
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
import Elm.Syntax.Expression as Expression exposing (Expression, FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Environment
import Eval.ResolvedIR as IR exposing (GlobalId, RExpr(..), RPattern(..))
import FastDict
import Syntax exposing (fakeNode)
import Types exposing (ImportedNames)


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
    , imports : ImportedNames
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
    , imports = emptyImports
    }


{-| Build a resolver context that also knows about the current module's
import table, so the resolver can canonicalize aliased module references
(`import Review.Project as Project` → `Project.addElmJson` resolves to
`(["Review", "Project"], "addElmJson")`) and pick up values exposed via
`exposing (..)` or `exposing (name, ...)`.

Without this the resolver fails with `UnknownName` on any real user
module that uses aliased imports — which is most of them.

-}
initContextWithImports :
    List String
    -> FastDict.Dict ( List String, String ) GlobalId
    -> ImportedNames
    -> ResolverContext
initContextWithImports currentModule globalIds imports =
    { localNames = []
    , globalIds = globalIds
    , currentModule = currentModule
    , imports = imports
    }


emptyImports : ImportedNames
emptyImports =
    { aliases = FastDict.empty
    , exposedValues = FastDict.empty
    , exposedConstructors = FastDict.empty
    }


{-| Resolve a top-level function declaration. Treats `let f a b = body in ...`
-style functions (and top-level declarations with positional arguments) as
sugar for `\a b -> body`, producing an `RLambda` wrapped IR. For zero-arity
declarations (plain values) the body is resolved directly.

This is the entry point Phase 2 iteration 2b uses to drive the resolver over
every user declaration at `buildProjectEnv` time.

-}
resolveDeclaration :
    ResolverContext
    -> FunctionImplementation
    -> Result ResolveError RExpr
resolveDeclaration ctx impl =
    case impl.arguments of
        [] ->
            resolveExpression ctx impl.expression

        args ->
            let
                ( rewrittenArgs, rewrittenBody ) =
                    desugarLambdaParams args impl.expression
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
                                    IR.mkLambda (List.length rewrittenArgs) body
                                )
                    )



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

        Expression.GLSLExpression _ ->
            Ok RGLSL


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
                    {- Uppercase reference. Either a real custom-type
                       constructor (`type Foo = Bar`) or a record alias
                       constructor (`type alias Bar = { ... }`).

                       Both are registered as functions in
                       `env.shared.functions` at buildProjectEnv time,
                       so both show up in `globalIds`. Prefer the
                       `RGlobal` form so the dispatch runs the actual
                       function body — which is what distinguishes
                       them: custom ctors accumulate `Custom qualRef
                       [...]` via `applyClosure`'s Custom branch; record
                       aliases have synthesized bodies that build a
                       `Record dict`. Without this lookup, the resolver
                       treated `Node 1 2 3` (a record alias) as an
                       opaque `Custom "Node" [1, 2, 3]` and any later
                       `.field` access on the result blew up with
                       "field access on non-record".

                       Fall back to `RCtor` only when the name is
                       missing from `globalIds` (e.g. `True`/`False`,
                       which the evaluator special-cases).
                    -}
                    case FastDict.get ( ctx.currentModule, name ) ctx.globalIds of
                        Just id ->
                            Ok (RGlobal id)

                        Nothing ->
                            case FastDict.get name ctx.imports.exposedConstructors of
                                Just ( canonicalModule, _ ) ->
                                    case FastDict.get ( canonicalModule, name ) ctx.globalIds of
                                        Just id ->
                                            Ok (RGlobal id)

                                        Nothing ->
                                            Ok (RCtor { moduleName = canonicalModule, name = name })

                                Nothing ->
                                    {- No local top-level and no exposed
                                       import match: assume the constructor
                                       is defined in the current module.
                                       Mirrors OLD eval's `evalVariant`,
                                       which resolves unqualified custom-
                                       type constructors to `env.currentModule`
                                       so `==` comparisons against qualified
                                       references from other modules (e.g.
                                       `Foo.Hearts` vs an unqualified `Hearts`
                                       inside `Foo`) produce consistent
                                       `Custom { moduleName, name }` values.
                                       Previously this branch dropped
                                       `moduleName` to `[]`, which caused
                                       the 5 `OrderTests` failures in the
                                       core-extra bench whenever a wrapper
                                       module qualified the ctor reference
                                       and the defining module's let-rec
                                       body compared it against an
                                       unqualified reference.
                                    -}
                                    Ok (RCtor { moduleName = ctx.currentModule, name = name })

                else
                    -- Unqualified value: try the current module first
                    -- (matches Elm's semantics for module-local references),
                    -- then fall back to imported exposed values, then to
                    -- the explicit-empty key that the caller may have
                    -- pre-populated.
                    case FastDict.get ( ctx.currentModule, name ) ctx.globalIds of
                        Just id ->
                            Ok (RGlobal id)

                        Nothing ->
                            case FastDict.get name ctx.imports.exposedValues of
                                Just ( canonicalModule, _ ) ->
                                    case FastDict.get ( canonicalModule, name ) ctx.globalIds of
                                        Just id ->
                                            Ok (RGlobal id)

                                        Nothing ->
                                            resolveGlobal ctx [] name

                                Nothing ->
                                    resolveGlobal ctx [] name

    else
        let
            -- Canonicalize the source module name through the imports
            -- alias table. `import Review.Project as Project` gives an
            -- `aliases` entry keyed by `"Project"` (the alias's module
            -- key) that maps to `(["Review", "Project"], ...)`.
            --
            -- If the alias isn't in the table, we assume the source
            -- module name is already canonical (e.g., an implicit-imported
            -- module like `Basics`, or a module that was written fully
            -- qualified without an alias).
            canonicalModule : List String
            canonicalModule =
                case FastDict.get (Environment.moduleKey moduleName) ctx.imports.aliases of
                    Just ( canonical, _ ) ->
                        canonical

                    Nothing ->
                        moduleName
        in
        if isConstructorName name then
            -- Same `RGlobal`-first treatment as the unqualified branch
            -- so record aliases dispatch through their synthesized
            -- function body instead of getting wrapped in a `Custom`.
            case FastDict.get ( canonicalModule, name ) ctx.globalIds of
                Just id ->
                    Ok (RGlobal id)

                Nothing ->
                    Ok (RCtor { moduleName = canonicalModule, name = name })

        else
            resolveGlobal ctx canonicalModule name


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
                            IR.mkLambda (List.length rewrittenArgs) body
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
    -- Every lambda parameter needs to occupy exactly one slot in the runtime
    -- locals stack, because the evaluator pushes one value per positional
    -- argument regardless of the parameter pattern. The resolver's context
    -- has to mirror that, so this desugaring normalizes every parameter to
    -- a named `VarPattern`:
    --
    --   * `VarPattern name` → stays `VarPattern name` (slot = that name)
    --   * `AllPattern` → `VarPattern "$_wildcardN"` (slot = unreferenceable
    --     placeholder)
    --   * Any non-trivial pattern → `VarPattern "$_argN"` plus a
    --     `let originalPattern = $_argN in ...` wrapping the body so the
    --     pattern's bindings get pushed on top of the raw-arg slot.
    --
    -- Placeholders start with `$` so they can never collide with a
    -- user-written identifier.
    let
        step :
            ( Int, Node Pattern )
            -> ( List (Node Pattern), Node Expression )
            -> ( List (Node Pattern), Node Expression )
        step ( idx, argNode ) ( accArgs, accBody ) =
            case classifyLambdaParam argNode of
                AlreadyNamed ->
                    ( argNode :: accArgs, accBody )

                WildcardParam ->
                    let
                        placeholder : String
                        placeholder =
                            "$_wildcard" ++ String.fromInt idx

                        newParam : Node Pattern
                        newParam =
                            fakeNode (Pattern.VarPattern placeholder)
                    in
                    ( newParam :: accArgs, accBody )

                NontrivialParam ->
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


type LambdaParamKind
    = AlreadyNamed
    | WildcardParam
    | NontrivialParam


classifyLambdaParam : Node Pattern -> LambdaParamKind
classifyLambdaParam (Node _ pat) =
    case pat of
        Pattern.VarPattern _ ->
            AlreadyNamed

        Pattern.AllPattern ->
            WildcardParam

        Pattern.ParenthesizedPattern inner ->
            classifyLambdaParam inner

        _ ->
            NontrivialParam


resolveLet :
    ResolverContext
    -> Expression.LetBlock
    -> Result ResolveError RExpr
resolveLet ctx letBlock =
    -- Let bindings use sequential scoping with self-recursion for function
    -- bindings. Concretely, when resolving binding N:
    --
    --   * Value bindings (arity 0) see only bindings 0..N-1 — they cannot
    --     reference themselves or later siblings. This matches Elm's
    --     semantics: `let x = x in ...` is a type error.
    --
    --   * Function bindings (arity > 0) see bindings 0..N-1 PLUS themselves,
    --     so single-binding self-recursion (`let fact n = ... fact (n-1)
    --     in ...`) works. Full MUTUAL recursion of sibling function
    --     bindings is NOT supported here — Phase 3 will revisit this when
    --     closures arrive and the evaluator can pre-allocate slots.
    --
    -- The let BODY sees every binding, exactly like the pre-body
    -- `extendedLocals` produced by the fold.
    --
    -- This sequential shape is consistent with a pure-Elm evaluator that
    -- evaluates bindings in order and prepends each result to the locals
    -- stack — no placeholder slot patching needed.
    collectLetBindings ctx letBlock.declarations
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
    -- Single-pass fold: for each let declaration, resolve its RHS against
    -- the CURRENT extendedLocals (which has previous siblings in scope),
    -- then prepend the binding's pattern bindings to extendedLocals for
    -- subsequent bindings and the let body.
    declarations
        |> List.foldl
            (\(Node _ decl) accResult ->
                case accResult of
                    Err e ->
                        Err e

                    Ok state ->
                        case decl of
                            LetFunction { declaration } ->
                                resolveLetFunction ctx state (Node.value declaration)

                            LetDestructuring patternNode bodyNode ->
                                resolveLetValue ctx state patternNode bodyNode
            )
            (Ok { bindings = [], extendedLocals = ctx.localNames })
        |> Result.map
            (\state ->
                { bindings = List.reverse state.bindings
                , extendedLocals = state.extendedLocals
                }
            )


type alias LetFoldState =
    { bindings : List IR.RLetBinding
    , extendedLocals : List String
    }


resolveLetFunction :
    ResolverContext
    -> LetFoldState
    -> Expression.FunctionImplementation
    -> Result ResolveError LetFoldState
resolveLetFunction ctx state impl =
    let
        name : String
        name =
            Node.value impl.name
    in
    case impl.arguments of
        [] ->
            -- Zero-argument function declaration is really a value binding.
            -- Elm's parser produces LetFunction for `let f = expr` because
            -- every `f = expr` is a "function declaration" in the grammar.
            -- Resolve it without self-rec scoping — values can't self-ref.
            let
                bodyCtx : ResolverContext
                bodyCtx =
                    { ctx | localNames = state.extendedLocals }
            in
            resolveExpression bodyCtx impl.expression
                |> Result.map
                    (\body ->
                        { bindings =
                            { pattern = RPVar
                            , arity = 0
                            , body = body
                            , debugName = name
                            }
                                :: state.bindings
                        , extendedLocals = name :: state.extendedLocals
                        }
                    )

        args ->
            -- Real function declaration (arity > 0). The body is a lambda,
            -- and the function gets to see its own name for self-recursion.
            -- Rewrite non-trivial parameter patterns to the same form
            -- resolveLambda uses, then resolve the body with the lambda's
            -- parameter slots on top of the self-recursive binding slot.
            let
                ( rewrittenArgs, rewrittenBody ) =
                    desugarLambdaParams args impl.expression

                -- Self-rec scope: name is visible in its own body. Added
                -- BEFORE the lambda's parameters so parameters shadow it
                -- if they share a name (matches Elm's shadowing rules).
                selfRecExtendedLocals : List String
                selfRecExtendedLocals =
                    name :: state.extendedLocals
            in
            resolvePatterns ctx rewrittenArgs
                |> Result.andThen
                    (\{ bindings } ->
                        let
                            lambdaCtx : ResolverContext
                            lambdaCtx =
                                { ctx | localNames = prependBindings bindings selfRecExtendedLocals }
                        in
                        resolveExpression lambdaCtx rewrittenBody
                            |> Result.map
                                (\body ->
                                    { bindings =
                                        { pattern = RPVar
                                        , arity = List.length rewrittenArgs
                                        , body = IR.mkLambda (List.length rewrittenArgs) body
                                        , debugName = name
                                        }
                                            :: state.bindings
                                    , extendedLocals = name :: state.extendedLocals
                                    }
                                )
                    )


resolveLetValue :
    ResolverContext
    -> LetFoldState
    -> Node Pattern
    -> Node Expression
    -> Result ResolveError LetFoldState
resolveLetValue ctx state patternNode bodyNode =
    -- Destructuring let binding: `let (a, b) = rhs in ...`. The RHS is
    -- resolved against state.extendedLocals (previous siblings only —
    -- values cannot reference themselves). After resolving, the pattern's
    -- bindings are prepended to extendedLocals for subsequent bindings.
    resolvePattern ctx patternNode
        |> Result.andThen
            (\{ resolved, bindings } ->
                let
                    bodyCtx : ResolverContext
                    bodyCtx =
                        { ctx | localNames = state.extendedLocals }
                in
                resolveExpression bodyCtx bodyNode
                    |> Result.map
                        (\body ->
                            { bindings =
                                { pattern = resolved
                                , arity = 0
                                , body = body
                                , debugName = firstBindingName bindings
                                }
                                    :: state.bindings
                            , extendedLocals = prependBindings bindings state.extendedLocals
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
