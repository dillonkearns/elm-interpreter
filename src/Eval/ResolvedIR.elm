module Eval.ResolvedIR exposing
    ( GlobalId
    , RExpr(..)
    , RLetBinding
    , RPattern(..)
    , slotCount
    )

{-| Resolved intermediate representation.

`RExpr` is what the resolver (`Eval.Resolver`, Phase 2 iteration 2+) produces
from an `Elm.Syntax.Expression.Expression`. Every variable reference has been
rewritten to one of:

  - `RLocal Int` — a De Bruijn index into the current locals list (`0` is the
    innermost binding, larger indices step outward). With a cons-list runtime
    env, `RLocal i` evaluates as `List.drop i env.locals |> List.head`, so no
    arithmetic at lookup time.

  - `RGlobal GlobalId` — a counter-interned top-level binding. The evaluator
    resolves this against a `Dict GlobalId Value` global table populated at
    `buildProjectEnv` time.

Design notes carried over from the plan (`enumerated-discovering-steele.md`):

1.  **Indices, not levels.** Indices give alpha-equivalence by construction:
    `\x -> x` and `\y -> y` both produce `RLambda { arity = 1, body = RLocal 0 }`.
    That matters for any future content-addressable cache on the IR itself.
    The "indices need shifting" objection only applies to rewriters, not strict
    evaluators that only ever prepend to the env.

2.  **Canonical form matters.** Record fields are stored sorted by name.
    Let-binding source order is preserved (semantically significant in Elm —
    later bindings can reference earlier ones). `RCase` branches are preserved
    in source order (first-match semantics).

3.  **No debug metadata on `RExpr`.** Source ranges, original local names, etc.
    live in a parallel sidecar (not yet defined) so that two `RExpr`s from the
    same declaration compare equal regardless of cosmetic debug differences.

4.  **Case expressions have no guards.** Elm 0.19 removed guards from case
    patterns, so the branch list is `(RPattern, RExpr)`, not
    `(RPattern, Maybe RExpr, RExpr)`.

5.  **`RLet` bindings are treated as a mutually-recursive group.** The
    evaluator pre-allocates all binding slots before evaluating any binding's
    body, so sibling references work uniformly. Elm's type checker already
    rejected recursive *value* bindings, so the interpreter doesn't need to
    re-enforce that rule.

6.  **Tuples are not n-ary lists.** Elm only has `(a, b)` and `(a, b, c)`.
    Matching the existing `Types.Value` variants (`Tuple`, `Triple`) keeps the
    evaluator paths uniform.

-}


{-| A resolved expression. The body of every top-level declaration is one of
these; the resolver produces them from `Elm.Syntax.Expression.Expression`.
-}
type RExpr
    = -- Literals
      RInt Int
    | RFloat Float
    | RString String
    | RChar Char
    | RUnit
      -- Variables
    | RLocal Int
    | RGlobal GlobalId
      -- Constructor and accessor references. Both can be partially applied.
    | RCtor { moduleName : List String, name : String }
    | RRecordAccessFunction String
      -- Control flow
    | RIf RExpr RExpr RExpr
    | RAnd RExpr RExpr
    | ROr RExpr RExpr
    | RCase RExpr (List ( RPattern, RExpr ))
      -- Binding forms
    | RLambda { arity : Int, body : RExpr }
    | RLet (List RLetBinding) RExpr
      -- Applications
    | RApply RExpr (List RExpr)
    | RNegate RExpr
      -- Collections
    | RList (List RExpr)
    | RTuple2 RExpr RExpr
    | RTuple3 RExpr RExpr RExpr
      -- Records. Fields are stored sorted by name for canonicalization.
    | RRecord (List ( String, RExpr ))
    | RRecordAccess RExpr String
      -- Record update targets a local slot because Elm's record update syntax
      -- is `{ name | field = ... }` where `name` must be a variable reference.
    | RRecordUpdate Int (List ( String, RExpr ))
      -- GLSL literals: content preserved as a raw string. The evaluator never
      -- inspects this — it's an opaque payload ferried to the runtime.
    | RGLSL String


{-| A single binding inside an `RLet`.

`arity` is `0` for simple value bindings and `> 0` for function bindings whose
body is wrapped in an `RLambda`. Storing the arity explicitly lets the
evaluator decide whether the binding produces a thunk (arity 0 — needs to be
evaluated at binding time) or a closure (arity > 0 — value is the lambda).

`debugName` is carried for error messages but is **not** part of the canonical
form — two bindings with identical `body` and `arity` but different
`debugName` should be treated as equal by any future content-addressing pass.

-}
type alias RLetBinding =
    { arity : Int
    , body : RExpr
    , debugName : String
    }


{-| A resolved pattern. Each pattern extends the locals environment by the
number of slots returned by `slotCount`.

The slot ordering convention: as the evaluator walks a pattern left-to-right,
it prepends each bound name to the locals list. So in `(x, y) as pair`, the
body sees `RLocal 0 = pair`, `RLocal 1 = y`, `RLocal 2 = x` — the innermost
binding is the most recently prepended.

-}
type RPattern
    = -- Matches anything, binds 1 slot.
      RPVar
      -- `_` — matches anything, binds no slots.
    | RPWildcard
      -- Matches `()`.
    | RPUnit
      -- Literal patterns. `RPFloat` is syntactically allowed by elm-syntax but
      -- the evaluator should reject it — Elm's language spec disallows
      -- pattern-matching on floats.
    | RPInt Int
    | RPFloat Float
    | RPChar Char
    | RPString String
      -- Composite patterns.
    | RPTuple2 RPattern RPattern
    | RPTuple3 RPattern RPattern RPattern
      -- Record destructuring: `{ x, y }` — each named field becomes one slot,
      -- stored sorted by name to match the canonical form of `RRecord`.
    | RPRecord (List String)
      -- `head :: tail`
    | RPCons RPattern RPattern
      -- `[a, b, c]`
    | RPList (List RPattern)
      -- `Just x`, `Maybe.Just x`, etc. The module name may be empty for
      -- unqualified constructors — the resolver should qualify them, but we
      -- keep the shape permissive to accommodate `True`/`False`/`Nothing`
      -- which are commonly written unqualified.
    | RPCtor { moduleName : List String, name : String } (List RPattern)
      -- `pattern as name` — binds the inner pattern's slots plus one more.
    | RPAs RPattern


{-| Count how many locals slots a pattern binds.

Matches the evaluator's convention: each `RPVar` or `RPAs` adds one slot;
wildcards, literals, and `()` add zero; composite patterns add the sum of
their children.

This is separate from the `RPattern` type so the resolver, evaluator, and any
future IR passes can all share one canonical count.

-}
slotCount : RPattern -> Int
slotCount pat =
    case pat of
        RPVar ->
            1

        RPWildcard ->
            0

        RPUnit ->
            0

        RPInt _ ->
            0

        RPFloat _ ->
            0

        RPChar _ ->
            0

        RPString _ ->
            0

        RPTuple2 a b ->
            slotCount a + slotCount b

        RPTuple3 a b c ->
            slotCount a + slotCount b + slotCount c

        RPRecord fields ->
            List.length fields

        RPCons head tail ->
            slotCount head + slotCount tail

        RPList items ->
            List.foldl (\item acc -> acc + slotCount item) 0 items

        RPCtor _ args ->
            List.foldl (\arg acc -> acc + slotCount arg) 0 args

        RPAs inner ->
            slotCount inner + 1


{-| A counter-interned top-level identifier. Assigned by the resolver during
`buildProjectEnv`, stable for the lifetime of a single `ProjectEnv`.

For now this is a fresh counter per `buildProjectEnv` call — not stable across
processes. Phase 5 will upgrade it to a content-addressed stable id derived
from `SemanticHash`, at which point cached `Value`s can be persisted to disk
and reused across runs. Until then the id is an in-process handle only.

-}
type alias GlobalId =
    Int
