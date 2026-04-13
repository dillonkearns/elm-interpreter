module Eval.ResolvedIR exposing
    ( GlobalId
    , RExpr(..)
    , RLetBinding
    , RPattern(..)
    , freeVars
    , mkLambda
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
    | RLambda
        { arity : Int
        , body : RExpr
        , captureSlots : List Int
        }
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
      -- GLSL literals: the evaluator treats these as Unsupported, so we
      -- don't carry the raw source through the IR.
    | RGLSL


{-| A single binding inside an `RLet`.

`pattern` is `RPVar` for the common case (`let x = ...`, `let f a b = ...`)
and a composite pattern for destructuring lets (`let (a, b) = pair`). The
evaluator uses the pattern's slot count to determine how many local slots the
binding introduces.

`arity` is `0` for simple value bindings and destructuring lets, and `> 0` for
function bindings whose body is wrapped in an `RLambda`. Storing the arity
explicitly lets the evaluator decide whether the binding produces a thunk
(arity 0 — needs to be evaluated at binding time) or a closure (arity > 0 —
value is the lambda).

`debugName` is carried for error messages but is **not** part of the canonical
form — two bindings with identical `pattern`/`arity`/`body` but different
`debugName` should be treated as equal by any future content-addressing pass.

-}
type alias RLetBinding =
    { pattern : RPattern
    , arity : Int
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



-- SMART CONSTRUCTOR + FREE-VAR ANALYSIS FOR RLambda (PHASE 2)


{-| Smart constructor for `RLambda`. Computes `captureSlots` from the body's
free variables so the runtime can build a copy-on-capture closure containing
only the values the body actually references, rather than the entire
enclosing locals list.

The Phase 2 runtime ignores `captureSlots` — it still captures the whole
locals list like it always has. Phase 3 will consume the field: at
`RLambda` eval time it will slice the current locals by `captureSlots` and
store the fresh slice on the closure, giving the Perconti/Ahmed safe-for-
space property without changing the storage layer from `List`.

-}
mkLambda : Int -> RExpr -> RExpr
mkLambda arity body =
    RLambda
        { arity = arity
        , body = body
        , captureSlots = freeVarIndicesSorted arity body
        }


{-| Public free-var helper. Returns the set of de Bruijn indices that
reference OUTER-SCOPE locals from the given expression, assuming the
expression is evaluated with `binderDepth` innermost locals already bound
by enclosing binders (lambda params, let bindings, case patterns).

Indices in the returned set are re-expressed in the outer-scope frame
(i.e. `i - binderDepth` for each `RLocal i ≥ binderDepth` in the body).
Useful for reasoning about closure capture and for property-test
verification.

-}
freeVars : Int -> RExpr -> List Int
freeVars binderDepth expr =
    collectFreeVars binderDepth expr []
        |> dedupSortedAsc


{-| Internal: same as `freeVars` but returns a sorted List for storing on
`RLambda.captureSlots`. Exactly equivalent to `freeVars arity body` — the
Phase 2 runtime uses the sorted-list form.
-}
freeVarIndicesSorted : Int -> RExpr -> List Int
freeVarIndicesSorted =
    freeVars


{-| Collect outer-scope de Bruijn indices referenced by `expr`, assuming
`binderDepth` innermost locals are already bound. Accumulates into `acc`
and returns the extended list (unsorted, may contain duplicates).

The algorithm is a one-pass syntactic walk. No fixpoint. For each binder
form we recurse with a `binderDepth` shifted by the number of slots the
binder introduces for its sub-expression:

  - `RLocal i` — one entry iff `i >= binderDepth`, re-expressed as
    `i - binderDepth` (now relative to the outer scope).

  - `RLambda { arity, body }` — nested regular lambda. Its body runs with
    `arity` extra slots bound (the nested lambda's params). `selfSlots`
    for regular lambdas is `0`, so no self-reference offset.

  - `RLet bindings body` — sequential Elm let. Walk each binding body with
    depth shifted by the count of PRIOR binding slots. Function bindings
    (`arity > 0`, body is an `RLambda`) see themselves inside the body at
    slot `innerArity`, so we add `+ 1` on top of the nested lambda's
    arity shift to account for the self-reference slot that the
    evaluator's `selfSlots = 1` mechanism prepends at call time. The let
    body itself runs with depth shifted by the full `totalLetSlots`.

  - `RCase scrutinee branches` — scrutinee at current depth, each branch
    body at depth shifted by the branch pattern's `slotCount`.

All other forms just walk children at the current `binderDepth`.

-}
collectFreeVars : Int -> RExpr -> List Int -> List Int
collectFreeVars binderDepth expr acc =
    case expr of
        RLocal i ->
            if i >= binderDepth then
                (i - binderDepth) :: acc

            else
                acc

        RLambda lambda ->
            -- Regular nested lambda: selfSlots = 0, so the body runs
            -- with just `arity` additional bindings. No self-ref offset.
            collectFreeVars (binderDepth + lambda.arity) lambda.body acc

        RLet bindings letBody ->
            let
                totalLetSlots : Int
                totalLetSlots =
                    bindings
                        |> List.foldl (\b n -> n + slotCount b.pattern) 0

                walkBindings : Int -> List RLetBinding -> List Int -> List Int
                walkBindings depth bs inner =
                    case bs of
                        [] ->
                            inner

                        b :: rest ->
                            let
                                bindingAcc : List Int
                                bindingAcc =
                                    case b.body of
                                        RLambda nestedLambda ->
                                            if b.arity > 0 then
                                                -- let-bound recursive function:
                                                -- selfSlots = 1 for this RLambda,
                                                -- so walk the nested body with an
                                                -- extra +1 on top of the nested
                                                -- lambda's arity shift.
                                                collectFreeVars
                                                    (depth + nestedLambda.arity + 1)
                                                    nestedLambda.body
                                                    inner

                                            else
                                                -- Value binding that happens to
                                                -- evaluate to a lambda value; no
                                                -- self-ref (resolver never emits
                                                -- this shape today, but handle it
                                                -- uniformly).
                                                collectFreeVars depth b.body inner

                                        _ ->
                                            collectFreeVars depth b.body inner

                                -- Subsequent bindings see this binding's slots.
                                newDepth : Int
                                newDepth =
                                    depth + slotCount b.pattern
                            in
                            walkBindings newDepth rest bindingAcc

                bindingsAcc : List Int
                bindingsAcc =
                    walkBindings binderDepth bindings acc

                bodyAcc : List Int
                bodyAcc =
                    collectFreeVars (binderDepth + totalLetSlots) letBody bindingsAcc
            in
            bodyAcc

        RCase scrutinee branches ->
            let
                scrutAcc : List Int
                scrutAcc =
                    collectFreeVars binderDepth scrutinee acc
            in
            branches
                |> List.foldl
                    (\( pat, branchBody ) inner ->
                        collectFreeVars (binderDepth + slotCount pat) branchBody inner
                    )
                    scrutAcc

        RIf cond t f ->
            acc
                |> collectFreeVars binderDepth cond
                |> collectFreeVars binderDepth t
                |> collectFreeVars binderDepth f

        RAnd a b ->
            acc
                |> collectFreeVars binderDepth a
                |> collectFreeVars binderDepth b

        ROr a b ->
            acc
                |> collectFreeVars binderDepth a
                |> collectFreeVars binderDepth b

        RApply head args ->
            let
                headAcc : List Int
                headAcc =
                    collectFreeVars binderDepth head acc
            in
            args
                |> List.foldl (collectFreeVars binderDepth) headAcc

        RNegate inner ->
            collectFreeVars binderDepth inner acc

        RList items ->
            items
                |> List.foldl (collectFreeVars binderDepth) acc

        RTuple2 a b ->
            acc
                |> collectFreeVars binderDepth a
                |> collectFreeVars binderDepth b

        RTuple3 a b c ->
            acc
                |> collectFreeVars binderDepth a
                |> collectFreeVars binderDepth b
                |> collectFreeVars binderDepth c

        RRecord fields ->
            fields
                |> List.foldl (\( _, e ) inner -> collectFreeVars binderDepth e inner) acc

        RRecordAccess inner _ ->
            collectFreeVars binderDepth inner acc

        RRecordUpdate i fields ->
            let
                base : List Int
                base =
                    if i >= binderDepth then
                        (i - binderDepth) :: acc

                    else
                        acc
            in
            fields
                |> List.foldl (\( _, e ) inner -> collectFreeVars binderDepth e inner) base

        -- Leaf forms: no free vars
        RGlobal _ ->
            acc

        RInt _ ->
            acc

        RFloat _ ->
            acc

        RString _ ->
            acc

        RChar _ ->
            acc

        RUnit ->
            acc

        RCtor _ ->
            acc

        RRecordAccessFunction _ ->
            acc

        RGLSL ->
            acc


{-| Sort ascending and dedupe a list of ints. Produces a canonical form so
`captureSlots` is stable across resolver runs (Phase 5 content-addressing
will care). Uses `List.sort` + a linear pass rather than a Set for small
lists — typical lambda free-var sets are 0–5 elements.
-}
dedupSortedAsc : List Int -> List Int
dedupSortedAsc xs =
    xs
        |> List.sort
        |> dedupeSortedList


dedupeSortedList : List Int -> List Int
dedupeSortedList sorted =
    case sorted of
        [] ->
            []

        x :: rest ->
            x :: dedupeHelp x rest


dedupeHelp : Int -> List Int -> List Int
dedupeHelp prev remaining =
    case remaining of
        [] ->
            []

        x :: rest ->
            if x == prev then
                dedupeHelp prev rest

            else
                x :: dedupeHelp x rest
