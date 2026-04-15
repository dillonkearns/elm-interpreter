module NormalizationFlags exposing (NormalizationFlags, all, default, experimental, none, packageAggressive)

{-| Feature flags for individual normalization passes.
Toggle independently to measure impact of each optimization.

The old `fold*` flags only affect the standalone `foldWithFlags` pass,
which has been removed from the hot path. They're retained for any
future experiments that want to re-enable constant-folding in a
targeted way.

The `fixpoint*` and `listFusion*` flags gate the two passes that run
unconditionally today in `runModuleNormalizationToFixpoint`. These
are the knobs the A/B benchmark harness wants.

-}


type alias NormalizationFlags =
    { foldConstantApplications : Bool
    , inlinePrecomputedRefs : Bool
    , inlineFunctions : Bool
    , inlineFunctionMaxSize : Int
    , fuseListMaps : Bool

    -- The two passes that run on every cold-user load. `runFixpoint`
    -- controls the 0-arg-constant `tryNormalizeConstant` fixpoint;
    -- `runListFusion` controls the per-body AST walk that does spine
    -- collection + list-map fusion. `fixpointPasses` caps the fixpoint
    -- at that many iterations (the current default is 3).
    , runFixpoint : Bool
    , runListFusion : Bool
    , fixpointPasses : Int

    -- Max evaluator steps budgeted for a single `tryNormalizeConstant`
    -- call. Expensive constants (e.g. `Array.initialize 65371 ...`)
    -- bail out once they exceed this, leaving the original AST
    -- untouched. The result is a runtime re-eval per reference, which
    -- is an acceptable tradeoff on workloads that don't hot-loop the
    -- big constant.
    , tryNormalizeMaxSteps : Int
    }


all : NormalizationFlags
all =
    { foldConstantApplications = True
    , inlinePrecomputedRefs = True
    , inlineFunctions = True
    , inlineFunctionMaxSize = 30
    , fuseListMaps = True
    , runFixpoint = True
    , runListFusion = True
    , fixpointPasses = 3
    , tryNormalizeMaxSteps = 10000000
    }


none : NormalizationFlags
none =
    { foldConstantApplications = False
    , inlinePrecomputedRefs = False
    , inlineFunctions = False
    , inlineFunctionMaxSize = 0
    , fuseListMaps = False
    , runFixpoint = False
    , runListFusion = False
    , fixpointPasses = 0
    , tryNormalizeMaxSteps = 0
    }


{-| Today's production default. Historical settings were
`fixpointPasses = 3`, but on the core-extra 8-file subset this was
costing ~1.3 s of cold-user load time without any measured benefit
— pass 2 was where the expensive `tryNormalizeConstant` evals
happened (pass 1 bails on candidates whose same-module deps come
later in alphabetical order, pass 2 re-runs them after pass 1 has
populated `acc.precomputed`). Since pass 1 alone recovers the Apr 11
baseline on the 8-file subset (1327 ms vs 1265 ms) and keeps the
full-set within noise (10192 ms vs 10097 ms fully disabled), 1 pass
is now the default.

The tradeoff: constants with alphabetically-wrong dep ordering stop
getting pre-computed at load time and fall back to runtime re-eval
per reference. Callers who know their project hits this pattern
(e.g. `String.Diacritics.lookupArray ← lookupTable`) can bump
`fixpointPasses` to 2 or 3 via the flag plumbing.

-}
default : NormalizationFlags
default =
    { foldConstantApplications = False
    , inlinePrecomputedRefs = False
    , inlineFunctions = False
    , inlineFunctionMaxSize = 30
    , fuseListMaps = True
    , runFixpoint = True
    , runListFusion = True
    , fixpointPasses = 1
    , tryNormalizeMaxSteps = 10000000
    }


{-| Single place to edit while running the A/B matrix. Every call
site that reaches `runModuleNormalizationToFixpoint` defaults to
this value, so swapping one field here and rebuilding is all it
takes to run a new experiment.
-}
experimental : NormalizationFlags
experimental =
    default


{-| Dependency/package-only body optimization profile.

This is intentionally more aggressive than `default`, because package
summary normalization is cached to disk and reused across many user-project
loads. That makes it a good place to spend extra work on:

  - inlining small helper functions
  - substituting already-precomputed zero-arg refs

We keep constant-application folding off here because it eagerly invokes the
evaluator across arbitrary subexpressions and is a much bigger compile-time
multiplier.

-}
packageAggressive : NormalizationFlags
packageAggressive =
    { default
        | inlinePrecomputedRefs = True
        , inlineFunctions = True
        , inlineFunctionMaxSize = 30
    }
