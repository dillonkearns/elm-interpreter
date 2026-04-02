# Bug: List.sortBy is not stable — elements with equal keys are reordered

## Summary

The interpreter's `List.sortBy` does not preserve the relative order of elements with equal sort keys. Elm's `List.sortBy` is guaranteed to be a stable sort.

## Reproduction

```elm
List.sortBy .score
    [ { name = "foo", score = -4 }
    , { name = "foo", score = 2 }
    , { name = "alpha", score = 2 }
    ]
```

After sorting by `.score`, the elements with `score = 2` should preserve their original order.

**Expected (Elm compiler, stable sort):**
```
[ { name = "foo", score = -4 }
, { name = "foo", score = 2 }     -- original order preserved
, { name = "alpha", score = 2 }   -- original order preserved
]
```
Names extracted: `foo, foo, alpha`

**Actual (interpreter, unstable sort):**
```
[ { name = "foo", score = -4 }
, { name = "alpha", score = 2 }   -- swapped!
, { name = "foo", score = 2 }     -- swapped!
]
```
Names extracted: `foo, alpha, foo`

## Analysis

The interpreter's sort implementation (in `Kernel/List.elm`, `sortWith` function) likely uses an algorithm that doesn't guarantee stability, or has a bug in its equality handling. The Elm compiler's runtime uses a stable insertion sort.

Looking at `src/Kernel/List.elm`, the `sortWith` function uses insertion sort which SHOULD be stable, but the comparison logic might have an issue where equal elements are being swapped instead of preserved in place.

## Impact

- Affects `List.sortBy` and potentially `List.sortWith` when elements have equal sort keys
- Can cause incorrect ordering in any code that relies on sort stability
- This is a semantic correctness bug, not just a cosmetic issue

## Reproduction seed

Property test seed 42, Test24, count 30

## Found by

Property-based testing comparison (random program generation with `List.sortBy` on records with duplicate sort keys)
