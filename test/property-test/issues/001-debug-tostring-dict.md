# Bug: Debug.toString shows raw internal structure for Dict and Set

## Summary

`Debug.toString` on `Dict` and `Set` values shows the internal Red-Black tree structure instead of the user-friendly `Dict.fromList [...]` / `Set.fromList [...]` format that the Elm compiler produces.

## Reproduction

```elm
import Dict
import Set

Debug.toString (Dict.fromList [("a", 1)])
Debug.toString (Set.fromList [1, 2, 3])
```

### Dict

**Expected (Elm compiler):**
```
Dict.fromList [("a",1)]
```

**Actual (interpreter):**
```
RBNode_elm_builtin Black "a" 1 RBEmpty_elm_builtin RBEmpty_elm_builtin
```

### Set

**Expected (Elm compiler):**
```
Set.fromList [1,2,3]
```

**Actual (interpreter):**
```
Set_elm_builtin (RBNode_elm_builtin Black 2 () (RBNode_elm_builtin Black 1 () RBEmpty_elm_builtin RBEmpty_elm_builtin) (RBNode_elm_builtin Black 3 () RBEmpty_elm_builtin RBEmpty_elm_builtin))
```

## Other types (Array, Record, Result, Tuple, Maybe) match correctly.

## Analysis

The interpreter's `Debug.toString` implementation uses the generic `Value.toString` which renders `Custom` value nodes directly, showing internal constructor names (`RBNode_elm_builtin`, `RBEmpty_elm_builtin`, `Set_elm_builtin`). It needs special handling to recognize Dict/Set internal structures and convert them to canonical `Dict.fromList`/`Set.fromList` representation.

## Impact

- Affects `Debug.toString` output for `Dict` and `Set` values only
- Does not affect Dict/Set functionality — only the debug string representation
- Other types (Array, Record, Maybe, Result, Tuple, primitives) are correct

## Found by

Property-based testing comparison (targeted Dict/Set test)
