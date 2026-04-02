# Bug: Parser fails on negative numbers at start of list literal without space

## Summary

The interpreter's parser rejects negative number literals immediately after `[` in list literals (no whitespace), while the Elm compiler accepts them.

## Reproduction

```elm
-- FAILS in interpreter:
List.sort [-3, 5, -1, 0, 4, -7, 2]

-- WORKS in interpreter:
List.sort [ -3, 5, -1, 0, 4, -7, 2 ]
```

**Error message:**
```
ExpectingSymbol "]"
Problem "if a negation sign is not preceded by whitespace, it's considered subtraction"
```

## Analysis

The parser treats `-3` after `[` as the subtraction operator being applied to `[` and `3`, rather than as a negative integer literal. When there's a space (`[ -3`), the parser correctly interprets `-3` as a negation/negative literal.

The Elm compiler handles both forms identically.

This affects:
- `[-3]` — negative at start of list
- `[1, -3]` likely works because `,` provides clear token separation (confirmed: `[1, -3]` works)
- The issue is specifically `[-` being ambiguous to the parser

## Impact

- Programs with negative numbers in list literals without leading spaces will fail to parse
- This is a common Elm code style (elm-format uses `[ -3 ]` with spaces, but hand-written code often uses `[-3]`)
- Generated code from tools may not always include the leading space

## Found by

Property-based testing comparison (SortStable test with `List.sort [-3, 5, -1, 0, 4, -7, 2]`)
