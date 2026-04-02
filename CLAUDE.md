# Elm Interpreter

An Elm interpreter written in Elm. Parses Elm source code using `stil4m/elm-syntax` and evaluates it directly.

## Setup (required in worktrees)

`generated/`, `build/`, and `node_modules/` are gitignored. Before compiling or testing:

```bash
npm install
make all
```

## Tests

```bash
npx elm-test-rs
```

## Property-Based Tests

```bash
cd test/property-test
SEED=42 COUNT=20 bash run.sh
```

## codegen/ Directory

`codegen/` has two subdirectories under `Elm/` that look similar but serve different purposes:

- `codegen/Elm/Kernel/*.elm` — hand-written kernel function AST fallbacks (e.g. sort, string ops). The Makefile copies ONLY this `Kernel/` directory into the build.
- `codegen/Elm/src/*.elm` — committed copies of elm/core source files that `Generate.elm` reads to produce `generated/Core/*.elm`.

These are both checked in. Do not confuse them with the downloaded library sources in `build/` (which are gitignored).

**Important:** If `build/src/elm/kernel/0.0.0/src/Elm/` ever contains a `src/` subdirectory (not just `Kernel/`), it's stale — delete `build/` and re-run `make all`. Stale copies cause the codegen to process every module twice, producing duplicate declarations.

## Eval.Module Public API

The main consumer-facing functions in `src/Eval/Module.elm`:

- `buildProjectEnv : List String -> Result Error ProjectEnv` — parse sources and build a reusable project environment
- `parseProjectSources : List String -> Result Error (List { file, moduleName, interface })` — phase 1: parse only
- `buildProjectEnvFromParsed : List { file, moduleName, interface } -> Result Error ProjectEnv` — phase 2: build env from parsed modules
- `evalWithEnv : ProjectEnv -> List String -> Expression -> Result Error Value` — eval with additional source strings
- `evalWithEnvFromFiles : ProjectEnv -> List File -> Expression -> Result Error Value` — eval with pre-parsed File ASTs (skips re-parsing)
- `traceWithEnv : ProjectEnv -> List String -> Expression -> (Result Error Value, Rope CallTree, Rope String)` — eval with tracing (takes source strings)
- `eval : String -> Expression -> Result Error Value` — simple single-source eval
- `trace : String -> Expression -> (Result Error Value, Rope CallTree, Rope String)` — simple single-source eval with tracing

For build tools / mutation testing that parse once and eval many times, the typical flow is:
1. `buildProjectEnv` with shared library sources (once)
2. `evalWithEnvFromFiles` or `evalWithEnv` per mutation (many times)
