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
