# Elm Interpreter

An Elm interpreter written in Elm. Parses Elm source code using `stil4m/elm-syntax` and evaluates it directly.

## Initial Setup (required in worktrees)

The `generated/`, `build/`, and `node_modules/` directories are gitignored. You must regenerate them before compiling or running tests. Run these commands from the project root:

```bash
npm install            # install Node dependencies
touch yarn.lock        # Makefile expects yarn.lock to exist
make all               # downloads elm/core source, runs elm-codegen, produces generated/
```

**What `make all` does:**
1. `yarn install` — installs elm-codegen CLI
2. Downloads elm/core, elm/json, elm/regex, etc. source tarballs from GitHub into `build/`
3. Copies `codegen/Elm/Kernel/*.elm` into `build/src/elm/kernel/` (ONLY the `Kernel/` subdirectory — not `codegen/Elm/src/`)
4. Runs `elm-codegen run --flags-from build/src` — reads the library sources and produces `generated/Core/*.elm`

The `generated/` directory contains the Core library as Elm AST data structures (e.g. `generated/Core/Basics.elm` has all `Basics` function implementations as `FunctionImplementation` records). Without this step, compilation will fail with missing module errors.

**Common errors:**
- "No rule to make target 'yarn.lock'" → you forgot `touch yarn.lock`
- Missing `generated/Core/Basics.elm` → you need to run `make all`
- `SyntaxError: Invalid or unexpected token` when running interpreter.js → `generated/` files are stale or from a different branch, re-run `make all`

## Running Tests

```bash
npx elm-test-rs                    # run all unit tests
npx elm-test-rs --filter "sort"    # run tests matching a pattern
```

## Project Structure

- `src/` - Interpreter source code
  - `Eval.elm` - Main API (`eval`, `trace`)
  - `Eval/Expression.elm` - Core expression evaluator
  - `Eval/Module.elm` - Module-level evaluation, multi-module support (`evalProject`)
  - `Kernel.elm` - Kernel function registry (maps `Elm.Kernel.*` to implementations)
  - `Kernel/*.elm` - Native implementations (Basics, List, String, Json, etc.)
  - `Types.elm` - Value types, Error types, EvalResult
  - `Value.elm` - Value utilities and Debug.toString
- `generated/` - **gitignored** - Generated from `make all`, contains Core library AST (e.g. `generated/Core/Basics.elm`, `generated/Core/List.elm`)
- `build/` - **gitignored** - Intermediate build artifacts from `make all` (downloaded elm/core source tarballs, extracted source)
- `codegen/` - elm-codegen configuration and source for generating Core modules
  - `Generate.elm` - The elm-codegen generator that reads library source and produces `generated/`
  - `elm.codegen.json` - Declares which packages to generate codegen helpers for
  - `Elm/Kernel/*.elm` - **Checked in.** Hand-written Elm implementations of kernel functions (List sort, String ops, Parser). These are AST fallbacks used when no native kernel function is registered in `src/Kernel.elm`. The Makefile copies ONLY this `Kernel/` dir into `build/src/elm/kernel/0.0.0/src/Elm/Kernel/`.
  - `Elm/src/*.elm` - **Checked in.** Elm source files from elm/core (Basics.elm, List.elm, etc.). These are the source that `Generate.elm` reads to produce the generated AST. Do NOT confuse these with the downloaded sources in `build/` — these are committed reference copies.
  - `Gen/` - **gitignored** - Auto-generated elm-codegen helper modules (produced by `npx elm-codegen install`)
- `helpers/` - Helper modules (H.elm) used by generated code for AST node construction
- `tests/` - Test suite
  - `EndToEnd.elm` - Integration tests
  - `CoreTests/*.elm` - Tests for standard library functions
- `test/property-test/` - Property-based testing harness

## Property-Based Tests

The property test system generates random Elm programs, compiles them with `elm make`, runs the compiled JS, independently evaluates the same source through the interpreter, and compares results.

```bash
cd test/property-test
npm install
npx elm-codegen install
cd runner && elm make src/InterpreterRunner.elm --output interpreter.js && elm make src/MultiModuleRunner.elm --output multi-runner.js && cd ..
npx elm-pages bundle-script GenerateProgram --output generate.mjs
node generate.mjs --seed 42 --count 20
node harness.mjs
```

Or use the orchestrator: `SEED=42 COUNT=20 bash run.sh`

## Key Architecture Notes

- **Kernel functions** are registered in `src/Kernel.elm`. When the interpreter encounters `Elm.Kernel.Module.function`, it looks up the native implementation. If not found, it falls back to the AST implementation from `generated/Core/`.
- **`Elm.fn2` causes infinite loops in elm-codegen** when passed as an argument to `Gen.*.call_` functions. Always use nested `Elm.fn` instead. See `elm-codegen/.scratch/elm-fn2-infinite-loop-bug.md`.
- **Negative numbers at start of list literals** need a space: `[ -3 ]` works but `[-3]` fails to parse (elm-syntax limitation).
- **The interpreter's sort** uses a stable insertion sort in `Kernel/List.elm`. `sortBy` and `sortWith` are both registered as kernel functions.
