#!/bin/bash
set -e

cd "$(dirname "$0")"

SEED=${SEED:-$RANDOM}
COUNT=${COUNT:-10}

echo "=== elm-interpreter Property Tests ==="
echo "Seed: $SEED  Count: $COUNT"
echo ""

# Install deps if needed
if [ ! -d "node_modules" ]; then
    echo "Installing dependencies..."
    npm install
fi

# Generate elm-codegen helpers if needed
if [ ! -d "codegen/Gen" ]; then
    echo "Generating elm-codegen helpers..."
    npx elm-codegen install
fi

# Bundle the generator script
echo "Bundling generator script..."
npx elm-pages bundle-script GenerateProgram --output generate.mjs 2>&1

# Clean previous generated files
rm -rf generated/src/*.elm generated/dist generated/manifest.json
mkdir -p generated/src generated/dist

# Generate programs
echo ""
echo "Generating $COUNT test programs with seed $SEED..."
node generate.mjs --seed "$SEED" --count "$COUNT"
echo ""

# Compile the interpreter runner
echo "Compiling interpreter runner..."
cd runner
elm make src/InterpreterRunner.elm --output interpreter.js 2>&1
cd ..

# Run the harness
echo ""
echo "Running property tests..."
echo ""
node harness.mjs

echo ""
echo "=== Done ==="
