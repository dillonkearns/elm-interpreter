#!/bin/bash
set -e

cd "$(dirname "$0")"
ROOT="../.."

SEED=${SEED:-$RANDOM}
COUNT=${COUNT:-10}

echo "=== elm-interpreter Property Tests ==="
echo "Seed: $SEED  Count: $COUNT"
echo ""

# Ensure generated/ exists (run make all from root if needed)
if [ ! -f "$ROOT/generated/Core/Basics.elm" ]; then
    echo "Running make all to generate Core modules..."
    (cd "$ROOT" && npm install && make all)
fi

# Install property test deps if needed
if [ ! -d "node_modules" ]; then
    echo "Installing dependencies..."
    npm install
fi

# Generate elm-codegen helpers if needed
if [ ! -d "codegen/Gen" ]; then
    echo "Generating elm-codegen helpers..."
    npx elm-codegen install
fi

# Sync runner elm.json from root elm.json (avoids dependency drift)
echo "Syncing runner elm.json from root..."
node -e "
const fs = require('fs');
const root = JSON.parse(fs.readFileSync('$ROOT/elm.json', 'utf8'));
const runner = JSON.parse(fs.readFileSync('runner/elm.json', 'utf8'));
// Copy all deps from root, keeping runner's source-directories
runner.dependencies = JSON.parse(JSON.stringify(root.dependencies));
fs.writeFileSync('runner/elm.json', JSON.stringify(runner, null, 4) + '\n');
"

# Bundle the generator script
echo "Bundling generator script..."
npx elm-pages bundle-script GenerateProgram --output generate.mjs 2>&1

# Clean previous generated files
rm -rf generated/src generated/dist generated/manifest.json
mkdir -p generated/src generated/dist

# Generate programs
echo ""
echo "Generating $COUNT test programs with seed $SEED..."
node generate.mjs --seed "$SEED" --count "$COUNT"
echo ""

# Compile interpreter runners
echo "Compiling interpreter runners..."
cd runner
elm make src/InterpreterRunner.elm --output interpreter.js 2>&1
elm make src/MultiModuleRunner.elm --output multi-runner.js 2>&1
cd ..

# Run the harness
echo ""
echo "Running property tests..."
echo ""
node harness.mjs

echo ""
echo "=== Done ==="
