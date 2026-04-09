#!/usr/bin/env node

/**
 * Run a single DictSetBenchmark scenario with the debug (non-minified)
 * Elm bundle, so --cpu-prof frames carry real Elm function names.
 *
 * Usage:
 *   node --cpu-prof --cpu-prof-dir benchmark/profiles --cpu-prof-name <name>.cpuprofile \
 *        benchmark/profile-one.js <benchmark-name>
 */

const path = require("path");

const name = process.argv[2];
if (!name) {
  console.error("Usage: profile-one.js <benchmark-name>");
  process.exit(2);
}

const { Elm } = require(path.join(__dirname, "dict-set-benchmark-debug.js"));
const app = Elm.DictSetBenchmark.init({ flags: { benchmark: name } });

app.ports.reportResults.subscribe(function (result) {
  console.error(JSON.stringify(result));
  process.exit(0);
});

setTimeout(() => {
  console.error(`Timeout: ${name}`);
  process.exit(1);
}, 300000);
