#!/usr/bin/env node

/**
 * Benchmark runner for the elm-interpreter.
 *
 * Usage:
 *   elm make benchmark/Benchmark.elm --optimize --output=benchmark/benchmark.js
 *   node benchmark/run.js
 *
 * The Elm app runs all benchmarks on init and reports via a port.
 * This script measures the total wall-clock time of init (which
 * includes all benchmark evaluations).
 */

const { performance } = require("perf_hooks");

// Load the compiled Elm module
const start = performance.now();
const { Elm } = require("./benchmark.js");

const app = Elm.Benchmark.init({ flags: null });

app.ports.reportResults.subscribe(function(results) {
  const elapsed = performance.now() - start;

  console.log(`Total time (--optimize): ${elapsed.toFixed(1)}ms\n`);

  if (Array.isArray(results)) {
    console.log("Benchmark results:");
    for (const b of results) {
      const status = b.status === "ok" ? "✓" : "✗";
      const detail = b.status === "ok" ? b.result : b.error;
      console.log(`  ${status} ${b.name}: ${detail}`);
    }
  }

  console.log(`\nNote: This measures ALL benchmarks in a single init call.`);
  console.log(`For per-benchmark timing, use the elm-test-rs suite.`);

  process.exit(0);
});

// Elm worker starts evaluation synchronously on init,
// but the port subscription fires asynchronously.
// Give it a moment.
setTimeout(() => {
  console.log("Timeout: benchmarks didn't complete within 30s");
  process.exit(1);
}, 30000);
