#!/usr/bin/env node

/**
 * Runs a single named DictSetBenchmark case and reports wall-clock time.
 * Called by run-dict-set.js in a child process.
 */

const { performance } = require("perf_hooks");
const path = require("path");

const name = process.argv[2];
if (!name) {
  console.error("Usage: run-dict-set-one.js <benchmark-name>");
  process.exit(2);
}

const start = performance.now();
const { Elm } = require(path.join(__dirname, "dict-set-benchmark.js"));
const app = Elm.DictSetBenchmark.init({ flags: { benchmark: name } });

app.ports.reportResults.subscribe(function (result) {
  const elapsed = performance.now() - start;
  result.elapsed_ms = elapsed;
  console.log(JSON.stringify(result));
  process.exit(0);
});

setTimeout(() => {
  console.error(`Timeout after 60s: ${name}`);
  process.exit(1);
}, 60000);
