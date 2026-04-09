#!/usr/bin/env node

/**
 * Per-benchmark runner for DictSetBenchmark.elm.
 *
 * Usage:
 *   elm make benchmark/DictSetBenchmark.elm --optimize --output=benchmark/dict-set-benchmark.js
 *   node benchmark/run-dict-set.js
 *
 * Runs each named benchmark in a fresh Elm worker init (separate process),
 * measuring wall-clock time per benchmark.
 */

const { spawnSync } = require("child_process");
const path = require("path");

const BENCHMARKS = [
  "dict_insert_string_1k",
  "dict_insert_string_5k",
  "dict_insert_tuple_1k",
  "dict_union_string_1k",
  "dict_member_string_1k",
  "dict_get_string_1k",
  "dict_foldl_string_1k",
  "set_insert_string_1k",
  "set_union_string_1k",
  "set_member_string_1k",
  "review_rule_mock",
];

const runnerPath = path.join(__dirname, "run-dict-set-one.js");

const results = [];
for (const name of BENCHMARKS) {
  const result = spawnSync("node", ["--stack-size=8192", runnerPath, name], {
    encoding: "utf8",
    cwd: path.dirname(__dirname),
  });
  if (result.status !== 0) {
    console.error(`FAIL ${name}: exit ${result.status}`);
    console.error(result.stderr);
    console.error(result.stdout);
    continue;
  }
  try {
    const parsed = JSON.parse(result.stdout.trim().split("\n").pop());
    results.push(parsed);
    const resultStr = parsed.status === "ok" ? parsed.result : parsed.error;
    console.log(`  ${parsed.status === "ok" ? "✓" : "✗"} ${name.padEnd(28)} ${parsed.elapsed_ms.toFixed(1).padStart(8)}ms  ${resultStr}`);
  } catch (err) {
    console.error(`FAIL ${name}: could not parse output`);
    console.error(result.stdout);
  }
}

process.exit(0);
