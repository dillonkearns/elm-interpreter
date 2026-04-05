#!/usr/bin/env node

/**
 * Profile runner. Use with:
 *   elm make benchmark/Profile.elm --optimize --output=benchmark/profile.js
 *   node --prof benchmark/run-profile.js
 *   node --prof-process isolate-*.log > profile.txt
 *
 * Or for a flame graph:
 *   node --cpu-prof benchmark/run-profile.js
 *   # Open the .cpuprofile in Chrome DevTools Performance tab
 */

const { performance } = require("perf_hooks");

const { Elm } = require("./profile.js");

const start = performance.now();

const app = Elm.Profile.init({ flags: null });

app.ports.reportDone.subscribe(function (result) {
  const elapsed = performance.now() - start;
  console.log(`\nCompleted in ${elapsed.toFixed(1)}ms\n`);
  console.log(result);
  process.exit(0);
});

setTimeout(() => {
  console.log("Timeout after 120s");
  process.exit(1);
}, 120000);
