import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';
import vm from 'vm';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const generatedDir = path.join(__dirname, 'generated');
const manifestPath = path.join(generatedDir, 'manifest.json');
const interpreterJsPath = path.join(__dirname, 'runner', 'interpreter.js');
const multiRunnerJsPath = path.join(__dirname, 'runner', 'multi-runner.js');

async function main() {
  if (!fs.existsSync(manifestPath)) {
    console.error('No manifest.json found. Run the generator first.');
    process.exit(1);
  }

  if (!fs.existsSync(interpreterJsPath)) {
    console.error('No interpreter.js found. Compile the interpreter runner first.');
    process.exit(1);
  }

  const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
  const interpreterJs = fs.readFileSync(interpreterJsPath, 'utf8');
  const interpreterScript = new vm.Script(interpreterJs, { filename: 'interpreter.js' });

  // Load multi-module runner if available
  let multiRunnerScript = null;
  if (fs.existsSync(multiRunnerJsPath)) {
    const multiRunnerJs = fs.readFileSync(multiRunnerJsPath, 'utf8');
    multiRunnerScript = new vm.Script(multiRunnerJs, { filename: 'multi-runner.js' });
  }

  console.log(`Running property tests for ${manifest.length} generated programs...\n`);

  let passed = 0;
  let failed = 0;
  const failures = [];

  for (const entry of manifest) {
    const isMulti = typeof entry === 'object' && entry.main;
    const displayName = isMulti ? `${entry.main} (multi)` : entry;

    try {
      const result = isMulti
        ? await runMultiModuleTest(entry, interpreterScript, multiRunnerScript)
        : await runPropertyTest(entry, interpreterScript);

      if (result.match) {
        passed++;
        process.stdout.write(`  ${displayName}: PASS\n`);
      } else {
        failed++;
        failures.push(result);
        process.stdout.write(`  ${displayName}: FAIL\n`);
        if (result.compileError) {
          console.log(`    Compile error: ${result.compileError}`);
        }
        if (result.mismatch) {
          console.log(`    Expected (elm compiler): ${JSON.stringify(result.elmOutput).slice(0, 200)}`);
          console.log(`    Got (interpreter):       ${JSON.stringify(result.interpreterOutput).slice(0, 200)}`);
        }
        if (result.elmError) {
          console.log(`    Elm runtime error: ${result.elmError}`);
        }
        if (result.interpreterError) {
          console.log(`    Interpreter error: ${result.interpreterError}`);
        }
      }
    } catch (e) {
      failed++;
      failures.push({ moduleName: displayName, error: e.message });
      process.stdout.write(`  ${displayName}: ERROR - ${e.message}\n`);
    }
  }

  console.log(`\nResults: ${passed} passed, ${failed} failed out of ${manifest.length} total`);

  if (failed > 0) {
    console.log('\nFailures:');
    for (const f of failures) {
      console.log(`  - ${f.moduleName}: ${f.error || f.compileError || 'output mismatch'}`);
    }
    process.exit(1);
  }
}

async function runPropertyTest(moduleName, interpreterScript) {
  const elmFile = path.join(generatedDir, 'src', `${moduleName}.elm`);
  const source = fs.readFileSync(elmFile, 'utf8');
  const distDir = path.join(generatedDir, 'dist');

  fs.mkdirSync(distDir, { recursive: true });

  // 1. Compile with elm make
  const compiledJsPath = path.join(distDir, `${moduleName}.js`);
  try {
    execSync(
      `elm make ${elmFile} --output=${compiledJsPath}`,
      { cwd: path.join(__dirname), stdio: 'pipe' }
    );
  } catch (e) {
    return {
      moduleName,
      match: false,
      compileError: e.stderr ? e.stderr.toString().slice(0, 500) : e.message,
    };
  }

  // 2. Run the compiled Elm via Node VM to get expected output
  let elmOutput;
  let elmError;
  try {
    const compiledJs = fs.readFileSync(compiledJsPath, 'utf8');
    elmOutput = await runElmInSandbox(compiledJs, moduleName);
  } catch (e) {
    elmError = e.message;
  }

  // 3. Run through the interpreter to get actual output
  let interpreterOutput;
  let interpreterError;
  try {
    interpreterOutput = await runInterpreter(interpreterScript, source);
  } catch (e) {
    interpreterError = e.message;
  }

  // 4. Clean up compiled file
  try { fs.unlinkSync(compiledJsPath); } catch (_) {}

  // 5. Compare
  if (elmError || interpreterError) {
    return {
      moduleName,
      match: false,
      elmOutput,
      interpreterOutput,
      elmError,
      interpreterError,
    };
  }

  // Check if interpreter returned an error string
  if (interpreterOutput && interpreterOutput.startsWith('INTERPRETER_')) {
    return {
      moduleName,
      match: false,
      elmOutput,
      interpreterOutput,
      interpreterError: interpreterOutput,
    };
  }

  const match = elmOutput === interpreterOutput;
  return {
    moduleName,
    match,
    elmOutput,
    interpreterOutput,
    mismatch: !match,
  };
}

async function runMultiModuleTest(entry, interpreterScript, multiRunnerScript) {
  const { main: mainModuleName, helpers } = entry;
  const moduleName = mainModuleName;
  const mainFile = path.join(generatedDir, 'src', `${mainModuleName}.elm`);
  const distDir = path.join(generatedDir, 'dist');

  fs.mkdirSync(distDir, { recursive: true });

  // 1. Compile main module with elm make (it will find helpers automatically)
  const compiledJsPath = path.join(distDir, `${mainModuleName}.js`);
  try {
    execSync(
      `elm make ${mainFile} --output=${compiledJsPath}`,
      { cwd: path.join(__dirname), stdio: 'pipe' }
    );
  } catch (e) {
    return {
      moduleName,
      match: false,
      compileError: e.stderr ? e.stderr.toString().slice(0, 500) : e.message,
    };
  }

  // 2. Run compiled Elm
  let elmOutput;
  let elmError;
  try {
    const compiledJs = fs.readFileSync(compiledJsPath, 'utf8');
    elmOutput = await runElmInSandbox(compiledJs, mainModuleName);
  } catch (e) {
    elmError = e.message;
  }

  // 3. Run through multi-module interpreter
  let interpreterOutput;
  let interpreterError;

  if (!multiRunnerScript) {
    interpreterError = 'Multi-module runner not compiled';
  } else {
    try {
      // Read all source files: helpers first, then main
      const sources = [];
      for (const helper of helpers) {
        sources.push(fs.readFileSync(path.join(generatedDir, 'src', `${helper}.elm`), 'utf8'));
      }
      sources.push(fs.readFileSync(mainFile, 'utf8'));

      interpreterOutput = await runMultiInterpreter(multiRunnerScript, sources);
    } catch (e) {
      interpreterError = e.message;
    }
  }

  // 4. Clean up
  try { fs.unlinkSync(compiledJsPath); } catch (_) {}

  // 5. Compare
  if (elmError || interpreterError) {
    return {
      moduleName,
      match: false,
      elmOutput,
      interpreterOutput,
      elmError,
      interpreterError,
    };
  }

  if (interpreterOutput && interpreterOutput.startsWith('INTERPRETER_')) {
    return {
      moduleName,
      match: false,
      elmOutput,
      interpreterOutput,
      interpreterError: interpreterOutput,
    };
  }

  const match = elmOutput === interpreterOutput;
  return {
    moduleName,
    match,
    elmOutput,
    interpreterOutput,
    mismatch: !match,
  };
}

function runElmInSandbox(jsSource, moduleName) {
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error(`Timeout running ${moduleName}`));
    }, 10000);

    try {
      const sandbox = {
        setTimeout: globalThis.setTimeout,
        clearTimeout: globalThis.clearTimeout,
        setInterval: globalThis.setInterval,
        clearInterval: globalThis.clearInterval,
        console: { log() {}, warn() {}, error() {} },
      };

      const context = vm.createContext(sandbox);
      const script = new vm.Script(jsSource, { filename: `${moduleName}.js` });
      script.runInContext(context);

      const elmApp = sandbox.Elm[moduleName];
      if (!elmApp) {
        clearTimeout(timeout);
        reject(new Error(`Module ${moduleName} not found in Elm object. Available: ${Object.keys(sandbox.Elm || {})}`));
        return;
      }

      const app = elmApp.init({ flags: {} });
      app.ports.output.subscribe((value) => {
        clearTimeout(timeout);
        resolve(value);
      });
    } catch (e) {
      clearTimeout(timeout);
      reject(e);
    }
  });
}

function runInterpreter(interpreterScript, source) {
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error('Interpreter timeout'));
    }, 30000);

    try {
      const sandbox = {
        setTimeout: globalThis.setTimeout,
        clearTimeout: globalThis.clearTimeout,
        setInterval: globalThis.setInterval,
        clearInterval: globalThis.clearInterval,
        console: { log() {}, warn() {}, error() {} },
      };

      const context = vm.createContext(sandbox);
      interpreterScript.runInContext(context);

      const app = sandbox.Elm.InterpreterRunner.init({ flags: source });
      app.ports.output.subscribe((value) => {
        clearTimeout(timeout);
        resolve(value);
      });
    } catch (e) {
      clearTimeout(timeout);
      reject(e);
    }
  });
}

function runMultiInterpreter(multiRunnerScript, sources) {
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error('Multi-module interpreter timeout'));
    }, 30000);

    try {
      const sandbox = {
        setTimeout: globalThis.setTimeout,
        clearTimeout: globalThis.clearTimeout,
        setInterval: globalThis.setInterval,
        clearInterval: globalThis.clearInterval,
        console: { log() {}, warn() {}, error() {} },
      };

      const context = vm.createContext(sandbox);
      multiRunnerScript.runInContext(context);

      const flagsJson = JSON.stringify(sources);
      const app = sandbox.Elm.MultiModuleRunner.init({ flags: flagsJson });
      app.ports.output.subscribe((value) => {
        clearTimeout(timeout);
        resolve(value);
      });
    } catch (e) {
      clearTimeout(timeout);
      reject(e);
    }
  });
}

main();
