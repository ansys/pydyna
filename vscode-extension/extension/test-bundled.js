#!/usr/bin/env node
/**
 * test-bundled.js
 *
 * Validates the bundled Python environment used by the LSP server by:
 *
 *   1. Ensuring the bundled Python runtime is present (fails fast if not —
 *      run `npm run fetch-python` first).
 *   2. (Re-)installing the server's Python dependencies into the bundled
 *      runtime via fetch-python.js's installPackages().
 *   3. Installing pytest into the bundled runtime (test-only, not shipped).
 *   4. Running server/tests/test_pydyna_env.py with the bundled Python.
 *
 * Usage:
 *   npm run test-bundled                   # normal (PyPI) mode
 *   PYDYNA_DEV_PATH=/path/to/pydyna npm run test-bundled   # dev/editable mode
 */

"use strict";

const path = require("path");
const { execFileSync } = require("child_process");
const { installPackages, bundledPythonExe } = require("./fetch-python.js");

const TEST_DIR = path.join(__dirname, "server", "tests");

function main() {
  // 1. Locate the bundled Python executable
  const pythonExe = bundledPythonExe();
  if (!pythonExe) {
    console.error(
      "\nERROR: Bundled Python not found.\n" +
      "Run `npm run fetch-python` to download and set it up, then retry.\n"
    );
    process.exit(1);
  }
  console.log(`\nBundled Python: ${pythonExe}`);

  // 2. Install / refresh server dependencies into the bundled runtime
  console.log("\n── Step 1: Install server dependencies ──────────────────────");
  installPackages(pythonExe);

  // 3. Install pytest (not part of the shipped bundle; test-only)
  console.log("\n── Step 2: Install pytest ────────────────────────────────────");
  execFileSync(
    pythonExe,
    ["-m", "pip", "install", "--quiet", "--disable-pip-version-check", "pytest>=7"],
    { stdio: "inherit" }
  );

  // 4. Run the tests
  console.log("\n── Step 3: Run tests ─────────────────────────────────────────");
  console.log(`   ${pythonExe} -m pytest ${TEST_DIR} -v\n`);
  execFileSync(
    pythonExe,
    ["-m", "pytest", TEST_DIR, "-v"],
    { stdio: "inherit" }
  );

  console.log("\nAll bundled-environment tests passed.\n");
}

main();
