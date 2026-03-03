#!/usr/bin/env node
/**
 * fetch-python.js
 *
 * Downloads a python-build-standalone CPython runtime appropriate for the
 * current platform, extracts it to ./python/, and installs pygls + lsprotocol
 * into it.  Run once before `npm run package`.
 *
 * Release: https://github.com/astral-sh/python-build-standalone
 */

"use strict";

const https = require("https");
const http = require("http");
const fs = require("fs");
const path = require("path");
const os = require("os");
const { execFileSync } = require("child_process");
const { createGunzip } = require("zlib");

// ── Configuration ────────────────────────────────────────────────────────────

const PYTHON_VERSION = "3.12.13";
const PBS_RELEASE = "20260303";
const DEST_DIR = path.join(__dirname, "python");

// Map Node platform/arch → python-build-standalone suffix
const PLATFORM_MAP = {
  "win32-x64":   { suffix: "x86_64-pc-windows-msvc-install_only_stripped.tar.gz",  pythonBin: path.join("python", "python.exe") },
  "darwin-arm64":{ suffix: "aarch64-apple-darwin-install_only_stripped.tar.gz",     pythonBin: path.join("python", "bin", "python3") },
  "darwin-x64":  { suffix: "x86_64-apple-darwin-install_only_stripped.tar.gz",      pythonBin: path.join("python", "bin", "python3") },
  "linux-x64":   { suffix: "x86_64-unknown-linux-gnu-install_only_stripped.tar.gz", pythonBin: path.join("python", "bin", "python3") },
  "linux-arm64": { suffix: "aarch64-unknown-linux-gnu-install_only_stripped.tar.gz",pythonBin: path.join("python", "bin", "python3") },
};

// ── Helpers ───────────────────────────────────────────────────────────────────

function platformKey() {
  const p = os.platform(); // win32 | darwin | linux
  const a = os.arch();     // x64 | arm64
  const k = `${p}-${a}`;
  if (!PLATFORM_MAP[k]) {
    throw new Error(`Unsupported platform: ${k}. Supported: ${Object.keys(PLATFORM_MAP).join(", ")}`);
  }
  return k;
}

function downloadFile(url, dest) {
  return new Promise((resolve, reject) => {
    console.log(`  Downloading ${url}`);
    const file = fs.createWriteStream(dest);
    const protocol = url.startsWith("https") ? https : http;

    function get(u) {
      protocol.get(u, (res) => {
        if (res.statusCode === 301 || res.statusCode === 302) {
          get(res.headers.location);
          return;
        }
        if (res.statusCode !== 200) {
          reject(new Error(`HTTP ${res.statusCode} for ${u}`));
          return;
        }
        const total = parseInt(res.headers["content-length"] || "0", 10);
        let received = 0;
        res.on("data", (chunk) => {
          received += chunk.length;
          if (total) {
            process.stdout.write(`\r  ${((received / total) * 100).toFixed(0)}%`);
          }
        });
        res.pipe(file);
        file.on("finish", () => { file.close(); process.stdout.write("\n"); resolve(); });
      }).on("error", reject);
    }
    get(url);
  });
}

function extractTarGz(tarPath, destDir) {
  console.log(`  Extracting to ${destDir} ...`);
  fs.mkdirSync(destDir, { recursive: true });
  // Use Node's built-in tar support (Node 18+ ships with @tapjs/which-pm, but
  // plain tar is universally available on macOS/Linux; on Windows we use
  // PowerShell's Expand-Archive via tar.exe which ships with Windows 10+).
  if (os.platform() === "win32") {
    execFileSync("tar", ["-xzf", tarPath, "-C", destDir], { stdio: "inherit" });
  } else {
    execFileSync("tar", ["-xzf", tarPath, "-C", destDir], { stdio: "inherit" });
  }
}

function installPackages(pythonExe) {
  const packages = ["pygls>=2.0", "lsprotocol>=2023.0"];
  console.log(`  Installing ${packages.join(", ")} into bundled Python ...`);
  execFileSync(
    pythonExe,
    ["-m", "pip", "install", "--quiet", "--disable-pip-version-check", ...packages],
    { stdio: "inherit" }
  );
}

// ── Main ──────────────────────────────────────────────────────────────────────

async function main() {
  const key = platformKey();
  const { suffix, pythonBin } = PLATFORM_MAP[key];
  const filename = `cpython-${PYTHON_VERSION}+${PBS_RELEASE}-${suffix}`;
  const url = `https://github.com/astral-sh/python-build-standalone/releases/download/${PBS_RELEASE}/${encodeURIComponent(filename)}`;

  // Skip if already set up (check for python executable)
  const pythonExe = path.join(DEST_DIR, pythonBin);
  if (fs.existsSync(pythonExe)) {
    console.log(`Bundled Python already present at ${pythonExe}, skipping download.`);
    console.log("Run `npm run fetch-python -- --force` to re-download.");
    if (!process.argv.includes("--force")) {
      return;
    }
  }

  console.log(`\nSetting up bundled Python for ${key}`);
  console.log(`  Version: CPython ${PYTHON_VERSION} (${PBS_RELEASE})`);

  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "pydyna-python-"));
  const tarPath = path.join(tmpDir, filename);

  try {
    // 1. Download
    await downloadFile(url, tarPath);

    // 2. Clean old extraction
    if (fs.existsSync(DEST_DIR)) {
      fs.rmSync(DEST_DIR, { recursive: true, force: true });
    }
    fs.mkdirSync(DEST_DIR, { recursive: true });

    // 3. Extract — the archive contains a top-level `python/` directory
    extractTarGz(tarPath, DEST_DIR);

    // After extraction the layout is DEST_DIR/python/... on all platforms
    // Verify the executable exists
    if (!fs.existsSync(pythonExe)) {
      throw new Error(`Expected Python executable not found at ${pythonExe}. Archive layout may have changed.`);
    }

    // 4. Install server dependencies
    installPackages(pythonExe);

    console.log(`\nBundled Python ready: ${pythonExe}`);
  } finally {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }
}

main().catch((err) => {
  console.error("\nERROR:", err.message);
  process.exit(1);
});
