# PyDyna VS Code Extension

## Architecture Decision: Bundled Python Runtime

**Decision**: Bundle a self-contained Python interpreter using [python-build-standalone](https://github.com/astral-sh/python-build-standalone) rather than relying on a system Python installation.

**Context**: The extension's LSP server (`server/server.py`) is written in Python and depends on `pygls` and `lsprotocol`. The target audience is anyone editing LS-DYNA keyword files — they may or may not have Python installed, and almost certainly do not have `pygls` installed in any Python they do have.

**Alternatives considered**:

| Approach | Why rejected |
|---|---|
| Require user's system `python` | Users editing `.k` files are not necessarily Python users. Extension would silently fail for most people. |
| Use VS Code Python extension API to find interpreter | Same problem — requires user to have Python + pip install pygls manually |
| Bundle only `pygls`/`lsprotocol` as pure-Python wheels, inject via `PYTHONPATH` | Still requires system Python to be present and version-compatible (≥3.10) |
| Rewrite server in TypeScript | Loses direct access to PyDyna's `Deck` parser and `kwd.json` metadata — the entire value proposition |
| PyInstaller/Nuitka frozen binary | Fragile build, hard to debug, complex CI matrix |

**Chosen approach**: Download a `python-build-standalone` `install_only_stripped` tarball (~20-30 MB compressed) at extension build time, install `pygls` and `lsprotocol` into it, and include it in the `.vsix`. The extension spawns this bundled Python directly.

VS Code supports platform-specific `.vsix` files via `vsce package --target <platform>`, so we publish separate packages for:
- `win32-x64`
- `darwin-arm64`
- `darwin-x64`
- `linux-x64`

**Tradeoffs**:
- ✅ Zero user setup — works out of the box
- ✅ Hermetic — extension always uses the exact Python + deps it was built with
- ✅ Precedent — same approach used by Ruff, other language tooling
- ⚠️ Extension size ~30 MB (comparable to Jupyter, acceptable)
- ⚠️ Build step requires internet access (downloads ~20 MB tarball)
- ⚠️ Platform-specific build matrix needed for release

## Development Setup

```bash
# Install Node dependencies
cd vscode-extension/extension
npm install

# Download bundled Python + install server deps (run once, or after deps change)
npm run fetch-python

# Build and package
npm run package
```

The `fetch-python` step downloads a platform-appropriate `python-build-standalone` build and installs `pygls`/`lsprotocol` into `python/`. This directory is git-ignored.

## Installing locally

First time:

```bash
cd vscode-extension/extension
npm install
npm run package
code --install-extension pydyna-lsdyna-0.1.0.vsix
```

Then **Ctrl+Shift+P** → `Developer: Reload Window`.

### Iterating (rebuild + reinstall in one step)

```bash
npm run reinstall
```

This compiles, packages, uninstalls the old version, and installs the new one. Then reload VS Code with **Ctrl+Shift+P** → `Developer: Reload Window`.

Then open any `.k` file and hover over a word to verify the **Hello, World! 👋** popup.

## Unit testing

There are two ways to run the server unit tests.

### 1. Dev environment (fast)

Runs against your current Python. Good for quick iteration.

```bash
cd /c/AnsysDev/code/pyansys/pydyna
python -m pytest vscode-extension/extension/server/tests/ -v
```

### 2. Bundled Python (authoritative)

Installs all server dependencies into the standalone CPython at
`extension/python/python/python.exe`, then runs the tests there.
This is the meaningful validation — it proves the extension's hermetic
environment actually works, with no contamination from your dev Python.

Requires the bundled Python to already be present (`npm run fetch-python` first).

**PyPI mode** (tests against the published `ansys-dyna-core` package):

```bash
cd vscode-extension/extension
npm run test-bundled
```

**Dev mode** (tests against a local editable checkout of pydyna):

```bash
cd vscode-extension/extension
PYDYNA_DEV_PATH=/c/AnsysDev/code/pyansys/pydyna npm run test-bundled
```

### What the tests cover

| File | What it tests |
|---|---|
| `test_pydyna_env.py` | All required packages importable; `Deck.loads()` parses keywords; `FieldSchema` attributes accessible; property docstrings accessible |
| `test_documents.py` | `extract_keyword_block()` isolates correct block text and start-line; `DocumentStore` open/change/close lifecycle |