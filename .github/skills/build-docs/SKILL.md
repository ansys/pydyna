---
name: build-docs
description: >
  Build PyDyna documentation locally. Use for: previewing docs changes,
  building HTML docs without running examples or auto-keyword API generation,
  building the full docs, checking for doc build errors or warnings.
argument-hint: "doc target or flag (e.g. html, clean, BUILD_EXAMPLES=true)"
---

# Build Docs Skill

## Quick Build (no examples, no auto-keyword API)

This is the fastest way to preview documentation changes:

```bash
BUILD_EXAMPLES=false BUILD_AUTOKEYWORDS_API=false ./doc/make.bat html
```

On macOS/Linux use `make` directly:

```bash
BUILD_EXAMPLES=false BUILD_AUTOKEYWORDS_API=false make -C doc html
```

## Build Options

| Variable | Default | Effect when `false` |
|----------|---------|---------------------|
| `BUILD_EXAMPLES` | `true` | Skip Jupyter example execution |
| `BUILD_AUTOKEYWORDS_API` | `true` | Skip auto-keyword API page generation (slow) |

## Full Build

```bash
make -C doc html
```

Runs all examples and generates the full auto-keyword API. Slow — use only before release.

## Clean Build

```bash
make -C doc clean
make -C doc html
```

Use when Sphinx caches produce stale output.

## Output

Built HTML is in `doc/build/html/`. Open `doc/build/html/index.html` in a browser.

## Troubleshooting

### Build fails with import errors
Ensure the package is installed in the active Python environment:
```bash
pip install -e .
```

### `make` not found on Windows
Use `./doc/make.bat` instead of `make -C doc`.

### Slow build
Always set `BUILD_EXAMPLES=false BUILD_AUTOKEYWORDS_API=false` for local iteration.
Only enable them for pre-release validation.
