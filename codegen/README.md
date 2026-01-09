# Auto-keyword Class Generator

The PyDyna keyword class generator produces Python classes for LS-DYNA keywords based on specifications in `kwd.json`, `manifest.json`, and `additional-cards.json`.
**⚠️ `kwd.json` is machine-generated from the LS-DYNA manual and MUST NOT be manually modified.**

For architecture details, see [architecture.md](architecture.md).

## Installation

```bash
pip install .[codegen]
```

## Basic Usage

```bash
# Generate all keyword classes
python codegen/generate.py

# Generate to a specific output directory
python codegen/generate.py -o /path/to/keyword_classes

# Generate a single keyword (for testing)
python codegen/generate.py -k SECTION_SHELL

# Clean all generated code
python codegen/generate.py -c
```

## Validation

**Key Principle**: If generated code doesn't change, tests don't need to run. The codegen system's correctness is validated by confirming that output remains unchanged.

### When to Run What

| Scenario | Validation Required |
|----------|---------------------|
| Refactoring codegen internals | `generate.py` + `git diff` (no changes = success) |
| Adding new handler/feature | `generate.py` + `git diff` + review intentional changes + run keywords tests|
| Changing generated output intentionally | `generate.py` + `git diff` + commit changes + run keywords tests |
| Modifying runtime keyword behavior | Run tests (`pytest tests/ -m keywords`) |

Note: only keywords tests are required because the codegen does not affect the "run", "pre", or "solver" modules.

### Validation Script

```bash
# Quick validation for fast iteration (clean, generate, git diff only)
# This is sufficient for most codegen refactoring work
bash codegen/validate.sh --quick

# Full validation (includes pre-commit, dead code, unit tests)
# Use before committing or when generated output intentionally changes
bash codegen/validate.sh

# Custom validation
bash codegen/validate.sh --skip-tests       # Skip unit tests
bash codegen/validate.sh --skip-precommit   # Skip pre-commit hooks
bash codegen/validate.sh --skip-deadcode    # Skip dead code detection

# Adjust coverage threshold
bash codegen/validate.sh --coverage-threshold 90

# Verbose output
bash codegen/validate.sh --verbose
```

### Manual Output Validation

```bash
python codegen/generate.py -c
python codegen/generate.py
git diff src/ansys/dyna/core/keywords/keyword_classes/auto/
```

If `git diff` shows changes:
1. The codegen logic has a bug that needs fixing, OR
2. The changes are intentional and should be committed

## Logging

Control verbosity with `--log-level` or `-l`:

```bash
# Detailed debug output
python codegen/generate.py -k SECTION_SHELL -l DEBUG

# Minimal output (warnings and errors only)
python codegen/generate.py -l WARNING

# Default (INFO level)
python codegen/generate.py -l INFO
```

**Log levels**:
- `DEBUG`: Execution flow, variable values, handler decisions, card insertions/deletions, wildcard matching, template rendering, file operations
- `INFO`: Progress updates, completion status, files loaded, keyword counts (default)
- `WARNING`: Recoverable issues, deprecated features
- `ERROR`: Failures with stack traces
- `CRITICAL`: Critical failures

## Bash Aliases

Add to `~/.bashrc` or `~/.bash_profile`:

```bash
alias pydyna='cd /c/AnsysDev/code/pyansys/pydyna'
alias codegen-validate='bash codegen/validate.sh'
alias codegen-quick='bash codegen/validate.sh --quick'
alias codegen-clean='python codegen/generate.py -c && python codegen/generate.py'
alias codegen-test='pytest -m codegen'
alias codegen-check='bash codegen/validate.sh --skip-tests --skip-deadcode'
alias codegen-dev='python codegen/generate.py -l DEBUG'
```

## Agent Guide: Adding Optional Keyword Features

When keywords need optional features (like `_ID` suffix for RIGIDWALL keywords), **prefer options over multiple class generation**.

### Prefer: `add-option` (Single Class with Options)

Creates one class with optional behavior controlled by setting properties:

```json
{
    "type": "prefix",
    "patterns": ["RIGIDWALL_GEOMETRIC", "RIGIDWALL_PLANAR"],
    "generation-options": {
        "skip-card": [{"ref": "id_title_card"}],
        "add-option": [{
            "card-order": -2,
            "title-order": 1,
            "cards": [{"source": "additional-cards", "card-name": "ID_TITLE"}],
            "option-name": "ID"
        }]
    },
    "labels": {"id_title_card": 0}
}
```

**Result**: `RigidwallGeometricFlat` class where setting `wall.id = 1` activates the ID option and writes `*RIGIDWALL_GEOMETRIC_FLAT_ID`.

### Avoid: `type: multiple` (Duplicate Classes)

Creates separate classes for each variant:

```json
"RIGIDWALL_GEOMETRIC_FLAT": {
    "type": "multiple",
    "generations": [
        {"keyword": "RIGIDWALL_GEOMETRIC_FLAT", ...},
        {"keyword": "RIGIDWALL_GEOMETRIC_FLAT_ID", ...}
    ]
}
```

**Problem**: Duplicates class count, requires explicit entries for each keyword variant.

### Key Concepts

1. **`add-option`** adds optional cards that activate when properties are set
   - `title-order`: Position in keyword title suffix (1 = first, e.g., `_ID`)
   - `card-order`: Position in card list (-2 = before all base cards)
   - `cards`: List of card definitions (from `additional-cards.json` or `kwd-data`)

2. **`skip-card`** removes cards from base generation (use with `add-option` to replace existing cards with optional versions)
   - Requires a `ref` to a label defined in the `labels` section
   - `labels` map names to card indices (0-based)

3. **Wildcards** apply settings to multiple keywords matching a pattern
   - `type: prefix` matches keywords starting with the pattern
   - Can include `labels` for use with `skip-card` refs
   - If a card already exists in `kwd.json`, add it to `additional-cards.json` and reference it

4. **Debugging**: Use `-k KEYWORD_NAME --log-level DEBUG` to trace handler execution for a specific keyword

### When to Use Each Approach

| Scenario | Approach |
|----------|----------|
| Optional suffix that modifies title (`_ID`, `_TITLE`) | `add-option` with `title-order` |
| Optional card(s) that don't change title | `add-option` with `card-order` only |
| Completely different card structures | `type: multiple` |
| Keyword has existing card that should become optional | `skip-card` + `add-option` |

