# Auto-keyword Class Generator

The PyDyna keyword class generator produces Python classes for LS-DYNA keywords based on specifications in `kwd.json`, `manifest.json`, and `additional-cards.json`.

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

A comprehensive validation script encapsulates the testing workflow:

```bash
# Full validation (clean, generate, git diff, pre-commit, dead code, unit tests)
bash codegen/validate.sh

# Quick validation for fast iteration (clean, generate, git diff only)
bash codegen/validate.sh --quick

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
