# Auto-keyword Class Generator

The PyDyna keyword class generator produces Python classes for LS-DYNA keywords based on specifications in `kwd.json`, `manifest.json`, and `additional-cards.json`.

**⚠️ `kwd.json` is machine-generated from the LS-DYNA manual and MUST NOT be manually modified.**

For architecture and agent guidance, see [../agents/codegen/](../agents/codegen/).

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

# Generate subset by domain
python codegen/generate.py --subset "boundary,contact,control"

# Clean all generated code
python codegen/generate.py -c
```

## Validation

```bash
# Quick validation (fast iteration)
bash codegen/validate.sh --quick

# Full validation (before commit)
bash codegen/validate.sh

# Options
bash codegen/validate.sh --skip-tests
bash codegen/validate.sh --skip-precommit
bash codegen/validate.sh --skip-deadcode
bash codegen/validate.sh --coverage-threshold 90
bash codegen/validate.sh --verbose
```

## Logging

```bash
python codegen/generate.py -k SECTION_SHELL -l DEBUG   # Detailed
python codegen/generate.py -l WARNING                   # Minimal
python codegen/generate.py -l INFO                      # Default
```

## Input Files

| File | Purpose | Editable |
|------|---------|----------|
| `kwd.json` | Primary keyword definitions (~250MB) | **NO** (machine-generated) |
| `manifest.json` | Corrections and supplements | Yes |
| `additional-cards.json` | Card definitions for insert/replace | Yes |

## Output

Generated files go to `src/ansys/dyna/core/keywords/keyword_classes/auto/`, organized by domain subdirectories (contact/, define/, section/, mat/, etc.).

## Further Documentation

- **Agent guides**: [../agents/codegen/](../agents/codegen/) - handlers, validation, architecture
- **Architecture**: [../agents/codegen/architecture.md](../agents/codegen/architecture.md)

