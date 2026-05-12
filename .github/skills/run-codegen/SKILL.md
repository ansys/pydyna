---
name: run-codegen
description: >
  Run and validate PyDyna codegen. Use for: regenerating keyword classes after
  manifest.json changes, validating codegen output, checking git diff for
  unintended changes, running single-keyword generation, debugging generation
  failures, running full pre-commit + test validation before committing.
argument-hint: "keyword name to target, or leave blank for full validation"
---

# Run Codegen Skill

## Key Principle

**If the generated code doesn't change, no tests need to run.**
After any manifest or handler change, the first check is always `git diff src/.../auto/`.

## Commands

### Fast validation (use during development)

```bash
bash codegen/validate.sh --quick
```

Does: clean → generate → `git diff`. Use for most codegen refactoring and manifest edits.
A clean diff means the change is safe.

### Full validation (before committing)

```bash
bash codegen/validate.sh
```

Does: clean → generate → `git diff` → pre-commit hooks → dead code detection → unit tests.

### Options

```bash
bash codegen/validate.sh --skip-tests        # Skip unit tests
bash codegen/validate.sh --skip-precommit    # Skip pre-commit hooks
bash codegen/validate.sh --skip-deadcode     # Skip dead code detection
bash codegen/validate.sh --verbose           # Verbose output
```

### Single keyword (fastest, use while iterating)

```bash
python codegen/generate.py -k SECTION_SHELL
python codegen/generate.py -k SECTION_SHELL -l DEBUG   # with debug logging
```

### Subset by domain

```bash
python codegen/generate.py --subset "boundary,contact,control"
```

### Dead code detection

```bash
python codegen/find_dead_code.py --threshold 80
```

Files below 80% coverage should be reviewed after handler refactoring.

## When to Run What

| Scenario | Command |
|----------|---------|
| Editing `manifest.json` for one keyword | `generate.py -k KEYWORD` then `validate.sh --quick` |
| Refactoring a handler | `validate.sh --quick` (clean diff = success) |
| Intentional output change | `validate.sh` (full) + commit the generated changes |
| Runtime bug in keyword behavior | `pytest tests/ -v` (not codegen — no diff needed) |

## Reading the Diff

After `validate.sh --quick`, run:

```bash
git diff src/ansys/dyna/core/keywords/keyword_classes/auto/
```

- **Empty diff** — generation is stable; change is safe
- **Diff in the target keyword only** — intentional; review and commit
- **Diff in unexpected keywords** — handler has a side effect; investigate

## Troubleshooting

### Diff shows changes after a refactor you expected to be neutral

1. Generate the affected keyword with debug logging:
   ```bash
   python codegen/generate.py -k AFFECTED_KEYWORD -l DEBUG
   ```
2. Compare handler execution traces between old and new output.
3. Check for reference-semantics issues — handlers that group cards use shared references;
   using `copy.deepcopy()` breaks the pattern. See [agents/codegen/handlers.md](../../agents/codegen/handlers.md).

### Tests fail but diff is empty

The issue is in runtime keyword behavior, not codegen. Run focused tests:

```bash
python -m pytest tests/test_keywords.py -v -k "my_test"
```

### Generation crashes on a specific keyword

```bash
python codegen/generate.py -k KEYWORD -l DEBUG
```

Check the traceback against the handler pipeline order in [agents/codegen/handlers.md](../../agents/codegen/handlers.md).

## Code Search Policy

- **Avoid opening `codegen/kwd.json`** — 80k+ lines; use grep for targeted lookups
- **Avoid browsing `src/.../auto/`** broadly — 3000+ files; search for specific keyword files only
