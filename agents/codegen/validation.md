# Codegen Validation Workflows

How to validate codegen changes at different stages of development.

## Key Principle

**If generated code doesn't change, tests don't need to run.**

The metric: `python codegen/generate.py` + `git diff src/.../auto/` shows no changes.

## Validation Commands

### Quick Validation (Fast Iteration)

```bash
bash codegen/validate.sh --quick
```

Does: clean → generate → git diff only. Use for most codegen refactoring.

### Full Validation (Before Commit)

```bash
bash codegen/validate.sh
```

Does: clean → generate → git diff → pre-commit → dead code → unit tests.

### Custom Validation

```bash
bash codegen/validate.sh --skip-tests       # Skip unit tests
bash codegen/validate.sh --skip-precommit   # Skip pre-commit hooks
bash codegen/validate.sh --skip-deadcode    # Skip dead code detection
bash codegen/validate.sh --coverage-threshold 90  # Adjust coverage
bash codegen/validate.sh --verbose          # Verbose output
```

### Manual Output Check

```bash
python codegen/generate.py -c
python codegen/generate.py
git diff src/ansys/dyna/core/keywords/keyword_classes/auto/
```

If `git diff` shows changes:
1. The codegen logic has a bug that needs fixing, OR
2. The changes are intentional and should be committed

## When to Run What

| Scenario | Validation Required |
|----------|---------------------|
| Refactoring codegen internals | `validate.sh --quick` (no changes = success) |
| Adding new handler/feature | `validate.sh --quick` + review intentional changes |
| Changing generated output intentionally | Full `validate.sh` + commit changes |
| Modifying runtime keyword behavior | Run tests (`pytest tests/ -m keywords`) |

## Testing Single Keywords

```bash
# Generate single keyword with debug output
python codegen/generate.py -k SECTION_SHELL -l DEBUG

# Generate subset by domain
python codegen/generate.py --subset "boundary,contact,control"
```

## Dead Code Detection

After refactoring, check for unused code:

```bash
python codegen/find_dead_code.py --threshold 80
```

Files with <80% coverage should be reviewed.

## CI Integration

The CI uses the same validation script to ensure local and CI validation are identical.

## Troubleshooting

### Diff shows changes after refactoring

1. Run `python codegen/generate.py -k AFFECTED_KEYWORD -l DEBUG`
2. Compare handler execution traces
3. Check for reference semantics issues (deepcopy, ordering)

### Tests fail but diff is empty

The issue is in runtime keyword behavior, not codegen. Run focused tests:

```bash
pytest tests/test_keywords.py -v -k "test_name"
```

### Handler produces wrong output

1. Add logging to the handler
2. Run single keyword with `-l DEBUG`
3. Check handler execution order in `registry.py`
