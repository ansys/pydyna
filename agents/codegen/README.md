# Codegen Agent Guide

Quick reference for AI agents working with the PyDyna keyword class generator.

## Quick Commands

```bash
# Fast validation (generate + diff only)
bash codegen/validate.sh --quick

# Full validation (includes tests)
bash codegen/validate.sh

# Test single keyword with debug output
python codegen/generate.py -k SECTION_SHELL -l DEBUG
```

## Key Principle

**If generated code doesn't change, tests don't need to run.**

Validation metric: `python codegen/generate.py` + `git diff src/.../auto/` shows no changes.

## Handler Execution Order (Critical)

Handlers run in order defined in `registry.py`. Order matters because:
- Handlers use **reference semantics** (not copies)
- Later handlers modify cards that earlier handlers referenced
- **Never use `copy.deepcopy()` when grouping cards**

Order: `reorder-card` → `skip-card` → `insert-card` → `table-card` → `override-field` → `replace-card` → `series-card` → `add-option` → `card-set` → `conditional-card` → `rename-property` → `table-card-group` → `external-card-implementation` → `shared-field`

## Modifying Auto-Generated Files

Files in `src/ansys/dyna/core/keywords/keyword_classes/auto/` are auto-generated.

**Options**:
1. **Systemic changes**: Update codegen templates or handlers
2. **Keyword-specific**: Create a manual subclass

## Common Pitfalls

1. **Index vs. position**: After `reorder-card`, use list positions `kwd_data.cards[i]`, not card's `index` property
2. **None sub_cards**: Some cards have `sub_cards: None` - add None checks
3. **Shared field negative indices**: Search option cards FIRST regardless of index value

## Critical Codegen Notes

**Handler execution order matters** - handlers run in order defined in `registry.py`.
**Reference semantics** - `card-set` and `table-card-group` use references, not copies. Later handlers modify cards in-place. **Never use `copy.deepcopy()` when grouping cards.**

**Auto-generated files** - Don't modify files in `src/.../keyword_classes/auto/` unless the intention is to change the codegen.
It is often a good strategy when intending to change codegen to hand-write the generated files you wish to see, test them, and then update the codegen.

## Deep References

- [handlers.md](handlers.md) - Detailed handler documentation
- [architecture.md](architecture.md) - Full architecture and data flow
- [validation.md](validation.md) - Validation workflows and troubleshooting
