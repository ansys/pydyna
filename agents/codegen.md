# Code Generation Agent Guide

This guide is for AI agents working with the PyDyna keyword class generator.

For usage instructions, see [codegen/README.md](../codegen/README.md).
For architecture details, see [codegen/architecture.md](../codegen/architecture.md).

## Quick Reference

**Validation workflow**:
```bash
bash codegen/validate.sh --quick  # Fast iteration
bash codegen/validate.sh          # Full validation before commit
```

**Test a single keyword**:
```bash
python codegen/generate.py -k SECTION_SHELL -l DEBUG
```

## Key Concepts for Agents

### Handler Execution Order Matters

Handlers run in a specific order defined in `registry.py`. Key constraints:
- `reorder-card` must run first (other handlers use positional indices)
- `card-set` and `table-card-group` must run before `conditional-card`
- See [architecture.md](../codegen/architecture.md) for full ordering

### Reference Semantics - Critical

Handlers that group cards (`card-set`, `table-card-group`) use **references**, not copies. Later handlers modify cards in-place, and changes appear in both places.

**Never use `copy.deepcopy()` when grouping cards** - it breaks this pattern.

### Index vs. Position

After `reorder-card`:
- Cards have an `index` property (original index)
- Handlers use **list positions** `kwd_data.cards[i]`, not card indices

### Dataclass Migration Status

- **Fields**: ✅ Fully migrated to `Field` instances
- **Cards**: ⚠️ Partially migrated - `table_card_group` handler still creates dict-based cards

Once `table_card_group` is updated, dict-like fallback methods can be removed.

## Modifying Auto-Generated Files

Files in `src/ansys/dyna/core/keywords/keyword_classes/auto/` are auto-generated. Don't modify directly.

**Options**:
1. **Systemic changes**: Update codegen templates or handlers
2. **Keyword-specific**: Create a manual subclass (see [architecture.md](../codegen/architecture.md#manual-subclasses))

## Refactoring Guidelines

Due to codegen complexity, verify between independent steps:
1. Make one change
2. Run `python codegen/generate.py`
3. Verify output unchanged (or intentionally changed)
4. Repeat

## Common Pitfalls

1. **Missing index field**: Cards from kwd.json don't have `index` initially
   - Solution: `Card.from_dict()` uses `data.get("index", -1)`
2. **None sub_cards**: Some cards have `sub_cards: None` instead of empty list
   - Solution: Add None checks: `if card["sub_cards"]:`
3. **Union types during transition**: Type hints need `Union[Card, Dict]` until all sources converted
4. **Missing optional attributes**: Ensure ALL dict keys represented as dataclass attributes
   - Review source dicts (e.g., `additional-cards.json`) for all possible keys

## Handler Order Reference

1. `reorder-card` - Must run first
2. `skip-card`
3. `insert-card`
4. `table-card`
5. `override-field`
6. `replace-card`
7. `series-card`
8. `add-option`
9. `card-set` - Uses references, not copies!
10. `conditional-card` - Modifies cards in-place
11. `rename-property`
12. `table-card-group`
13. `external-card-implementation`
14. `shared-field`

## Insert-Card Index Computation

The `insert-card` handler processes insertions in **reverse index order**. Python's `list.insert(i, x)` appends if `i >= len(list)`, creating non-intuitive index mapping.

**Example**: To insert X, Y, Z after 3 original cards [A, B, C]:

```python
# Wrong: indices [3, 4, 5] → result X@3, Z@4, Y@5
# Correct: indices [3, 6, 4] → result X@3, Y@4, Z@5
```

**Rule**: First card uses target index P. Subsequent cards use out-of-range indices that will be pushed into position by earlier insertions.

## Known Issues

- CONSTRAINED.BEAM_IN_SOLID has manifest data issue (shared-field with 0 occurrences) - handled gracefully with warning

## Shared Field Handler - Negative Indices

**Critical**: The `shared_field` handler's `do_negative_shared_fields` function must search option cards FIRST, regardless of index value. Option cards can have indices that overlap with base card ranges (e.g., option card with index=2 when num_cards=3).

**Logic must**:
1. Always search all option cards for the target index
2. Only check base cards if not found in options
3. Never assume `index >= num_cards` means "must be in options" - this assumption is incorrect

