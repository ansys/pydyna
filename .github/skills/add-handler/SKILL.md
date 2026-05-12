---
name: add-handler
description: >
  Add or modify a codegen handler in PyDyna. Use for: creating a new manifest.json
  handler, understanding handler execution order, fixing handler reference-semantics
  bugs, adding conditional cards, adding table cards, inserting new cards,
  renaming properties, adding fields missing from kwd.json.
argument-hint: "handler name or keyword being configured (e.g. conditional-card, table-card)"
---

# Add Handler Skill

## What Handlers Do

Handlers transform `KeywordData` in the codegen pipeline before Jinja2 templates render
the final Python class. Each handler reads from and mutates `KeywordData` in-place.

Full reference: [agents/codegen/handlers.md](../../agents/codegen/handlers.md)
Architecture overview: [agents/codegen/architecture.md](../../agents/codegen/architecture.md)

## Handler Execution Order

Handlers run in registration order (defined in `codegen/keyword_generation/handlers/registry.py`).
Order is **critical** — later handlers depend on transformations made by earlier ones.

| Order | Handler | Purpose |
|-------|---------|---------|
| 1 | `reorder-card` | Reorder cards by index — must run first |
| 2 | `skip-card` | Mark cards to exclude from generation |
| 3 | `insert-card` | Insert new cards at specific indices |
| 4 | `table-card` | Make a card repeat as a 2D table |
| 5 | `override-field` | Modify existing field properties |
| 6 | `add-field` | Add fields missing from `kwd.json` |
| 7 | `replace-card` | Replace an entire card |
| 8 | `series-card` | Make a card a variable-length series |
| 9 | `add-option` | Add keyword options |
| 10 | `card-set` | Group cards into a reusable set |
| 11 | `conditional-card` | Add a condition controlling card presence |
| 12 | `rename-property` | Rename property accessors |
| 13 | `table-card-group` | Group cards into a repeating unit |
| 14 | `external-card-implementation` | Link to external card class |
| 15 | `add-mixin` | Add mixin class inheritance |
| 16 | `additional-imports` | Inject top-level imports |
| 17 | `shared-field` | Create shared field definitions |

## Critical: Reference Semantics

Handlers that group cards (`card-set`, `table-card-group`) store **references**, not copies.
A later handler (e.g. `conditional-card`) that sets `func` on a card modifies the same
object that the set holds.

**Never use `copy.deepcopy()` when grouping cards** — it breaks this pattern.

## Common Handler Patterns

### Reorder cards

Use when `kwd.json` has cards in the wrong logical order:

```json
"MY_KEYWORD": {
  "reorder-card": [0, 1, 4, 2, 3, 5]
}
```

After reorder, subsequent handlers use list positions, not `card.index`.

### Skip an optional card

First add a label to the card in `manifest.json`, then reference it:

```json
"MY_KEYWORD": {
  "labels": {"optional_card": {"card": 3}},
  "skip-card": [{"ref": "optional_card"}]
}
```

### Make a card conditional

The card must exist (not skipped). Add an `active_func`:

```json
"MY_KEYWORD": {
  "labels": {"unit_card": {"card": 2}},
  "conditional-card": [
    {"ref": "unit_card", "func": "lambda self: self.unit == 3"}
  ]
}
```

### Make a card a repeating table

```json
"MY_KEYWORD": {
  "labels": {"data_card": {"card": 4}},
  "table-card": [
    {"ref": "data_card", "length_func": "self.nrows"}
  ]
}
```

### Add a field missing from `kwd.json`

```json
"MY_KEYWORD": {
  "labels": {"main_card": {"card": 0}},
  "add-field": [
    {
      "ref": "main_card",
      "fields": [
        {
          "name": "EXTRA_FLAG",
          "type": "integer",
          "position": 48,
          "width": 8,
          "default": null,
          "help": "Optional extra flag."
        }
      ]
    }
  ]
}
```

### Insert a card from `additional-cards.json`

```json
"MY_KEYWORD": {
  "insert-card": [
    {"index": 3, "source": "additional-cards", "card-name": "MY_EXTRA_CARD"}
  ]
}
```

Define `MY_EXTRA_CARD` in `codegen/additional-cards.json`.

**Index calculation**: `list.insert(i, x)` appends when `i >= len(list)`. Plan indices
after `reorder-card` has run if both are present.

## Adding a New Handler Type

1. Create `codegen/keyword_generation/handlers/my_handler.py` implementing the handler interface
2. Register it in `codegen/keyword_generation/handlers/registry.py` at the correct order position
3. Add tests in `codegen/tests/`
4. Run `bash codegen/validate.sh --quick` to confirm no unintended output changes

## Validation After Handler Changes

```bash
# Check for unintended output changes
bash codegen/validate.sh --quick

# Generate a specific keyword with debug output
python codegen/generate.py -k MY_KEYWORD -l DEBUG

# Full validation before commit
bash codegen/validate.sh
```
