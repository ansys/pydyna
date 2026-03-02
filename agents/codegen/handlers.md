# Handler Pipeline Reference

Detailed documentation for each handler in the codegen pipeline.

## Handler Execution Order

Handlers are executed in registration order (see `handlers/registry.py`). Order is **critical** due to reference semantics.

| Order | Handler | Purpose |
|-------|---------|---------|
| 1 | `reorder-card` | Reorders cards; must run first |
| 2 | `skip-card` | Marks cards to skip |
| 3 | `insert-card` | Inserts new cards |
| 4 | `table-card` | Transforms cards into repeatable tables |
| 5 | `override-field` | Modifies field properties |
| 6 | `replace-card` | Replaces entire cards |
| 7 | `series-card` | Transforms cards into variable-length series |
| 8 | `add-option` | Adds keyword options |
| 9 | `card-set` | Groups cards into reusable sets |
| 10 | `conditional-card` | Adds conditional logic |
| 11 | `rename-property` | Renames property accessors |
| 12 | `table-card-group` | Groups cards into repeating units |
| 13 | `external-card-implementation` | Links to external cards |
| 14 | `add-mixin` | Adds mixin class inheritance |
| 15 | `additional-imports` | Injects extra top-level imports |
| 16 | `shared-field` | Creates shared field definitions |

## Reference Semantics (Critical)

Handlers that group cards (`card-set`, `table-card-group`) use **references**, not copies.

```python
# card-set creates a set by appending references
# conditional-card later sets `func` on those same objects
# Changes appear in both places because they're the same objects
```

**Never use `copy.deepcopy()` when grouping cards** - it breaks this pattern.

## Handler Details

### reorder-card

Reorders cards based on specified indices. **Must run first** because other handlers use positional indices.

```json
"reorder-card": [0, 1, 4, 2, 3, 5]
```

After reorder, use list positions `kwd_data.cards[i]`, not the card's `index` property.

### skip-card

Marks cards to skip during generation.

```json
"skip-card": [{"ref": "card_label"}]
```

Requires a `ref` to a label defined in the `labels` section.

### insert-card

Inserts new cards at specified indices. Processes in **reverse index order**.

```json
"insert-card": [
  {"index": 3, "source": "additional-cards", "card-name": "NEW_CARD"}
]
```

**Index Calculation**: Python's `list.insert(i, x)` appends if `i >= len(list)`. See [architecture.md](architecture.md#insert-card-index-computation) for detailed examples.

### table-card

Transforms cards into repeatable tables (2D data).

```json
"table-card": [{"ref": "data_card", "length_func": "num_rows"}]
```

### series-card

Transforms fields into 1D arrays across cards.

```json
"series-card": [{"ref": "param_card", "control_field": "nip"}]
```

### card-set

Groups adjacent cards into reusable sets that may repeat.

```json
"card-set": [{
  "source-indices": [2, 3, 4],
  "name": "layer_data",
  "length_func": "num_layers"
}]
```

### conditional-card

Cards active only under conditions.

```json
"conditional-card": [{"ref": "special_card", "func": "lambda self: self.type == 1"}]
```

### add-option

Adds keyword options (e.g., `_ID`, `_TITLE` suffixes).

```json
"add-option": [{
  "option-name": "ID",
  "title-order": 1,
  "card-order": -2,
  "cards": [{"source": "additional-cards", "card-name": "ID_TITLE"}]
}]
```

#### How `title-order` Affects the Keyword Title

Each option is represented at runtime as an `OptionSpec(name, card_order, title_order)`. The keyword title is affected by the option's title order:

```python
# OptionSpec("ID", card_order=-2, title_order=1)
# When inactive:  writes *RIGIDWALL_PLANAR
# When active:    writes *RIGIDWALL_PLANAR_ID
```

A single class handles both the plain and option-aware forms depending on whether the option is activated. When a Deck is read from a file the existing of an option name in the title in the appropriate location activates the option automatically.

There is some redundancy in kwd.json where keywords are defined in situations where an option would be sufficient. This is inherited (we can't change kwd.json), and in those cases it is preferable to remove the redundant keyword and use the option only. Example: *RIGIDWALL_PLANAR_ID

### table-card-group

Groups adjacent cards that repeat together as 2D tables.

```json
"table-card-group": [{
  "refs": ["stress_data", "hisv"],
  "length_func": "num_elements"
}]
```

### shared-field

Creates fields shared across multiple cards.

```json
"shared-field": [{"field": "heading", "indices": [-1, -2]}]
```

**Critical**: Search option cards FIRST regardless of index value. See [README.md](README.md#common-pitfalls).

### additional-imports

Injects extra top-level `import` statements into the generated module. Use this when an inline expression (e.g. `length-func`, `active-func`) references a symbol not imported by default.

```json
"additional-imports": [
  {"name": "math"}
]
```

Produces `import math` in the generated `.py` file, placed after `import typing`.
Duplicates are silently ignored.

## Handler Settings Base Classes

Handlers use typed dataclasses with shared base utilities in `handlers/base_settings.py`:

- `LabelRefSettings` - Base for handlers using label-based card references
- `parse_settings_list()` - Generic settings parser
- `find_field_in_card()` - Find field by name in a card
- `modify_field_in_cards()` - Apply modifications to multiple cards
