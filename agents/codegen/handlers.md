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
| 6 | `add-field` | Appends new fields missing from kwd.json |
| 7 | `replace-card` | Replaces entire cards |
| 8 | `series-card` | Transforms cards into variable-length series |
| 9 | `add-option` | Adds keyword options |
| 10 | `card-set` | Groups cards into reusable sets |
| 11 | `conditional-card` | Adds conditional logic |
| 12 | `rename-property` | Renames property accessors |
| 13 | `table-card-group` | Groups cards into repeating units |
| 14 | `external-card-implementation` | Links to external cards |
| 15 | `add-mixin` | Adds mixin class inheritance |
| 16 | `additional-imports` | Injects extra top-level imports |
| 17 | `shared-field` | Creates shared field definitions |

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

### add-field

Appends new fields to an existing card. Use this when a field is missing from `kwd.json` and cannot be added there. Unlike `override-field`, this handler **creates** fields that do not yet exist.

```json
"add-field": [
  {
    "ref": "elements_card",
    "fields": [
      {
        "name": "MWD",
        "type": "integer",
        "position": 48,
        "width": 8,
        "default": null,
        "help": "Optional flag for mass-weighted distribution."
      }
    ]
  }
]
```

Required field properties: `name`, `type`, `position`, `width`. Optional: `default`, `help`, `link`.
Duplicate field names (case-insensitive) are silently skipped.

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
  "overall-name": "stresses",
  "length-func": "self.num_elements"
}]
```

`conditional-card` on a sub-card (`card_active_funcs`) enables or disables it for every row. When a
sub-card exists only in some rows, give it a row condition instead. `*ELEMENT_SHELL_THICKNESS` has a
THIC5-THIC8 line only for elements with mid-side nodes:

```json
"table-card-group": [{
  "refs": ["element_card", "thickness_card", "midside_thickness_card"],
  "overall-name": "elements",
  "row-conditions": [{
    "ref": "midside_thickness_card",
    "active-if": {"op": "ne", "left": {"field": "n5"}, "right": {"literal": 0}}
  }]
}]
```

`active-if` is an expression tree over fields of **earlier** sub-cards, named by their generated
property names:

| Expression | Meaning |
|------------|---------|
| `{"op": "and" \| "or", "args": [e1, e2, ...]}` | All / any of at least two expressions |
| `{"op": "not", "arg": e}` | Negation |
| `{"op": "is_set", "field": f}` | Field is not blank |
| `{"op": "eq" \| "ne" \| "lt" \| "le" \| "gt" \| "ge", "left": o, "right": o}` | Comparison; operands are `{"field": f}` or `{"literal": v}` |
| `{"op": "in", "left": {"field": f}, "right": {"literal": [v1, ...]}}` | Membership |

A comparison or `in` with a blank field is false, so `not` of a comparison is true for a blank field.
Codegen checks literal types against the field types, then emits a `RowCondition` with a scalar form,
used while reading line by line, and a vectorized pandas form, used for writing:

```python
RowCondition(
    ("n5",),
    lambda n5: not is_missing(n5) and n5 != 0,
    lambda table: (table["n5"].notna() & table["n5"].ne(0)).to_numpy(dtype=bool),
)
```

A sub-card disabled by `conditional-card` has no rows, whatever its row condition.

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
