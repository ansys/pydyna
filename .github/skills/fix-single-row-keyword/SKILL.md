---
name: fix-single-row-keyword
description: >
  Fix a PyDyna keyword that only reads one row when it should support multiple
  rows. Use for: keywords that accept unbounded repeating data rows (like
  ELEMENT_MASS_PART, NODE, ELEMENT_SHELL), diagnosing "only first row loaded",
  converting a scalar Card to a TableCard via manifest.json, adding a DataFrame
  property to replace scalar field properties.
argument-hint: "LS-DYNA keyword that is missing multi-row support (e.g. ELEMENT_MASS_PART)"
---

# Fix Single-Row Keyword Skill

## Symptom

A keyword accepts multiple data rows in an LS-DYNA deck but PyDyna only reads
the first row, ignoring the rest. For example:

```python
deck = Deck()
deck.loads("""*ELEMENT_MASS_PART
  1463  0.00491  0.0  0
  1464  0.00491  0.0  0
  415   0.00382  0.0  0
*END""")
kw = deck.get(kwd.ElementMassPart)
print(len(kw.elements))   # Expected 3, got 1
```

## Root Cause

The keyword was generated with a plain `Card` (single row). It needs a
`TableCard` (unbounded repeating rows, stored as a `pandas.DataFrame`).

## Fix: Add `table-card` to `manifest.json`

### Minimal case — entire keyword is a repeating table

```json
"MY_KEYWORD": {
  "generation-options": {
    "table-card": [
      {
        "property-name": "rows",
        "ref": "data_card"
      }
    ]
  },
  "labels": {
    "data_card": 0
  }
}
```

- **`property-name`**: Name of the DataFrame property on the generated class (e.g. `"elements"`, `"rows"`, `"parts"`)
- **`ref`**: Label name that maps to a card index
- **`labels`**: Maps label names to card indices (0-based after any `reorder-card`)

### With a fixed header card followed by a repeating table

```json
"MY_KEYWORD": {
  "generation-options": {
    "table-card": [
      {
        "property-name": "elements",
        "ref": "data_card"
      }
    ]
  },
  "labels": {
    "header_card": 0,
    "data_card": 1
  }
}
```

### With a controlled row count (known from a header field)

```json
"MY_KEYWORD": {
  "generation-options": {
    "table-card": [
      {
        "property-name": "points",
        "length-func": "self.nrows",
        "ref": "data_card"
      }
    ]
  },
  "labels": {
    "data_card": 1
  }
}
```

- **`length-func`**: Python expression evaluated at write time to determine row count.
  Omit when the table is open-ended (read until the next `*KEYWORD` line).

### With a conditional table (only present in some variants)

```json
"MY_KEYWORD": {
  "generation-options": {
    "table-card": [
      {
        "property-name": "integration_points",
        "length-func": "self.nipp",
        "active-func": "self.elform in [101, 102, 103, 104, 105]",
        "ref": "integration_points_card"
      }
    ]
  },
  "labels": {
    "integration_points_card": 3
  }
}
```

- **`active-func`**: Python expression; if `False`, the table card is not written and not read.

### Multi-card groups (each row spans multiple physical cards)

Use `table-card-group` when a logical row requires two or more LS-DYNA card lines:

```json
"MY_KEYWORD": {
  "generation-options": {
    "table-card-group": [
      {
        "refs": ["main_card", "extra_card"],
        "overall-name": "elements"
      }
    ]
  },
  "labels": {
    "main_card": 0,
    "extra_card": 1
  }
}
```

Real-world example: `ELEMENT_SHELL_BETA` (see `manifest.json`).

## What the Generated Class Looks Like

Before (scalar `Card`):
```python
@property
def pid(self) -> int:
    return self._cards[0].get_value("pid")

@pid.setter
def pid(self, value: int) -> None:
    self._cards[0].set_value("pid", value)
```

After (`TableCard`):
```python
@property
def elements(self) -> pd.DataFrame:
    """Get the table of elements."""
    return self._cards[0].table

@elements.setter
def elements(self, df: pd.DataFrame):
    """Set elements from the dataframe df"""
    self._cards[0].table = df
```

Users interact with a DataFrame:
```python
import pandas as pd
kw = kwd.MyKeyword()
kw.elements = pd.DataFrame({"pid": [101, 102], "addmass": [500.0, 200.0]})
print(kw.write())
```

## Steps

1. Identify the card index in `kwd.json` that should be a table
2. Add `table-card` (or `table-card-group`) to `manifest.json`
3. Regenerate:
   ```bash
   python codegen/generate.py -k MY_KEYWORD
   ```
4. Verify the generated class uses `TableCard` and exposes a DataFrame property
5. Run validation:
   ```bash
   bash codegen/validate.sh --quick
   ```
6. Write tests — see the `keyword-tests` skill. The key assertions are:
   ```python
   assert len(kw.elements) == expected_row_count
   assert list(kw.elements["field_name"]) == [val1, val2, ...]
   ```

## Real-World Example: `ELEMENT_MASS_PART` (PR #1170)

`manifest.json` change:
```json
"ELEMENT_MASS_PART": {
  "generation-options": {
    "table-card": [
      {
        "property-name": "elements",
        "ref": "elements_card"
      }
    ]
  },
  "labels": {
    "elements_card": 0
  }
}
```

Result: `kw.elements` returns a DataFrame with columns `pid`, `addmass`, `finmass`, `lcid`.
All rows round-trip correctly through `write()` and `loads()`.

## Common Pitfalls

- **Omit `length-func` for open-ended tables**: if you provide a `length-func` that returns 0
  or `None`, the table will be empty on write even when rows are present. Only use it when a
  header field explicitly controls the row count.
- **Column names come from `kwd.json` field names**: check the generated field names in the
  class before writing tests — they may differ from the LS-DYNA manual names.
- **`table-card-group` vs `table-card`**: use `table-card-group` only when a logical row
  spans two or more consecutive physical card lines.
