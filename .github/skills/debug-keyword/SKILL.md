---
name: debug-keyword
description: >
  Debug a PyDyna keyword that is not rendering, parsing, or behaving correctly.
  Use for: wrong field values after loads(), card ordering problems, optional
  cards consuming wrong data lines, parameter substitution not working, keyword
  linking broken, legacy keyword API migration, deprecation warnings.
argument-hint: "keyword name or symptom (e.g. AirbagParticle, wrong field value after loads)"
---

# Debug Keyword Skill

## Diagnostic Checklist

Work through these in order — most bugs fall into one of these categories.

### 1. Card ordering / optional cards consuming wrong data lines

**Symptom**: Field X has the value that should be in field Y; some fields stay `None`.

**Cause**: Optional cards (e.g. `TSTOP`, `SFIAIR4`) are present in the card list but absent
from the input file. If the codegen card order doesn't match the keyword spec, the reader
assigns data lines to the wrong cards.

**Fix**:
1. Check `codegen/manifest.json` for the keyword — look for `reorder-card`, `skip-card`,
   or `conditional-card` entries.
2. Regenerate and diff:
   ```bash
   python codegen/generate.py -k MY_KEYWORD
   git diff src/ansys/dyna/core/keywords/keyword_classes/auto/
   ```
3. Add a `conditional-card` with the correct `active_func` if a card should only appear
   when a controlling field has a specific value.

See [agents/codegen/handlers.md](../../agents/codegen/handlers.md) and the `add-handler` skill.

### 2. Field value is `None` after `loads()` despite being in the input

**Symptom**: `kw.my_field` is `None` after loading a valid deck string.

**Steps**:
```python
from ansys.dyna.core import Deck, keywords as kwd

deck = Deck()
deck.loads(your_input_string)
kw = deck.get(kwd.MyKeyword)
print(kw.my_field)        # None?
print(kw.write())         # Does write() show the right value?
```

- If `write()` is also wrong: the field was never read. Check card index alignment (see above).
- If `write()` is correct but `my_field` is `None`: the property name may differ from the
  field name. Check the generated class in `auto/` for property name or a `rename-property` entry.

### 3. Keyword linking broken

**Symptom**: `table.linked_curves` is empty after loading a deck with `DefineCurve` entries.

**Cause**: Import handlers populate linked collections during `Deck.loads()`. If the handler
is not registered or the deck order is unexpected, linking silently fails.

**Steps**:
1. Check `src/ansys/dyna/core/lib/deck.py` for `_import_handlers` registration.
2. Check `src/ansys/dyna/core/lib/import_handlers/` for the relevant handler.
3. Verify the handler's `after_import` logic matches the keyword order in the deck.

See [agents/keywords/linking.md](../../agents/keywords/linking.md).

### 4. Parameter `&param` not substituted

**Symptom**: A field value stays as the string `"&density"` instead of the numeric value.

**Steps**:
```python
print(deck.parameters.get("density"))    # Is it defined?
print(deck.parameters)                   # All parameters
```

- If `None`: parameter was not loaded (wrong scope, or keyword not parsed).
- If defined: check if the field type is `str` — string fields store `&param` literally and
  are not substituted. Only numeric fields (`int`, `float`) receive substituted values.
- `*PARAMETER_LOCAL` parameters are scoped to their include file — they won't be visible
  in parent deck scope.

See [agents/keywords/parameters.md](../../agents/keywords/parameters.md).

### 5. Legacy keyword API — `DeprecationWarning` or missing property

**Symptom**: Code using an old property name warns about deprecation, or a property that
existed in a previous release is missing.

**Steps**:
1. Check `src/ansys/dyna/core/keywords/keyword_classes/manual/` for a `*_legacy.py` or
   `*_version_*.py` file for the keyword.
2. The legacy class can be loaded explicitly via `ImportContext.keyword_overrides`:
   ```python
   from ansys.dyna.core.lib.import_handler import ImportContext
   from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

   deck = Deck()
   context = ImportContext(deck=deck, keyword_overrides={"*MAT_295": Mat295Legacy})
   deck.loads(data, context=context)
   ```
3. For permanent migration, update code to use the new property API.

See [agents/keywords/legacy.md](../../agents/keywords/legacy.md).

### 6. `write()` produces unexpected output

**Steps**:
```python
kw = kwd.MyKeyword()
kw.some_field = 42
print(repr(kw.write()))   # Use repr() to see whitespace and escape chars
```

- **Trailing spaces on lines**: expected — LS-DYNA fields are fixed-width padded.
- **Wrong field width**: keyword may need `format=format_type.long` (20-char fields).
- **Missing card**: check for a `conditional-card` `active_func` that is evaluating to `False`.

## Quick Reference: Useful Inspection Snippets

```python
from ansys.dyna.core import Deck, keywords as kwd

# Load and inspect
deck = Deck()
deck.loads(input_string)
kw = deck.get(kwd.MyKeyword)

# Check all field values
print(kw.write())

# Check parameter state
print(deck.parameters)

# Check card count (via internal repr)
print(repr(kw))

# Try long format
from ansys.dyna.core.lib.format_type import format_type
kw2 = kwd.MyKeyword(format=format_type.long)
kw2.some_field = 42
print(kw2.write())
```
