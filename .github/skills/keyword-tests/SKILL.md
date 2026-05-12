---
name: keyword-tests
description: >
  Write, run, and debug tests for PyDyna keywords and cards.
  Use for: writing new pytest tests for keyword rendering/parsing, adding
  reference files, using conftest fixtures (string_utils, file_utils,
  ref_string, runner), understanding the run marker, checking test failures.
  Do NOT use for codegen validation tests — those are separate.
argument-hint: "keyword or test file to target (e.g. ContactAutomatic, test_card.py)"
---

# Keyword Tests Skill

## Test Locations

| Path | Purpose |
|------|---------|
| `tests/` | All pytest tests |
| `tests/testfiles/keywords/` | Reference `.k` files and `reference_string.py` |
| `tests/testfiles/initial/` | Initial deck `.k` files used as fixtures |
| `tests/testfiles/output/` | Expected output files for comparison |
| `tests/test_codegen/` | **Codegen-only** — do NOT run with normal pytest |

## Running Tests

Use whichever Python environment has PyDyna installed. The commands below use
`python` as a placeholder — substitute your actual interpreter or prefix as needed.

> **Tip:** Create `.github/copilot-instructions.md` (already gitignored) to tell
> the agent which Python environment to use. For example:
> ```
> Use `uv run --no-project python` when running Python commands in this repo.
> ```
> or
> ```
> Use `/path/to/my/venv/bin/python` when running Python commands in this repo.
> ```

```bash
# Run all non-run tests (standard)
python -m pytest tests/ -v

# Run a specific file
python -m pytest tests/test_keywords.py -v

# Run a specific test
python -m pytest tests/test_card.py::test_load_card_errors -v

# Run with -m run (requires LS-DYNA environment)
python -m pytest tests/ -v -m run
```

## Key Fixtures (from `tests/conftest.py`)

| Fixture | Type | Use |
|---------|------|-----|
| `string_utils` | `StringUtils` | Convert a string to a readable `io.StringIO` buffer via `.as_buffer(s)` |
| `file_utils` | `FileUtils` | Read/compare files in `testfiles/`; compare output with `.compare_string_with_file(output, ref_file)` |
| `ref_string` | module | Loads `testfiles/keywords/reference_string.py`; access expected strings as attributes |
| `runner` | `DynaRunner` | Run LS-DYNA simulations; only used with `@pytest.mark.run` tests |
| `resolve_standard_path` | `str` | Path to `testfiles/standard/` |
| `resolve_output_path` | `str` | Path to `testfiles/output/` |
| `*_initialfile` | `str` | Paths to specific initial `.k` files (e.g. `mech_initialfile`, `icfd_initialfile`) |

## The `run` Marker

Tests that require a live LS-DYNA installation must be marked with `@pytest.mark.run`:

```python
@pytest.mark.run
def test_something_that_needs_lsdyna(runner):
    ...
```

- **Skipped by default** when running `pytest tests/` without `-m run`
- Enabled only with `pytest -m run` in environments with LS-DYNA available
- Do NOT use this marker for pure unit/rendering tests

## Writing a Keyword Rendering Test

Pattern: instantiate keyword, set fields, call `.write()`, compare to expected string.

```python
from ansys.dyna.core import keywords as kwd
import pytest

def test_my_keyword_renders(file_utils):
    k = kwd.MyKeyword()
    k.some_field = 42
    output = k.write()
    file_utils.compare_string_with_file(output, "my_keyword_reference.k")
```

Then create `tests/testfiles/keywords/my_keyword_reference.k` with the expected output.

> **Trailing whitespace:** LS-DYNA fixed-width fields are padded to their full column width,
> so reference `.k` files **will contain trailing spaces** on data lines. Do not strip or
> trim whitespace in these files — doing so will cause tests to fail.

## Writing a Keyword Parsing Test

Pattern: use `string_utils.as_buffer()` or load a `.k` file via `file_utils`, call `deck.loads()` or card `.read()`.

```python
from ansys.dyna.core import Deck, keywords as kwd

def test_my_keyword_parses(string_utils):
    input_str = "*MY_KEYWORD\n        42\n*END\n"
    deck = Deck()
    deck.loads(input_str)
    kw = deck.get(kwd.MyKeyword)
    assert kw is not None
    assert kw.some_field == 42
```

## Writing a Card-Level Test

Use `Card.from_field_schemas` for low-level card tests:

```python
from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.field_schema import FieldSchema

def test_card_reads_correctly(string_utils):
    field_schemas = (
        FieldSchema("foo", int, 0, 10, None),
        FieldSchema("bar", float, 10, 10, None),
    )
    card = Card.from_field_schemas(field_schemas)
    result = card.read(string_utils.as_buffer("        42     3.14"))
    assert card.foo == 42
```

## Adding a Reference String

For tests using `ref_string`, add the expected value as an attribute in `tests/testfiles/keywords/reference_string.py`:

```python
# In reference_string.py
test_my_keyword_output = "*MY_KEYWORD\n        42\n"
```

> **Trailing whitespace:** String values in `reference_string.py` may contain trailing
> spaces within lines — this is intentional. LS-DYNA fixed-width fields are padded to
> their full column width and the output reflects that exactly. Do not strip these strings.

Then in the test:
```python
def test_my_keyword(ref_string):
    k = kwd.MyKeyword()
    k.some_field = 42
    assert k.write() == ref_string.test_my_keyword_output
```

## Common Pitfalls

- **Do not collect `tests/test_codegen/`** — these are for the codegen validation system only, not for testing PyDyna behavior
- **Trailing whitespace is intentional** — reference `.k` files and strings in `reference_string.py` contain trailing spaces on data lines because LS-DYNA fields are fixed-width padded. Never strip or auto-trim whitespace in these files.
- **Line endings**: `file_utils` normalizes `\r\n` → `\n`; always use `\n` in reference files
- **`write()` output includes trailing newline** — include it in reference strings
- **Import**: use `from ansys.dyna.core import keywords as kwd`, not direct module imports
- **Format type**: some tests require `format_type.long` — check with `kwd.MyKeyword(format=format_type.long)`
