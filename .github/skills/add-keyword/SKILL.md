---
name: add-keyword
description: >
  Add a new LS-DYNA keyword class to PyDyna via codegen.
  Use for: adding a keyword missing from the library, creating a new keyword
  class from kwd.json, editing manifest.json to configure generation,
  running codegen after manifest changes, verifying generated output.
  See also: run-codegen skill for validation steps after generating.
argument-hint: "LS-DYNA keyword name to add (e.g. SECTION_SHELL, MAT_ELASTIC)"
---

# Add Keyword Skill

## Overview

Most keyword classes are **auto-generated** from `codegen/kwd.json` via `codegen/generate.py`.
To add or configure a keyword, edit `codegen/manifest.json` then re-run codegen.

**Do not** edit files in `src/ansys/dyna/core/keywords/keyword_classes/auto/` directly —
they are overwritten on every codegen run.

## Step 1 — Check if the keyword already exists

```python
from ansys.dyna.core import keywords as kwd
print(hasattr(kwd, "MyKeyword"))  # True if already generated
```

Also check `codegen/manifest.json` for the keyword name — it may exist but be set to `"action": "skip"`.

## Step 2 — Find the keyword in `kwd.json`

Search `codegen/kwd.json` for the keyword name (e.g. `SECTION_SHELL`). Note the `cards` array structure —
this drives the generated class layout.

> **Do not open `kwd.json` fully** — it is 80k+ lines. Use grep or targeted search.

## Step 3 — Add or update the entry in `manifest.json`

`codegen/manifest.json` controls generation for each keyword. Minimal entry:

```json
"MY_KEYWORD": {
  "action": "generate"
}
```

### `action` values

| Value | Effect |
|-------|--------|
| `"generate"` | Auto-generate a class (default) |
| `"manual"` | Skip generation; use manual implementation in `keyword_classes/manual/` |
| `"skip"` | Skip generation and type mapping (e.g. option variants merged into base class) |

### Common manifest handlers

Add these under the keyword entry to customize the generated class:

```json
"MY_KEYWORD": {
  "action": "generate",
  "reorder-card": [0, 1, 3, 2],
  "skip-card": [{"ref": "optional_card_label"}],
  "conditional-card": [
    {"ref": "optional_card_label", "func": "lambda self: self.flag == 1"}
  ],
  "table-card": [
    {"ref": "data_card", "length_func": "self.nrows"}
  ],
  "add-field": [
    {
      "ref": "card_label",
      "fields": [{"name": "EXTRA", "type": "integer", "position": 48, "width": 8, "default": null, "help": ""}]
    }
  ]
}
```

See [agents/codegen/handlers.md](../../agents/codegen/handlers.md) for the full handler reference.

## Step 4 — Run codegen

```bash
# Generate only the affected keyword (fast, use during development)
python codegen/generate.py -k MY_KEYWORD

# Generate all keywords
python codegen/generate.py
```

Check the output in `src/ansys/dyna/core/keywords/keyword_classes/auto/`.

## Step 5 — Validate

```bash
bash codegen/validate.sh --quick
```

A clean diff (no unexpected changes) means the manifest edit is correct.
See the `run-codegen` skill for full validation details.

## Step 6 — Write tests

Add a test in `tests/` to verify parsing and rendering. See the `keyword-tests` skill.

## Manual Implementations

If the keyword requires custom Python logic (complex card logic, special DataFrame handling),
use `"action": "manual"` in the manifest and create a class in:

```
src/ansys/dyna/core/keywords/keyword_classes/manual/my_keyword.py
```

The manual class must subclass `KeywordBase` and be registered in `manual_keywords.py`.

## Coding Style

- Line limit: 120 characters
- Use `logging` module, not `print()`; add `logger = logging.getLogger(__name__)` to new modules
- No inline comments on imports
