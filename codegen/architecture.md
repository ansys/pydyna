# Codegen Architecture

This document describes the internal architecture of the PyDyna keyword class generator system.

## Overview

The codegen system generates Python classes for LS-DYNA keywords. It transforms keyword metadata from JSON schema files into ~3,173 fully-featured Python classes that provide a Pythonic API for LSDYNA keywords across multiple engineering domains (contact, boundary, define, section, mat, part, load, icfd, em, etc.).

### High-Level Flow Diagram

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│  kwd.json    │     │ manifest     │     │ additional-  │
│  (250MB)     │────▶│  .json       │────▶│ cards.json   │
│ Keyword defs │     │ Settings     │     │ Extra cards  │
└──────────────┘     └──────────────┘     └──────────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │  generate.py    │
                    │  (Entry Point)  │
                    │  - Load config  │
                    │  - Setup Jinja  │
                    │  - Orchestrate  │
                    └────────┬────────┘
                             │
                             ▼
            ┌────────────────────────────────┐
            │   For Each Keyword (~3,173)    │
            └────────────────────────────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │ class_generator │
                    │ .py             │
                    │ 1. Load data    │
                    │ 2. Run handlers │
                    │ 3. Render       │
                    └────────┬────────┘
                             │
            ┌────────────────┼────────────────┐
            │                │                │
            ▼                ▼                ▼
    ┌───────────────┐  ┌──────────┐  ┌──────────────┐
    │ Handler       │  │ KeywordData│  │ Jinja2       │
    │ Pipeline      │  │ Transform│  │ Templates    │
    │ (14 handlers) │  │          │  │ (keyword.j2) │
    └───────┬───────┘  └────┬─────┘  └──────┬───────┘
            │               │               │
            └───────────────┴───────────────┘
                            │
                            ▼
                  ┌──────────────────┐
                  │  Python Classes  │
                  │  src/.../auto/   │
                  │  - contact/      │
                  │  - define/       │
                  │  - section/      │
                  │  - mat/          │
                  │  - etc.          │
                  └──────────────────┘
```

### Handler Pipeline Architecture

The handler system uses **mutable reference semantics** (by design) where handlers modify `KeywordData` in place:

```
┌────────────────────────────────────────────────────────┐
│                 Handler Pipeline                       │
├────────────────────────────────────────────────────────┤
│                                                        │
│  ┌──────────────┐   ┌──────────────┐                 │
│  │ Pre-Process  │   │ Main Handlers│                 │
│  │ - Add indices│──▶│ (14 handlers)│                 │
│  │ - Prep insert│   │ In order:    │                 │
│  └──────────────┘   │ 1. reorder   │                 │
│                      │ 2. insert    │                 │
│                      │ 3. card-set  │──┐              │
│                      │ 4. conditional  │ Shared       │
│                      │ 5. table-group│◀┘ References!  │
│                      │ 6. shared-fld│                 │
│                      │ 7. rename-prop│                │
│                      │ ... etc       │                 │
│                      └───────┬───────┘                 │
│                              │                        │
│                              ▼                        │
│                      ┌──────────────┐                 │
│                      │Post-Process  │                 │
│                      │ 2 handlers:  │                 │
│                      │ - shared-fld │                 │
│                      │ - rename-prop│                 │
│                      └───────┬───────┘                 │
│                              │                        │
│                              ▼                        │
│                      ┌──────────────┐                 │
│                      │  Cleanup     │                 │
│                      │ - Insertions │                 │
│                      │ - Deletions  │                 │
│                      │ - Reindex    │                 │
│                      └──────────────┘                 │
│                                                        │
└────────────────────────────────────────────────────────┘
```

**Critical Design**: Handlers like `card-set` append card **references** (not copies), so later handlers' modifications (e.g., `conditional-card` adding `func`) automatically appear everywhere the card is referenced.

### Key Components

The codegen system consists of:

- **Specification Loading**: Reads keyword/card/field definitions from JSON manifest files (`kwd.json`, `manifest.json`, `additional-cards.json`)
- **Handler Pipeline**: Transforms loaded specifications through 14 specialized handlers
- **Template Rendering**: Uses Jinja templates with typed context objects to generate Python code
- **Output Management**: Writes generated files to `src/ansys/dyna/core/keywords/keyword_classes/auto/`

### Generation Flow

1. **Run `generate.py`**: Entry point for code generation
2. **Load Specs**: Manifest (`manifest.json`) and keyword definitions (`kwd.json`) are loaded and validated
3. **Initialize Label Registry**: Labels mapped to card objects for position-independent referencing
4. **Apply Handler Pipeline**: Each of 14 handlers processes the data sequentially in registration order
   - Handlers mutate `KeywordData` in place using reference semantics
   - Later handlers see and modify objects created by earlier handlers
5. **Post-Processing**: 2 handlers finalize operations requiring complete transformed data
6. **Cleanup**: Apply card insertions, delete marked cards, reindex
7. **Render Templates**: Jinja templates rendered using typed `TemplateContext` objects
8. **Write Output**: Generated files written to disk organized by domain subdirectories

Note: `kwd.json` is extremely large (~250MB) and should never be read in its entirety.

## Input Files

### kwd.json
Primary specification containing basic definitions for most keywords (cards, fields with offset, name, default value, option, width, helpstring). **Machine-generated** - do not modify by hand.

### manifest.json
Corrections and supplements to kwd.json:

**Corrections**:
- `reorder-card` - fixing card order
- `skip-card` - skipping unnecessary cards
- `override-subkeyword` - changing subkeyword names
- `override-field` - changing field definitions
- `replace-card` - replacing entire cards
- `insert-card` - inserting new cards
- `rename-property` - changing Python property names

**Supplements**:
- `table-card` - cards that repeat as 2D tables
- `series-card` - fields as 1D arrays across cards
- `card-set` - adjacent cards with their own specification (may repeat)
- `conditional-card` - cards active only under conditions
- `table-card-group` - adjacent cards that repeat as 2D tables
- `add-option` - option cards
- `shared-field` - fields shared across multiple cards
- Aliases (see below)

### additional-cards.json
Card definitions referenced by `insert-card` and `replace-card` handlers.

## Data Structures

**Core classes** (`data_model/keyword_data.py`):
- `KeywordData` - Top-level keyword specification
- `Card` - Individual card with fields and metadata
- `Field` - Field definition with type, description, default, etc.

**Metadata classes** (`data_model/metadata.py`):
- `TableCardMetadata` - for table cards
- `VariableCardMetadata` - for series cards
- `ExternalCardMetadata` - for external implementations
- `OptionGroup`, `CardSet`, `LinkData`, `MixinImport`, `DataclassDefinition`

### Label Registry

The `LabelRegistry` (`data_model/label_registry.py`) provides stable named references to cards:

- Cards from kwd.json are assigned labels via `"labels"` in manifest entries
- Handlers reference cards by label (e.g., `"refs": ["stress_data", "hisv"]`)
- Labels resolve to current positions, handling insertions/reorderings automatically

## Handler System

### Execution Order

Handlers are executed in a specific order defined in `handlers/registry.py`. **Order is critical.**

**Why Explicit Order?** The handler system uses **explicit registration order** to ensure reference semantics work correctly. Handlers that group cards (`card-set`, `table-card-group`) rely on **reference semantics** - they append references to card objects, not copies. Later handlers modify these same objects in-place, so the order must be carefully controlled.

**Standard order** (Jan 2026):
1. `reorder-card` - Reorders cards; must run first
2. `skip-card` - Marks cards to skip
3. `insert-card` - Inserts new cards
4. `table-card` - Transforms cards into repeatable tables
5. `override-field` - Modifies field properties
6. `replace-card` - Replaces entire cards
7. `series-card` - Transforms cards into variable-length series
8. `add-option` - Adds keyword options
9. `card-set` - Groups cards into reusable sets
10. `conditional-card` - Adds conditional logic
11. `rename-property` - Renames property accessors
12. `table-card-group` - Groups cards into repeating units
13. `external-card-implementation` - Links to external cards
14. `shared-field` - Creates shared field definitions

### Reference Semantics

**CRITICAL**: Handlers that group cards (`card-set`, `table-card-group`) use **reference semantics** - they append actual card objects, NOT deep copies.

**Why?** Later handlers modify cards in-place:
- `card-set` creates a set by appending references
- `conditional-card` later sets `func` on those same objects
- Changes appear in both places because they're the same objects

**Do NOT use `copy.deepcopy()` when grouping cards** - it breaks this pattern.

### Index vs. Position

After `reorder-card`:
- Cards have an `index` property (original index)
- Handlers use **list positions** `kwd_data.cards[i]`, not card indices
- `card-set` stores `source_index` (original) and assigns new sequential indices

**Example** (SECTION_SHELL):
```
Original cards: [card0, card1, card2, card3, card4, card5]
Reorder: [0, 1, 4, 2, 3, 5]
After reorder: [card0, card1, card4, card2, card3, card5]
               pos0   pos1   pos2   pos3   pos4   pos5

conditional-card with index:3 operates on position 3 (which is original card2)
card-set with source-indices:[0,1,2,3,4,5] copies from positions 0-5
```

### Insert-Card Index Computation

The `insert-card` handler processes insertions in **reverse index order** to preserve index validity. However, Python's `list.insert(i, x)` has special behavior: if `i >= len(list)`, it appends to the end. This creates a non-intuitive mapping between specified indices and final positions.

**The Problem**: With 3 original cards [A, B, C], if you want to insert X, Y, Z to get final positions [A, B, C, X, Y, Z]:

```python
# Naive approach - specify indices [3, 4, 5]:
# Reverse sort: [5, 4, 3]
# Insert @5 (appends): [A, B, C, Z]      # Z intended for position 5
# Insert @4 (appends): [A, B, C, Z, Y]   # Y intended for position 4
# Insert @3:           [A, B, C, X, Z, Y] # X at position 3
# Result: X@3, Z@4, Y@5 - WRONG ORDER!
```

**The Solution**: Calculate indices that account for reverse processing and append behavior:

```python
# Correct approach - specify indices [3, 6, 4]:
# Reverse sort: [6, 4, 3]
# Insert @6 (appends): [A, B, C, Y]      # Y (for final pos 4)
# Insert @4 (appends): [A, B, C, Y, Z]   # Z (for final pos 5)
# Insert @3:           [A, B, C, X, Y, Z] # X (for final pos 3)
# Result: X@3, Y@4, Z@5 - CORRECT!
```

**Index Calculation Rule**: To insert N cards starting at position P into a list of length L:
1. First card: use index P (will be inserted at position P)
2. Second card: use index > L so it appends, will be pushed to P+1 by first insertion
3. Continue: each subsequent card uses a higher out-of-range index

**Real Example** (INITIAL_STRESS_SHELL LARGE format):
- Original cards: [0:main, 1:stress, 2:hisv] (3 cards)
- Want to insert: LARGE_CARD1@3, LARGE_CARD2@4, LARGE_HISV@5
- Manifest specifies: `[{index:3, card:CARD1}, {index:6, card:CARD2}, {index:4, card:HISV}]`
- Processing: @6→CARD2 appends, @4→HISV appends, @3→CARD1 inserts
- Final: [main, stress, hisv, CARD1, CARD2, HISV] with positions 0-5

### Handler Settings

Each handler defines its own settings dataclass:
```python
@dataclass
class TableCardSettings:
    index: int
    length_func: str
    # ...
```

Settings are validated at parse time via `_parse_settings()`.

### Handler Base Utilities (Jan 2026 Refactoring)

To eliminate code duplication across handlers, the system provides shared base classes and utilities in `handlers/base_settings.py`:

**`LabelRefSettings` Base Class**:
```python
@dataclass
class LabelRefSettings:
    """Base class for handler settings that use label-based card references."""

    ref: str  # Label reference to the card

    def resolve_index(self, registry: LabelRegistry, cards: List[Any]) -> int:
        """Resolve the label reference to a concrete card index."""
        return registry.resolve_index(self.ref, cards)
```

Handlers that use label-based references inherit from `LabelRefSettings`:
- `conditional_card.py` - ConditionalCardSettings
- `skip_card.py` - SkipCardSettings
- `replace_card.py` - ReplaceCardSettings
- `override_field.py` - OverrideFieldSettings
- `rename_property.py` - RenamePropertySettings
- `external_card.py` - ExternalCardSettings
- `series_card.py` - SeriesCardSettings
- `table_card.py` - TableCardSettings

**`parse_settings_list()` Utility**:
```python
def parse_settings_list(settings_class, settings: List[Dict[str, Any]]) -> List[Any]:
    """Generic settings parser - eliminates duplicated _parse_settings methods."""
    return [settings_class.from_dict(s) for s in settings]
```

Replaces duplicated `_parse_settings()` methods in all handlers.

**Field Utilities**:
```python
def find_field_in_card(card: Card, field_name: str) -> Optional[Field]:
    """Find a field by name (case-insensitive) in a card."""


def modify_field_in_cards(
    cards: List[Card],
    indices: List[int],
    field_name: str,
    modifications: Dict[str, Any],
) -> None:
    """Apply field modifications to multiple cards."""
```

**Benefits**:
- ~150+ lines of duplicate code eliminated
- Consistent error handling and validation
- Single source of truth for common patterns
- Easier to maintain and extend

**Keyword Utilities** (`utils/keyword_utils.py`):
```python
@dataclass
class KeywordNames:
    """Consolidated keyword name transformations."""

    original: str
    fixed: str
    classname: str
    filename: str

    @classmethod
    def from_keyword(cls, keyword: str) -> "KeywordNames":
        """Generate all name variants from a keyword."""


def filter_keywords_by_domain(keywords: List[str], domains: List[str]) -> List[str]:
    """Filter keywords by domain (e.g., 'contact', 'define')."""
```

Eliminates repetitive name processing logic in `generate.py`.

## Testing

### Validation Workflow

The validation script (`codegen/validate.sh`) runs:
1. Clean and regenerate all keyword classes
2. Git diff check for unintended changes (both auto/ and doc/)
3. Pre-commit hooks (formatting, linting)
4. Dead code detection (coverage analysis)
5. Unit tests (`pytest -m codegen`)

**CI Integration**: The CI uses the same validation script to ensure local and CI validation are identical.

### Dead Code Detection

After refactoring, run:
```bash
python codegen/find_dead_code.py --threshold 80
```
Files with <80% coverage should be reviewed.

### Output Validation

Generated files must remain unchanged after refactoring:
```bash
python codegen/generate.py -c
python codegen/generate.py
git diff src/ansys/dyna/core/keywords/keyword_classes/auto/
```

## Extending the System

### For Systemic Changes

For changes affecting all keywords, update handlers or templates.

### For Complex Keywords

In some cases, the generation system can be difficult to extend to support keyword semantics, especially if the functionality in the keyword library (`ansys/dyna/core/lib`) is lacking.

**Strategy**: Hand-write the keyword, extend the keyword library as needed, then work backwards to update the code-generation system to produce an equivalent keyword, and finally delete the hand-written keyword.

### Manual Subclasses

For keyword-specific customizations:

1. Create file under `keyword_classes/manual/`:
   ```python
   from ansys.dyna.core.keywords.keyword_classes.auto.define_table import (
       DefineTable as _Auto,
   )


   class DefineTable(_Auto):
       # Add custom properties, methods
       pass
   ```

2. Export from `manual_keywords.py`:
   ```python
   from .manual.define_table import DefineTable  # noqa: F401
   ```

Benefits:
- Regenerating auto classes won't overwrite customizations
- Codegen checks verify auto files match templates
- Changes are isolated and clear

## What the Generator Produces

The class generator uses Jinja templates to generate three distinct things:

1. **Python classes** - What users of PyDyna interact with directly
2. **Import machinery** - `auto_keywords.py` with import statements
3. **Keyword to type mapping** - Dictionary mapping keyword names to Python classes

## Aliasing

Some keywords are defined identically (e.g., `MAT_058` and `MAT_LAMINATED_COMPOSITE_FABRIC`). The generator produces two classes where one aliases the other's behavior. If both are in kwd.json, one is ignored. If only one is defined (e.g., `SET_NODE` and `SET_NODE_LIST`), aliasing still works.

