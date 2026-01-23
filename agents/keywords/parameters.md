# Parameters and Deck Expansion in PyDyna

## Overview

PyDyna supports LS-DYNA parameter substitution through `*PARAMETER` (global) and `*PARAMETER_LOCAL` (local) keywords. This document explains how parameters work, how deck expansion processes includes, and the critical scoping mechanisms that ensure proper parameter isolation.

## Parameter Types

### Global Parameters (`*PARAMETER`)

Global parameters are visible everywhere in the deck and all included files:

```lsdyna
*PARAMETER
R_density    7850.0
I_elements   1000
```

- Accessible in the file where defined
- Accessible in all included files (child scopes)
- Accessible after returning to parent files
- Used with `&` prefix: `&density`, `&elements`

### Local Parameters (`*PARAMETER_LOCAL`)

Local parameters are scoped to the file where they're defined:

```lsdyna
*PARAMETER_LOCAL
R_temp_val   999.0
```

- Only accessible in the file where defined
- Accessible in files included FROM that file (descendant scopes)
- **NOT** accessible in parent files
- **NOT** accessible in sibling includes
- Used with `&` prefix just like global parameters

### Expression Parameters (`*PARAMETER_EXPRESSION`)

Compute parameter values from arithmetic expressions:

```lsdyna
*PARAMETER
R gravtime 0.3
R tramp    0.001
R diemv    145.45
R clsv     1000.0
*PARAMETER_EXPRESSION
R tramp1   tramp+gravtime
R endtime  tramp1+(abs(diemv)-0.5*clsv*tramp)/clsv
```

**Features:**
- Operators: `+`, `-`, `*`, `/`, `**` (power), `%` (mod)
- Functions: `sin`, `cos`, `tan`, `sqrt`, `abs`, `exp`, `log`, `min`, `max`, etc.
- References: Use parameter names with or without `&` prefix
- Dependencies: Auto-resolved via topological sort
- Variants: `*PARAMETER_EXPRESSION_LOCAL`, `*PARAMETER_EXPRESSION_NOECHO`

**Constraints:**
- Parameters must be defined before referenced
- Circular dependencies detected and fail gracefully
- Evaluation happens during deck loading (before other parameter substitutions)
- Parameter names ≤7 chars (including type prefix) to avoid field truncation

## Parameter Naming

- Type prefix: `R` (real), `I` (integer), `C` (character)
- Followed by underscore or name characters
- Field width: 10 characters total
- Recommended: ≤7 chars total (including prefix) for safe field usage

Examples: `Rgbl`, `I_count`, `R_density`

## How Parameters Work

### ParameterSet Class

Manages parameters with hierarchical scoping:

```python
class ParameterSet:
    def __init__(self, parent: Optional['ParameterSet'] = None):
        self._params = dict()  # Local scope
        self._parent = parent  # Parent scope
```

Key methods:
- `add(param, value)`: Add global parameter
- `add_local(param, value)`: Add local-only parameter
- `get(param)`: Lookup (local → parent chain)
- `copy_with_child_scope()`: Create child scope

### Scope Resolution

`get(param)` search order:
1. Check local `_params`
2. Recursively check `_parent.get()` if not found
3. Raise `KeyError` if not found anywhere

Child scopes see parent parameters; child modifications don't affect parents.

## Deck Expansion Process

### Overview

Deck expansion replaces `*INCLUDE` keywords with the actual contents of included files. This happens through the `Deck.expand()` method.

### Expansion Flow

**1. Initial Import** (`Deck.import_file()`):
```python
deck = Deck()
deck.import_file("top.k")
```
- Loads top-level file
- `*PARAMETER` → stored in `deck.parameters`
- `*INCLUDE` → stored but not processed
- Undefined param references → `string_keywords`

**2. Expansion** (`Deck.expand()`):
```python
expanded_deck = deck.expand(recurse=True, cwd=cwd)
```
- Creates new deck sharing parent parameters
- Processes includes via `_expand_helper()`
- Returns flattened deck

**3. Processing Includes** (`_expand_helper()`):
```python
for keyword in self.all_keywords:
    if keyword.keyword == "INCLUDE":
        include_deck = self._prepare_deck_for_expand(keyword)
        context = ImportContext(xform, include_deck, expand_include_file)
        include_deck._import_file(expand_include_file, encoding, context)
        expanded = include_deck._expand_helper(search_paths, True)
        keywords.extend(expanded)
```

### ImportContext and Scoping

**Critical**: `ImportContext` must reference the include deck, not parent:

```python
# CORRECT
include_deck = self._prepare_deck_for_expand(keyword)
context = ImportContext(xform, include_deck, expand_include_file)
include_deck._import_file(expand_include_file, encoding, context)
```

When `ParameterHandler.after_import()` runs, it uses `context.deck`:

```python
def after_import(self, context: ImportContext, keyword):
    if isinstance(keyword, kwd.ParameterLocal):
        _load_parameters(context.deck, keyword, local=True)
```

Wrong context → parameters added to wrong scope!

### Preparing Include Decks

The `_prepare_deck_for_expand()` method sets up child scopes:

```python
def _prepare_deck_for_expand(self, keyword: KeywordBase):
    include_deck = Deck(format=keyword.format)
    # Create child scope - include can see parent params but not vice versa
    include_deck.parameters = self.parameters.copy_with_child_scope()
    # Copy import handlers so includes process parameters correctly
    for import_handler in self._import_handlers:
        include_deck.register_import_handler(import_handler)
    return include_deck
```

Key points:
- `copy_with_child_scope()` creates NEW `ParameterSet` with `_params = {}` and `_parent = self.parameters`
- Child can read parent parameters via scope chain
- Parameters added to child's `_params` don't leak to parent

## Parameter Substitution Timeline

**1. During `Deck.loads()`** (called by `_import_file()`):
- Keyword parsed from text
- `keyword.loads(keyword_data, deck.parameters)` called
- Parameter references (`&density`) resolved immediately
- If undefined → becomes `string_keyword`

**2. After loading**:
- `ImportHandler.after_import()` called
- `ParameterHandler` processes `*PARAMETER*` keywords
- Parameters added to `deck.parameters`

**Implication**: Parameters must be defined BEFORE use in same file.

## Common Pitfalls

**Wrong Context Deck**:
```python
# WRONG - parameters go to parent
context = ImportContext(xform, self, expand_include_file)
include_deck = self._prepare_deck_for_expand(keyword)

# CORRECT - parameters go to include_deck
include_deck = self._prepare_deck_for_expand(keyword)
context = ImportContext(xform, include_deck, expand_include_file)
```

**Copying Instead of Scoping**:
```python
# WRONG - breaks scope chain
include_deck.parameters = copy.deepcopy(self.parameters)

# CORRECT - creates child scope
include_deck.parameters = self.parameters.copy_with_child_scope()
```

**Parameter Before Definition**:
```lsdyna
# WRONG
*SECTION_SOLID
        10  &density    $ ERROR: not defined yet
*PARAMETER
R_density    7850.0

# CORRECT
*PARAMETER
R_density    7850.0
*SECTION_SOLID
        10  &density    $ OK
```

**Option Cards Without Parameter Pass-Through**:
```python
# WRONG - option cards won't receive parameters
def _try_read_options_with_no_title(self, buf):
    # ...
    card.read(buf)  # Missing parameter_set!

# CORRECT - pass parameters to option cards
def _try_read_options_with_no_title(self, buf, parameters=None):
    # ...
    if parameters is not None:
        with parameters.scope(f"option_{card.name}"):
            card.read(buf, parameters)
    else:
        card.read(buf, parameters)
```

Note: This was fixed in issue #1067. Option cards (cards with `title_order=0`) now properly receive the `ParameterSet` when being read, enabling parameter substitution in optional keyword cards like CONTACT's SOFT option.

## Testing Parameter Scoping

### Test Structure

Tests for parameter scoping should verify:

1. **Global parameters are visible everywhere**:
   - In file where defined
   - In included files
   - In files that include the file where defined

2. **Local parameters are properly isolated**:
   - Accessible in file where defined
   - Accessible in files included FROM that file
   - NOT accessible in parent files (isolation from parent)
   - NOT accessible in sibling includes (isolation from siblings)

### Example Test Fixture Structure

```
tests/testfiles/keywords/expand_parameters/local/
├── top.k                     # Parent file with global param
├── include_with_local.k      # Included file with local param
├── sibling_test_top.k        # Parent including multiple siblings
├── sibling_a.k               # First sibling with local param
└── sibling_b.k               # Second sibling (should not see sibling_a's local)
```

### Test Pattern for Isolation

```python
def test_local_parameter_isolation():
    deck = Deck()
    deck.import_file("top.k")  # Has global param
    deck = deck.expand(recurse=True, cwd=test_dir)

    # Global param should be accessible
    assert deck.parameters.get("global_param") == expected_value

    # Local param from include should NOT be accessible
    with pytest.raises(KeyError):
        deck.parameters.get("local_param")
```

## Implementation Checklist

When implementing parameter-related features:

- [ ] Consider scope: Is this global or local?
- [ ] Check context: Which deck should receive the parameter?
- [ ] Verify timing: Is parameter defined before use?
- [ ] Test isolation: Verify parent/sibling isolation for local params
- [ ] Test visibility: Verify child scopes can see parent params
- [ ] Add logging: Use `logger.debug()` for parameter operations

## Parameter Substitution in Card Types

All card types now support LS-DYNA parameter substitution (`&parameter` and `-&parameter` syntax). This feature allows keywords to reference parameters defined via `*PARAMETER` or `*PARAMETER_LOCAL` keywords.

### Supported Card Types

**Card** (standard single-line cards):
- Used for most keyword cards with fixed fields
- Full parameter support via `load_dataline()`
- Example: `*SECTION_SHELL` with `&thickness` parameter

**SeriesCard** (arrays/lists):
- Used for curve data, material properties arrays, etc.
- Parameters supported in bounded and unbounded modes
- Implementation: Passes `parameter_set` through to `load_dataline()`
- Example: `*DEFINE_CURVE` with parametric Y-values

**TableCard** (tabular data):
- Used for node coordinates, element connectivity, etc.
- Parameters detected automatically via `_has_parameters()`
- Smart path selection: Uses fast pandas path when no parameters, falls back to `load_dataline()` when parameters detected
- Implementation: `_load_lines_with_parameters()` processes line-by-line
- Example: `*NODE` with parametric coordinates

**TableCardGroup** (multiple interleaved tables):
- Used for keywords with multiple cards per row
- Inherits parameter support from TableCard
- Parameters work across all sub-cards in the group

**OptionCardSet** (optional cards):
- Used for keyword options (e.g., SOFT option in CONTACT keywords)
- Parameters supported via pass-through to underlying cards
- Implementation: Passes `parameter_set` to each card in the option
- Parameter scoping: Uses `option_{name}` scope for reference retention
- Example: `*CONTACT_TIED_SHELL_EDGE_TO_SURFACE_BEAM_OFFSET` with `&soft` parameter on SOFT option card
- Note: Fixed in issue #1067 to properly pass parameters to option cards read via `_try_read_options_with_no_title()`

### Parameter Name Constraints

LS-DYNA parameter names must fit within field widths. For a 10-character field:
- `&dens` fits (5 chars)
- `&density` may be truncated (8 chars)

Always verify parameter names fit within the field widths defined in your keyword schema.

### Example Usage

```python
from ansys.dyna.core.lib.deck import Deck

deck_text = """*KEYWORD
*PARAMETER
R_dens   7850.0
I_nid    1000
*DEFINE_CURVE
     &nid
       0.0       0.0
     &dens     100.0
*NODE
    &nid     &dens       0.0       0.0
*END"""

deck = Deck()
deck.loads(deck_text)

# All keywords parse successfully with substituted values
curve = deck.keywords[0]
assert curve.lcid == 1000  # From I_nid parameter

node = deck.keywords[1]
assert node.nodes.table["nid"][0] == 1000
assert node.nodes.table["x"][0] == 7850.0
```

### Implementation Details

**SeriesCard** (`src/ansys/dyna/core/lib/series_card.py`):
- `read()` accepts `parameter_set` and passes to load methods
- `_read_line()` passes `parameter_set` to `load_dataline()`
- Works in both bounded and unbounded modes

**TableCard** (`src/ansys/dyna/core/lib/table_card.py`):
- `_has_parameters()` detects `&` in data lines
- `_load_lines_with_parameters()` uses `load_dataline()` for each row
- `_load_lines()` chooses appropriate path based on parameter detection
- No performance impact when parameters not present

**TableCardGroup** (`src/ansys/dyna/core/lib/table_card_group.py`):
- Passes `parameter_set` to child `TableCard` instances
- No special handling needed (inherits from TableCard)

### Testing

See `tests/test_parameter_substitution.py` for comprehensive test coverage including:
- Bounded and unbounded modes for all card types
- Negative parameters (`-&param`)
- Type conversion and validation
- Error handling for missing/mismatched parameters
- Mixed parameters and literal values
- Regression testing for non-parameter cases

### Error Handling

The parameter substitution system provides clear error messages:

- **Missing parameter**: `KeyError` with parameter name
- **Type mismatch**: `TypeError` explaining the conversion failure
- **No parameter set**: `ValueError` when `&` found but no `parameter_set` provided

## Implementation Checklist (Extended)

When implementing parameter-related features:

- [ ] Consider scope: Is this global or local?
- [ ] Check context: Which deck should receive the parameter?
- [ ] Verify timing: Is parameter defined before use?
- [ ] Test isolation: Verify parent/sibling isolation for local params
- [ ] Test visibility: Verify child scopes can see parent params
- [ ] Add logging: Use `logger.debug()` for parameter operations
- [ ] Card type support: Ensure parameter_set is passed through card read chains
- [ ] Field width constraints: Verify parameter names fit in field widths

## Key Files

- `src/ansys/dyna/core/lib/parameters.py`: ParameterSet class and ParameterHandler
- `src/ansys/dyna/core/lib/deck.py`: Deck expansion and include processing
- `src/ansys/dyna/core/lib/deck_loader.py`: Keyword loading and parameter substitution
- `src/ansys/dyna/core/lib/import_handler.py`: ImportContext and ImportHandler base
- `src/ansys/dyna/core/lib/kwd_line_formatter.py`: `load_dataline()` with parameter substitution
- `src/ansys/dyna/core/lib/card.py`: Standard card parameter support
- `src/ansys/dyna/core/lib/series_card.py`: SeriesCard parameter support
- `src/ansys/dyna/core/lib/table_card.py`: TableCard parameter support
- `src/ansys/dyna/core/lib/table_card_group.py`: TableCardGroup parameter support
- `tests/test_parameter_substitution.py`: Comprehensive parameter substitution tests

## Related Documentation

- [../codegen/](../codegen/): Auto-generated keyword classes
- [linking.md](linking.md): Keyword relationships and properties
- [legacy.md](legacy.md): Legacy keyword handling
- GitHub Issue #641: Parameter substitution for all card types
