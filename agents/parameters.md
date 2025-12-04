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

## Parameter Naming Convention

LS-DYNA parameters follow a strict naming convention:

- First character indicates type: `R` (real/float), `I` (integer), `C` (character/string)
- Followed by underscore or name
- Total field width is 10 characters
- Parameter name (including type prefix) should be ≤7 characters for safe field usage

Examples:
- `Rgbl` - Real parameter named "gbl" (4 chars total)
- `I_count` - Integer parameter named "count" (7 chars total)
- `R_density` - Real parameter named "density" (9 chars total)

## How Parameters Work

### Parameter Storage: ParameterSet Class

The `ParameterSet` class (`src/ansys/dyna/core/lib/parameters.py`) manages parameter storage with hierarchical scoping:

```python
class ParameterSet:
    def __init__(self, parent: Optional['ParameterSet'] = None):
        self._params = dict()  # Local parameters in this scope
        self._parent = parent  # Parent scope for lookup
```

Key methods:

- `add(param, value)`: Add global parameter to local scope
- `add_local(param, value)`: Add local-only parameter to local scope
- `get(param)`: Retrieve parameter, checking local scope then parent chain
- `copy_with_child_scope()`: Create child scope with this as parent

### Scope Chain Resolution

When accessing a parameter via `get()`, the search follows this order:

1. Check local `_params` dictionary
2. If not found and `_parent` exists, recursively call `parent.get()`
3. If not found anywhere, raise `KeyError`

This creates a scope chain where child scopes can see parent parameters, but modifications to child scopes don't affect parents.

## Deck Expansion Process

### Overview

Deck expansion replaces `*INCLUDE` keywords with the actual contents of included files. This happens through the `Deck.expand()` method.

### Expansion Flow

1. **Initial Import** (`Deck.import_file()`):
   ```python
   deck = Deck()
   deck.import_file("top.k")
   ```
   - Loads top-level file
   - `*PARAMETER` keywords are loaded into `deck.parameters`
   - `*INCLUDE` keywords are stored but not processed
   - Keywords referencing undefined parameters become `string_keywords`

2. **Expansion** (`Deck.expand()`):
   ```python
   expanded_deck = deck.expand(recurse=True, cwd=cwd)
   ```
   - Creates a new deck: `new_deck = Deck()`
   - Shares parent parameters: `new_deck.parameters = self.parameters`
   - Calls `_expand_helper()` to process includes
   - Returns flattened deck with includes resolved

3. **Processing Each Include** (`_expand_helper()`):
   ```python
   for keyword in self.all_keywords:
       if keyword.keyword == "INCLUDE":
           include_deck = self._prepare_deck_for_expand(keyword)
           context = ImportContext(xform, include_deck, expand_include_file)
           include_deck._import_file(expand_include_file, encoding, context)
           expanded = include_deck._expand_helper(search_paths, True)
           keywords.extend(expanded)
   ```

### Critical: ImportContext and Parameter Scoping

The `ImportContext` determines which deck receives imported parameters. This is **critical** for proper `PARAMETER_LOCAL` scoping:

```python
# CORRECT - context references include_deck
include_deck = self._prepare_deck_for_expand(keyword)
context = ImportContext(xform, include_deck, expand_include_file)
include_deck._import_file(expand_include_file, encoding, context)
```

When `ParameterHandler.after_import()` is called, it uses `context.deck` to add parameters:

```python
def after_import(self, context: ImportContext, keyword):
    if isinstance(keyword, kwd.ParameterLocal):
        _load_parameters(context.deck, keyword, local=True)
```

If `context.deck` points to the wrong deck, parameters will be added to the wrong scope!

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

Understanding WHEN parameter substitution happens is crucial:

1. **During `Deck.loads()`** (called by `_import_file()`):
   - Each keyword is parsed from text
   - `keyword.loads(keyword_data, deck.parameters)` is called
   - Parameter references like `&density` are resolved at this moment
   - If parameter not found, keyword becomes `string_keyword` (unparsed)

2. **AFTER keyword is loaded**:
   - `ImportHandler.after_import()` is called
   - `ParameterHandler` processes `*PARAMETER` and `*PARAMETER_LOCAL` keywords
   - Parameters are added to `deck.parameters` (the deck from context)

This ordering means:
- Parameters must be defined BEFORE they're used in the same file
- Parameters defined in includes are NOT available in parent files (because parent is already loaded)
- Parameters defined in parent ARE available in includes (because include loads after parent)

## Common Pitfalls and Solutions

### Pitfall 1: Wrong Context Deck

**Problem**: Creating `ImportContext` with parent deck instead of include deck:

```python
# WRONG - parameters go to parent!
context = ImportContext(xform, self, expand_include_file)
include_deck = self._prepare_deck_for_expand(keyword)
include_deck._import_file(expand_include_file, encoding, context)
```

**Solution**: Create context AFTER creating include_deck, and use include_deck:

```python
# CORRECT - parameters go to include_deck
include_deck = self._prepare_deck_for_expand(keyword)
context = ImportContext(xform, include_deck, expand_include_file)
include_deck._import_file(expand_include_file, encoding, context)
```

### Pitfall 2: Copying Instead of Scoping

**Problem**: Using `copy.deepcopy()` or direct assignment for include parameters:

```python
# WRONG - creates independent copy or shares same object
include_deck.parameters = copy.deepcopy(self.parameters)
# or
include_deck.parameters = self.parameters  # shares same object!
```

**Solution**: Use scope chain:

```python
# CORRECT - creates child scope with parent reference
include_deck.parameters = self.parameters.copy_with_child_scope()
```

### Pitfall 3: Parameter Reference Before Definition

**Problem**: Using parameter before it's defined:

```lsdyna
*SECTION_SOLID
$#   secid    elform
        10  &density    $ ERROR: density not defined yet
*PARAMETER
R_density    7850.0
```

**Solution**: Define parameters before use:

```lsdyna
*PARAMETER
R_density    7850.0
*SECTION_SOLID
$#   secid    elform
        10  &density    $ OK: density already defined
```

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

## Key Files

- `src/ansys/dyna/core/lib/parameters.py`: ParameterSet class and ParameterHandler
- `src/ansys/dyna/core/lib/deck.py`: Deck expansion and include processing
- `src/ansys/dyna/core/lib/deck_loader.py`: Keyword loading and parameter substitution
- `src/ansys/dyna/core/lib/import_handler.py`: ImportContext and ImportHandler base

## Related Documentation

- [Codegen Guide](codegen.md): Auto-generated keyword classes
- [Linked Keywords](linked_keywords.md): Keyword relationships and properties
- GitHub Issue #641: PARAMETER_LOCAL scoping implementation
