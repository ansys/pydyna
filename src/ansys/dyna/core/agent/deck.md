# Deck Operations

Detailed guide for working with the `Deck` class in PyDyna.

## Overview

The `Deck` class is the main container for LS-DYNA keywords. It represents a complete keyword file and provides methods for loading, manipulating, and writing keyword data.

## Creating Decks

### Empty Deck

```python
from ansys.dyna.core import Deck

# Create a new empty deck
deck = Deck(title="My Analysis")
```

### From File

```python
# Load an existing keyword file
deck = Deck()
deck.import_file("model.k")

# Load with encoding detection (automatic)
deck.import_file("model_utf8.k")
```

### From String

```python
# Load from string content
keyword_content = """*KEYWORD
*TITLE
My Model
*END
"""
deck = Deck()
deck.loads(keyword_content)
```

## Accessing Keywords

### Iteration

```python
# Iterate through all keywords
for kw in deck.keywords:
    print(f"{kw.keyword}_{kw.subkeyword}")

# Access by index (KeywordCollection is list-like)
first_kw = deck.keywords[0]
last_kw = deck.keywords[-1]
```

### Filtering by Type

```python
# Get all keywords of a specific type
materials = list(deck.get_kwds_by_type("MAT"))
sections = list(deck.get_kwds_by_type("SECTION"))
nodes = list(deck.get_kwds_by_type("NODE"))

# Get keywords by full type and subtype
beam_sections = list(deck.get_kwds_by_full_type("SECTION", "BEAM"))
shell_sections = list(deck.get_kwds_by_full_type("SECTION", "SHELL"))
```

### Advanced Filtering with KeywordCollection

The `keywords` property returns a `KeywordCollection` with fluent filtering:

```python
from ansys.dyna.core import keywords

# Filter with lambda
high_id_materials = deck.keywords.where(
    lambda k: isinstance(k, keywords.Mat001) and k.mid > 100
)

# Chain filters
steel_materials = deck.keywords.where(
    lambda k: k.keyword == "MAT" and k.ro > 7000
)
```

## Modifying Decks

### Adding Keywords

```python
from ansys.dyna.core import keywords

# Add a single keyword
mat = keywords.Mat001()
mat.mid = 1
mat.ro = 7850
deck.append(mat)

# Add multiple keywords
contacts = []
for i in range(10):
    contact = keywords.ContactAutomaticSingleSurface(ssid=i+1)
    contacts.append(contact)
deck.extend(contacts)
```

### Removing Keywords

```python
# Remove by index
deck.remove(0)  # Remove first keyword

# Remove multiple by indices
deck.remove([0, 2, 4])

# Find and remove by condition
indices_to_remove = []
for i, kw in enumerate(deck.keywords):
    if kw.keyword == "MAT" and kw.mid > 100:
        indices_to_remove.append(i)

# Remove in reverse order to maintain valid indices
for idx in reversed(indices_to_remove):
    deck.remove(idx)
```

### Modifying Existing Keywords

```python
# Find and modify a specific keyword
for kw in deck.get_kwds_by_type("MAT"):
    if kw.mid == 1:
        kw.e = 2.5e11  # Update Young's modulus
        break

# Bulk modifications
for node in deck.get_kwds_by_type("NODE"):
    node.z = node.z + 10.0  # Shift all nodes up
```

### Clearing Deck

```python
# Remove all keywords
deck.clear()
```

## Working with INCLUDE Files

### Loading with Includes

```python
# Load main file
deck = Deck()
deck.import_file("main.k")

# Expand includes (brings included files into main deck)
deck.expand(
    search_paths=["/path/to/includes", "/another/path"],
    recurse=True  # Also expand includes within includes
)
```

### Adding Include Keywords

```python
from ansys.dyna.core import keywords

# Add an include
include = keywords.Include()
include.filename = "materials.k"
deck.append(include)

# Add include with path
include_path = keywords.IncludePath()
include_path.filename = "/path/to/includes"
deck.append(include_path)
```

### Include with Transform

```python
# Include with transformation
include_xform = keywords.IncludeTransform()
include_xform.filename = "component.k"
# Set transformation parameters
deck.append(include_xform)
```

## Combining Decks

```python
# Add two decks together
deck1 = Deck()
deck1.load("model1.k")

deck2 = Deck()
deck2.load("model2.k")

combined = deck1 + deck2

# Or extend in place
deck1.extend(deck2.keywords)
```

## Format Types

Control how fields are written (column widths):

```python
from ansys.dyna.core.lib.format_type import format_type

# Set deck format
deck.format = format_type.default  # Standard format
deck.format = format_type.long      # Long format (wider fields)

# Format affects write output
deck.export_file("output_long.k")
```

## Writing Decks

### To File

```python
# Write to file
deck.export_file("output.k")

# Write with specific format
deck.export_file("output_long.k", format=format_type.long)

# Write with validation
deck.export_file("output.k", validate=True)
```

### To String

```python
# Get content as string
content = deck.write()

# With format option
content = deck.write(format=format_type.long)
```

### With Parameters

```python
# Retain parameters in output (default behavior)
deck.export_file("output.k", retain_parameters=True)

# Substitute parameters with their values
deck.export_file("output_expanded.k", retain_parameters=False)
```

## Parameters

Decks have a parameter system for variable substitution:

```python
# Access deck parameters
params = deck.parameters

# Parameters are automatically loaded from PARAMETER keywords
# and available for substitution

# Parameters are scoped:
# - Global (*PARAMETER): Visible everywhere
# - Local (*PARAMETER_LOCAL): Scoped to file and its includes
```

## Validation

```python
# Validate deck
errors = deck.validate()
if errors:
    for error in errors:
        print(f"Validation error: {error}")
else:
    print("Deck is valid")

# Validate during write
deck.export_file("output.k", validate=True)
```

## Import Handlers

Advanced: Register custom handlers that process keywords during import:

```python
from ansys.dyna.core.lib.import_handler import ImportHandler

class MyHandler(ImportHandler):
    def after_import(self, context, keyword):
        # Custom processing
        pass

deck.register_import_handler(MyHandler())
deck.import_file("model.k")  # Handler runs during load
```

## Properties

```python
# Deck title
deck.title = "My Analysis"
print(deck.title)

# Comment header (text before *KEYWORD)
deck.comment_header = "Generated by PyDyna"

# Access all keywords (including strings)
all_items = deck.all_keywords  # Includes non-keyword strings

# Access only KeywordBase instances
keyword_objects = deck.keywords
```

## Common Patterns

### Load, Modify, Save

```python
# Standard workflow
deck = Deck()
deck.import_file("input.k")

# Modify
for mat in deck.get_kwds_by_type("MAT"):
    mat.e *= 1.1  # Increase stiffness by 10%

# Save
deck.export_file("output.k")
```

### Build from Scratch

```python
from ansys.dyna.core import Deck, keywords

deck = Deck(title="Beam Model")

# Add control cards
control = keywords.ControlTermination()
control.endtim = 1.0
deck.append(control)

# Add material
mat = keywords.Mat001()
mat.mid = 1
mat.ro = 7850
mat.e = 2.1e11
mat.pr = 0.3
deck.append(mat)

# Add section
section = keywords.Section001()  # SECTION_BEAM
section.secid = 1
deck.append(section)

deck.export_file("beam_model.k")
```

### Extract Subset

```python
# Create a new deck with only specific keywords
material_deck = Deck(title="Materials Only")

for mat in deck.get_kwds_by_type("MAT"):
    material_deck.append(mat)

material_deck.export_file("materials.k")
```

## Performance Tips

1. **Use type filtering**: `get_kwds_by_type()` is faster than iterating all keywords
2. **Batch modifications**: Modify multiple keywords in one pass
3. **Avoid repeated searches**: Cache filtered results if using multiple times
4. **Large files**: Consider using includes to split large models

## See Also

- [Keywords Guide](keywords.md) - Working with individual keywords
- [Common Patterns](patterns.md) - Complete workflows and examples
