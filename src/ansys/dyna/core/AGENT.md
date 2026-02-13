# PyDyna Agent Instructions

Instructions for AI assistants helping **users** work with PyDyna (ansys-dyna-core).

> **Developer working on PyDyna itself?** See `agents/` directory in the repository (not shipped with package).

## Documentation Structure

- **This file**: Quick reference
- **[Keywords Guide](ansys.dyna.core/keywords.md)**: keyword usage patterns
- **[Deck Operations](ansys.dyna.core/deck.md)**: Loading, filtering, writing
- **[Common Patterns](ansys.dyna.core/patterns.md)**: Complete workflows

## Overview

PyDyna is a Python interface to LS-DYNA, providing programmatic access to keyword files (`.k`, `.key` files).

```python
from ansys.dyna.core import Deck, keywords

# Load an existing model
deck = Deck()
deck.import_file("model.k")

# Modify a material
for mat in deck.get_kwds_by_type("MAT"):
    if mat.mid == 1:
        mat.e = 2.5e11  # Update Young's modulus

# Add a new keyword
new_mat = keywords.Mat001()  # MAT_ELASTIC
new_mat.mid = 2
new_mat.ro = 2700  # Aluminum
new_mat.e = 7.0e10
deck.append(new_mat)

# Save
deck.export_file("modified_model.k")
```

## Core Concepts

### Deck Class

The `Deck` is the main container for keywords. [See detailed guide](ansys.dyna.core/deck.md)

```python
deck = Deck()
deck.import_file("model.k")        # Load from file
deck.append(keyword)        # Add keyword
deck.remove(index)          # Remove keyword
deck.export_file("output.k")      # Save to file
```

### Keywords Module

Keywords are Python classes representing LS-DYNA cards. [See detailed guide](ansys.dyna.core/keywords.md)

```python
from ansys.dyna.core import keywords

mat = keywords.Mat001()     # MAT_ELASTIC (type 001)
mat.mid = 1                 # Material ID
mat.ro = 7850               # Density
```

**Naming**: `Mat001` = MAT type 1, `Section002` = SECTION type 2, etc.

## Common Operations

**[See Deck Operations guide for details](ansys.dyna.core/deck.md)**

### Accessing Keywords

```python
# By type
materials = list(deck.get_kwds_by_type("MAT"))
nodes = list(deck.get_kwds_by_type("NODE"))

# By type and subtype
beam_sections = list(deck.get_kwds_by_full_type("SECTION", "BEAM"))

# Iterate
for kw in deck.keywords:
    print(kw.keyword, kw.subkeyword)
```

### Modifying

```python
# Single modification
for mat in deck.get_kwds_by_type("MAT"):
    if mat.mid == 1:
        mat.e = 2.5e11
```

### Adding/Removing

```python
# Add
deck.append(keyword)
deck.extend([kw1, kw2, kw3])

# Remove
deck.remove(0)              # By index
deck.remove([0, 2, 4])      # Multiple indices
```

### Includes

```python
# Expand includes into main deck
deck.expand(search_paths=["/path"], recurse=True)

# Add include
include = keywords.Include()
include.filename = "materials.k"
deck.append(include)
```

## Important Notes

**[See full guides for details](ansys.dyna.core/)**

### Critical Warnings

1. **Auto-generated files**: Avoid searching `src/ansys/dyna/core/keywords/keyword_classes/auto/` - it contains 3000+ generated files that will bloat your context.

2. **Use filtering**: Call `deck.get_kwds_by_type("MAT")` instead of iterating all keywords.

### Quick Tips

- **Discover keywords**: Use IDE autocomplete on `keywords.`
- **Field access**: `mat.mid`, `node.nodes` (use autocomplete)
- **Format**: Set `deck.format = format_type.long` for wider fields
- **Parameters**: Automatically loaded from `*PARAMETER` keywords

## Common Workflows

**[See complete examples in patterns guide](ansys.dyna.core/patterns.md)**

### Load → Modify → Save

```python
deck = Deck()
deck.import_file("model.k")

# Modify
for mat in deck.get_kwds_by_type("MAT"):
    mat.e *= 1.1  # Increase stiffness

deck.export_file("modified.k")
```

### Build from Scratch

```python
deck = Deck(title="New Model")

mat = keywords.Mat001()
mat.mid = 1
mat.ro = 7850
deck.append(mat)

deck.export_file("new_model.k")
```

## Extended Documentation

- **[Deck Operations](ansys.dyna.core/deck.md)** - Loading, filtering, modifying, writing
- **[Keywords Guide](ansys.dyna.core/keywords.md)** - Creating, accessing, understanding keywords
- **[Common Patterns](ansys.dyna.core/patterns.md)** - Complete workflows, transformations, validation

## Resources

- **Documentation**: https://dyna.docs.pyansys.com
- **Help**: `help(Deck)`, `help(keywords.Mat001)`
- **Discovery**: IDE autocomplete on `keywords.`
