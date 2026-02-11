# Working with Keywords

Detailed guide for creating, modifying, and understanding PyDyna keyword classes.

## Overview

Keywords are the building blocks of LS-DYNA input files. PyDyna provides Python classes for each keyword type, with most classes auto-generated from the LS-DYNA schema.

## Keyword Class Structure

### Naming Convention

Keyword classes use **CamelCase**:

```python
from ansys.dyna.core import keywords

keywords.MatElastic()      # *MAT_ELASTIC
keywords.MatRigid()      # *MAT_RIGID
keywords.SectionBeam()  # *SECTION_BEAM
keywords.SectionShell()  # *SECTION_SHELL
```

**Tip**: Use IDE autocomplete to discover available keywords - type `keywords.` and browse.

### Class Location

- **Auto-generated**: `ansys.dyna.core.keywords.keyword_classes.auto/` (3000+ files)
- **Manual overrides**: `ansys.dyna.core.keywords.keyword_classes.manual/`

**Important**: Avoid searching the `auto/` directory - it will bloat your context with thousands of generated files.

## Creating Keywords

### Basic Creation

```python
from ansys.dyna.core import keywords

# Create a material
mat = keywords.Mat001()  # MAT_001
mat.mid = 1              # Material ID
mat.ro = 7850            # Density
mat.e = 2.1e11           # Young's modulus
mat.pr = 0.3             # Poisson's ratio

# or

mat = keywords.Mat001(mid=1, ro=7850, e=2.1e11, pr=0.3)

# Create nodes
node = keywords.Node()
node.nodes = pd.DataFrame({"nid": [1,2,3], "x": [0.0, 0.1, 0.2], "y": [0.0, 0.0, 0.0], "z": [0.0, 0.0, 0.0]})
```

# Displaying keywords
```pycon
>>> mat = keywords.Mat001(mid=1, ro=7850, e=2.1e11, pr=0.3)
>>> mat
*MAT_001
$#     mid        ro         e        pr        da        db    unused
         1    7850.0   2.1e+11       0.3
```

### With User Comments

```python
# Add comment before keyword in output
mat = keywords.Mat001()
mat.user_comment = "Steel material properties"
mat.mid = 1
mat.ro = 7850
```

### Using Long Format

```python
from ansys.dyna.core.lib.format_type import format_type

# Create with specific format
mat = keywords.Mat001(format=format_type.long)
mat.mid = 1
```

## Accessing Keyword Properties

### Field Access

Keyword fields are accessed as attributes:

```python
# Read fields
material_id = mat.mid
density = mat.ro

# Write fields
mat.e = 2.5e11
node.x = 10.0
```

### Introspection

```python
# Get keyword type
print(mat.keyword)      # "MAT"
print(mat.subkeyword)   # "ELASTIC" or "001"

# Get title as it appears in file
print(mat.get_title())  # "*MAT_ELASTIC"

# Check format
print(mat.format)       # format_type.default
```

### Deck Association

Keywords track which deck they belong to:

```python
# Check if keyword is in a deck
if mat.deck is not None:
    print("Material is in a deck")

# Keywords can only belong to one deck at a time
# This will raise an error:
deck1.append(mat)
deck2.append(mat)  # Error: already in deck1
```

## Keyword Types

### Simple Keywords

Single-card keywords with fixed fields:

```python
control = keywords.ControlTermination()
control.endtim = 1.0
control.endcyc = 0
```

### Table/Array Keywords

Keywords with repeating rows:

```python
# DEFINE_CURVE with data points
curve = keywords.DefineCurve()
curve.lcid = 1
curve.title = "Load curve"

#curve.curves is a pandas dataframe with columns a1 and o1
```

### Option Keywords

Keywords with optional cards:

```pycon
# Some keywords have optional additional cards
# Access via specific attributes defined in the class
>>> c=kwd.ContactAutomaticSingleSurface()
>>> c.options
Options:
    ID option is not active.
    MPP option is not active.
    A option is not active.
    B option is not active.
    C option is not active.
    D option is not active.
    E option is not active.
    F option is not active.
    G option is not active.
>>> c.options["ID"].active=True
>>> c
*CONTACT_AUTOMATIC_SINGLE_SURFACE_ID
$#     cid                                                               heading

$#    ssid      msid     sstyp     mstyp    sboxid    mboxid       spr       mpr

$#      fs        fd        dc        vc       vdc    penchk        bt        dt
       0.0       0.0       0.0       0.0       0.0         0       0.0       0.0
$#     sfs       sfm       sst       mst      sfst      sfmt       fsf       vsf
       1.0       1.0                           1.0       1.0       1.0       1.0
```

### Complex Keywords

Keywords with nested structures:

```python
# Some keywords have card sets, series, or grouped cards
# Refer to the specific keyword's docstring for structure
```

## Modifying Keywords

### Direct Field Modification

```python
# Simple modification
mat.e = 2.5e11
mat.pr = 0.33

# Conditional modification
if mat.ro < 8000:
    mat.ro = 7850
```

### Bulk Modifications

```python
# Modify all materials in a deck
for mat in deck.get_kwds_by_type("MAT"):
    mat.e *= 1.1  # Increase stiffness by 10%
```

## Keyword Validation

Keywords can be validated for correctness:

```python
# Validate individual keyword
errors = mat.validate()
if errors:
    for error in errors:
        print(f"Error: {error}")
```

## Common Keyword Patterns

### Materials

```python
# Elastic material (MAT_001)
mat_elastic = keywords.Mat001()
mat_elastic.mid = 1
mat_elastic.ro = 7850
mat_elastic.e = 2.1e11
mat_elastic.pr = 0.3

# Rigid material (MAT_020)
mat_rigid = keywords.Mat020()
mat_rigid.mid = 2
mat_rigid.ro = 7850
mat_rigid.e = 2.1e11
mat_rigid.pr = 0.3
```

### Sections

```python
# Beam section (SECTION_001)
section_beam = keywords.Section001()
section_beam.secid = 1
section_beam.elform = 1

# Shell section (SECTION_002)
section_shell = keywords.Section002()
section_shell.secid = 2
section_shell.elform = 16  # Fully integrated shell
section_shell.t1 = 1.0     # Thickness
```

### Parts

```python
part = keywords.Part()
part.parts = pd.DataFrame({"pid": [1], "secid": [1], "mid": [1]})
```

### Elements

```python

# Element (varies by type)
elem = keywords.ElementShell()
elem.elements = pd.DataFrame({"eid": [200], "pid": [1], "n1": [100], "n2": [101], "n3": [102], "n4": [103]})
```

### Control Cards

```python
# Termination control
control_term = keywords.ControlTermination()
control_term.endtim = 10.0

# Output control
control_out = keywords.DatabaseBinaryD3Plot()
control_out.dt = 0.01  # Output interval
```

## Finding Keywords in Documentation

Use these strategies to discover keyword capabilities:

### 1. IDE Autocomplete
```python
from ansys.dyna.core import keywords
keywords.Mat  # <-- Autocomplete shows Mat001, Mat002, etc.
```

### 2. Python Help
```python
help(keywords.Mat001)
help(keywords.Section002)
```

### 3. Docstrings
```python
print(keywords.Mat001.__doc__)
```

### 4. LS-DYNA Manual
Refer to the LS-DYNA Keyword User's Manual for field meanings and valid values.

## Important Notes

### Auto-Generated Classes

Most keyword classes (3000+) are auto-generated from the LS-DYNA schema. This means:

1. **Field names match LS-DYNA**: Use the manual for field documentation
2. **Consistent API**: All keywords follow the same patterns

### Searching for Keywords

**Don't** search the auto-generated directory:
```python
# ❌ Avoid this - will load 3000+ files
grep("some_field", path="src/ansys/dyna/core/keywords/keyword_classes/auto/")
```

**Do** use type filtering or help:
```python
# ✅ Use these approaches
help(keywords.Mat001)
deck.get_kwds_by_type("MAT")
```

## Advanced Topics

### Checking Keyword Type

```python
from ansys.dyna.core import keywords

# Using isinstance
if isinstance(kw, keywords.Mat001):
    print("This is an elastic material")

# Using keyword attribute
if kw.keyword == "MAT":
    print("This is some type of material")

# Checking both keyword and subkeyword
if kw.keyword == "SECTION" and kw.subkeyword == "SHELL":
    print("This is a shell section")
```

### Keyword Relationships

Some keywords reference others (e.g., Part references Section and Material):

```python
# Part references section and material by ID
part = keywords.Part()
part.pid = 1
part.secid = 1  # References SECTION with secid=1
part.mid = 1    # References MAT with mid=1

# PyDyna can track these relationships (see linking documentation)
```

## Troubleshooting

### Field Not Found

If a field doesn't exist as an attribute:
1. Check the LS-DYNA manual for correct field name
2. Verify you're using the correct keyword type number
3. Check spelling (case-sensitive)

### Validation Errors

If validation fails:
1. Check required fields are set
2. Verify field values are in valid ranges
3. Ensure referenced IDs exist (e.g., material ID in part)

### Type Errors

If you get type errors:
1. Ensure numeric fields get numbers (not strings)
2. Check field types match expectations
3. Some fields may have specific enums or constraints

## See Also

- [Deck Operations](deck.md) - Working with collections of keywords
- [Common Patterns](patterns.md) - Complete workflows and examples
