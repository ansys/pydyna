# Legacy and Obsolete Keyword Handling

When breaking changes are made to keyword classes (e.g., changing from `TableCardGroup` to `CardSet`), users who depend on the old API need a migration path. This guide documents how to preserve legacy implementations while introducing improved versions.

## Overview

The pattern involves:
1. Creating a standalone legacy class that preserves the old implementation
2. Using `ImportContext.keyword_overrides` to allow users to opt-in to the legacy class during deck loading
3. Emitting `DeprecationWarning` when legacy classes are instantiated

## Step-by-Step Process

### 1. Create the Legacy Class

Create a new file in `src/ansys/dyna/core/keywords/keyword_classes/manual/` with a version suffix:

```
mat_295_version_0_9_1.py  # Version when the class was last working as expected
```

**Key points:**
- Copy the **entire** auto-generated class into the legacy file (since the auto class will be regenerated)
- Rename the class with a `Legacy` suffix (e.g., `Mat295Legacy`)
- Add `# flake8: noqa: E501` if the auto-generated code has long lines
- Add `# noqa: F401` for any unused imports inherited from auto-generated code
- Include the `before_read` method and any other manual customizations

### 2. Add Deprecation Warning

In the `__init__` method, emit a `DeprecationWarning`:

```python
import warnings

class Mat295Legacy(KeywordBase):
    def __init__(self, **kwargs):
        warnings.warn(
            "Mat295Legacy is deprecated and will be removed in a future version. "
            "This legacy class has a known limitation: all fiber families must use the same FTYPE. "
            "Use Mat295 instead, which supports per-fiber FTYPE values via the fiber_families API.",
            DeprecationWarning,
            stacklevel=2,
        )
        super().__init__(**kwargs)
```

### 3. Do NOT Export from `manual_keywords.py`

The legacy class should **not** be exported by default. Users must explicitly import it:

```python
# Users must do this explicit import
from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy
```

### 4. Document Usage with ImportContext

Users can use the legacy class when loading decks via `ImportContext.keyword_overrides`:

```python
from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.lib.import_handler import ImportContext
from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

deck = Deck()
context = ImportContext(
    deck=deck,
    keyword_overrides={"*MAT_295": Mat295Legacy},
)
deck.loads(data, context=context)
```

## Implementation Details

### ImportContext.keyword_overrides

The `keyword_overrides` field on `ImportContext` is a dictionary mapping keyword names (with asterisk) to keyword classes:

```python
@dataclasses.dataclass
class ImportContext:
    xform: typing.Any = None
    deck: typing.Any = None
    path: str = None
    keyword_overrides: typing.Dict[str, type] = dataclasses.field(default_factory=dict)
```

When `deck_loader.py` processes a keyword, it checks `context.keyword_overrides` before falling back to the default `TypeMapping`.

### Testing Legacy Classes

Add tests in `tests/test_keyword_registration.py` to verify:

1. Legacy class emits `DeprecationWarning` on construction
2. Legacy class can be loaded via `ImportContext.keyword_overrides`
3. Default loading uses the new (non-legacy) class
4. Overrides are context-specific (don't affect other decks)

## Example: MAT_295

The first legacy keyword is `Mat295Legacy` in `mat_295_version_0_9_1.py`. This preserves the `TableCardGroup`-based `anisotropic_settings` API that has a known limitation: all fiber families must use the same `ftype` value.

The new `Mat295` class will use a `CardSet` for `fiber_families`, allowing each fiber family to have its own `ftype`.

## Example: INITIAL_STRAIN_SHELL and INITIAL_STRESS_SHELL

`InitialStrainShellLegacy` and `InitialStressShellLegacy` (in `initial_strain_shell_version_0_9_1.py` and `initial_stress_shell_version_0_9_1.py`) preserve the `TableCard`/DataFrame API that does not support the LARGE format option.

The old API exposed strains/stresses via a `strains`/`stresses` property that returned a pandas DataFrame.

The new API uses CardSet with individual element cards, supporting both standard and LARGE formats. Users can access data through:
- Individual properties on each set item
- The list-based `sets` property

## File Naming Convention

Use the pattern: `{keyword_name}_version_{major}_{minor}_{patch}.py`

Examples:
- `mat_295_version_0_9_1.py`
- `section_shell_version_1_0_0.py`

This makes it clear which pydyna version the legacy class corresponds to.
