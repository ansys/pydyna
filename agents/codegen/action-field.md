# Action Field Summary

An `action` field in `manifest.json` provides fine-grained control:

| Action | Class Generation | Type Mapping | Use Case |
|--------|-----------------|--------------|----------|
| `"generate"` (default) | ✓ Yes | ✓ Yes | Standard keywords |
| `"manual"` | ✗ No | ✓ Yes | Manually implemented keywords |
| `"skip"` | ✗ No | ✗ No | Option-source or merged keywords |

### Examples
- ✓ `*DEFINE_FUNCTION` → `DefineFunction` (manual)
- ✓ `*ELEMENT_SOLID` → `ElementSolid` (manual)
- ✓ `*ELEMENT_SOLID_ORTHO` → `ElementSolidOrtho` (manual)
- ✓ `*SECTION_SHELL_MISC` → NOT in mapping (skip, resolves to `SectionShell` via suffix stripping)
- ✓ `*ELEMENT_SOLID (ten nodes format)` → NOT in mapping (skip, merged into base class)
- ✓ `*ELEMENT_SOLID_ORTHO (ten nodes format)` → NOT in mapping (skip)

### Class Generation
- No auto-generated files for `action="skip"` or `action="manual"` keywords
- Manual implementations in `src/ansys/dyna/core/keywords/keyword_classes/manual/` are used

