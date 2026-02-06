# Link Type Issues and Special Cases

This document tracks known issues, inconsistencies, and special cases in the `agent_reference_revised_manual.json` link mappings that require manual attention.

## Inconsistent Links (Verified Against Manual)

### Link -11 and -12: Inconsistent
**Status**: ❌ Inconsistent with manual  
**Action**: Do not implement these link types until kwd.json is corrected

---

### Link -2: Master/Slave Convention
**Status**: ⚠️ Outdated terminology  
**Issue**: Some keywords still use old "master/slave" convention  
**Solution**: Wait for updated kwd.json, then manually update these links  
**Keywords Affected**: Contact-related keywords

---

### Link -3: Inconsistent
**Status**: ❌ Inconsistent with manual  
**Action**: Do not implement until verified

---

### Link -7168: False Positive
**Keywords**: 
- `CONTROL_IMPLICIT_GENERAL_DYN`
- `CONTROL_IMPLICIT_GENERAL_SPR`

**Field**: `CNSTN`  
**Status**: ❌ Not a link according to manual  
**Issue**: AI incorrectly identified this as a link field  
**Action**: Exclude from link generation

---

### Link 22: DEFINE_VECTOR vs DEFINE_COORDINATE_SYSTEM
**Status**: ⚠️ Can be unified with conditional logic  
**Current mapping**: Mixed DEFINE_VECTOR and DEFINE_COORDINATE_SYSTEM  
**Proposed solution**: Use sign-based conditional:
```python
if field_value > 0:
    target = LinkType.DEFINE_VECTOR
else:  # field_value < 0
    target = LinkType.DEFINE_COORDINATE_SYSTEM
```

**Implementation note**: This is a good candidate for numeric conditional links (different from SIDTYP-style string conditionals)

---

### Link 83: ALE_MULTI_MATERIAL_GROUP Inconsistency
**Status**: ⚠️ Partially inconsistent  
**Problem keyword**: `INITIAL_VOLUME_FRACTION_GEOMETRY`  
**Action**: 
- Implement link 83 for other keywords
- Exclude or special-case `INITIAL_VOLUME_FRACTION_GEOMETRY`
- Document the exception

---

### Link 85: DEFINE_CPM_BAG_INTERACTION Special Case
**Status**: ⚠️ Very special reference pattern  
**Keyword**: `DEFINE_CPM_BAG_INTERACTION`  
**Field**: `NSPEC`  

**Issue**: This is NOT a simple ID reference!  
**Actual behavior**: References a specific line in a specific card of `AIRBAG_PARTICLE`

**Example**:
```
*DEFINE_CPM_BAG_INTERACTION
$ NSPEC references which species index in AIRBAG_PARTICLE
  1     # References line 1 in AIRBAG_PARTICLE's species card
  
*AIRBAG_PARTICLE
  ...
  # Species card (line 1 = first species, line 2 = second, etc.)
  SPECIES1_DATA
  SPECIES2_DATA
```

**Implementation**: 
- This cannot use standard link framework
- Needs custom lookup logic
- May need manual implementation in keyword class
- **Recommendation**: Do NOT auto-generate this link

---

### Link 97: INTERFACE_LINKING_FILE Inconsistent
**Status**: ❌ Inconsistent with manual  
**Action**: Do not implement until verified

---

### Link 137: SET_IGA_FACE Variant Subtypes
**Status**: ✓ Consistent but has variations  
**Target**: `SET_IGA_FACE*` keywords with different subtypes:
- `SET_IGA_FACE_XYZ`
- `SET_IGA_FACE_UVW`

**Implementation**: Should resolve to any `SET_IGA_FACE*` keyword using subtype prefix matching (similar to existing SET_* pattern)

```python
# In class_generator.py
def _add_set_iga_face_link_data(link_data, link_fields):
    """Add link data for SET_IGA_FACE keywords."""
    _add_set_link_data(
        link_data, 
        link_fields, 
        subtype="IGA_FACE",  # Will match IGA_FACE_XYZ, IGA_FACE_UVW, etc.
        link_type_name="SET_IGA_FACE"
    )
```

---

## Recommendations for Implementation

### ✅ Safe to Implement (High Confidence)

These link types have been verified or have consistent patterns:

- **Link 92**: DEFINE_FUNCTION - No issues reported
- **Link 84**: DEFINE_CPM_CHAMBER - No issues reported  
- **Link 120**: DEFINE_CPM_VENT - No issues reported
- **Link 137**: SET_IGA_FACE - Consistent with subtype variations
- **Link 30**: SET_SHELL - No issues reported
- **Link 32**: SET_TSHELL - No issues reported

### ⚠️ Implement with Caution

These need special handling or exclusions:

- **Link 22**: Needs numeric sign-based conditional logic
- **Link 83**: Exclude INITIAL_VOLUME_FRACTION_GEOMETRY
- **Link 137**: Use prefix matching for subtypes

### ❌ Do Not Implement

These are inconsistent or incorrect:

- **Links -11, -12, -3**: Inconsistent with manual
- **Link -7168**: False positive (CNSTN is not a link)
- **Link -2**: Wait for updated kwd.json
- **Link 85**: Too special for auto-generation (needs manual implementation)
- **Link 97**: Inconsistent with manual

---

## Special Cases Framework

### Numeric Conditional Links (Link 22 Pattern)

Some links use numeric value to determine target type:

```python
@dataclass
class NumericConditionalLink:
    """Link where target depends on numeric value comparison."""
    positive_target: LinkType  # When value > 0
    negative_target: LinkType  # When value < 0
    zero_behavior: Optional[LinkType] = None  # When value == 0
```

**Example usage**:
```python
_link_fields = {
    "cid": NumericConditionalLink(
        positive_target=LinkType.DEFINE_VECTOR,
        negative_target=LinkType.DEFINE_COORDINATE_SYSTEM,
        zero_behavior=None  # No link when 0
    )
}
```

### Cross-Card Line References (Link 85 Pattern)

Some fields reference a line index within another keyword's repeating card:

```python
@dataclass
class LineIndexLink:
    """Link that references a line index in another keyword's card."""
    target_keyword: str  # e.g., "AIRBAG_PARTICLE"
    target_card: str     # e.g., "species_card"
    index_base: int = 1  # 1-based or 0-based indexing
```

**Implementation**: These likely need manual implementation in keyword classes rather than auto-generation.

---

## Validation Checklist

When implementing a new link type:

- [ ] Check this document for known issues
- [ ] Verify against LS-DYNA manual if possible
- [ ] Check for special cases (conditionals, line refs, etc.)
- [ ] Test with real deck files if available
- [ ] Document any quirks in link implementation
- [ ] Add exclusions to codegen if needed

---

## How to Exclude Fields from Link Generation

If a field is incorrectly marked as a link in kwd.json:

### Option 1: Manifest Exclusion (Preferred)
```json
// In codegen/manifest.json
"CONTROL_IMPLICIT_GENERAL_DYN": {
    "generation-options": {
        "exclude-links": ["CNSTN"]
    }
}
```

### Option 2: Codegen Filter
```python
# In class_generator.py _get_links()
EXCLUDED_LINKS = {
    ("CONTROL_IMPLICIT_GENERAL_DYN", "CNSTN"),
    ("CONTROL_IMPLICIT_GENERAL_SPR", "CNSTN"),
}

for field in fields:
    if "link" not in field:
        continue
    
    # Check exclusion list
    kwd_name = kwd_data.title.upper()
    field_name = field["name"].upper()
    if (kwd_name, field_name) in EXCLUDED_LINKS:
        continue  # Skip this link
```

### Option 3: Manual Override in Keyword Class
```python
# In src/ansys/dyna/core/keywords/keyword_classes/manual/control_implicit_general.py
class ControlImplicitGeneralDyn(ControlImplicitGeneralDynBase):
    """Manual override to remove incorrect link."""
    
    # Override to remove bad link
    _link_fields = {
        k: v for k, v in ControlImplicitGeneralDynBase._link_fields.items()
        if k != "cnstn"
    }
```

---

## Update History

- **2026-02-02**: Initial documentation of known issues based on manual verification
- Issues discovered: Links -11, -12, -3, -7168, 22, 83, 85, 97, 137

---

## Next Steps

1. **Immediate**: Update `analyze_links.py` to flag these known issues
2. **Short term**: Implement safe links (92, 84, 120, 30, 32, 137)
3. **Medium term**: Design numeric conditional framework for link 22
4. **Long term**: Manual implementation for special cases like link 85
5. **Ongoing**: Cross-reference new links with manual before implementation
