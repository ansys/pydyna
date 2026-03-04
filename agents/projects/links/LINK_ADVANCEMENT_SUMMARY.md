# Keyword Linking Advancement Summary

## Current State (February 2, 2026)

### Implementation Status
- **Implemented**: 20 link types
- **Identified in JSON**: 118 link types  
- **Missing**: 98 link types
- **Conditional links**: 842 entries (type -1)

### Successfully Implemented Link Types
The following 20 link types are now functional:

| ID | Type | References | Confidence | Notes |
|----|------|------------|------------|-------|
| 1 | NODE | 2,263 | 0.90 | ✓ Working |
| 3 | ELEMENT_BEAM | 48 | 0.95 | ✓ Working |
| 4 | ELEMENT_SHELL | 58 | 0.90 | ✓ Working |
| 5 | ELEMENT_SOLID | 92 | 0.95 | ✓ Working |
| 13 | PART | 496 | 0.95 | ✓ Working |
| 14 | MAT | 92 | 0.95 | ✓ Working |
| 15 | SECTION | 35 | 0.90 | ✓ Working |
| 17 | HOURGLASS | 33 | 0.89 | ✓ Working |
| 19 | DEFINE_CURVE | 3,178 | 0.95 | ✓ Working (most common) |
| 20 | DEFINE_BOX | 269 | 0.95 | ✓ Working |
| 21 | DEFINE_COORDINATE_SYSTEM | 156 | 0.95 | ✓ Working |
| 22 | DEFINE_VECTOR | 205 | 0.93 | ✓ Working |
| 25 | SET_BEAM | 44 | 0.95 | ✓ Working |
| 26 | SET_DISCRETE | 17 | 0.95 | ✓ Working |
| 27 | SET_NODE | 291 | 0.95 | ✓ Working |
| 28 | SET_PART | 514 | 0.72 | ✓ Working (low conf) |
| 29 | SET_SEGMENT | 183 | 0.95 | ✓ Working |
| 31 | SET_SOLID | 35 | 0.70 | ✓ Working (low conf) |
| 40 | DEFINE_TRANSFORMATION | 4 | 0.90 | ✓ Working |
| 86 | DEFINE_CURVE_OR_TABLE | 99 | 0.95 | ✓ Polymorphic |

---

## ⚠️ Known Issues

**IMPORTANT**: Some link types in the JSON have been verified against the manual and found to be inconsistent or require special handling. See **`LINK_ISSUES.md`** for complete details.

**Key issues**:
- Links -11, -12, -3, -7168, 97: Inconsistent with manual ❌
- Link -2: Old master/slave convention (wait for kwd.json update) ⚠️
- Link 22: Needs numeric conditional logic (>0 → VECTOR, <0 → COORD_SYS) ⚠️
- Link 83: Partially inconsistent (exclude specific keyword) ⚠️
- Link 85: Special line-index reference (needs manual implementation) ⚠️
- Link 137: Consistent but has subtype variations ✓

**Before implementing any link type**: Check `LINK_ISSUES.md` first!

---

## Recommendations for Next Steps

### Phase 1: High-Value Missing Types (Immediate Priority - ✅ Verified Safe)

These have high reference counts (>100), high confidence (>=0.9), and NO known issues:

1. **Link 92: DEFINE_FUNCTION** (131 refs, 0.95 conf) ✅
   - References `*DEFINE_FUNCTION` keywords
   - Used in control and sensor definitions
   - **Status**: Safe to implement

2. **Link 84: DEFINE_CPM_CHAMBER** (128 refs, 0.95 conf) ✅
   - CPM (Corpuscular Particle Method) chamber references
   - Used in AIRBAG and CPM keywords
   - **Status**: Safe to implement

3. **Link 120: DEFINE_CPM_VENT** (128 refs, 0.95 conf) ✅
   - CPM vent hole definitions
   - Used in AIRBAG keywords
   - **Status**: Safe to implement

### Phase 2: Medium-Value Types (50-100 refs, conf >= 0.9)

4. **Link 77: DEFINE_TABLE** (91 refs, 0.95 conf)
   - Already partially handled via polymorphic link 86
   - May need dedicated link type for non-polymorphic cases

5. **Link 124: IGA_EDGE_XYZ** (79 refs, 0.95 conf)
   - Isogeometric analysis edge definitions

6. **Link 55: SENSOR_DEFINE_\*** (53 refs, 0.94 conf)
   - Generic sensor definition references
   - Multiple sensor subtypes

7. **Link 131: IGA_FACE_UVW** (53 refs, 0.95 conf)
   - IGA face parametric space references

8. **Link 134: IGA_POINT_UVW** (55 refs, 0.95 conf)
   - IGA point parametric space references

### Phase 3: Complete SET_* Family

Missing SET_* types with significant usage:

- **Link 30: SET_SHELL** (44 refs, 0.95 conf)
- **Link 33: CONTACT** (40 refs, 0.95 conf) - may be SET_CONTACT
- **Link 32: SET_TSHELL** (21 refs, 0.95 conf)
- **Link 16: SET_\*** (29 refs, 0.60 conf) - generic/polymorphic SET

### Phase 4: Conditional Links (Complex Feature)

**Type -1: Conditional/Polymorphic Links** (842 entries)

All conditional links use `SIDTYP` field to determine target type. Common patterns:

```python
SIDTYP values determine target:
  0 → SET_SEGMENT
  1 → SET_SHELL
  2 → SET_PART  
  3 → PART
  4 → SET_NODE
  5 → PART (or "all")
  6 → SET_PART (exempted)
  7 → SET_PART_TREE
```

**Implementation approach:**
- Add `ConditionalLink` metadata to keyword field definitions
- Extend `get_links()` to evaluate conditions at runtime
- Generate `_link` properties that check control field value

---

## Technical Architecture Considerations

### Current Pattern (Simple Link)
```python
_link_fields = {
    "nsid": LinkType.SET_NODE,
}

@property
def nsid_link(self):
    """Get the SET_NODE_* keyword for nsid."""
    if self._deck is None:
        return None
    return self._deck.get_link(LinkType.SET_NODE, self.nsid)
```

### Proposed Pattern (Conditional Link)
```python
_link_fields = {
    "sid": ConditionalLink(
        control_field="sidtyp",
        value_map={
            0: LinkType.SET_SEGMENT,
            4: LinkType.SET_NODE,
            # etc.
        }
    ),
}

@property
def sid_link(self):
    """Get the referenced keyword for sid (type depends on sidtyp)."""
    if self._deck is None:
        return None
    link_type = self._resolve_conditional_link("sid")
    return self._deck.get_link(link_type, self.sid)
```

---

## Coverage Metrics

### By Domain
- **DEFINE_\***: 8/20 types (40% - good coverage)
- **SET_\***: 7/~15 types (47% - good progress)
- **ELEMENT_\***: 3/7 types (43%)
- **IGA_\***: 0/8 types (0% - specialized domain)
- **SENSOR_\***: 0/2 types (0%)
- **COMPONENT_\***: 0/2 types (0%)

### By Impact
- **High-impact** (>500 refs): 3/3 implemented (100%) ✓
- **Medium-impact** (100-500 refs): 7/10 implemented (70%)
- **Low-impact** (<100 refs): 10/105 implemented (9.5%)

---

## Data Quality Notes

### High Confidence Types (>= 0.95)
Most missing types have excellent confidence scores:
- 92 out of 98 missing types have confidence >= 0.90
- 85 out of 98 missing types have confidence >= 0.95

### Low Confidence Types (< 0.80)
Only a few implemented types have lower confidence:
- Link 28 (SET_PART): 0.72 - polymorphic SET_
- Link 31 (SET_SOLID): 0.70 - polymorphic SET_
- Link 16 (not implemented): 0.60 - generic SET_

These likely need conditional logic rather than simple linking.

---

## Implementation Strategy

### Short Term (Next Sprint)
1. Add link types 92, 84, 120 (DEFINE_FUNCTION, CPM_CHAMBER, CPM_VENT)
2. Add missing SET_* types (30, 32, 33)
3. Update tests to cover new link types

### Medium Term (Next Quarter)
1. Implement IGA_* family (7-8 types)
2. Add SENSOR_* links
3. Add ELEMENT_DISCRETE, ELEMENT_TSHELL

### Long Term (Ongoing)
1. Design and implement conditional link framework
2. Generate conditional links from JSON metadata
3. Validate conditional link behavior with test decks
4. Document conditional link patterns for users

---

## JSON Data Structure

The `agent_reference_revised_manual.json` provides:

1. **Simple links** (numeric keys): Direct field → keyword type mappings
2. **Conditional links** (key "-1"): Field → keyword type depends on control field
3. **Metadata**: Confidence scores, source keywords, target keywords

### Example Entry
```json
{
  "19": [
    {
      "source_keyword": "AIRBAG_ALE",
      "source_field": "LCVEL",
      "referenced_keywords": ["DEFINE_CURVE"],
      "confidence": 0.95
    }
  ]
}
```

### Conditional Entry
```json
{
  "-1": [
    {
      "source_keyword": "CONTACT_2D_AUTOMATIC",
      "source_field": "SURFA",
      "referenced_keywords": ["SET_SEGMENT", "SET_NODE", "PART"],
      "confidence": 0.95,
      "condition": {
        "field": "SURFATYP",
        "value_map": {
          "0": "SET_SEGMENT",
          "4": "SET_NODE",
          "3": "PART"
        }
      }
    }
  ]
}
```

---

## Benefits of Advancing Linking

1. **Better IDE Support**: Autocomplete and navigation between related keywords
2. **Validation**: Detect broken references before solver runs
3. **Deck Refactoring**: Easier to update IDs when restructuring models
4. **Documentation**: Auto-generate relationship diagrams
5. **User Experience**: Properties like `material.section_link` more intuitive than manual deck searches

---

## Conclusion

The keyword linking system has made strong progress with 20 types implemented covering the most common use cases (DEFINE_CURVE, SET_NODE, MAT, etc.). The `agent_reference_revised_manual.json` provides a clear roadmap for the remaining 98 types.

**Recommended next action**: Implement the 3 high-priority DEFINE_* types (92, 84, 120) which will cover an additional ~380 field references with 95% confidence.
