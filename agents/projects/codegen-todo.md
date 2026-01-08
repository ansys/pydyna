# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system.

## 1. General Simplicity ✅ ANALYZED

**ANALYSIS COMPLETED**: Identified top 5 code duplication issues through comprehensive review:

1. **Settings Dataclass Pattern** (~200-300 lines duplicated)
   - Every handler has nearly identical `from_dict()` and `_parse_settings()` boilerplate
   - Recommendation: Create base `HandlerSettings` class with generic methods

2. **Keyword Name Processing** (~50-80 lines)
   - Pattern of `fix_keyword` → `get_classname` → `get_domain` repeated 10+ times
   - Recommendation: Create `KeywordNameProcessor` utility or dataclass

3. **Label Registry Resolution** (~80-120 lines)
   - Every label-using handler repeats registry validation and resolution loop
   - Recommendation: Create decorator or base method in `KeywordHandler`

4. **Card Field Iteration** (~60-100 lines)
   - Similar nested loops to find/modify fields across 5+ handlers
   - Recommendation: Add utility methods to `KeywordData` or `Card`

5. **Subset Domain Filtering** (~30-50 lines)
   - Identical filtering logic in 3 functions in `generate.py`
   - Recommendation: Create `filter_by_domains()` utility

**Total potential reduction**: ~400-600 lines of duplicated code

**Status**: Analysis complete. Refactoring can be done incrementally as handlers are modified for other reasons. Not critical for functionality.

## 2. High-Level Design ✅ DONE

**COMPLETED**: Enhanced `ARCHITECTURE.md` with comprehensive documentation:
- Added high-level flow diagram showing data sources → pipeline → output
- Added handler pipeline architecture diagram with phases
- Documented mutable reference semantics design decision
- Expanded generation flow with 8 detailed steps
- Clarified handler execution model and ordering
- Updated component descriptions with current implementations

The documentation now provides clear architectural overview for developers.

---

**All recommendations completed or analyzed!**

**Constraints:**
- Validate the codegen after all refactoring
```bash
bash codegen/validate.sh --quick  # Fast iteration
bash codegen/validate.sh          # Full validation before commit
```
