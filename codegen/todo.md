# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system.

## 1. Data Structures
- Add consistent type hints throughout the codebase (dataclasses are available in `data_model/keyword_data.py` for gradual migration)

## 2. Handler System

### Further Considerations
- **Handler Duplication**: `OverrideSubkeywordHandler` and `RenamePropertyHandler` have identical functionality - should consolidate into one handler or document the distinction if intentional.
- **Post-Processing Phase**: Only `SharedFieldHandler` uses `post_process()` - clarify purpose of this phase. Consider renaming to `finalize()` or merging into `handle()` if the two-phase approach is not broadly needed.
- **Mutable vs Immutable**: Current design mutates shared `kwd_data` dict via reference semantics (critical for card-set/conditional-card interaction). Consider whether an immutable approach is feasible, though this would require significant architectural changes.

## 3. Configuration and Manifest Loading
- Centralize manifest/spec/config loading in a dedicated module or class.
- Remove global state and side effects from config loading.

## 4. Jinja Template Usage
- Use context objects or dataclasses for template context instead of manual dict updates.
- Document template variables and expected context structure.

## 5. Documentation
- Consider generating API docs for the codegen system.

## 6. Testing
- Add unit tests for handlers, template rendering, and manifest parsing.
- Ensure all tests validate that generated files remain unchanged.

## 7. General Simplicity
- Reduce code duplication and improve readability.

## 8. High-Level Design
- Add a diagram or high-level description of the codegen flow to the documentation.

---

**Constraint:**
> Any refactor must not change the output of the generated files. Use `generate.py -c` and `generate.py` to validate.
> Always run pre-commit to confirm, use pre-commit run --all-files
