# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system.

## 1. Data Structures
- Add consistent type hints throughout the codebase (dataclasses and typed metadata classes are available in `data_model/` for gradual migration - see `agents/codegen.md` for details)

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

## 5. Testing
- Add unit tests for handlers, template rendering, and manifest parsing.
- Ensure all tests validate that generated files remain unchanged.

## 6. General Simplicity
- Reduce code duplication and improve readability.

## 7. High-Level Design
- Add a diagram or high-level description of the codegen flow to the documentation.

## 8. Maintenance
- Use beartype to enforce/check type hints, with a CLI option to opt in
    - Add beartype to the codegen dependencies in pyproject.toml
- Use the coverage module to confirm that every line of code in the codegen system is necessary
- Incorporate the above into the codegen agent instructions so this is done every time the codegen is touched
- Add a validation script that encapsulates all of these and the below constraints
---

**Constraint:**
> Any refactor must not change the output of the generated files. Use `generate.py -c` and `generate.py` to validate.
> Always run pre-commit to confirm, use pre-commit run --all-files
