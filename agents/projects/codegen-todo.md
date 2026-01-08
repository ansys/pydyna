# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system.

## 1. Data Structures

### Remaining Dataclass Migration Work

The dataclass migration is nearly complete:
- **Fields**: ✅ Fully migrated to `Field` instances
- **Cards**: ⚠️ Partially migrated - most are `Card` instances, but `table_card_group` handler creates dict-based cards

**Remaining task**: Update `table_card_group` handler (`handlers/table_card_group.py`) to create `Card` instances instead of `{"table_group": True, "sub_cards": [...]}` dicts. Once complete:
- Remove dict-like fallback methods from `Card` class
- Remove `Union[Card, Dict]` type hints
- Enable strict type checking throughout

## 2. Handler System

- **Post-Processing Phase**: Only `SharedFieldHandler` uses `post_process()` - clarify purpose of this phase. Consider renaming to `finalize()` or merging into `handle()` if the two-phase approach is not broadly needed.
- **Mutable vs Immutable**: Current design mutates shared `kwd_data` dict via reference semantics (critical for card-set/conditional-card interaction). Consider whether an immutable approach is feasible, though this would require significant architectural changes.

## 3. Configuration and Manifest Loading
- Centralize manifest/spec/config loading in a dedicated module or class.
- Remove global state and side effects from config loading.

## 4. Jinja Template Usage
- Use context objects or dataclasses for template context instead of manual dict updates.
- Document template variables and expected context structure.

## 5. Testing
- Handler-specific tests (with fixtures for global state), generator module tests (class/entrypoint generation), config/manifest loading tests, utility module tests
- Integration/E2E tests deferred to CI which already validates generated output remains unchanged

## 6. General Simplicity
- Reduce code duplication and improve readability.

## 7. High-Level Design
- Add a diagram or high-level description of the codegen flow to the documentation.

## 8. Maintenance
- Use the coverage module to confirm that every line of code in the codegen system is necessary
- Incorporate the above into the codegen agent instructions so this is done every time the codegen is touched
- Add a validation script that encapsulates all of these and the below constraints
---

**Constraints:**

> **Dead Code Detection**: After any refactoring, run `python codegen/find_dead_code.py --threshold 80` to identify potential dead code. Files with <80% coverage should be reviewed and potentially removed if they're genuinely unused.

> **Output Validation**: Any refactor must not change the output of the generated files. Use `generate.py -c` and `generate.py` to validate.

> **Code Quality**: Always run `pre-commit run --all-files` to confirm linting and formatting standards are met.
