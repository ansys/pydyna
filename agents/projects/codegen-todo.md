# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system.

## 1. Handler System

- **Post-Processing Phase**: Only `SharedFieldHandler` uses `post_process()` - clarify purpose of this phase. Consider renaming to `finalize()` or merging into `handle()` if the two-phase approach is not broadly needed.
- **Mutable vs Immutable**: Current design mutates shared `kwd_data` via reference semantics (critical for card-set/conditional-card interaction). Consider whether an immutable approach is feasible, though this would require significant architectural changes.

## 2. Configuration and Manifest Loading
- Centralize manifest/spec/config loading in a dedicated module or class.
- Remove global state and side effects from config loading.

## 3. Jinja Template Usage
- Use context objects for template context instead of manual dict updates.
- Document template variables and expected context structure.

## 4. Testing
- Handler-specific tests (with fixtures for global state), generator module tests (class/entrypoint generation), config/manifest loading tests, utility module tests
- Integration/E2E tests deferred to CI which already validates generated output remains unchanged

## 5. General Simplicity
- Reduce code duplication and improve readability.

## 6. High-Level Design
- Add a diagram or high-level description of the codegen flow to the documentation.

## 7. Maintenance
- Use the coverage module to confirm that every line of code in the codegen system is necessary
- Incorporate the above into the codegen agent instructions so this is done every time the codegen is touched
---

**Constraints:**
- Validate the codegen after all refactoring
```bash
bash codegen/validate.sh --quick  # Fast iteration
bash codegen/validate.sh          # Full validation before commit
```
