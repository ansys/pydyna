# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system.

## 1. Handler System

- **Post-Processing Phase**: Only `SharedFieldHandler` uses `post_process()` - clarify purpose of this phase. Consider renaming to `finalize()` or merging into `handle()` if the two-phase approach is not broadly needed.
- **Mutable vs Immutable**: Current design mutates shared `kwd_data` via reference semantics (critical for card-set/conditional-card interaction). Consider whether an immutable approach is feasible, though this would require significant architectural changes.

## 2. Jinja Template Usage
- Use context objects for template context instead of manual dict updates.
- Document template variables and expected context structure.

## 3. General Simplicity
- Reduce code duplication and improve readability.

## 4. High-Level Design
- Add a diagram or high-level description of the codegen flow to the documentation.
---

**Constraints:**
- Validate the codegen after all refactoring
```bash
bash codegen/validate.sh --quick  # Fast iteration
bash codegen/validate.sh          # Full validation before commit
```
