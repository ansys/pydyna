# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system.


## 1. Jinja Template Usage
- Use context objects for template context instead of manual dict updates.
- Document template variables and expected context structure.

## 2. General Simplicity
- Reduce code duplication and improve readability.

## 3. High-Level Design
- Add a diagram or high-level description of the codegen flow to the documentation.
---

**Constraints:**
- Validate the codegen after all refactoring
```bash
bash codegen/validate.sh --quick  # Fast iteration
bash codegen/validate.sh          # Full validation before commit
```
