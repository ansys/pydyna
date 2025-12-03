# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system. Each item should be addressed individually, with the constraint that the output of `generate.py -C` followed by `generate.py` must remain unchanged.

## 1. Data Structures
- Refactor keyword, card, and field representations to use Python `dataclasses` instead of raw dicts.
- Add consistent type hints throughout the codebase.

## 2. Handler System
- Refactor handler registration and invocation to use a strategy pattern or a plugin/hook system for extensibility and clarity.
- Document each handler's purpose, expected input, and output.

## 3. Configuration and Manifest Loading
- Centralize manifest/spec/config loading in a dedicated module or class.
- Remove global state and side effects from config loading.

## 4. Jinja Template Usage
- Use context objects or dataclasses for template context instead of manual dict updates.
- Document template variables and expected context structure.

## 5. Output Management
- Encapsulate file writing and output path logic in a dedicated output manager class.
- Remove direct file operations from main scripts.

## 6. Documentation
- Add docstrings to all public functions and classes.
- Document the overall flow in `generate.py` and in the README.
- Consider generating API docs for the codegen system.

## 7. Testing
- Add unit tests for handlers, template rendering, and manifest parsing.
- Ensure all tests validate that generated files remain unchanged.

## 8. General Simplicity
- Merge similar handlers or use composition where possible.
- Reduce code duplication and improve readability.

## 9. High-Level Design
- Add a diagram or high-level description of the codegen flow to the documentation.

---

**Constraint:**
> Any refactor must not change the output of the generated files. Use `generate.py -C` and `generate.py` to validate.
