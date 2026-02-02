# Contribute

Overall guidance on contributing to a PyAnsys library appears in the
[Contributing] topic in the *PyAnsys developer's guide*. Ensure that you
are thoroughly familiar with this guide before attempting to contribute to
PyDYNA.

The following contribution information is specific to PyDYNA.

[Contributing]: https://dev.docs.pyansys.com/how-to/contributing.html

<!-- Begin content specific to your library here. -->

## Codegen System

PyDYNA includes an automated code generation system for LS-DYNA keyword classes. If you are contributing changes to the codegen system, use the validation script to ensure your changes don't break the generation:

```bash
# Quick validation during development
bash codegen/validate.sh --quick

# Full validation before submitting PR
bash codegen/validate.sh
```

See `codegen/README.md` and `AGENTS.md` for detailed documentation on the codegen system.
