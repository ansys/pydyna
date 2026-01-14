# PyDyna Agent Entry Point

See https://deepwiki.com/ansys/pydyna for codebase overview.

**Detailed guides**: [agents/README.md](agents/README.md)

## Shell Hints (Critical)

- Don't write to `/dev/null` - triggers VS Code security prompt
- PowerShell: use `>$null 2>&1` not `>/dev/null 2>&1`
- Or just run commands without redirection

## Quick Commands

```bash
# Codegen validation
bash codegen/validate.sh --quick    # Fast (generate + diff)
bash codegen/validate.sh            # Full (includes tests)

# Tests
uv run --no-project python -m pytest tests/ -v

# Linting
uv run --no-project pre-commit run --all-files

# Docs (no examples)
BUILD_EXAMPLES=false BUILD_AUTOKEYWORDS_API=false ./doc/make.bat html
```

## Coding Style

- **Line limit**: 120 characters
- **Logging**: Use `logging` module, not `print()`. Add `logger = logging.getLogger(__name__)` to modules.
- **Imports**: No inline comments explaining imports; use module docstrings instead.

## Agent Guides Index

| Topic | Guide |
|-------|-------|
| **Codegen** | [agents/codegen/](agents/codegen/) |
| **Keywords** | [agents/keywords/](agents/keywords/) |
| **Projects** | [agents/projects/](agents/projects/) |

