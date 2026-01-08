See https://deepwiki.com/ansys/pydyna for a description of the codebase
Assume an appropriate virtual environment is activated. If it isn't, just abort.

## Agent Hints

**CRITICAL: Shell-specific output redirection**:
- **In PowerShell**: Use `>$null 2>&1` (never use `/dev/null` - triggers VS Code security prompt)
- **In Bash**: Use `>/dev/null 2>&1` (standard Unix redirection)
- For commands where you want to suppress output: Use appropriate shell redirection or just run without redirection
- For commands where you want to check output: Capture in a variable or use `| Out-Null` (PowerShell)
- For checking if a command succeeded: Just run it normally and check `$?` or `$LASTEXITCODE`
- Examples:
  - ❌ BAD (PowerShell): `python codegen/generate.py >/dev/null 2>&1`
  - ✅ GOOD (PowerShell): `python codegen/generate.py >$null 2>&1`
  - ✅ GOOD (Bash): `python codegen/generate.py >/dev/null 2>&1`
  - ✅ GOOD (Any): `python codegen/generate.py` (no redirection)

**Documentation builds**: To build docs without examples:

Ensure that the python3.13 environment is active, then:

```bash
# Build docs without examples or autokeywords (fast)
BUILD_EXAMPLES=false BUILD_AUTOKEYWORDS_API=false ./doc/make.bat html

# Build docs with autokeywords but no examples (slow, ~8+ min for keyword imports alone)
BUILD_EXAMPLES=false BUILD_AUTOKEYWORDS_API=true ./doc/make.bat html
```

## Agent Coding Style Preferences

- Do not use inline comments to explain imports. Instead, use a module-level docstring to indicate the purpose and usage of imported modules. This applies to all agent-generated code, not just codegen modules.
- There is a line limit of 120 characters, and other linting rules. Use precommit run --all-files to run the linters after making changes.
- **Handlers use typed dataclasses** (Dec 2025): All handlers now use:
  - `kwd_data.field` attribute access (dataclass instances, not dicts)
  - `KeywordData.cards` is `List[Card]` with dict-like backward compatibility (`card["index"]` or `card.index`)
  - Typed settings dataclasses in each handler file (e.g., `SkipCardSettings`)
  - Typed metadata instances (e.g., `DuplicateCardMetadata` not `Dict[str, Any]`)
- **Dict-like access for compatibility**: Card and Field dataclasses implement `__getitem__`, `__setitem__` to support gradual migration
- **Do not mark items in `codegen/todo.md` as "COMPLETED"**. This is a living document of architectural recommendations, not a task tracker. When implementing recommendations, document the implementation in the appropriate `agents/*.md` guide file instead.
- **Add comprehensive logging**: Use Python's `logging` module extensively throughout the codebase. Add a logger instance (`logger = logging.getLogger(__name__)`) to each module and use it liberally:
  - Use `logger.debug()` for detailed traceability of execution flow, variable values, and decisions
  - Use `logger.info()` for high-level progress updates and successful operations
  - Use `logger.warning()` for recoverable issues or unexpected conditions
  - Use `logger.error()` for failures with `exc_info=True` to include stack traces
  - Replace any `print()` statements with appropriate logging calls
  - Log function entry/exit for complex operations, parameter values, counts, and decision points

**Codegen validation**: When modifying the codegen system:
- **If generated code doesn't change, tests don't need to run.** The metric is: `python codegen/generate.py` + `git diff src/.../auto/` shows no changes.
- Only run keyword tests when generated output intentionally changes or when modifying runtime keyword behavior.
- Use `bash codegen/validate.sh --quick` for fast iteration (generates + diffs only) and `bash codegen/validate.sh` for full validation.

## Agent Guides

Detailed documentation for common tasks and patterns:

- **[agents/codegen.md](agents/codegen.md)**: **CODE GENERATION SYSTEM** - Complete guide for working with the codegen pipeline, handlers, validation workflow, auto-generated classes, and manual subclasses.
- **[agents/linked_keywords.md](agents/linked_keywords.md)**: How to expose relationships between keywords (e.g., DefineTable → DefineCurve) using properties and import handlers.

## Notes on the Keyword Submodule

### Code Generation Handler System

The keyword class generator uses a **handler pipeline** to transform keyword metadata from the JSON schema into Python classes. See `agents/codegen.md` for detailed documentation.

**Key Principles**:
1. **Execution order matters** - handlers are executed in registration order (see `registry.py`)
2. **Reference semantics** - handlers like `card-set` and `table-card-group` use references, not deep copies
3. **In-place mutations** - later handlers modify cards that earlier handlers referenced
4. **Index vs. position** - after reordering, use list positions not the card's `index` property

**Common Pitfall**: Using `copy.deepcopy()` when grouping cards breaks the generation because later handlers' modifications won't appear in the groups.

### Deck Import Handlers

Deck registers import handlers (e.g., `DefineTableProcessor`) and runs them during `Deck.loads()` and include expansion. Handlers see keywords as they are imported and should only assume immediate neighbor context unless explicitly coded otherwise.

For detailed guidance on linking keywords, see [agents/linked_keywords.md](agents/linked_keywords.md).

### Shared Field Handler - Negative Indices

**Critical**: The `shared_field` handler's `do_negative_shared_fields` function must search option cards FIRST, regardless of index value. Option cards can have indices that overlap with base card ranges (e.g., option card with index=2 when num_cards=3). The logic should:
1. Always search all option cards for the target index
2. Only check base cards if not found in options
3. Never assume `index >= num_cards` means "must be in options" - this assumption is incorrect.

### Auto-Generated Keywords

The files in `src/ansys/dyna/core/keywords/keyword_classes/auto/` are auto-generated by Jinja templates. If they need to be modified, update the codegen system. If modifications are not straightforward to add to codegen, create a manual subclass instead.

For detailed guidance on working with auto-generated files, see [agents/codegen.md](agents/codegen.md).
