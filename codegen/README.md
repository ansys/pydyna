# Auto-keyword class generator system

## What it is
The PyDyna auto-keyword class generator system generates python classes for
dyna keywords based on specifications in kwd.json, manifest.json, and additional-cards.json
It is implemented in `codegen/generate.py` and has a command line interface.

## Critical Implementation Notes

### Handler Execution Order

Handlers transform keyword metadata in a specific order defined in `keyword_generation/handlers/registry.py`. **This order is critical** - changing it can break generation. Key constraints:

- `reorder-card` must run first (other handlers use positional indices)
- `card-set` and `table-card-group` must run before `conditional-card`
- See `agents/codegen.md` for complete ordering and rationale

### Reference Semantics

Handlers that group cards (e.g., `card-set`, `table-card-group`) append **references** to card dictionaries, not deep copies. This allows later handlers to modify cards in-place, with changes appearing in both the main cards list and the grouped collections.

**Do NOT use `copy.deepcopy()` when grouping cards** - it breaks this pattern.

### Index vs. Position

After `reorder-card` runs:
- Cards have an `index` property (original index)
- Handlers use **list positions** `kwd_data["cards"][i]`, not card indices
- `card-set` stores `source_index` (original) and assigns new sequential indices

## To use
It is recommended to use a virtual environment

- Install dependencies:
``pip install .[codegen]``

- To run the code generation system for all classes:
``python codegen/generate.py -o /path/to/keyword_classes``

- To run the code generation system for a single keyword, e.g. SECTION_SHELL:
``python codegen/generate.py -k SECTION_SHELL``

- To remove all the generated code:
``python codegen/generate.py -c``

### Validation Script

A comprehensive validation script is provided to encapsulate the complete testing workflow used by both developers and CI:

```bash
# Full validation (clean, generate, git diff, pre-commit, dead code, unit tests)
bash codegen/validate.sh

# Quick validation for fast iteration (clean, generate, git diff only)
bash codegen/validate.sh --quick

# Custom validation (skip specific steps)
bash codegen/validate.sh --skip-tests --skip-deadcode
bash codegen/validate.sh --skip-precommit

# Adjust dead code coverage threshold
bash codegen/validate.sh --coverage-threshold 90

# Verbose output for debugging
bash codegen/validate.sh --verbose
```

**Bash Aliases (add to `~/.bashrc` or `~/.bash_profile`):**

```bash
# Navigate to pydyna root (adjust path as needed)
alias pydyna='cd /c/AnsysDev/code/pyansys/pydyna'

# Codegen shortcuts
alias codegen-validate='bash codegen/validate.sh'
alias codegen-quick='bash codegen/validate.sh --quick'
alias codegen-full='bash codegen/validate.sh'
alias codegen-clean='python codegen/generate.py -c && python codegen/generate.py'
alias codegen-test='pytest -m codegen'

# Common workflows
alias codegen-check='bash codegen/validate.sh --skip-tests --skip-deadcode'
alias codegen-dev='python codegen/generate.py -l DEBUG'
```

**Usage Examples:**

```bash
# Quick iteration while developing handlers
codegen-quick

# Full validation before pushing
codegen-validate

# Check output without running tests
codegen-check

# Generate with debug logging
codegen-dev -k SECTION_SHELL
```

### Output Validation

The CI system automatically validates that generated keyword classes remain unchanged after code generation runs. This ensures that refactoring the codegen system doesn't inadvertently modify the output. To manually validate output locally:

```bash
# Clean and regenerate to validate output hasn't changed
python codegen/generate.py -c
python codegen/generate.py
git diff src/ansys/dyna/core/keywords/keyword_classes/auto/
```

If `git diff` shows changes, either:
1. The codegen logic has a bug that needs fixing
2. The changes are intentional (e.g., new handlers or template improvements) and should be committed

### Logging

The code generator includes comprehensive logging to help debug and understand the generation process:

- **Control log verbosity** with the `--log-level` or `-l` flag:
  - `DEBUG`: Detailed traceability of execution flow, variable values, and decisions
  - `INFO`: High-level progress updates and successful operations (default)
  - `WARNING`: Only recoverable issues or unexpected conditions
  - `ERROR`: Only failures and errors
  - `CRITICAL`: Only critical failures

- **Examples**:
  ```bash
  # Generate with detailed debug output
  python codegen/generate.py -k SECTION_SHELL -l DEBUG

  # Generate with minimal output (warnings and errors only)
  python codegen/generate.py -l WARNING

  # Generate with high-level progress info (default)
  python codegen/generate.py -l INFO
  ```

- **What gets logged**:
  - **DEBUG**: Card insertions, deletions, wildcard matching, handler execution, template rendering, file operations
  - **INFO**: Files loaded, keyword counts, generation progress, completion status
  - **WARNING**: Deprecated features, potential issues
  - **ERROR**: Generation failures with full stack traces

## How it works
The class generator uses Jinja templates to generate three distinct things:
- Python classes
- Import machinery
- keyword to type mapping

The python classes are what users of PyDyna interact with directly. The import machinery produces
`auto_keywords.py`, which contains a list of import statements that import classes from the
Python files where they are defined. The keyword to type mapping produces a dictionary mapping the
keyword name with the python class that defines it.

The primary specification for keywords is found in `kwd.json`. It contains basic definitions
for most keywords, including their cards and fields (which are defined by offset, name, default
value, option, width, and helpstring). `kwd.json` must not be modified by hand, it is
produced by machine in a process that is external to PyDyna.

While this specification is expansive, providing definitions for thousands of keywords, not all
information pertaining to a keyword can be found there. In addition, there are some errors in that
specification. Due to this, `manifest.json` and `additional-cards.json` contain that information
that either supplements or corrects the information in `kwd.json`.

Corrections include:
    - fixing the order of cards ("reorder-card")
    - skipping an unnecessary card ("skip-card")
    - changing the name of a subkeyword ("override-subkeyword")
    - changing the definition of a field ("override-field")
    - replacing a card ("replace-card")
    - inserting a card ("insert-card")
    - changing the name of a python property ("rename-property")

Supplements include:
    - adding aliases - see `Appendix A - Aliasing`
    - cards that repeat, handled as a two dimensional table ("table-card")
    - A field represented as a one-dimensional array that repeat across cards ("series-card")
    - A set of adjacent cards with their own specification. These may repeat ("card-set")
    - A card that is only active under a condition ("conditional-card")
    - A set of adjacent cards that repeat, handled as a two dimensional table ("table-card-group")
    - Adding option cards ("add-option")
    - A field shared across multiple cards with only one meaning ("shared-field")


## Appendix A

### Aliasing

In some cases, two keywords are defined in exactly the same way and have the same meaning. This is called
an alias. Examples of this are `MAT_058` and `MAT_LAMINATED_COMPOSITE_FABRIC`. In such cases, the class
generator will generate two classes, but one of the classes will alias the behavior of the other, the only
difference being the name of the keyword. In the case of `MAT_058` and its alias, both keywords are defined
in `kwd.json`, so one of them will be ignored by the code generator. It is possible for only one of the two
keywords to be defined in `kwd.json`, such as is the case for `SET_NODE` and `SET_NODE_LIST`. In that case,
the class generator will produce the same effect, except that it does not need to ignore anything in
`kwd.json`.

