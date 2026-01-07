# Codegen Refactor Recommendations

This file collects architectural and code-level recommendations for improving the codegen system.

## 1. Data Structures
- Typed dataclasses implemented for handlers, metadata, cards, and fields
- Remove Union types once all sources exclusively use dataclasses

## 2. Handler System

### Label Registry Design (Proposed)

The current handler system uses numeric indices to reference cards, which becomes fragile when multiple handlers modify the card list. This design proposes a **Label Registry** that provides stable, named references to cards and structures.

#### Problem Statement

Currently, handlers reference cards by index:
```json
{
  "handler": "insert-card",
  "settings": {
    "index": [3, 6, 4],
    "cards": ["CARD1", "CARD2", "HISV"]
  }
}
```

Issues:
1. **Index computation is non-intuitive** - Must account for reverse-order insertion and list.insert() semantics
2. **Fragile to upstream changes** - Adding a card to kwd.json shifts all downstream indices
3. **No stable identity** - Cards from different sources (kwd.json, insert-card, duplicate-card) have no consistent naming
4. **Multi-handler complexity** - CardSet, table-card-group create nested structures where flat indices don't apply

#### Proposed Solution: Label Registry

**Phase 1: Initial Labels** - Assign names to cards from kwd.json:
```json
{
  "INITIAL_STRESS_SHELL": {
    "labels": {
      "header": 0,
      "stress_data": 1,
      "hisv": 2
    },
    "handlers": [...]
  }
}
```

**Phase 2: Handler-Produced Labels** - Handlers that create entities label their outputs:
```json
{
  "handler": "insert-card",
  "settings": {
    "after": "stress_data",
    "cards": [
      {"label": "large_card1", "def": "INITIAL_STRESS_SHELL_LARGE_CARD1"},
      {"label": "large_card2", "def": "INITIAL_STRESS_SHELL_LARGE_CARD2"},
      {"label": "large_hisv", "def": "INITIAL_STRESS_SHELL_LARGE_HISV"}
    ]
  }
},
{
  "handler": "card-set",
  "settings": {
    "source_refs": ["large_card1", "large_card2", "large_hisv"],
    "active_func": "lambda kwd: kwd.large == 1",
    "label": "large_stress_set"
  }
}
```

#### Core Data Structures

```python
@dataclass
class CardAddress:
    """Location of a card in the (possibly nested) structure."""

    path: List[int]  # [3] for flat, [3, 0, 2] for nested in CardSets
    entity_type: str  # "card", "card_set", "table_card", "series_card"


@dataclass
class LabelRegistry:
    """Manages named references to cards and card structures."""

    _labels: Dict[str, CardAddress] = field(default_factory=dict)

    def register(self, label: str, address: CardAddress) -> None:
        """Register a label pointing to a card address."""
        if label in self._labels:
            raise ValueError(f"Label '{label}' already registered")
        self._labels[label] = address

    def resolve(self, label: str) -> CardAddress:
        """Resolve a label to its current address."""
        if label not in self._labels:
            raise KeyError(f"Unknown label: '{label}'")
        return self._labels[label]

    def update_after_insert(self, insert_position: int, count: int) -> None:
        """Shift all labels at or after insert_position by count."""
        for label, addr in self._labels.items():
            if addr.path[0] >= insert_position:
                addr.path[0] += count
```

#### Handler Interface Update

```python
class HandlerBase:
    def process(
        self,
        kwd_data: KeywordData,
        settings: Settings,
        labels: LabelRegistry,  # NEW: handlers get access to labels
    ) -> None:
        """Process the keyword data."""
        ...
```

#### Benefits

1. **Self-documenting manifests** - Names describe intent, not positions
2. **Refactoring-safe** - Change a card's position in kwd.json, labels still resolve correctly
3. **Composable** - Handler outputs become inputs to other handlers by name
4. **Debuggable** - Can dump label state at any point to understand the current structure
5. **Validation** - Detect undefined label references at manifest load time

#### Design Principles (from LLVM, databases, compilers)

- **Position-Independent Handles**: Reference by identity (label), not position (index)
- **Two-Phase Commit**: Collect all handler intents, then resolve and apply
- **Frozen Original State**: Labels initially reference kwd.json indices; a resolver maps to current positions
- **Hierarchical Addressing**: CardAddress.path supports nested structures (CardSet within CardSet)

#### Migration Path

1. Add LabelRegistry infrastructure (backward compatible)
2. Support both `"index"` and `"after"` in insert-card (deprecate index over time)
3. Migrate manifests one keyword at a time
4. Remove index-based references once all manifests are converted

### Further Considerations
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
