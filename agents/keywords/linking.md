# Linking Related Keywords

When LS-DYNA keywords reference other keywords (e.g., DefineTable → DefineCurve), expose the relationship via a property on the parent keyword.

## Pattern

1. **Property naming**: Use a `_link` or plural suffix on the parent keyword class (e.g., `linked_curves`).
2. **Storage**: Store the linked children in a private `_linked_*` list on the parent; do NOT add back-references to child objects.
3. **Population**: Use an import handler to populate the list during `Deck.loads()` based on deck order or custom logic.
4. **Customization**: This approach allows users to subclass or customize the handler behavior, enabling validation and fixing of keyword files.
5. **Implementation**: Place the property in a manual subclass (under `keyword_classes/manual/`) to avoid conflicts with auto-generated files.

## Example: DefineTable → DefineCurve

**Manual subclass** (`keyword_classes/manual/define_table.py`):
```python
@property
def linked_curves(self) -> typing.List[typing.Any]:
    """List of DefineCurve keywords linked to this table."""
    return self._linked_curves
```

**Import handler** (`import_handlers/define_table_processor.py`):
```python
class DefineTableProcessor(ImportHandler):
    def __init__(self):
        self._current_table = None
    
    def after_import(self, context, keyword):
        if isinstance(keyword, DefineTable):
            self._current_table = keyword
        elif isinstance(keyword, DefineCurve):
            if self._current_table is not None:
                self._current_table._linked_curves.append(keyword)
        else:
            self._current_table = None
```

**Registration** (`deck.py`):
```python
self._import_handlers.append(DefineTableProcessor())
```

Users can now access `table.linked_curves` after loading a deck, and can subclass or replace the handler to customize linking logic.
