"""Narrow hover handler for the PyDyna LSP server.

Algorithm
---------
1. Extract the keyword block at the cursor position (via DocumentStore).
2. Resolve the keyword class via the deck-loader's type mapping to get a ``KeywordBase`` instance.
3. Compute ``block_relative_line`` (0 = ``*NAME`` line).
4. Walk the keyword's active cards to find which card owns that line.
5. From the card's schema, find the field that spans the cursor column.
6. Return a ``types.Hover`` with the field's docstring as Markdown.

Line mapping
------------
``KeywordBase.write()`` emits lines in this order::

    *KEYWORD_NAME\\n               <- block_relative_line 0  (*NAME)
    [user_comment lines]           <- rarely present when parsing from text
    $#<field names>\\n             <- comment line for card 0  (line 1)
    <field values>                 <- data line for card 0     (line 2)
    $#<field names>\\n             <- comment line for card 1  (line 3)
    <field values>                 <- data line for card 1     (line 4)
    ...

Each *active* fixed-width card contributes exactly 2 lines: one comment and
one data line.  Inactive cards are skipped entirely.
"""

from __future__ import annotations

import logging
import typing

from lsprotocol import types

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------


def narrow_hover(store, params: types.HoverParams) -> typing.Optional[types.Hover]:
    """Return hover documentation for the LS-DYNA field under the cursor.

    Parameters
    ----------
    store : DocumentStore
        The shared document store (from ``documents.py``).
    params : types.HoverParams
        The LSP hover request parameters.

    Returns
    -------
    types.Hover or None
        A ``Hover`` containing Markdown documentation for the field, or
        ``None`` when the cursor is not on a recognisable field.
    """
    uri = params.text_document.uri
    line = params.position.line
    col = params.position.character

    block = store.get_keyword_block(uri, line)
    if block is None:
        return None

    kw = _parse_block(block.text)
    if kw is None:
        return None

    block_relative_line = line - block.start_line
    result = _resolve_field(kw, block_relative_line, col)
    if result is None:
        return None

    _card, field_schema = result
    md = _format_markdown(kw, field_schema)

    hover_range = types.Range(
        start=types.Position(line=line, character=field_schema.offset),
        end=types.Position(line=line, character=field_schema.offset + field_schema.width),
    )

    return types.Hover(
        contents=types.MarkupContent(kind=types.MarkupKind.Markdown, value=md),
        range=hover_range,
    )


# ---------------------------------------------------------------------------
# Internal helpers (also exported for unit testing)
# ---------------------------------------------------------------------------


def _parse_block(block_text: str):
    """Parse a keyword block and return a ``KeywordBase`` instance, or ``None``.

    Uses the deck-loader's type-mapping directly to instantiate the correct
    ``KeywordBase`` subclass without calling ``Deck.loads()``.  This means
    ``&param`` references in the block text are irrelevant — we only need the
    keyword name on the first line to look up the class, and the resulting
    object already carries the full card/field schema.

    Parameters
    ----------
    block_text : str
        Raw block text starting with the ``*KEYWORD_NAME`` line.

    Returns
    -------
    KeywordBase or None
    """
    try:
        from ansys.dyna.core.lib.deck_loader import _create_keyword_object, _get_kwd_class_and_format

        first_line = block_text.lstrip().splitlines()[0].strip()
        # _get_kwd_class_and_format expects the raw *KEYWORD_NAME token
        # (with the leading asterisk) as it appears in the file.
        type_name, _fmt = _get_kwd_class_and_format(first_line)
        if type_name is None:
            return None
        return _create_keyword_object(None, type_name)
    except Exception:
        logger.debug("_parse_block failed for block:\n%s", block_text, exc_info=True)
        return None


def _resolve_field(kw, block_relative_line: int, col: int):
    """Return ``(card, FieldSchema)`` for the field at *block_relative_line*/*col*.

    Parameters
    ----------
    kw : KeywordBase
        The parsed keyword.
    block_relative_line : int
        0-based line offset within the block (0 = ``*NAME`` line).
    col : int
        0-based column (character index).

    Returns
    -------
    tuple(card, FieldSchema) or None
    """
    if block_relative_line <= 0:
        # Hovering over the *NAME line itself — no field data here.
        return None

    try:
        all_cards = kw._get_all_cards()
    except Exception:
        logger.debug("_resolve_field: _get_all_cards() failed", exc_info=True)
        return None

    # Optional user-comment lines sit between *NAME and the first card when a
    # keyword has an embedded comment.  During narrow-hover parsing the
    # user_comment is always empty, but we compute it correctly for robustness.
    user_comment_line_count = len(kw._get_user_comment_lines())
    line_cursor = 1 + user_comment_line_count

    for card in all_cards:
        if not card.active:
            continue

        # CardSet: contains a list of sub-keyword items, each with their own
        # cards.  The first item writes comment+data per sub-card; subsequent
        # items write data-only per sub-card.  Recurse into the first item.
        if hasattr(card, "_items"):
            result = _resolve_field_in_card_set(card, block_relative_line, col, line_cursor)
            if result is not None:
                return result
            # Advance past all lines this CardSet occupies.
            line_cursor = _card_set_line_end(card, line_cursor)
            continue

        schema = getattr(card, "_schema", None)
        if schema is None or not getattr(schema, "fields", None):
            # TableCard / SeriesCard / TextCard — variable line count.
            # Advance conservatively by 2 lines (comment + 1 data row).
            line_cursor += 2
            continue

        comment_line = line_cursor
        data_line = line_cursor + 1
        line_cursor += 2

        if block_relative_line == comment_line:
            # $# column-header line — no value to document.
            return None

        if block_relative_line == data_line:
            # Data line for this card — find the field spanning `col`.
            for fs in schema.fields:
                if fs.offset <= col < fs.offset + fs.width:
                    return (card, fs)
            # Cursor is past all fields or in an inter-field gap.
            return None

        # This card's lines are behind the cursor; keep walking.

    return None


def _active_sub_cards(item):
    """Yield active fixed-schema cards from a CardSet item."""
    try:
        sub_cards = item._get_all_cards()
    except Exception:
        return
    for sub_card in sub_cards:
        if sub_card.active:
            yield sub_card


def _card_set_line_end(card_set, line_cursor: int) -> int:
    """Return the line_cursor value after all lines consumed by *card_set*."""
    items = card_set._items
    if not items:
        return line_cursor
    for item_idx, item in enumerate(items):
        first_item = item_idx == 0
        for sub_card in _active_sub_cards(item):
            schema = getattr(sub_card, "_schema", None)
            if schema is None or not getattr(schema, "fields", None):
                line_cursor += 2 if first_item else 1
            else:
                line_cursor += 2 if first_item else 1
    return line_cursor


def _resolve_field_in_card_set(card_set, block_relative_line: int, col: int, line_cursor: int):
    """Recurse into a CardSet to find the field at *block_relative_line*/*col*.

    CardSet.write() structure::

        item 0:  $#comment + data for each active sub-card  (2 lines per sub-card)
        item 1+: data only for each active sub-card         (1 line per sub-card)
    """
    items = card_set._items
    if not items:
        return None
    for item_idx, item in enumerate(items):
        first_item = item_idx == 0
        for sub_card in _active_sub_cards(item):
            schema = getattr(sub_card, "_schema", None)
            if schema is None or not getattr(schema, "fields", None):
                line_cursor += 2 if first_item else 1
                continue
            if first_item:
                comment_line = line_cursor
                data_line = line_cursor + 1
                line_cursor += 2
                if block_relative_line == comment_line:
                    return None
            else:
                data_line = line_cursor
                line_cursor += 1
            if block_relative_line == data_line:
                for fs in schema.fields:
                    if fs.offset <= col < fs.offset + fs.width:
                        return (sub_card, fs)
                return None
    return None


def _format_markdown(kw, field_schema) -> str:
    """Build Markdown hover text for *field_schema* in *kw*.

    Parameters
    ----------
    kw : KeywordBase
        The parsed keyword owning the field.
    field_schema : FieldSchema
        The matched field schema entry.

    Returns
    -------
    str
        Markdown string suitable for ``MarkupContent(kind=Markdown, …)``.
    """
    kwd_name = type(kw).__name__
    field_name = field_schema.name

    # Retrieve the property's docstring when available.
    doc = ""
    try:
        prop = getattr(type(kw), field_name, None)
        if prop is not None and hasattr(prop, "fget") and prop.fget.__doc__:
            doc = prop.fget.__doc__.strip()
    except Exception:
        pass

    ftype = field_schema.type
    if ftype is None:
        type_str = "unknown"
    elif hasattr(ftype, "__name__"):
        type_str = ftype.__name__
    else:
        type_str = str(ftype)

    parts: typing.List[str] = [f"**`{field_name}`**"]
    if doc:
        parts.append(f"\n{doc}")
    parts.append(f"\n**Keyword:** `{kwd_name}`  |  **Type:** `{type_str}`  |  **Width:** {field_schema.width}")

    return "\n".join(parts)
