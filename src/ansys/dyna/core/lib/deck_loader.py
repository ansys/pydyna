# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import io
import typing

import ansys.dyna.core
from ansys.dyna.core.lib.encrypted_keyword import EncryptedKeyword
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.import_handler import ImportContext, ImportHandler
from ansys.dyna.core.lib.keyword_base import KeywordBase


class IterState:
    USERCOMMENT = 0
    KEYWORD_BLOCK = 1
    TITLE = 2
    KEYWORDS = 3
    END = 4
    ENCRYPTED = 5


class DeckLoaderResult:
    """A class containing the result of an attempted deck load."""

    def __init__(self):
        self._unprocessed_keywords = []

    def add_unprocessed_keyword(self, name):
        self._unprocessed_keywords.append(name)

    def get_summary(self) -> str:
        summary = io.StringIO()
        for unprocessed_keyword in self._unprocessed_keywords:
            summary.write(f"Failed to process: {unprocessed_keyword}\n")
        return summary.getvalue()


def _get_kwd_class_and_format(keyword_name: str) -> str:
    # handle spaces in keyword_name, such as
    # *ELEMENT_SOLID (ten nodes format) => *ELEMENT_SOLID
    # the spaces are used as hints for LSPP but not needed
    # by the dyna solver
    from ansys.dyna.core.keywords.keyword_classes.type_mapping import TypeMapping

    keyword_name = keyword_name.split()[0]
    title_tokens = keyword_name.split("_")

    # Handling title is a hack right now. Should be able to find the correct
    # keyword object type given any prefix and suffix options
    if keyword_name.endswith("-"):
        format = format_type.standard
        keyword_name = keyword_name[:-1]
    elif keyword_name.endswith("+"):
        format = format_type.long
        keyword_name = keyword_name[:-1]
    else:
        format = format_type.default

    keyword_object_type = TypeMapping.get(keyword_name, None)

    while keyword_object_type is None:
        if len(title_tokens) == 0:
            break
        title_tokens = title_tokens[:-1]
        keyword_name = "_".join(title_tokens)
        keyword_object_type = TypeMapping.get(keyword_name, None)
    return keyword_object_type, format


def _update_iterstate(line: str):
    if line.startswith("*KEYWORD"):
        return IterState.KEYWORD_BLOCK
    if line.startswith("*TITLE"):
        return IterState.TITLE
    if line.startswith("*END"):
        return IterState.END
    if "BEGIN PGP MESSAGE" in line:
        return IterState.ENCRYPTED
    return IterState.KEYWORDS


def _update_deck_format(block: typing.List[str], deck: "ansys.dyna.core.deck.Deck") -> None:
    if len(block) != 1:
        raise ValueError("Format line should only have one line")
    line = block[0].upper()
    if "LONG" in line:
        format_setter = line[line.find("LONG") + 4 :].strip()
        tokens = format_setter.split("=")
        if len(tokens) < 2:
            raise ValueError("Invalid format line")
        format = tokens[1]
        if format == "S":
            deck.format = format_type.default
        if format == "K":
            deck.format = format_type.standard
        if format == "Y":
            deck.format = format_type.long


def _update_deck_comment(block: typing.List[str], deck: "ansys.dyna.core.deck.Deck") -> None:
    def remove_comment_symbol(line: str):
        if not line.startswith("$"):
            raise Exception("Only comments can precede *KEYWORD")
        return line[1:]

    block_without_comment_symbol = [remove_comment_symbol(line) for line in block]
    deck.comment_header = "\n".join(block_without_comment_symbol)


def _before_import(
    block: typing.List[str],
    keyword: str,
    keyword_data: str,
    import_handlers: typing.List[ImportHandler],
    context: ImportContext,
) -> bool:
    if len(import_handlers) == 0:
        return True

    if context is None:
        raise ValueError("Context cannot be None")
    s = io.StringIO()
    s.write(keyword_data)
    s.seek(0)

    for handler in import_handlers:
        if not handler.before_import(context, keyword, s):
            return False
        s.seek(0)
    return True


def _update_deck_title(block: typing.List[str], deck: "ansys.dyna.core.deck.Deck") -> None:
    block = [line for line in block if not line.startswith("$")]
    if len(block) != 2:
        raise ValueError("Title block can only have one line")
    deck.title = block[1]


def _on_error(error, import_handlers: typing.List[ImportHandler]):
    for handler in import_handlers:
        handler.on_error(error)


def _after_import(keyword, import_handlers: typing.List[ImportHandler], context: ImportContext):
    for handler in import_handlers:
        handler.after_import(context, keyword)


def _get_format_from_keyword_suffix(keyword: str) -> format_type:
    """Determine format type from keyword suffix (- or +)."""
    if keyword.endswith("-"):
        return format_type.standard
    elif keyword.endswith("+"):
        return format_type.long
    return format_type.default


def _resolve_keyword_class(
    keyword: str, context: typing.Optional[ImportContext]
) -> typing.Tuple[typing.Optional[type], typing.Optional[str], format_type]:
    """Resolve the keyword class to use, checking overrides first.

    Returns
    -------
    tuple
        (keyword_class, keyword_type_name, format)
        - keyword_class: Direct class reference if from override, else None
        - keyword_type_name: Class name string if from TypeMapping, else None
        - format: The format type to use
    """
    keyword_name = keyword.split()[0]
    keyword_overrides = context.keyword_overrides if context else {}

    # Check for override first
    override_class = keyword_overrides.get(keyword_name)
    if override_class is not None:
        return override_class, None, _get_format_from_keyword_suffix(keyword)

    # Fall back to TypeMapping lookup
    type_name, format = _get_kwd_class_and_format(keyword)
    return None, type_name, format


def _create_keyword_object(
    keyword_class: typing.Optional[type], keyword_type_name: typing.Optional[str]
) -> typing.Optional[KeywordBase]:
    """Create a keyword object from either a direct class or type name."""
    if keyword_class is not None:
        return keyword_class()

    if keyword_type_name is not None:
        import ansys.dyna.core.keywords

        return getattr(ansys.dyna.core.keywords, keyword_type_name)()

    return None


def _load_keyword(
    keyword_object: KeywordBase,
    keyword: str,
    keyword_data: str,
    deck: "ansys.dyna.core.deck.Deck",
    import_handlers: typing.List[ImportHandler],
    result: DeckLoaderResult,
    context: ImportContext,
) -> None:
    """Load keyword data into object and append to deck, handling errors."""
    try:
        keyword_object.loads(keyword_data, deck.parameters)
        deck.append(keyword_object)
        _after_import(keyword_object, import_handlers, context)
    except Exception as e:
        _on_error(e, import_handlers)
        result.add_unprocessed_keyword(keyword)
        deck.append(keyword_data)
        _after_import(keyword_data, import_handlers, context)


def _handle_keyword(
    block: typing.List[str],
    deck: "ansys.dyna.core.deck.Deck",
    import_handlers: typing.List[ImportHandler],
    result: DeckLoaderResult,
    context: ImportContext,
) -> None:
    """Process a keyword block and add it to the deck."""
    keyword = block[0].strip()
    keyword_data = "\n".join(block)

    if not _before_import(block, keyword, keyword_data, import_handlers, context):
        return

    keyword_class, keyword_type_name, format = _resolve_keyword_class(keyword, context)
    keyword_object = _create_keyword_object(keyword_class, keyword_type_name)

    if keyword_object is None:
        result.add_unprocessed_keyword(keyword)
        deck.append(keyword_data)
        _after_import(keyword_data, import_handlers, context)
        return

    if format == format_type.default:
        format = deck.format
    keyword_object.format = format

    _load_keyword(keyword_object, keyword, keyword_data, deck, import_handlers, result, context)


def _handle_block(
    iterstate: int,
    deck: "ansys.dyna.core.deck.Deck",
    block: typing.List[str],
    import_handlers: typing.List[ImportHandler],
    result: DeckLoaderResult,
    context: ImportContext,
) -> bool:
    if iterstate == IterState.END:
        return True
    if iterstate == IterState.USERCOMMENT:
        _update_deck_comment(block, deck)
    elif iterstate == IterState.KEYWORD_BLOCK:
        _update_deck_format(block, deck)
    elif iterstate == IterState.TITLE:
        _update_deck_title(block, deck)
    else:
        _handle_keyword(block, deck, import_handlers, result, context)
    return False


def _try_load_deck_from_buffer(
    deck: "ansys.dyna.core.deck.Deck",
    buffer: typing.TextIO,
    result: DeckLoaderResult,
    context: typing.Optional[ImportContext],
    import_handlers: typing.List[ImportHandler],
) -> None:
    iterstate = IterState.USERCOMMENT
    block = []
    encrypted_section = None
    while True:
        close_previous_block = False
        try:
            line = buffer.readline()
            if len(line) == 0:
                _handle_block(iterstate, deck, block, import_handlers, result, context)
                return
            line = line.rstrip("\n")
            if line.startswith("*"):
                close_previous_block = True
            if "BEGIN PGP MESSAGE" in line:
                close_previous_block = True
                encrypted_section = io.StringIO()
            if "END PGP MESSAGE" in line:
                kwd = EncryptedKeyword()
                kwd.data = encrypted_section.getvalue()
                encrypted_section = None
                deck.append(kwd)
                _after_import(kwd, import_handlers, context)
                break
            if close_previous_block:
                # handle the previous block
                end = _handle_block(iterstate, deck, block, import_handlers, result, context)
                if end:
                    return
                # set the new iterstate, start building the next block
                iterstate = _update_iterstate(line)
                block = [line]
            else:
                if iterstate == IterState.ENCRYPTED:
                    encrypted_section.write(line)
                    encrypted_section.write("\n")
                if iterstate == IterState.KEYWORD_BLOCK:
                    # reset back to user comment after the keyword line?
                    iterstate = IterState.USERCOMMENT
                    block = []
                else:
                    block.append(line)
        except StopIteration:
            _handle_block(iterstate, deck, block, import_handlers, result, context)
            return


def load_deck(
    deck: "ansys.dyna.core.deck.Deck",
    text: str,
    context: typing.Optional[ImportContext],
    import_handlers: typing.List[ImportHandler],
) -> DeckLoaderResult:
    result = DeckLoaderResult()
    buffer = io.StringIO()
    buffer.write(text)
    buffer.seek(0)
    _try_load_deck_from_buffer(deck, buffer, result, context, import_handlers)
    return result


def load_deck_from_buffer(
    deck: "ansys.dyna.core.deck.Deck",
    buffer: typing.TextIO,
    context: typing.Optional[ImportContext],
    import_handlers: typing.List[ImportHandler],
) -> DeckLoaderResult:
    result = DeckLoaderResult()
    _try_load_deck_from_buffer(deck, buffer, result, context, import_handlers)
    return result
