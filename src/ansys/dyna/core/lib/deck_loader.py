# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.keyword_base import KeywordBase


class IterState:
    USERCOMMENT = 0
    KEYWORD_BLOCK = 1
    TITLE = 2
    KEYWORDS = 3
    END = 4


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


def _try_load_deck(deck: "ansys.dyna.core.deck.Deck", text: str, result: DeckLoaderResult) -> None:
    lines = text.splitlines()
    iterator = iter(lines)
    iterstate = IterState.USERCOMMENT

    def update_iterstate(line: str):
        if line.startswith("*KEYWORD"):
            return IterState.KEYWORD_BLOCK
        if line.startswith("*TITLE"):
            return IterState.TITLE
        if line.startswith("*END"):
            return IterState.END
        return IterState.KEYWORDS

    def update_deck_format(block: typing.List[str], deck: "ansys.dyna.core.deck.Deck") -> None:
        assert len(block) == 1
        line = block[0].upper()
        if "LONG" in line:
            format_setter = line[line.find("LONG") + 4 :].strip()
            tokens = format_setter.split("=")
            assert len(tokens) >= 2
            format = tokens[1]
            if format == "S":
                deck.format = format_type.default
            if format == "K":
                deck.format = format_type.standard
            if format == "Y":
                deck.format = format_type.long

    def update_deck_comment(block: typing.List[str], deck: "ansys.dyna.core.deck.Deck") -> None:
        def remove_comment_symbol(line: str):
            if not line.startswith("$"):
                raise Exception("Only comments can precede *KEYWORD")
            return line[1:]

        block_without_comment_symbol = [remove_comment_symbol(line) for line in block]
        deck.comment_header = "\n".join(block_without_comment_symbol)

    def update_deck_title(block: typing.List[str], deck: "ansys.dyna.core.deck.Deck") -> None:
        block = [line for line in block if not line.startswith("$")]
        assert len(block) == 2, "Title block can only have one line"
        deck.title = block[1]

    def handle_keyword(block: typing.List[str], deck: "ansys.dyna.core.deck.Deck") -> None:
        keyword = block[0].strip()
        keyword_data = "\n".join(block)
        keyword_object_type, format = _get_kwd_class_and_format(keyword)
        if keyword_object_type == None:
            result.add_unprocessed_keyword(keyword)
            deck.append(keyword_data)
        else:
            import ansys.dyna.core.keywords

            keyword_object: KeywordBase = getattr(ansys.dyna.core.keywords, keyword_object_type)()
            if format == format_type.default:
                format = deck.format
            keyword_object.format = format
            try:
                keyword_object.loads(keyword_data)
                deck.append(keyword_object)
            except Exception:
                result.add_unprocessed_keyword(keyword)
                deck.append(keyword_data)

    def handle_block(iterstate: int, block: typing.List[str]) -> bool:
        if iterstate == IterState.END:
            return True
        if iterstate == IterState.USERCOMMENT:
            update_deck_comment(block, deck)
        elif iterstate == IterState.KEYWORD_BLOCK:
            update_deck_format(block, deck)
        elif iterstate == IterState.TITLE:
            update_deck_title(block, deck)
        else:
            handle_keyword(block, deck)
        return False

    block = []
    while True:
        try:
            line = next(iterator)
            if line.startswith("*"):
                # handle the previous block
                end = handle_block(iterstate, block)
                if end:
                    return
                # set the new iterstate, start building the next block
                iterstate = update_iterstate(line)
                block = [line]
            else:
                if iterstate == IterState.KEYWORD_BLOCK:
                    # reset back to user comment after the keyword line?
                    iterstate = IterState.USERCOMMENT
                    block = []
                else:
                    block.append(line)
        except StopIteration:
            handle_block(iterstate, block)
            return


def load_deck(deck: "ansys.dyna.core.deck.Deck", text: str) -> DeckLoaderResult:
    result = DeckLoaderResult()
    _try_load_deck(deck, text, result)
    return result
