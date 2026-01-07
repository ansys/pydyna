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

"""
Insert Card Handler: Inserts new cards at specified positions.

Cards are inserted immediately during the handle() phase, sorted by index in
reverse order to ensure indices remain valid during insertion. This allows
subsequent handlers (like series-card, card-set) to reference the inserted cards.

IMPORTANT: Due to Python's list.insert() behavior with out-of-range indices
(which appends instead of inserting), and the reverse-order processing,
the manifest indices may need careful calculation to achieve desired final
positions. See the Index Computation section in agents/codegen.md for details.
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict

from keyword_generation.data_model import get_card
from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class InsertCardSettings:
    """Configuration for inserting additional cards."""

    index: int
    target_class: str
    card: Dict[str, Any]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "InsertCardSettings":
        return cls(
            index=data["index"],
            target_class=data["target-class"],
            card=data["card"],
        )


@handler(
    name="insert-card",
    dependencies=["reorder-card"],
    description="Inserts new cards at specified indices into kwd_data.cards",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer"},
                "card": {"type": "object"},
            },
            "required": ["index", "card"],
        },
    },
    output_description="Inserts Card objects directly into kwd_data.cards list",
)
class InsertCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Inserts new cards at specified positions.

    Cards are inserted immediately during handle(), sorted by index in reverse
    order. This ensures that inserting at index N doesn't shift indices for
    insertions at indices < N.

    IMPORTANT - Index Computation:
    Python's list.insert(i, x) appends if i >= len(list). Combined with
    reverse-order processing, the final card positions depend on both the
    specified indices AND the order of insertions.

    Example: Starting with 3 cards [A, B, C], inserting cards X, Y, Z:
        - Specify indices [3, 4, 5] → Reverse sort: [5, 4, 3]
        - Insert Z@5 (appends): [A, B, C, Z]
        - Insert Y@4 (appends): [A, B, C, Z, Y]
        - Insert X@3: [A, B, C, X, Z, Y]  ← Final: X@3, Z@4, Y@5

    To get X@3, Y@4, Z@5, specify indices [3, 6, 4]:
        - Reverse sort: [6, 4, 3]
        - Insert Y@6 (appends): [A, B, C, Y]
        - Insert Z@4 (appends): [A, B, C, Y, Z]
        - Insert X@3: [A, B, C, X, Y, Z]  ← Final: X@3, Y@4, Z@5

    Input Settings Example:
        [
            {
                "index": 3,
                "card": {
                    "source": "additional-cards",
                    "card-name": "MY_CARD_1"
                }
            }
        ]

    Output Modification:
        Directly inserts Card objects into kwd_data.cards list
    """

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[typing.Dict[str, typing.Any]]:
        """Keep dict settings for insert-card - target-class not in manifest."""
        return settings

    def handle(self, kwd_data: KeywordData, settings: typing.List[typing.Dict[str, typing.Any]]) -> None:
        """
        Insert cards immediately into keyword data.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of {"index", "card"} dicts
        """
        import logging

        logger = logging.getLogger(__name__)

        typed_settings = self._parse_settings(settings)
        # Sort by index in reverse order so insertions don't affect subsequent indices
        sorted_settings = sorted(typed_settings, key=lambda x: x["index"], reverse=True)
        for card_settings in sorted_settings:
            index = card_settings["index"]
            card = get_card(card_settings["card"])
            # Card is a dataclass - insert it directly (it supports dict-like access)
            kwd_data.cards.insert(index, card)
            logger.debug(f"Inserted card at index {index}, total cards now: {len(kwd_data.cards)}")

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
