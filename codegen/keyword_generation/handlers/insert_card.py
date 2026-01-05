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

Queues cards for insertion during the post-handler phase. Actual insertion
happens after all handlers run via _do_insertions().
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict

import keyword_generation.data_model as gen
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
    description="Queues new cards for insertion at specified indices",
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
    output_description="Appends Insertion objects to kwd_data['card_insertions'] list",
)
class InsertCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Queues cards for insertion.

    Does not insert cards immediately. Instead, creates Insertion objects
    that are processed later by _do_insertions() in the generation pipeline.

    Input Settings Example:
        [
            {
                "index": 0,
                "card": {
                    "source": "additional-cards",
                    "card-name": "CONTACT_CARD_1"
                }
            }
        ]

    Output Modification:
        Appends Insertion(target_index, "", card) to kwd_data["card_insertions"]
    """

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[typing.Dict[str, typing.Any]]:
        """Keep dict settings for insert-card - target-class not in manifest."""
        return settings

    def handle(self, kwd_data: KeywordData, settings: typing.List[typing.Dict[str, typing.Any]]) -> None:
        """
        Queue cards for insertion.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of {"index", "card"} dicts
        """
        typed_settings = self._parse_settings(settings)
        for card_settings in typed_settings:
            index = card_settings["index"]
            card = get_card(card_settings["card"])
            insertion = gen.Insertion(index, "", card)
            kwd_data.card_insertions.append(insertion)

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
