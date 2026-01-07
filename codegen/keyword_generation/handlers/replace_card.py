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
Replace Card Handler: Replaces entire cards with alternatives.

Uses cards from additional-cards.json or other sources to completely
replace existing cards in the keyword structure.
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict, List

from keyword_generation.data_model import get_card
from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class ReplaceCardSettings:
    """Configuration for replacing card fields."""

    index: int
    fields: List[Dict[str, Any]]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ReplaceCardSettings":
        return cls(index=data["index"], fields=data["fields"])


@handler(
    name="replace-card",
    dependencies=["reorder-card"],
    description="Replaces entire cards with alternative definitions from additional-cards.json",
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
    output_description="Replaces cards at specified indices with loaded card definitions",
)
class ReplaceCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Replaces complete card definitions.

    Loads card from additional-cards.json and replaces the card at the
    specified index. Useful for substituting standardized card definitions.

    Input Settings Example:
        [
            {
                "index": 1,
                "card": {
                    "source": "additional-cards",
                    "card-name": "BLANK"
                }
            }
        ]

    Output Modification:
        Replaces kwd_data["cards"][index] with loaded card definition
    """

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[typing.Dict[str, typing.Any]]:
        """Keep dict settings for replace-card - manifest uses 'card' field."""
        return settings

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Replace cards with new card definitions.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of {"index", "card"} dicts
        """
        typed_settings = self._parse_settings(settings)
        for setting in typed_settings:
            index = setting["index"]
            replacement = get_card(setting["card"])
            replacement["index"] = index
            kwd_data.cards[index] = replacement

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
