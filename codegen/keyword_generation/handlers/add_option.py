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
Add Option Handler: Creates optional card groups in keywords.

This handler adds optional sections to keywords, allowing cards to be conditionally
included based on keyword title options (e.g., *KEYWORD_OPTION1_OPTION2).
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict, List, Optional

from keyword_generation.data_model import get_card
from keyword_generation.data_model.keyword_data import Card, KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class AddOptionSettings:
    """Configuration for adding keyword options."""

    name: str
    card_order: int
    title_order: int
    cards: List[Dict[str, Any]]
    func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "AddOptionSettings":
        return cls(
            name=data["name"],
            card_order=data["card-order"],
            title_order=data["title-order"],
            cards=data["cards"],
            func=data.get("func"),
        )


@handler(
    name="add-option",
    description="Adds optional card groups that appear based on keyword title options",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "option-name": {"type": "string", "description": "Name of the option (e.g., 'ID')"},
                "card-order": {"type": "integer", "description": "Card ordering priority"},
                "title-order": {"type": "integer", "description": "Order in keyword title"},
                "cards": {
                    "type": "array",
                    "description": "List of card definitions or references",
                },
            },
            "required": ["option-name", "card-order", "title-order", "cards"],
        },
    },
    output_description="Adds 'options' list to kwd_data with optional card group definitions",
)
class AddOptionHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Adds optional card groups to keywords based on title options.

    This handler enables keywords to have optional sections that are only included
    when specific options appear in the keyword title. For example, *CONTACT_ID
    would include ID-specific cards.

    Input Settings Example:
        [
            {
                "option-name": "ID",
                "card-order": 1,
                "title-order": 1,
                "cards": [
                    {"name": "id_card", "fields": [...]},
                    "existing_card_reference"
                ]
            },
            {
                "option-name": "MPP",
                "card-order": 2,
                "title-order": 2,
                "cards": [{...}]
            }
        ]

    Output Modification:
        Adds kwd_data["options"] = [
            {
                "name": "ID",
                "card_order": 1,
                "title_order": 1,
                "cards": [...],  # expanded with 'func' for active conditions
            },
            ...
        ]
    """

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[typing.Dict[str, typing.Any]]:
        """Keep dict settings for add-option - uses 'option-name' not 'name'."""
        return settings

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Create optional card groups from settings.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of option definitions with cards
        """
        typed_settings = self._parse_settings(settings)

        def expand(card: Card):
            if "active" in card:
                card["func"] = card["active"]
            return card

        new_options = []
        for option_settings in typed_settings:
            cards = [get_card(card) for card in option_settings["cards"]]
            cards = [expand(card) for card in cards]
            new_option = {
                "card_order": option_settings["card-order"],
                "title_order": option_settings["title-order"],
                "name": option_settings["option-name"],
                "cards": cards,
            }
            new_options.append(new_option)
        kwd_data.options = new_options

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
