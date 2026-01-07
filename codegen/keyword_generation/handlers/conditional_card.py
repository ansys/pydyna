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
Conditional Card Handler: Adds conditional rendering logic to cards.

This handler enables cards to be rendered only when specific conditions are met,
supporting dynamic card structures based on field values.
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict

from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class ConditionalCardSettings:
    """Configuration for conditional card inclusion."""

    index: int
    func: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ConditionalCardSettings":
        return cls(index=data["index"], func=data["func"])


@handler(
    name="conditional-card",
    dependencies=[],
    description="Adds conditional rendering logic to cards based on field values",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer", "description": "Card index to make conditional"},
                "func": {"type": "string", "description": "Python expression for condition"},
            },
            "required": ["index", "func"],
        },
    },
    output_description="Adds 'func' property to card dict containing conditional expression",
)
class ConditionalCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Makes cards conditional based on field values.

    This handler adds a 'func' property to cards that contains a Python expression.
    The template uses this to generate conditional logic that determines whether
    the card should be rendered in the output.

    Input Settings Example:
        [
            {
                "index": 1,
                "func": "self.iauto == 3"
            }
        ]

    Output Modification:
        Adds 'func' key to card dict:
        card["func"] = "self.iauto == 3"
    """

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[ConditionalCardSettings]:
        """Convert dict settings to typed ConditionalCardSettings instances."""
        return [ConditionalCardSettings.from_dict(s) for s in settings]

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Add conditional logic to specified cards.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of {"index": int, "func": str} dicts
        """
        # Parse settings into typed instances
        typed_settings = self._parse_settings(settings)

        for setting in typed_settings:
            card = kwd_data.cards[setting.index]
            card["func"] = setting.func

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
