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
Reorder Card Handler: Changes the order of cards within a keyword.

This handler must run before all other handlers that reference card indices,
as reordering affects the index values used by other handlers.
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.handlers.base_settings import parse_settings_list
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class ReorderCardSettings:
    """Configuration for card reordering."""

    order: List[int]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ReorderCardSettings":
        return cls(order=data["order"])


@handler(
    name="reorder-card",
    description="Reorders cards within a keyword based on specified index sequence",
    input_schema={
        "type": "array",
        "minItems": 1,
        "maxItems": 1,
        "items": {
            "type": "object",
            "properties": {"order": {"type": "array", "items": {"type": "integer"}}},
            "required": ["order"],
        },
    },
    output_description="Modifies 'cards' list to match the specified order",
)
class ReorderCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Reorders cards within a keyword definition.

    CRITICAL: This handler affects card indices used by all other handlers.
    It has no dependencies and should run early in the handler pipeline.

    Input Settings Example:
        [
            {
                "order": [0, 1, 4, 2, 3, 5]
            }
        ]

    Output Modification:
        Reorders kwd_data.cards list according to the specified positions.
        The "order" list specifies which original card goes to each position.
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Reorder cards based on the specified position sequence.

        Args:
            kwd_data: Complete keyword data
            settings: List containing single dict with "order" key (list of positions)

        Note: This reorders the list but does NOT update each card's 'index' property.
        Labels track card objects by reference, so they remain valid after reordering.
        """
        # Parse settings into typed instances
        typed_settings = parse_settings_list(ReorderCardSettings, settings)

        assert (
            len(typed_settings) == 1
        ), f"reorder-card handler expects exactly 1 settings dict, got {len(typed_settings)}"
        # TODO - mark the reorders and let that get settled after the handlers run
        kwd_data.cards = [kwd_data.cards[i] for i in typed_settings[0].order]
