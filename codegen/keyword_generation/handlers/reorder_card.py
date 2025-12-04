# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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

import typing

import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="reorder-card",
    dependencies=[],
    description="Reorders cards within a keyword based on specified index sequence",
    input_schema={
        "type": "object",
        "properties": {"order": {"type": "array", "items": {"type": "integer"}}},
        "required": ["order"],
    },
    output_description="Modifies 'cards' list to match the specified order",
)
class ReorderCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Reorders cards within a keyword definition.

    CRITICAL: This handler affects card indices used by all other handlers.
    It has no dependencies and should run early in the handler pipeline.

    Input Settings Example:
        {
            "order": [0, 1, 4, 2, 3, 5]
        }

    Output Modification:
        Reorders kwd_data["cards"] list according to the specified indices.
        Original card at index 0 stays at 0, card at index 4 moves to position 2, etc.
    """

    def handle(self, kwd_data: typing.Any, settings: typing.Dict[str, typing.Any]) -> None:
        """
        Reorder cards based on the specified index sequence.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: Dict with "order" key containing list of indices

        Note: This reorders the list but does NOT update each card's 'index' property.
        Subsequent handlers use list positions (kwd_data["cards"][3]) not card indices.
        """
        # TODO - mark the reorders and let that get settled after the handlers run
        order = settings["order"]
        kwd_data.cards = [kwd_data.cards[i] for i in order]

    def post_process(self, kwd_data: typing.Any) -> None:
        """No post-processing required."""
        pass
