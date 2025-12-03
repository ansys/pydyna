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
Table Card Handler: Marks cards that represent 2D tabular data.

Indicates that a card repeats to form a table structure (e.g., integration points).
The card repeats based on a length function.
"""

import typing

import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="table-card",
    dependencies=["reorder-card"],
    description="Marks cards as repeating table structures with dynamic row count",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer"},
                "property-name": {"type": "string"},
                "length-func": {"type": "string"},
                "active-func": {"type": "string"},
            },
            "required": ["index", "property-name"],
        },
    },
    output_description="Sets kwd_data['duplicate']=True and adds 'duplicate' dict to card",
)
class TableCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Marks cards as table structures.

    Table cards repeat to form 2D data structures where the number of
    repetitions is determined dynamically by a length function.

    Input Settings Example:
        [
            {
                "index": 4,
                "property-name": "integration_points",
                "length-func": "self.nipp",
                "active-func": "self.elform in [101, 102, 103]"
            }
        ]

    Output Modification:
        - Sets kwd_data["duplicate"] = True
        - Adds card["duplicate"] dict with name, length_func, active_func
    """

    def handle(self, kwd_data: typing.Any, settings: typing.Dict[str, typing.Any]) -> None:
        """
        Mark cards as table structures.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of table card specifications
        """
        kwd_data.duplicate = True
        settings_list = typing.cast(typing.List[typing.Dict[str, typing.Any]], settings)
        for card_settings in settings_list:
            duplicate_card = kwd_data.cards[card_settings["index"]]
            duplicate_card["duplicate"] = {
                "name": card_settings["property-name"],
                "length_func": card_settings.get("length-func", ""),
                "active_func": card_settings.get("active-func", ""),
            }

    def post_process(self, kwd_data: typing.Any) -> None:
        """No post-processing required."""
        pass
