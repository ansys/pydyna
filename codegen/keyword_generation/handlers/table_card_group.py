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
Table Card Group Handler: Creates duplicate card groups for table-like data.

This handler groups multiple cards together to form a repeating structure,
commonly used for table or matrix data where multiple related cards repeat as a unit.
"""

import typing

import keyword_generation.data_model as gen
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="table-card-group",
    dependencies=["reorder-card"],
    description="Creates duplicate card groups for table-like repeating card structures",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "indices": {
                    "type": "array",
                    "items": {"type": "integer"},
                    "description": "Card indices to group together",
                },
                "overall-name": {"type": "string", "description": "Name of the duplicate group"},
                "length-func": {"type": "string", "description": "Function to compute group count"},
                "active-func": {"type": "string", "description": "Function to determine if group is active"},
            },
            "required": ["indices", "overall-name"],
        },
    },
    output_description=(
        "Sets kwd_data['duplicate_group']=True, adds card insertion with duplicate group, "
        "marks source cards for removal"
    ),
)
class TableCardGroupHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Groups cards together to form repeating table-like structures.

    This handler creates duplicate card groups where multiple cards are treated
    as a single repeatable unit. Common use cases include table data where each
    row consists of multiple cards that must be repeated together.

    CRITICAL: Uses reference semantics (not deep copies). Cards are appended
    to sub_cards by reference so that later handler modifications (e.g.,
    conditional-card setting 'func') appear in the group.

    Input Settings Example:
        [
            {
                "indices": [2, 3, 4],
                "overall-name": "table_row",
                "length-func": "self.nrows",
                "active-func": "self.has_table"
            }
        ]

    Output Modification:
        - Sets kwd_data["duplicate_group"] = True
        - Creates duplicate group structure:
          {
              "duplicate_group": True,
              "sub_cards": [...],  # Cards from indices
              "overall_name": "table_row",
              "length_func": "self.nrows",
              "active_func": "self.has_table"
          }
        - Inserts group at minimum index position
        - Marks all source cards with "mark_for_removal" = 1
    """

    def handle(self, kwd_data: typing.Any, settings: typing.Any) -> None:
        """
        Create duplicate card groups from card indices.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of card group definitions
        """
        kwd_data.duplicate_group = True
        settings_list = typing.cast(typing.List[typing.Dict[str, typing.Any]], settings)
        for card_settings in settings_list:
            indices_raw = card_settings["indices"]
            indices: typing.List[int] = typing.cast(typing.List[int], indices_raw)
            # build the card group
            group = {
                "duplicate_group": True,
                "sub_cards": [],
                "overall_name": card_settings["overall-name"],
                "length_func": card_settings.get("length-func", ""),
                "active_func": card_settings.get("active-func", ""),
            }
            for index in indices:
                sub_card = kwd_data.cards[index]
                sub_card["mark_for_removal"] = 1
                group["sub_cards"].append(sub_card)
            # remove all the sub-cards
            indices.sort(reverse=True)
            for index in indices:
                kwd_data.cards[index]["mark_for_removal"] = 1
            insertion = gen.Insertion(min(indices), "", group)
            kwd_data.card_insertions.append(insertion)

    def post_process(self, kwd_data: typing.Any) -> None:
        """No post-processing required."""
        pass
