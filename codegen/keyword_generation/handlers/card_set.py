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
Card Set Handler: Creates reusable card collections with dynamic sizing.

This handler enables keywords to define card sets - groups of cards that can be
repeated with variable length, optionally including their own sub-options.
"""

import copy
import typing

import keyword_generation.data_model as gen
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="card-set",
    dependencies=["reorder-card", "add-option", "series-card", "table-card", "conditional-card"],
    description="Creates reusable card sets with dynamic sizing and optional sub-options",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Name of the card set"},
                "source-indices": {
                    "type": "array",
                    "items": {"type": "integer"},
                    "description": "Card indices to include in the set",
                },
                "source-options": {
                    "type": "array",
                    "items": {"type": "integer"},
                    "description": "Option indices to include in the set",
                },
                "target-index": {"type": "integer", "description": "Where to insert the set card"},
                "target-name": {"type": "string", "description": "Target option name (if inserting in option)"},
                "length-func": {"type": "string", "description": "Function to compute set length"},
                "active-func": {"type": "string", "description": "Function to determine if set is active"},
            },
            "required": ["name", "source-indices", "target-index"],
        },
    },
    output_description="Adds 'card_sets' dict and card insertions, marks source cards for removal",
)
class CardSetHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Creates reusable card sets with dynamic sizing.

    This handler enables complex card structures by grouping multiple cards into
    a reusable set that can be repeated. Card sets can include their own options
    and are dynamically sized at runtime.

    CRITICAL IMPLEMENTATION NOTES:
    1. **Reference Semantics**: This handler MUST append card references (not copies)
       to source_cards. Later handlers (e.g., conditional-card) modify these same
       card objects, so the modifications appear in both the main cards list AND
       the card-set's source_cards list. Using deepcopy breaks this behavior.

    2. **Handler Ordering**: This handler traditionally runs BEFORE conditional-card,
       series-card, and table-card. The source-indices refer to positions AFTER
       reorder-card has run but BEFORE transformations are applied. Later handlers
       modify the cards in-place via shared references.

    3. **Index Rewriting**: Cards store their original index in 'source_index' and
       get a new sequential 'index' within the card-set (0, 1, 2, ...). The
       'mark_for_removal' flag prevents them from appearing in the main cards list.

    Input Settings Example:
        [
            {
                "name": "LoadSet",
                "source-indices": [1, 2, 3],
                "source-options": [0],  # Include option 0 in the set
                "target-index": 1,
                "target-name": "",  # Empty = base keyword
                "length-func": "len(self.load_sets)",
                "active-func": "self.nsets > 0"
            }
        ]

    Output Modification:
        - Creates kwd_data["card_sets"] = {"sets": [...], "options": bool}
        - Each set contains: name, source_cards (with updated indices)
        - Optionally includes set-specific options
        - Adds insertion to kwd_data["card_insertions"]
        - Marks source cards/options with "mark_for_removal" = 1
    """

    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """
        Create card sets from source cards and options.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of card set definitions

        Raises:
            Exception: If more than one default target (empty target-name) is specified
        """
        card_sets = []
        has_options = False
        default_target = 0

        for card_settings in settings:
            card_set = {"name": card_settings["name"], "source_cards": []}
            target_name = card_settings.get("target-name", "")
            if target_name == "":
                default_target = default_target + 1
                if default_target > 1:
                    raise Exception("Currently only one card set on the base keyword is supported!")
            for card_index, source_index in enumerate(card_settings["source-indices"]):
                source_card = kwd_data["cards"][source_index]
                source_card["source_index"] = source_card["index"]
                source_card["index"] = card_index
                source_card["mark_for_removal"] = 1
                card_set["source_cards"].append(source_card)

            if "source-options" in card_settings:
                has_options = True
                for option_index in card_settings["source-options"]:
                    source_option = kwd_data["options"][int(option_index)]
                    option = copy.deepcopy(source_option)
                    for card in option["cards"]:
                        card_index += 1
                        card["index"] = card_index
                    if "options" not in card_set:
                        card_set["options"] = [option]
                    else:
                        card_set["options"].extend([option])
                    source_option["mark_for_removal"] = 1

            card = {
                "set": {"name": card_settings["name"]},
                "fields": [],
                "index": card_settings["target-index"],
                "target_index": card_settings["target-index"],
                "length_func": card_settings.get("length-func", ""),
                "active_func": card_settings.get("active-func", ""),
            }
            target_name = card_settings.get("target-name", "")
            target_index = card_settings["target-index"]
            insertion = gen.insertion.Insertion(target_index, target_name, card)
            kwd_data["card_insertions"].append(insertion)
            card_sets.append(card_set)
        kwd_data["card_sets"] = {"sets": card_sets, "options": has_options}

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """No post-processing required."""
        pass
