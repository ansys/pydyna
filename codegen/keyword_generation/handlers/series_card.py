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
Series Card Handler: Creates variable-length card arrays in keywords.

This handler enables keywords to contain arrays of cards with dynamic sizing,
supporting repetitive data structures like multiple loads, materials, or entities.
"""

import typing

import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="series-card",
    dependencies=["reorder-card"],
    description="Creates variable-length card arrays for repetitive data structures",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer", "description": "Card index to make variable"},
                "name": {"type": "string", "description": "Variable array name"},
                "card-size": {"type": "integer", "description": "Number of cards per element"},
                "element-width": {"type": "integer", "description": "Width of each element"},
                "type": {"type": "string", "description": "Element type (primitive or 'struct')"},
                "help": {"type": "string", "description": "Help text for the array"},
                "length-func": {"type": "string", "description": "Function to compute array length"},
                "active-func": {"type": "string", "description": "Function to determine if card is active"},
                "struct-info": {
                    "type": "object",
                    "description": "Dataclass definition for struct types",
                },
            },
            "required": ["index", "name", "card-size", "element-width", "type", "help"],
        },
    },
    output_description="Sets kwd_data['variable']=True, adds 'variable' dict to cards, may add 'dataclasses' list",
)
class SeriesCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Transforms cards into variable-length arrays.

    This handler enables keywords to contain dynamically-sized arrays of cards.
    Each series can be a primitive type array or a structured dataclass array.
    Multiple series can be defined in a single keyword.

    Input Settings Example:
        [
            {
                "index": 1,
                "name": "loads",
                "card-size": 1,
                "element-width": 8,
                "type": "float",
                "help": "Load values",
                "length-func": "len(self.loads)",
                "active-func": "self.nloads > 0"
            },
            {
                "index": 2,
                "name": "properties",
                "card-size": 2,
                "element-width": 8,
                "type": "struct",
                "help": "Property data",
                "struct-info": {
                    "name": "PropertyData",
                    "fields": [{"name": "value", "type": "float"}, ...]
                }
            }
        ]

    Output Modification:
        - Sets kwd_data["variable"] = True
        - Adds card["variable"] dict with: name, size, width, type, help, length_func, active_func
        - If struct types used, adds kwd_data["dataclasses"] list with struct definitions
    """

    def handle(self, kwd_data: typing.Any, settings: typing.Any) -> None:
        """
        Convert specified cards into variable-length series.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of series card configurations
        """
        kwd_data.variable = True
        dataclasses = []
        settings_list = typing.cast(typing.List[typing.Dict[str, typing.Any]], settings)
        for card_settings in settings_list:
            card_index = card_settings["index"]
            type_name = card_settings["type"]
            variable_card = kwd_data.cards[card_index]
            if type_name == "struct":
                struct_info = card_settings["struct-info"]
                struct_name = struct_info["name"]
                dataclass = {"name": struct_name, "fields": struct_info["fields"]}
                dataclasses.append(dataclass)
                type_name = f"self.{struct_name}"

            # use abbreviations for some fields to make the jinja template more concise
            variable_card["variable"] = {
                "name": card_settings["name"],
                "size": card_settings["card-size"],
                "width": card_settings["element-width"],
                "length_func": card_settings.get("length-func", ""),
                "active_func": card_settings.get("active-func", ""),
                "type": type_name,
                "help": card_settings["help"],
            }
        if len(dataclasses) > 0:
            kwd_data.dataclasses = dataclasses

    def post_process(self, kwd_data: typing.Any) -> None:
        """No post-processing required."""
        return
