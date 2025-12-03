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
Override Subkeyword Handler: Changes Python property names for fields.

NOTE: This handler appears to be functionally identical to RenamePropertyHandler.
See codegen/todo.md for discussion of potential consolidation.
"""

import typing

import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="override-subkeyword",
    dependencies=[],
    description="Renames Python properties for fields (appears duplicate of rename-property)",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer", "description": "Card index"},
                "name": {"type": "string", "description": "Field name to rename"},
                "property-name": {"type": "string", "description": "New Python property name"},
            },
            "required": ["index", "name", "property-name"],
        },
    },
    output_description="Sets 'property_name' on matching field dicts",
)
class OverrideSubkeywordHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Changes Python property names for fields.

    WARNING: This handler appears functionally identical to RenamePropertyHandler.
    Consider consolidating these handlers (see todo.md).

    Input Settings Example:
        [
            {
                "index": 0,
                "name": "PID",
                "property-name": "part_id"
            }
        ]

    Output Modification:
        Sets field["property_name"] = "part_id" for matching field
    """

    def handle(self, kwd_data: typing.Any, settings: typing.Dict[str, typing.Any]) -> None:
        """
        Rename Python properties for specified fields.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of {"index", "name", "property-name"} dicts
        """
        settings_list = typing.cast(typing.List[typing.Dict[str, typing.Any]], settings)
        for setting in settings_list:
            index = setting["index"]
            name = setting["name"]
            property_name = setting["property-name"]
            card = kwd_data.cards[index]
            for field in card["fields"]:
                if field["name"].lower() == name:
                    field["property_name"] = property_name

    def post_process(self, kwd_data: typing.Any) -> None:
        """No post-processing required."""
        pass
