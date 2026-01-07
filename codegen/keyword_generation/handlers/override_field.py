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
Override Field Handler: Modifies field properties.

Allows fine-grained control over field attributes like type, defaults,
readonly status, position, width, and valid options.
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict

from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class OverrideFieldSettings:
    """Configuration for field property overrides."""

    card_index: int
    field_index: int
    properties: Dict[str, Any]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "OverrideFieldSettings":
        return cls(
            card_index=data["card-index"],
            field_index=data["field-index"],
            properties=data["properties"],
        )


@handler(
    name="override-field",
    dependencies=["reorder-card"],
    description="Modifies field properties (type, default, readonly, position, width, options, name)",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer"},
                "name": {"type": "string"},
                "readonly": {"type": "boolean"},
                "type": {"type": "string"},
                "position": {"type": "integer"},
                "width": {"type": "integer"},
                "default": {},
                "options": {"type": "array"},
                "new-name": {"type": "string"},
            },
            "required": ["index", "name"],
        },
    },
    output_description="Modifies specified field properties in place",
)
class OverrideFieldHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Overrides field properties.

    Allows modifying any aspect of a field's definition including type,
    position, width, default value, readonly status, valid options, and name.

    Input Settings Example:
        [
            {
                "index": 1,
                "name": "SECID",
                "type": "int",
                "readonly": true,
                "default": 0,
                "new-name": "section_id"
            }
        ]

    Output Modification:
        Modifies field dict properties for matching field in specified card
    """

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[typing.Dict[str, typing.Any]]:
        """Keep dict settings for override-field due to dynamic property access."""
        return settings

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Override field properties in cards.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of field override definitions
        """
        typed_settings = self._parse_settings(settings)
        for setting in typed_settings:
            index = setting["index"]
            name = setting["name"]
            card = kwd_data.cards[index]
            for field in card["fields"]:
                if field["name"].lower() == name:
                    if "readonly" in setting:
                        field["readonly"] = setting["readonly"]
                    if "type" in setting:
                        field["type"] = setting["type"]
                    if "position" in setting:
                        field["position"] = setting["position"]
                    if "width" in setting:
                        field["width"] = setting["width"]
                    if "default" in setting:
                        field["default"] = setting["default"]
                    if "options" in setting:
                        field["options"] = setting["options"]
                    if "new-name" in setting:
                        field["name"] = setting["new-name"]

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
