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

Uses label-based card references:
    {"ref": "main_card", "name": "field_name", "type": "int", ...}

Labels must be defined in the keyword's labels section or auto-generated.
"""

from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict

from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.base_settings import LabelRefSettings, parse_settings_list
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class OverrideFieldSettings(LabelRefSettings):
    """Configuration for field property overrides.

    Attributes:
        ref: Label-based reference to the card (resolved via LabelRegistry)
        field_name: Name of the field to modify (case-insensitive match)
        properties: Dict of properties to override
    """

    field_name: str
    properties: Dict[str, Any]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "OverrideFieldSettings":
        """Create settings from dict.

        Args:
            data: Dict with 'ref', 'name', and any override properties

        Returns:
            OverrideFieldSettings instance

        Raises:
            KeyError: If 'ref' or 'name' is missing
        """
        # Extract properties (everything except 'ref' and 'name')
        properties = {k: v for k, v in data.items() if k not in ("ref", "name")}
        return cls(
            ref=data["ref"],
            field_name=data["name"],
            properties=properties,
        )


@handler(
    name="override-field",
    description="Modifies field properties (type, default, readonly, position, width, options, name)",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Card label reference"},
                "name": {"type": "string", "description": "Field name to modify (case-insensitive)"},
                "readonly": {"type": "boolean"},
                "type": {"type": "string"},
                "position": {"type": "integer"},
                "width": {"type": "integer"},
                "default": {},
                "options": {"type": "array"},
                "new-name": {"type": "string"},
            },
            "required": ["ref", "name"],
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
                "ref": "header_card",
                "name": "SECID",
                "type": "int",
                "readonly": true,
                "default": 0,
                "new-name": "section_id"
            }
        ]

    Output Modification:
        Modifies field dict properties for matching field in specified card

    Requires:
        - LabelRegistry must be available on kwd_data.label_registry
        - Labels must be defined in the manifest 'labels' section
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Override field properties in cards.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts with 'ref', 'name', and override properties

        Raises:
            ValueError: If label_registry is not available on kwd_data
            UndefinedLabelError: If a referenced label is not defined
        """
        typed_settings = parse_settings_list(OverrideFieldSettings, settings)
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "OverrideFieldHandler requires LabelRegistry on kwd_data.label_registry. "
                "Ensure labels are defined in the manifest."
            )

        for card_settings in typed_settings:
            index = card_settings.resolve_index(registry, kwd_data.cards)
            card = kwd_data.cards[index]
            field_name_lower = card_settings.field_name.lower()

            logger.debug(
                f"Overriding field '{card_settings.field_name}' in card {index} "
                f"(ref='{card_settings.ref}'): {card_settings.properties}"
            )

            for field in card["fields"]:
                if field["name"].lower() == field_name_lower:
                    props = card_settings.properties
                    if "readonly" in props:
                        field["readonly"] = props["readonly"]
                    if "type" in props:
                        field["type"] = props["type"]
                    if "position" in props:
                        field["position"] = props["position"]
                    if "width" in props:
                        field["width"] = props["width"]
                    if "default" in props:
                        field["default"] = props["default"]
                    if "options" in props:
                        field["options"] = props["options"]
                    if "new-name" in props:
                        field["name"] = props["new-name"]
