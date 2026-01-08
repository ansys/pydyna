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
Rename Property Handler: Changes Python property names for fields.

Allows field names (as they appear in keyword files) to map to different
Python property names in the generated code for improved API clarity.

Uses label-based references (ref) for card addressing.
"""

from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict, Optional

from keyword_generation.data_model.keyword_data import KeywordData, RenamedProperty
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.base_settings import LabelRefSettings, parse_settings_list
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class RenamePropertySettings(LabelRefSettings):
    """Configuration for renaming a card property.

    Uses label-based addressing (ref) for card references.
    """

    name: str  # Field name to rename
    property_name: str  # New Python property name
    description: Optional[str] = None  # Optional description for docs

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "RenamePropertySettings":
        return cls(
            name=data["name"],
            property_name=data["property-name"],
            ref=data["ref"],
            description=data.get("description"),
        )


@handler(
    name="rename-property",
    description="Renames Python properties for fields to improve API clarity",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Label reference for the card"},
                "name": {"type": "string", "description": "Field name to rename"},
                "property-name": {"type": "string", "description": "New Python property name"},
                "description": {"type": "string", "description": "Optional description for docs"},
            },
            "required": ["ref", "name", "property-name"],
        },
    },
    output_description="Sets 'property_name' on matching field dicts",
)
class RenamePropertyHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Changes Python property names for fields.

    The field name (from keyword file) remains unchanged, but the Python
    property accessor uses the custom name. Useful for making APIs more
    Pythonic (e.g., PID -> part_id).

    Uses label-based references (ref) for card addressing.

    Input Settings Example:
        [
            {
                "ref": "header_card",
                "name": "PID",
                "property-name": "part_id"
            }
        ]

    Output Modification:
        Sets field["property_name"] = "part_id" for matching field
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Rename Python properties for specified fields.

        Args:
            kwd_data: Complete keyword data
            settings: List of dicts with ref, name, property-name
        """
        # Parse settings to typed dataclasses
        typed_settings = parse_settings_list(RenamePropertySettings, settings)

        # Get registry for label resolution
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "rename-property handler requires a label registry. Ensure labels are defined in manifest."
            )

        for setting in typed_settings:
            index = setting.resolve_index(registry, kwd_data.cards)
            name = setting.name
            property_name = setting.property_name
            logger.debug(f"Renaming field '{name}' on card {index} to property '{property_name}'")

            card = kwd_data.cards[index]
            for field in card["fields"]:
                if field["name"].lower() == name.lower():
                    field["property_name"] = property_name
                    # Track this rename for class docstring and template lookups
                    kwd_data.renamed_properties.append(
                        RenamedProperty(
                            field_name=name.upper(),
                            property_name=property_name,
                            card_index=index,
                            description=setting.description or "",
                        )
                    )

    def post_process(self, kwd_data: KeywordData) -> None:
        """Detect field name collisions and add notes to property_collisions map."""
        if not kwd_data.renamed_properties:
            return

        # Build a map of original field names to their renamed property info
        renamed_map = {rp.field_name.lower(): rp for rp in kwd_data.renamed_properties}

        # Find fields that have the same name as a renamed field but are on different cards
        for card in kwd_data.cards:
            for field in card["fields"]:
                field_name_lower = field["name"].lower()
                # Skip if this field itself was renamed (has a different property_name)
                property_name = field.get("property_name") or field["name"].lower()
                if property_name != field_name_lower:
                    continue
                # Check if this field's name collides with a renamed property
                if field_name_lower in renamed_map:
                    rp = renamed_map[field_name_lower]
                    # Add a note pointing to the renamed property
                    # Use description if available, otherwise just field name
                    if rp.description:
                        collision_note = (
                            f"For the {rp.description} {rp.field_name} (Card {rp.card_index + 1}), "
                            f"use ``{rp.property_name}``."
                        )
                    else:
                        collision_note = (
                            f"For the {rp.field_name} (Card {rp.card_index + 1}), " f"use ``{rp.property_name}``."
                        )
                    # Store in property_collisions map keyed by the property name
                    kwd_data.property_collisions[property_name] = collision_note
