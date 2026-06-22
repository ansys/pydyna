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
Add Field Handler: Appends new fields to an existing card.

Allows injecting fields that are missing from kwd.json into a card definition
without modifying kwd.json or the generated Python files.

Uses label-based card references:
    {"ref": "elements_card", "fields": [{"name": "MWD", "type": "integer", ...}]}

Labels must be defined in the keyword's labels section or auto-generated.
"""

from dataclasses import dataclass, field
import logging
import typing
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import Field, KeywordData
from keyword_generation.handlers.base_settings import LabelRefSettings, parse_settings_list
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class AddFieldSettings(LabelRefSettings):
    """Configuration for adding new fields to a card.

    Attributes
    ----------
        ref: Label-based reference to the card (resolved via LabelRegistry)
        fields: List of field dicts to append to the card
    """

    fields: List[Dict[str, Any]] = field(default_factory=list)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "AddFieldSettings":
        """Create settings from dict.

        Args:
            data: Dict with 'ref' and 'fields'

        Returns
        -------
            AddFieldSettings instance

        Raises
        ------
            KeyError: If 'ref' or 'fields' is missing
        """
        return cls(
            ref=data["ref"],
            fields=data["fields"],
        )


@handler(
    name="add-field",
    description="Appends new fields to an existing card (for fields missing from kwd.json)",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Card label reference"},
                "fields": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "name": {"type": "string"},
                            "type": {"type": "string"},
                            "position": {"type": "integer"},
                            "width": {"type": "integer"},
                            "default": {},
                            "help": {"type": "string"},
                            "link": {"type": "integer"},
                        },
                        "required": ["name", "type", "position", "width"],
                    },
                },
            },
            "required": ["ref", "fields"],
        },
    },
    output_description="Appends new field dicts to the fields list of the specified card",
)
class AddFieldHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Appends new fields to an existing card.

    Useful for injecting fields that are absent from kwd.json into a card
    without touching kwd.json or hand-maintaining the generated Python class.

    Input Settings Example:
        [
            {
                "ref": "elements_card",
                "fields": [
                    {
                        "name": "MWD",
                        "type": "integer",
                        "position": 48,
                        "width": 8,
                        "default": null,
                        "help": "Optional flag for mass-weighted distribution."
                    }
                ]
            }
        ]

    Output Modification:
        Appends each field dict to card["fields"] for the card at the resolved index.

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
        Append new fields to cards.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts with 'ref' and 'fields'

        Raises
        ------
            ValueError: If label_registry is not available on kwd_data
            UndefinedLabelError: If a referenced label is not defined
        """
        typed_settings = parse_settings_list(AddFieldSettings, settings)
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "AddFieldHandler requires LabelRegistry on kwd_data.label_registry. "
                "Ensure labels are defined in the manifest."
            )

        for card_settings in typed_settings:
            index = card_settings.resolve_index(registry, kwd_data.cards)
            card = kwd_data.cards[index]

            existing_names = {f["name"].lower() for f in card["fields"]}
            for new_field_dict in card_settings.fields:
                if new_field_dict["name"].lower() in existing_names:
                    logger.warning(
                        f"Field '{new_field_dict['name']}' already exists in card {index} "
                        f"(ref='{card_settings.ref}'), skipping."
                    )
                    continue
                logger.debug(
                    f"Adding field '{new_field_dict['name']}' to card {index} "
                    f"(ref='{card_settings.ref}')"
                )
                card["fields"].append(Field.from_dict(new_field_dict))
                existing_names.add(new_field_dict["name"].lower())
