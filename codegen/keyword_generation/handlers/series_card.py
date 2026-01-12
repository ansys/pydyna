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
Series Card Handler: Creates variable-length card arrays in keywords.

This handler enables keywords to contain arrays of cards with dynamic sizing,
supporting repetitive data structures like multiple loads, materials, or entities.

Uses label-based card references:
    {"ref": "data_card", "name": "data_array", "card-size": 1, ...}

Labels must be defined in the keyword's labels section or auto-generated.
"""

from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict, Optional

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.metadata import DataclassDefinition, DataclassField, VariableCardMetadata
from keyword_generation.handlers.base_settings import LabelRefSettings, parse_settings_list
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class SeriesCardSettings(LabelRefSettings):
    """Configuration for variable-length card arrays.

    Attributes:
        ref: Label-based reference (resolved via LabelRegistry)
        name: Property name for the array
        card_size: Number of cards per element
        element_width: Width of each element in columns
        type: Element type (primitive or 'struct')
        help: Help text for the array
        length_func: Optional Python expression for array length
        active_func: Optional Python expression for conditional activation
        struct_info: Optional dataclass definition for struct types
    """

    name: str
    card_size: int
    element_width: int
    type: str
    help: str
    length_func: Optional[str] = None
    active_func: Optional[str] = None
    struct_info: Optional[Dict[str, Any]] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SeriesCardSettings":
        """Create settings from dict.

        Args:
            data: Dict with 'ref', 'name', 'card-size', 'element-width', 'type', 'help', etc.

        Returns:
            SeriesCardSettings instance

        Raises:
            KeyError: If required keys are missing
        """
        return cls(
            ref=data["ref"],
            name=data["name"],
            card_size=data["card-size"],
            element_width=data["element-width"],
            type=data["type"],
            help=data["help"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
            struct_info=data.get("struct-info"),
        )


@handler(
    name="series-card",
    description="Creates variable-length card arrays for repetitive data structures",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Card label reference"},
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
            "required": ["ref", "name", "card-size", "element-width", "type", "help"],
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
                "ref": "loads_card",
                "name": "loads",
                "card-size": 1,
                "element-width": 8,
                "type": "float",
                "help": "Load values",
                "length-func": "len(self.loads)",
                "active-func": "self.nloads > 0"
            },
            {
                "ref": "properties_card",
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
        Convert specified cards into variable-length series.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts with 'ref', 'name', 'card-size', etc.

        Raises:
            ValueError: If label_registry is not available on kwd_data
            UndefinedLabelError: If a referenced label is not defined
        """
        typed_settings = parse_settings_list(SeriesCardSettings, settings)
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "SeriesCardHandler requires LabelRegistry on kwd_data.label_registry. "
                "Ensure labels are defined in the manifest."
            )

        kwd_data.variable = True
        dataclasses = []
        for card_settings in typed_settings:
            card_index = card_settings.resolve_index(registry, kwd_data.cards)
            logger.debug(f"Creating series '{card_settings.name}' at index {card_index} (ref='{card_settings.ref}')")
            type_name = card_settings.type
            variable_card = kwd_data.cards[card_index]
            if type_name == "struct":
                struct_info = card_settings.struct_info
                struct_name = struct_info["name"]
                dataclass = DataclassDefinition(
                    name=struct_name, fields=[DataclassField.from_dict(f) for f in struct_info["fields"]]
                )
                dataclasses.append(dataclass)
                type_name = f"self.{struct_name}"

            # use abbreviations for some fields to make the jinja template more concise
            variable_card["variable"] = VariableCardMetadata(
                name=card_settings.name,
                size=card_settings.card_size,
                width=card_settings.element_width,
                type=type_name,
                help=card_settings.help,
                length_func=card_settings.length_func or "",
                active_func=card_settings.active_func or "",
            )
        if len(dataclasses) > 0:
            kwd_data.dataclasses = dataclasses
