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
Table Card Handler: Marks cards that represent 2D tabular data.

Indicates that a card repeats to form a table structure (e.g., integration points).
The card repeats based on a length function.

Uses label-based card references:
    {"ref": "integration_card", "property-name": "integration_points", ...}

Labels must be defined in the keyword's labels section or auto-generated.
"""

from dataclasses import dataclass
import logging
from typing import Any, Dict, List, Optional

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.metadata import TableCardMetadata
from keyword_generation.handlers.base_settings import LabelRefSettings, parse_settings_list
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class TableCardSettings(LabelRefSettings):
    """Configuration for marking a card as a repeating table structure.

    Attributes
    ----------
        ref: Label-based reference (resolved via LabelRegistry)
        property_name: Name for the table property in the generated class
        length_func: Optional Python expression for dynamic row count
        active_func: Optional Python expression for conditional activation
    """

    property_name: str
    length_func: Optional[str] = None
    active_func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "TableCardSettings":
        """Create settings from dict.

        Args:
            data: Dict with 'ref', 'property-name', optional 'length-func', 'active-func'

        Returns
        -------
            TableCardSettings instance

        Raises
        ------
            KeyError: If 'ref' or 'property-name' is missing
        """
        return cls(
            ref=data["ref"],
            property_name=data["property-name"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
        )


@handler(
    name="table-card",
    description="Marks cards as repeating table structures with dynamic row count",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Card label reference"},
                "property-name": {"type": "string"},
                "length-func": {"type": "string"},
                "active-func": {"type": "string"},
            },
            "required": ["ref", "property-name"],
        },
    },
    output_description="Sets kwd_data['table']=True and adds 'table' dict to card",
)
class TableCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """Marks cards as repeating table structures with dynamic row count.

    Input Settings Example:
        [{"ref": "integration_card", "property-name": "integration_points",
          "length-func": "self.nip", "active-func": "self.elform in [101,102]"}]

    Output Modification:
        Sets kwd_data.table = True
        Adds TableCardMetadata to card's 'table' attribute

    Requires:
        - LabelRegistry must be available on kwd_data.label_registry
        - Labels must be defined in the manifest 'labels' section
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: List[Dict[str, Any]],
    ) -> None:
        """
        Mark cards as table structures.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts with 'ref', 'property-name', etc.

        Raises
        ------
            ValueError: If label_registry is not available on kwd_data
            UndefinedLabelError: If a referenced label is not defined
        """
        typed_settings = parse_settings_list(TableCardSettings, settings)
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "TableCardHandler requires LabelRegistry on kwd_data.label_registry. "
                "Ensure labels are defined in the manifest."
            )

        kwd_data.table = True
        for card_settings in typed_settings:
            index = card_settings.resolve_index(registry, kwd_data.cards)
            logger.debug(
                f"Marking card {index} (ref='{card_settings.ref}') as table "
                f"with property '{card_settings.property_name}'"
            )
            table_card = kwd_data.cards[index]
            table_card["table"] = TableCardMetadata(
                name=card_settings.property_name,
                length_func=card_settings.length_func or "",
                active_func=card_settings.active_func or "",
            )
