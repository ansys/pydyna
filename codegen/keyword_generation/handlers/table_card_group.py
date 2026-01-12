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
Table Card Group Handler: Creates table card groups for table-like data.

This handler groups multiple cards together to form a repeating structure,
commonly used for table or matrix data where multiple related cards repeat as a unit.
"""

from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict, List, Optional

import keyword_generation.data_model as gen
from keyword_generation.data_model.keyword_data import Card, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
from keyword_generation.handlers.base_settings import parse_settings_list
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class TableCardGroupSettings:
    """Configuration for grouping multiple cards into a table.

    Attributes:
        refs: List of label references for cards to group
        property_name: Name of the table card group property
        length_func: Optional function to compute group count
        active_func: Optional function to determine if group is active
    """

    refs: List[str]
    property_name: str
    length_func: Optional[str] = None
    active_func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "TableCardGroupSettings":
        return cls(
            refs=data["refs"],
            property_name=data["overall-name"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
        )

    def resolve_indices(self, registry: LabelRegistry, cards: List[Any]) -> List[int]:
        """Resolve label refs to card indices."""
        return [registry.resolve_index(ref, cards) for ref in self.refs]


@handler(
    name="table-card-group",
    description="Creates table card groups for table-like repeating card structures",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "refs": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "Label references for cards to group together",
                },
                "overall-name": {"type": "string", "description": "Name of the table card group"},
                "length-func": {"type": "string", "description": "Function to compute group count"},
                "active-func": {"type": "string", "description": "Function to determine if group is active"},
            },
            "required": ["refs", "overall-name"],
        },
    },
    output_description=(
        "Sets kwd_data['table_group']=True, adds card insertion with table card group, marks source cards for removal"
    ),
)
class TableCardGroupHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Groups cards together to form repeating table-like structures.

    This handler creates table card groups where multiple cards are treated
    as a single repeatable unit. Common use cases include table data where each
    row consists of multiple cards that must be repeated together.

    CRITICAL: Uses reference semantics (not deep copies). Cards are appended
    to sub_cards by reference so that later handler modifications (e.g.,
    conditional-card setting 'func') appear in the group.

    Input Settings Example:
        [
            {
                "refs": ["row_card_1", "row_card_2", "row_card_3"],
                "overall-name": "table_row",
                "length-func": "self.nrows",
                "active-func": "self.has_table"
            }
        ]

    Output Modification:
        - Sets kwd_data["table_group"] = True
        - Creates table card group structure:
          {
              "table_group": True,
              "sub_cards": [...],  # Cards from refs
              "overall_name": "table_row",
              "length_func": "self.nrows",
              "active_func": "self.has_table"
          }
        - Inserts group at minimum index position
        - Marks all source cards with "mark_for_removal" = 1
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Create table card groups from card label refs.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of card group definitions

        Raises:
            ValueError: If label_registry is not initialized
        """
        if kwd_data.label_registry is None:
            raise ValueError("table-card-group handler requires label_registry to be initialized")

        registry = kwd_data.label_registry
        typed_settings = parse_settings_list(TableCardGroupSettings, settings)
        kwd_data.table_group = True

        for card_settings in typed_settings:
            # Resolve refs to indices
            indices = card_settings.resolve_indices(registry, kwd_data.cards)
            logger.debug(
                f"table-card-group '{card_settings.property_name}': refs {card_settings.refs} -> indices {indices}"
            )

            # Skip empty refs
            if not indices:
                logger.debug(f"table-card-group '{card_settings.property_name}': skipping empty refs")
                continue

            # Collect sub_cards using reference semantics
            sub_cards: List[Card] = []
            for index in indices:
                sub_card = kwd_data.cards[index]
                sub_card.mark_for_removal = 1
                sub_cards.append(sub_card)

            # Build the card group as a Card instance
            group = Card(
                index=-1,  # Will be set during insertion
                fields=[],  # No direct fields, sub_cards hold the fields
                table_group=True,
                sub_cards=sub_cards,
                overall_name=card_settings.property_name,
                length_func=card_settings.length_func or "",
                active_func=card_settings.active_func or "",
            )

            # Mark all source cards for removal and insert group at minimum position
            insertion = gen.Insertion(min(indices), "", group)
            kwd_data.card_insertions.append(insertion)
            logger.debug(f"Created table card group at position {min(indices)}")
