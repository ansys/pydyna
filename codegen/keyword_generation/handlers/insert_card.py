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
Insert Card Handler: Inserts new cards using label-based positioning.

Cards are inserted relative to labeled cards, eliminating fragile index-based
positioning. Use `after` or `before` to specify the insertion point by label reference.

Positioning options (mutually exclusive - use exactly one):
- `after`: Insert after the referenced card
  - "END": Insert at the end of the cards list
  - Any other string: Label of the card to insert after
- `before`: Insert before the referenced card
  - Any string: Label of the card to insert before

Each inserted card MUST have a `label` so subsequent handlers and insertions
can reference it by name.

Cards are processed in manifest order. When multiple cards need to be inserted
in sequence, earlier insertions' labels become available for later insertions.
"""

from dataclasses import dataclass
import logging
from typing import Any, Dict, List

from keyword_generation.data_model import get_card
from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


class InsertCardError(Exception):
    """Raised when insert-card encounters an error."""

    pass


@dataclass
class InsertCardSettings:
    """Configuration for inserting additional cards.

    Attributes:
        after: Label of card to insert after, or "END" for end boundary (mutually exclusive with before)
        before: Label of card to insert before (mutually exclusive with after)
        card: Dict with 'source' and 'card-name' for loading the card
        label: Label to register for the inserted card (required)
    """

    card: Dict[str, Any]
    label: str
    after: str | None = None
    before: str | None = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "InsertCardSettings":
        has_after = "after" in data
        has_before = "before" in data
        if not has_after and not has_before:
            raise ValueError("insert-card requires 'after' or 'before' field")
        if has_after and has_before:
            raise ValueError("insert-card cannot have both 'after' and 'before' fields")
        if "label" not in data:
            raise ValueError("insert-card requires 'label' field for the inserted card")
        return cls(
            after=data.get("after"),
            before=data.get("before"),
            card=data["card"],
            label=data["label"],
        )


@handler(
    name="insert-card",
    description="Inserts new cards using label-based positioning",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "after": {
                    "type": "string",
                    "description": "Label of card to insert after, or 'END' (mutually exclusive with 'before')",
                },
                "before": {
                    "type": "string",
                    "description": "Label of card to insert before (mutually exclusive with 'after')",
                },
                "card": {"type": "object", "description": "Card definition to load"},
                "label": {"type": "string", "description": "Label for the inserted card (required)"},
            },
            "required": ["card", "label"],
            "oneOf": [
                {"required": ["after"]},
                {"required": ["before"]},
            ],
        },
    },
    output_description="Inserts Card objects into kwd_data.cards, registers labels in registry",
)
class InsertCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Inserts new cards using label-based positioning.

    Cards are inserted relative to labeled reference points, eliminating
    fragile index-based positioning. Use one of:
    - `after`: Insert after the referenced card (or "END" for end of list)
    - `before`: Insert before the referenced card

    Cards are processed in manifest order, so earlier insertions can be
    referenced by later ones.

    Input Settings Example:
        [
            {
                "before": "first_card",
                "card": {
                    "source": "additional-cards",
                    "card-name": "MY_CARD_1"
                },
                "label": "my_inserted_card"
            },
            {
                "after": "my_inserted_card",
                "card": {
                    "source": "additional-cards",
                    "card-name": "MY_CARD_2"
                },
                "label": "my_second_card"
            }
        ]

    Output Modification:
        - Inserts Card objects into kwd_data.cards list
        - Registers labels in kwd_data.label_registry for inserted cards
    """

    @classmethod
    def _parse_settings(cls, settings: List[Dict[str, Any]]) -> List[InsertCardSettings]:
        """Convert dict settings to typed InsertCardSettings instances."""
        return [InsertCardSettings.from_dict(s) for s in settings]

    def handle(
        self,
        kwd_data: KeywordData,
        settings: List[Dict[str, Any]],
    ) -> None:
        """
        Insert cards using label-based positioning.

        Args:
            kwd_data: KeywordData instance with cards list and label_registry
            settings: List of {"after", "card", "label"} dicts

        Raises:
            InsertCardError: If label_registry is not initialized or label not found
        """
        if kwd_data.label_registry is None:
            raise InsertCardError("LabelRegistry must be initialized before insert-card handler runs")

        registry = kwd_data.label_registry
        typed_settings = self._parse_settings(settings)

        # Process in manifest order - each insertion can reference previous ones
        for card_settings in typed_settings:
            # Resolve where to insert
            if card_settings.after is not None:
                # Insert after the referenced card
                if card_settings.after == "END":
                    insert_pos = len(kwd_data.cards)
                else:
                    ref_index = registry.resolve_index(card_settings.after, kwd_data.cards)
                    insert_pos = ref_index + 1
                position_desc = f"after '{card_settings.after}'"
            else:
                # Insert before the referenced card
                ref_index = registry.resolve_index(card_settings.before, kwd_data.cards)
                insert_pos = ref_index
                position_desc = f"before '{card_settings.before}'"

            # Load and insert the card
            card = get_card(card_settings.card)
            kwd_data.cards.insert(insert_pos, card)
            logger.debug(
                f"Inserted card '{card_settings.label}' {position_desc} "
                f"at position {insert_pos}, total cards now: {len(kwd_data.cards)}"
            )

            # Register the label for this card (pointing to the object, not index)
            registry.register(card_settings.label, card)

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
