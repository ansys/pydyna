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
Replace Card Handler: Replaces entire cards with alternatives.

Uses cards from additional-cards.json or other sources to completely
replace existing cards in the keyword structure.

Uses label-based card references:
    {"ref": "target_card", "card": {"source": "...", "card-name": "..."}}

Labels must be defined in the keyword's labels section or auto-generated.
"""

from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict, List

from keyword_generation.data_model import get_card
from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class ReplaceCardSettings:
    """Configuration for replacing card fields.

    Attributes:
        ref: Label-based reference to the card (resolved via LabelRegistry)
        card: Dict with 'source' and 'card-name' for loading replacement
    """

    ref: str
    card: Dict[str, Any]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ReplaceCardSettings":
        """Create settings from dict.

        Args:
            data: Dict with 'ref' and 'card'

        Returns:
            ReplaceCardSettings instance

        Raises:
            KeyError: If 'ref' or 'card' is missing
        """
        return cls(ref=data["ref"], card=data["card"])

    def resolve_index(self, registry: LabelRegistry, cards: List[Any]) -> int:
        """Resolve the label reference to a concrete card index."""
        return registry.resolve_index(self.ref, cards)


@handler(
    name="replace-card",
    description="Replaces entire cards with alternative definitions from additional-cards.json",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Card label reference"},
                "card": {"type": "object"},
            },
            "required": ["ref", "card"],
        },
    },
    output_description="Replaces cards at specified label references with loaded card definitions",
)
class ReplaceCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Replaces complete card definitions.

    Loads card from additional-cards.json and replaces the card at the
    specified index. Useful for substituting standardized card definitions.

    Input Settings Example:
        [
            {
                "ref": "target_card",
                "card": {
                    "source": "additional-cards",
                    "card-name": "BLANK"
                }
            }
        ]

    Output Modification:
        Replaces kwd_data["cards"][index] with loaded card definition

    Requires:
        - LabelRegistry must be available on kwd_data.label_registry
        - Labels must be defined in the manifest 'labels' section
    """

    @classmethod
    def _parse_settings(cls, settings: typing.List[typing.Dict[str, typing.Any]]) -> typing.List[ReplaceCardSettings]:
        """Convert dict settings to typed ReplaceCardSettings instances."""
        return [ReplaceCardSettings.from_dict(s) for s in settings]

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Replace cards with new card definitions.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts with 'ref' and 'card'

        Raises:
            ValueError: If label_registry is not available on kwd_data
            UndefinedLabelError: If a referenced label is not defined
        """
        typed_settings = self._parse_settings(settings)
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "ReplaceCardHandler requires LabelRegistry on kwd_data.label_registry. "
                "Ensure labels are defined in the manifest."
            )

        for card_settings in typed_settings:
            index = card_settings.resolve_index(registry, kwd_data.cards)
            replacement = get_card(card_settings.card)
            replacement["index"] = index
            logger.debug(f"Replacing card {index} (ref='{card_settings.ref}') with {card_settings.card}")
            kwd_data.cards[index] = replacement
            # Update the label registry to point to the new card object
            # This is necessary because we're replacing the card instance, not mutating it
            registry.update_reference(card_settings.ref, replacement)
