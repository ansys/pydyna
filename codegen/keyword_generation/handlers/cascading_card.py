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
Cascading Card Handler: Adds cascading activation logic to optional cards.

This handler enables cards to be automatically activated when any later card
in a cascade chain has values explicitly set. This is useful for keywords like
*CONTROL_SHELL where cards 2-5 are optional but implicitly required if a
subsequent card has values set.

The cascading logic uses chained active_func expressions where each card
checks if it has values set OR if the next card is active. This creates
a cascade effect where setting a value on card N activates cards 1 through N.

Example manifest configuration:
    "cascading-card": [
        {
            "start": 1,
            "end": 4
        }
    ]

This generates active_func lambdas like:
- Card 4: self._cards[4].has_nondefault_values()
- Card 3: self._cards[3].has_nondefault_values() or self._cards[4].active
- Card 2: self._cards[2].has_nondefault_values() or self._cards[3].active
- Card 1: self._cards[1].has_nondefault_values() or self._cards[2].active
"""

from dataclasses import dataclass
import logging
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.handlers.base_settings import parse_settings_list
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class CascadingCardSettings:
    """Configuration for cascading card activation.

    Attributes:
        start: Index of the first card in the cascade (inclusive)
        end: Index of the last card in the cascade (inclusive)
    """

    start: int
    end: int

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "CascadingCardSettings":
        """Create settings from dict.

        Args:
            data: Dict with 'start' and 'end' keys

        Returns:
            CascadingCardSettings instance

        Raises:
            KeyError: If 'start' or 'end' is missing
        """
        return cls(start=data["start"], end=data["end"])


@handler(
    name="cascading-card",
    description="Adds cascading activation logic to optional cards based on explicit value setting",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "start": {"type": "integer", "description": "Index of first card in cascade (inclusive)"},
                "end": {"type": "integer", "description": "Index of last card in cascade (inclusive)"},
            },
            "required": ["start", "end"],
        },
    },
    output_description="Adds 'func' property to cards in the cascade range with chained activation logic",
)
class CascadingCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Makes cards conditionally active based on cascading value detection.

    This handler adds 'func' properties to cards that create a cascade effect:
    each card becomes active if it has values set OR if any subsequent card
    in the chain is active.

    The cascade is built from the end to the start:
    - Last card: active if self._cards[N].has_nondefault_values()
    - Previous cards: active if self._cards[i].has_nondefault_values() or self._cards[i+1].active

    Input Settings Example:
        [{"start": 1, "end": 4}]

    Output: Cards 1-4 get 'func' properties with chained lambda expressions.
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: List[Dict[str, Any]],
    ) -> None:
        """
        Add cascading activation logic to specified card ranges.

        Args:
            kwd_data: KeywordData instance containing cards
            settings: List of dicts with 'start' and 'end' keys

        Raises:
            ValueError: If start/end indices are out of range or invalid
        """
        typed_settings = parse_settings_list(CascadingCardSettings, settings)

        for setting in typed_settings:
            start = setting.start
            end = setting.end
            num_cards = len(kwd_data.cards)

            # Validate indices
            if start < 0 or start >= num_cards:
                raise ValueError(f"Cascading card start index {start} out of range (0-{num_cards - 1})")
            if end < 0 or end >= num_cards:
                raise ValueError(f"Cascading card end index {end} out of range (0-{num_cards - 1})")
            if start > end:
                raise ValueError(f"Cascading card start ({start}) must be <= end ({end})")

            logger.debug(f"Adding cascading activation for cards {start} to {end}")

            # Build cascade from end to start
            # Last card: just checks itself
            last_card = kwd_data.cards[end]
            last_card["func"] = f"self._cards[{end}].has_nondefault_values()"
            logger.debug(f"Card {end}: func = {last_card['func']}")

            # Previous cards: check self OR next card active
            for i in range(end - 1, start - 1, -1):
                card = kwd_data.cards[i]
                card["func"] = f"self._cards[{i}].has_nondefault_values() or self._cards[{i + 1}].active"
                logger.debug(f"Card {i}: func = {card['func']}")
