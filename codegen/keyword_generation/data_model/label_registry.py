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
Label Registry for position-independent card referencing.

This module provides a stable, named reference system for cards in the codegen pipeline.
Labels map to card OBJECTS, not indices. When you need an index, the registry finds
the card's current position in the cards list dynamically.

This design means:
- Insertions, deletions, and reorderings don't require updating the registry
- Labels are stable references that survive any list manipulation
- Index resolution is always accurate because it's computed on-demand

Example:
    registry.register("stress_card", card_obj)
    # Later, even after insertions...
    index = registry.resolve_index("stress_card", cards_list)  # Always correct
"""

from dataclasses import dataclass, field
import logging
import re
from typing import Any, Dict, List, Optional

logger = logging.getLogger(__name__)


class LabelError(Exception):
    """Base exception for label registry errors."""

    pass


class DuplicateLabelError(LabelError):
    """Raised when attempting to register a label that already exists."""

    pass


class UndefinedLabelError(LabelError):
    """Raised when resolving a label that doesn't exist."""

    pass


class CardNotFoundError(LabelError):
    """Raised when a labeled card is not found in the cards list."""

    pass


@dataclass
class LabelRegistry:
    """
    Manages named references to card objects.

    Labels map directly to card objects (by identity), not indices.
    Index resolution happens dynamically by searching the cards list.

    This design eliminates all index bookkeeping - insertions, deletions,
    and reorderings are automatically handled because we find the card's
    current position when needed.
    """

    _labels: Dict[str, Any] = field(default_factory=dict)  # label -> card object
    _keyword: str = ""

    def register(self, label: str, card: Any) -> None:
        """
        Register a label pointing to a card object.

        Args:
            label: Human-readable label (e.g., "stress_card", "hisv_card")
            card: The card object to label

        Raises
        ------
            DuplicateLabelError: If label already registered
            ValueError: If label format is invalid
        """
        self._validate_label_format(label)

        if label in self._labels:
            raise DuplicateLabelError(f"Label '{label}' already registered for {self._keyword}")

        self._labels[label] = card
        logger.debug(f"Registered label '{label}' -> card object")

    def resolve(self, label: str) -> Any:
        """
        Resolve a label to its card object.

        Args:
            label: The label to resolve

        Returns
        -------
            The card object

        Raises
        ------
            UndefinedLabelError: If label not found
        """
        if label not in self._labels:
            available = ", ".join(sorted(self._labels.keys())[:10])
            raise UndefinedLabelError(
                f"Undefined label '{label}' for keyword '{self._keyword}'. "
                f"Available labels: {available}{'...' if len(self._labels) > 10 else ''}"
            )
        return self._labels[label]

    def resolve_index(self, label: str, cards: List[Any]) -> int:
        """
        Resolve a label to the card's current index in the cards list.

        This is the primary resolution method for handlers. The index is
        computed dynamically by finding the card object in the list.

        Args:
            label: The label to resolve
            cards: The current cards list to search

        Returns
        -------
            Integer index of the card in the list

        Raises
        ------
            UndefinedLabelError: If label not found
            CardNotFoundError: If labeled card not in the cards list
        """
        card = self.resolve(label)
        try:
            return cards.index(card)
        except ValueError:
            raise CardNotFoundError(
                f"Card labeled '{label}' not found in cards list for '{self._keyword}'. The card may have been removed."
            )

    def update_reference(self, label: str, card: Any) -> None:
        """
        Update an existing label to point to a new card object.

        Use this when a card is replaced (not mutated) and the label
        needs to track the new object. Typically used by replace-card handler.

        Args:
            label: The label to update (must already exist)
            card: The new card object to associate with the label

        Raises
        ------
            UndefinedLabelError: If label not found
        """
        if label not in self._labels:
            available = ", ".join(sorted(self._labels.keys())[:10])
            raise UndefinedLabelError(
                f"Cannot update undefined label '{label}' for keyword '{self._keyword}'. "
                f"Available labels: {available}{'...' if len(self._labels) > 10 else ''}"
            )
        old_card = self._labels[label]
        self._labels[label] = card
        logger.debug(f"Updated label '{label}' reference from {id(old_card)} to {id(card)}")

    def has_label(self, label: str) -> bool:
        """Check if a label is registered."""
        return label in self._labels

    def get_all_labels(self) -> Dict[str, Any]:
        """Get a copy of all registered labels."""
        return self._labels.copy()

    def _validate_label_format(self, label: str) -> None:
        """
        Validate label format.

        Valid labels: lowercase/uppercase alphanumeric with underscores.

        Args:
            label: Label to validate

        Raises
        ------
            ValueError: If label format is invalid
        """
        if not label:
            raise ValueError("Label cannot be empty")

        pattern = r"^[a-zA-Z_][a-zA-Z0-9_]*$"
        if not re.match(pattern, label):
            raise ValueError(
                f"Invalid label format: '{label}'. "
                "Labels must start with a letter or underscore, "
                "followed by alphanumeric characters or underscores."
            )

    @classmethod
    def from_cards(
        cls,
        cards: List[Any],
        keyword: str = "",
        initial_labels: Optional[Dict[str, int]] = None,
    ) -> "LabelRegistry":
        """
        Create a LabelRegistry from a cards list with optional initial labels.

        Initial labels from manifest map label names to indices. We resolve
        these to card objects immediately, so the registry holds object refs.

        Args:
            cards: List of Card objects
            keyword: Keyword name (for error messages)
            initial_labels: Optional dict mapping label names to card indices

        Returns
        -------
            Initialized LabelRegistry
        """
        registry = cls(_keyword=keyword)

        # Register explicit labels from manifest
        if initial_labels:
            for label, index in initial_labels.items():
                if index < 0 or index >= len(cards):
                    raise ValueError(
                        f"Invalid index {index} for label '{label}' in {keyword}. "
                        f"Cards list has {len(cards)} cards (indices 0-{len(cards) - 1})."
                    )
                card = cards[index]
                registry.register(label, card)
                logger.debug(f"Registered explicit label '{label}' -> card at index {index}")

        logger.debug(f"Initialized LabelRegistry for '{keyword}' with {len(registry._labels)} labels")
        return registry
