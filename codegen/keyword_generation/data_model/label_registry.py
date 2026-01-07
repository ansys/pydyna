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
Labels allow manifest.json configurations to reference cards by meaningful names rather
than fragile integer indices that shift when cards are inserted or reordered.

Design Goals:
- URI-style labels (REST-like with `/` separator) for nested structures
- Global uniqueness per keyword
- Hybrid resolution: accepts both labels ("ref") and legacy indices ("index")
- Fail-fast validation at manifest load time

Example URI labels:
- "stress_data" - flat card reference
- "fiber_families/ftype_card" - card within a CardSet
- "stress_set/item:0/hisv" - explicit item index within CardSet
"""

from dataclasses import dataclass, field
import logging
import re
from typing import Any, Dict, List, Optional, Union

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


@dataclass
class CardAddress:
    """
    Location of a card in the (possibly nested) structure.

    Supports both flat and nested addressing:
    - Flat: path=[3] for card at index 3
    - Nested: path=[3, 0, 2] for CardSet at index 3 → item 0 → card 2

    Attributes:
        path: List of integer indices representing the location path
        entity_type: Type of entity ("card", "card_set", "table_card", "series_card", "option")
        label: Optional human-readable label for debugging
    """

    path: List[int]
    entity_type: str = "card"
    label: Optional[str] = None

    def __post_init__(self):
        """Validate path is non-empty."""
        if not self.path:
            raise ValueError("CardAddress path cannot be empty")

    @property
    def root_index(self) -> int:
        """Get the top-level index in kwd_data.cards."""
        return self.path[0]

    @property
    def is_nested(self) -> bool:
        """Check if this address refers to a nested structure."""
        return len(self.path) > 1

    def shifted(self, delta: int) -> "CardAddress":
        """Return a new CardAddress with root index shifted by delta."""
        new_path = self.path.copy()
        new_path[0] += delta
        return CardAddress(path=new_path, entity_type=self.entity_type, label=self.label)


@dataclass
class LabelRegistry:
    """
    Manages named references to cards and card structures.

    Labels are globally unique within a keyword. The registry supports:
    - Simple labels: "stress_data" → CardAddress([1], "card")
    - URI-style nested labels: "fiber_families/theta_card" → CardAddress([7, 0, 2], "card")

    The registry is initialized after reorder-card runs, with auto-generated labels
    (card_0, card_1, ...) that can be overridden by manifest's "labels" section.
    """

    _labels: Dict[str, CardAddress] = field(default_factory=dict)
    _keyword: str = ""

    def register(self, label: str, address: CardAddress) -> None:
        """
        Register a label pointing to a card address.

        Args:
            label: URI-style label (e.g., "stress_data" or "fiber_families/theta")
            address: CardAddress specifying the location

        Raises:
            DuplicateLabelError: If label already registered
            ValueError: If label format is invalid
        """
        self._validate_label_format(label)

        if label in self._labels:
            existing = self._labels[label]
            raise DuplicateLabelError(
                f"Label '{label}' already registered for {self._keyword}: "
                f"existing={existing.path}, new={address.path}"
            )

        address.label = label
        self._labels[label] = address
        logger.debug(f"Registered label '{label}' → {address.path} ({address.entity_type})")

    def resolve(self, label: str) -> CardAddress:
        """
        Resolve a label to its current card address.

        Args:
            label: The label to resolve

        Returns:
            CardAddress for the labeled entity

        Raises:
            UndefinedLabelError: If label not found
        """
        if label not in self._labels:
            available = ", ".join(sorted(self._labels.keys())[:10])
            raise UndefinedLabelError(
                f"Undefined label '{label}' for keyword '{self._keyword}'. "
                f"Available labels: {available}{'...' if len(self._labels) > 10 else ''}"
            )
        return self._labels[label]

    def resolve_index(self, ref_or_index: Union[str, int]) -> int:
        """
        Resolve a reference (label or index) to a flat integer index.

        This is the primary resolution method for handlers, supporting both
        legacy index-based and new label-based references.

        Args:
            ref_or_index: Either a label string or an integer index

        Returns:
            Integer index into kwd_data.cards

        Raises:
            UndefinedLabelError: If label not found
            ValueError: If nested label resolves to non-flat address
        """
        if isinstance(ref_or_index, int):
            return ref_or_index

        address = self.resolve(ref_or_index)
        if address.is_nested:
            raise ValueError(
                f"Label '{ref_or_index}' resolves to nested address {address.path}. "
                "Use resolve() for nested addresses or resolve_index() only for flat cards."
            )
        return address.root_index

    def has_label(self, label: str) -> bool:
        """Check if a label is registered."""
        return label in self._labels

    def get_all_labels(self) -> Dict[str, CardAddress]:
        """Get a copy of all registered labels."""
        return self._labels.copy()

    def update_after_insert(self, insert_position: int, count: int = 1) -> None:
        """
        Shift all labels at or after insert_position by count.

        Called by handlers that insert cards (e.g., insert-card) to keep
        label addresses in sync with the modified cards list.

        Args:
            insert_position: The position where cards were inserted
            count: Number of cards inserted (default 1)
        """
        shifted_count = 0
        for label, address in self._labels.items():
            if address.path[0] >= insert_position:
                old_path = address.path[0]
                address.path[0] += count
                shifted_count += 1
                logger.debug(f"Shifted label '{label}': {old_path} → {address.path[0]}")

        if shifted_count:
            logger.debug(f"Shifted {shifted_count} labels after insert at position {insert_position}")

    def update_after_remove(self, remove_position: int, count: int = 1) -> None:
        """
        Shift all labels after remove_position by -count.

        Called when cards are removed from the list.

        Args:
            remove_position: The position where cards were removed
            count: Number of cards removed (default 1)
        """
        shifted_count = 0
        for label, address in self._labels.items():
            if address.path[0] > remove_position:
                old_path = address.path[0]
                address.path[0] -= count
                shifted_count += 1
                logger.debug(f"Shifted label '{label}': {old_path} → {address.path[0]}")

        if shifted_count:
            logger.debug(f"Shifted {shifted_count} labels after remove at position {remove_position}")

    def _validate_label_format(self, label: str) -> None:
        """
        Validate label format.

        Valid labels:
        - Simple: lowercase alphanumeric with underscores (e.g., "stress_data")
        - URI-style: segments separated by "/" (e.g., "fiber_families/theta_card")
        - Segments may include type prefixes (e.g., "item:0", "card:theta")

        Args:
            label: Label to validate

        Raises:
            ValueError: If label format is invalid
        """
        if not label:
            raise ValueError("Label cannot be empty")

        # Allow alphanumeric, underscore, colon (for type prefix), slash (for path)
        pattern = r"^[a-zA-Z_][a-zA-Z0-9_]*(:[a-zA-Z0-9_]+)?(/[a-zA-Z_][a-zA-Z0-9_]*(:[a-zA-Z0-9_]+)?)*$"
        if not re.match(pattern, label):
            raise ValueError(
                f"Invalid label format: '{label}'. "
                "Labels must be alphanumeric with underscores, optionally with "
                "type prefixes (type:name) and path separators (/)"
            )

    @classmethod
    def from_cards(cls, cards: List[Any], keyword: str = "", initial_labels: Optional[Dict[str, int]] = None):
        """
        Create a LabelRegistry with auto-generated labels for a card list.

        Auto-generates labels like "card_0", "card_1", etc. for each card.
        These can be overridden by explicit labels from the manifest's "labels" section.

        Args:
            cards: List of Card objects
            keyword: Keyword name (for error messages)
            initial_labels: Optional dict mapping label names to card indices from manifest

        Returns:
            Initialized LabelRegistry
        """
        registry = cls(_keyword=keyword)

        # First, register explicit labels from manifest
        if initial_labels:
            for label, index in initial_labels.items():
                if index < 0 or index >= len(cards):
                    raise ValueError(
                        f"Invalid index {index} for label '{label}' in keyword '{keyword}'. "
                        f"Card list has {len(cards)} cards."
                    )
                registry.register(label, CardAddress(path=[index], entity_type="card"))
                logger.debug(f"Registered explicit label '{label}' → index {index}")

        # Then, auto-generate labels for cards without explicit labels
        labeled_indices = {addr.root_index for addr in registry._labels.values()}
        for i in range(len(cards)):
            if i not in labeled_indices:
                auto_label = f"card_{i}"
                registry.register(auto_label, CardAddress(path=[i], entity_type="card"))

        logger.info(
            f"Initialized LabelRegistry for '{keyword}' with {len(registry._labels)} labels "
            f"({len(initial_labels or {})} explicit, {len(cards) - len(initial_labels or {})} auto-generated)"
        )

        return registry


def resolve_setting_index(
    setting: Dict[str, Any], registry: LabelRegistry, index_key: str = "index", ref_key: str = "ref"
) -> int:
    """
    Resolve a card index from a handler setting, supporting both legacy and label-based refs.

    This is a convenience function for handlers to use during migration. It checks for
    both "ref" (new label-based) and "index" (legacy) keys in the setting dict.

    Args:
        setting: Handler setting dict (e.g., {"index": 3, "func": "..."} or {"ref": "stress_data", "func": "..."})
        registry: LabelRegistry for resolving labels
        index_key: Key name for legacy index (default "index")
        ref_key: Key name for label reference (default "ref")

    Returns:
        Integer index into kwd_data.cards

    Raises:
        ValueError: If neither index nor ref is provided
        UndefinedLabelError: If ref label is not found
    """
    if ref_key in setting:
        return registry.resolve_index(setting[ref_key])
    elif index_key in setting:
        return setting[index_key]
    else:
        raise ValueError(f"Setting must contain either '{ref_key}' or '{index_key}': {setting}")


def resolve_setting_indices(
    setting: Dict[str, Any],
    registry: LabelRegistry,
    indices_key: str = "indices",
    refs_key: str = "refs",
) -> List[int]:
    """
    Resolve multiple card indices from a handler setting.

    Supports both legacy "indices" (list of ints) and new "refs" (list of labels).

    Args:
        setting: Handler setting dict
        registry: LabelRegistry for resolving labels
        indices_key: Key name for legacy indices list (default "indices")
        refs_key: Key name for label references list (default "refs")

    Returns:
        List of integer indices

    Raises:
        ValueError: If neither indices nor refs is provided
        UndefinedLabelError: If any ref label is not found
    """
    if refs_key in setting:
        return [registry.resolve_index(ref) for ref in setting[refs_key]]
    elif indices_key in setting:
        return setting[indices_key]
    else:
        raise ValueError(f"Setting must contain either '{refs_key}' or '{indices_key}': {setting}")
