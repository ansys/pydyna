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
Base classes for handler settings to reduce duplication.

This module provides common functionality for handler settings classes,
eliminating the need for repeated from_dict() and resolve_index() implementations.
"""

from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from keyword_generation.data_model.label_registry import LabelRegistry


@dataclass
class LabelRefSettings:
    """
    Base class for handler settings that use label-based card references.

    Provides common resolve_index() functionality for settings that need to
    reference cards by label.

    Attributes:
        ref: Label reference to a card (e.g., "header_card", "data_card")
    """
    ref: str

    def resolve_index(self, registry: LabelRegistry, cards: List[Any]) -> int:
        """
        Resolve label reference to card index.

        Args:
            registry: Label registry for resolving card references
            cards: List of cards (used for validation)

        Returns:
            Integer index of the referenced card

        Raises:
            UndefinedLabelError: If label is not found in registry
        """
        return registry.resolve_index(self.ref, cards)


def parse_settings_list(settings_class, settings: List[Dict[str, Any]]) -> List[Any]:
    """
    Generic helper to parse a list of dict settings into typed dataclass instances.

    This eliminates the need for each handler to implement its own _parse_settings()
    classmethod with the exact same logic.

    Args:
        settings_class: The dataclass to instantiate (must have from_dict classmethod)
        settings: List of setting dictionaries from manifest.json

    Returns:
        List of typed settings instances

    Example:
        >>> typed_settings = parse_settings_list(ConditionalCardSettings, settings)
    """
    return [settings_class.from_dict(s) for s in settings]


def find_field_in_card(card: Any, field_name: str, case_sensitive: bool = False) -> Optional[Dict]:
    """
    Find a field in a card by name.

    Handles both Card instances and dict-based cards. Searches through all fields
    in the card and returns the first matching field.

    Args:
        card: Card instance or dict containing fields
        field_name: Name of the field to find
        case_sensitive: Whether to use case-sensitive matching (default: False)

    Returns:
        Field dict if found, None otherwise
    """
    # Handle both Card instances and dicts
    if hasattr(card, 'get_all_fields'):
        fields = card.get_all_fields()
    else:
        fields = card.get("fields", [])

    # Search with appropriate case sensitivity
    if case_sensitive:
        for field in fields:
            if field.get("name") == field_name:
                return field
    else:
        target = field_name.lower()
        for field in fields:
            if field.get("name", "").lower() == target:
                return field

    return None


def modify_field_in_cards(kwd_data: Any, card_index: int, field_name: str,
                          modifications: Dict[str, Any]) -> bool:
    """
    Find and modify a field in a specific card.

    Args:
        kwd_data: KeywordData instance
        card_index: Index of the card containing the field
        field_name: Name of the field to modify
        modifications: Dict of field properties to update

    Returns:
        True if field was found and modified, False otherwise
    """
    card = kwd_data.cards[card_index]
    field = find_field_in_card(card, field_name)

    if field is None:
        return False

    # Apply modifications
    for key, value in modifications.items():
        field[key] = value

    return True
