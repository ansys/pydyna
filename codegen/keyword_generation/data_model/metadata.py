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
Metadata dataclasses for card and keyword configuration.

These dataclasses replace the Dict[str, Any] patterns used throughout the codegen
system to provide type safety and better IDE support.
"""

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional


@dataclass
class TableCardMetadata:
    """
    Metadata for table cards (card['table']).

    Used by table-card handler to create repeatable card groups.
    """

    name: str
    length_func: Optional[str] = None
    active_func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "TableCardMetadata":
        """Create from dictionary."""
        return cls(
            name=data["name"],
            length_func=data.get("length_func"),
            active_func=data.get("active_func"),
        )


@dataclass
class VariableCardMetadata:
    """
    Metadata for variable/series cards (card['variable']).

    Used by series-card handler to create variable-length card series.
    """

    name: str
    size: int
    width: int
    type: str
    help: str
    length_func: Optional[str] = None
    active_func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "VariableCardMetadata":
        """Create from dictionary."""
        return cls(
            name=data["name"],
            size=data["size"],
            width=data["width"],
            type=data["type"],
            help=data["help"],
            length_func=data.get("length_func"),
            active_func=data.get("active_func"),
        )


@dataclass
class ExternalCardMetadata:
    """
    Metadata for external card implementation (card['external']).

    Used by external-card-implementation handler to reference externally defined cards.
    """

    name: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ExternalCardMetadata":
        """Create from dictionary."""
        return cls(name=data["name"])


@dataclass
class OptionGroup:
    """
    Represents an option group in kwd_data['options'].

    Option groups define keyword options with their card order and activation functions.
    """

    name: str
    card_order: int
    title_order: int
    cards: List[Dict[str, Any]]  # List of card dicts, will be typed later
    func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "OptionGroup":
        """Create from dictionary."""
        return cls(
            name=data["name"],
            card_order=data["card_order"],
            title_order=data["title_order"],
            cards=data["cards"],
            func=data.get("func"),
        )

    def get_all_cards(self) -> List[Dict[str, Any]]:
        """Get all cards in this option group.

        Returns:
            List of cards (may be Card instances or dicts during transition)
        """
        return self.cards


@dataclass
class CardSet:
    """
    Represents a card set in kwd_data["card_sets"]["sets"].

    Card sets group related cards that repeat together.
    """

    name: str
    source_cards: List[Dict[str, Any]]  # References to card dicts
    options: List[Dict[str, Any]] = field(default_factory=list)
    length_func: Optional[str] = None
    active_func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "CardSet":
        """Create from dictionary."""
        return cls(
            name=data["name"],
            source_cards=data["source_cards"],
            options=data.get("options", []),
            length_func=data.get("length_func"),
            active_func=data.get("active_func"),
        )

    def get_all_cards(self) -> List[Dict[str, Any]]:
        """Get all cards in this card set, including nested option cards.

        Returns:
            List of cards from source_cards and all option groups
        """
        all_cards = []
        all_cards.extend(self.source_cards)
        for option in self.options:
            # option may be dict with 'cards' key
            if isinstance(option, dict):
                all_cards.extend(option.get("cards", []))
            else:
                # If option has cards attribute/method
                if hasattr(option, "cards"):
                    all_cards.extend(option.cards)
        return all_cards


@dataclass
class CardSetsContainer:
    """
    Container for card sets in kwd_data['card_sets'].

    Holds all card sets and whether they have options.
    """

    sets: List[CardSet] = field(default_factory=list)
    options: bool = False

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "CardSetsContainer":
        """Create from dictionary."""
        return cls(
            sets=[CardSet.from_dict(s) for s in data.get("sets", [])],
            options=data.get("options", False),
        )


@dataclass
class LinkData:
    """
    Represents linked keyword relationship in kwd_data['links'].

    Links define relationships between keywords (e.g., DEFINE_TABLE -> DEFINE_CURVE).
    """

    classname: str
    modulename: str
    keyword_type: str
    keyword_subtype: str
    fields: List[str]
    linkid: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "LinkData":
        """Create from dictionary."""
        return cls(
            classname=data["classname"],
            modulename=data["modulename"],
            keyword_type=data["keyword_type"],
            keyword_subtype=data["keyword_subtype"],
            fields=data["fields"],
            linkid=data["linkid"],
        )


@dataclass
class MixinImport:
    """
    Represents a mixin import in kwd_data['mixin_imports'].

    Mixin imports specify external classes to import for keyword mixins.
    """

    source: str
    names: List[str]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "MixinImport":
        """Create from dictionary."""
        return cls(
            source=data["source"],
            names=data["names"],
        )


@dataclass
class DataclassField:
    """
    Represents a field in a custom dataclass definition.
    """

    name: str
    type: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "DataclassField":
        """Create from dictionary."""
        return cls(
            name=data["name"],
            type=data["type"],
        )


@dataclass
class DataclassDefinition:
    """
    Represents a custom dataclass definition in kwd_data['dataclasses'].

    Used by series-card handler to define custom dataclasses for repeating data.
    """

    name: str
    fields: List[DataclassField]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "DataclassDefinition":
        """Create from dictionary."""
        return cls(
            name=data["name"],
            fields=[DataclassField.from_dict(f) for f in data["fields"]],
        )
