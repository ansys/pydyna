# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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
Typed settings dataclasses for keyword generation handlers.

Each handler has a corresponding settings dataclass that defines the expected
structure of its configuration from manifest.json. This provides type safety
and IDE support for handler configurations.
"""

from dataclasses import dataclass
from typing import Any, Dict, List, Optional


@dataclass
class ReorderCardSettings:
    """Settings for reorder-card handler."""

    order: List[int]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ReorderCardSettings":
        """Create from dictionary."""
        return cls(order=data["order"])


@dataclass
class TableCardSettings:
    """Settings for table-card handler."""

    index: int
    property_name: str
    length_func: Optional[str] = None
    active_func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "TableCardSettings":
        """Create from dictionary."""
        return cls(
            index=data["index"],
            property_name=data["property-name"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
        )


@dataclass
class OverrideFieldSettings:
    """Settings for override-field handler."""

    card_index: int
    field_index: int
    properties: Dict[str, Any]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "OverrideFieldSettings":
        """Create from dictionary."""
        return cls(
            card_index=data["card-index"],
            field_index=data["field-index"],
            properties=data["properties"],
        )


@dataclass
class ReplaceCardSettings:
    """Settings for replace-card handler."""

    index: int
    fields: List[Dict[str, Any]]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ReplaceCardSettings":
        """Create from dictionary."""
        return cls(
            index=data["index"],
            fields=data["fields"],
        )


@dataclass
class InsertCardSettings:
    """Settings for insert-card handler."""

    index: int
    target_class: str
    card: Dict[str, Any]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "InsertCardSettings":
        """Create from dictionary."""
        return cls(
            index=data["index"],
            target_class=data["target-class"],
            card=data["card"],
        )


@dataclass
class SeriesCardSettings:
    """Settings for series-card handler."""

    index: int
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
        """Create from dictionary."""
        return cls(
            index=data["index"],
            name=data["name"],
            card_size=data["card-size"],
            element_width=data["element-width"],
            type=data["type"],
            help=data["help"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
            struct_info=data.get("struct-info"),
        )


@dataclass
class AddOptionSettings:
    """Settings for add-option handler."""

    name: str
    card_order: int
    title_order: int
    cards: List[Dict[str, Any]]
    func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "AddOptionSettings":
        """Create from dictionary."""
        return cls(
            name=data["name"],
            card_order=data["card-order"],
            title_order=data["title-order"],
            cards=data["cards"],
            func=data.get("func"),
        )


@dataclass
class CardSetSettings:
    """Settings for card-set handler."""

    name: str
    source_indices: List[int]
    length_func: Optional[str] = None
    active_func: Optional[str] = None
    options: Optional[List[Dict[str, Any]]] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "CardSetSettings":
        """Create from dictionary."""
        return cls(
            name=data["name"],
            source_indices=data["source-indices"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
            options=data.get("options"),
        )


@dataclass
class ConditionalCardSettings:
    """Settings for conditional-card handler."""

    index: int
    func: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ConditionalCardSettings":
        """Create from dictionary."""
        return cls(
            index=data["index"],
            func=data["func"],
        )


@dataclass
class RenamePropertySettings:
    """Settings for rename-property handler."""

    old_name: str
    new_name: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "RenamePropertySettings":
        """Create from dictionary."""
        return cls(
            old_name=data["old-name"],
            new_name=data["new-name"],
        )


@dataclass
class SkipCardSettings:
    """Settings for skip-card handler."""

    index: int

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SkipCardSettings":
        """Create from dictionary."""
        return cls(index=data["index"])


@dataclass
class TableCardGroupSettings:
    """Settings for table-card-group handler."""

    indices: List[int]
    property_name: str
    length_func: Optional[str] = None
    active_func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "TableCardGroupSettings":
        """Create from dictionary."""
        return cls(
            indices=data["indices"],
            property_name=data["property-name"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
        )


@dataclass
class ExternalCardSettings:
    """Settings for external-card-implementation handler."""

    index: int
    name: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ExternalCardSettings":
        """Create from dictionary."""
        return cls(
            index=data["index"],
            name=data["name"],
        )


@dataclass
class SharedFieldSettings:
    """Settings for shared-field handler."""

    field_name: str
    card_indices: List[int]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SharedFieldSettings":
        """Create from dictionary."""
        return cls(
            field_name=data["field-name"],
            card_indices=data["card-indices"],
        )


@dataclass
class OverrideSubkeywordSettings:
    """Settings for override-subkeyword handler."""

    old_name: str
    new_name: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "OverrideSubkeywordSettings":
        """Create from dictionary."""
        return cls(
            old_name=data["old-name"],
            new_name=data["new-name"],
        )
