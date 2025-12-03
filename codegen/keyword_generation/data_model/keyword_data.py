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
Typed data models for keyword generation.

This module defines strongly-typed dataclasses that represent the structure of keyword data,
cards, and fields used throughout the code generation process. These models replace the
dictionary-based structures for improved type safety and IDE support.
"""

from dataclasses import dataclass, field
import logging
import typing
from typing import Any, Dict, List, Optional, Union

from .metadata import (
    CardSetsContainer,
    DataclassDefinition,
    DuplicateCardMetadata,
    ExternalCardMetadata,
    LinkData,
    MixinImport,
    OptionGroup,
    VariableCardMetadata,
)

logger = logging.getLogger(__name__)


@dataclass
class Field:
    """
    Represents a single field within a card.

    Attributes:
        name: Field name as it appears in the keyword file
        type: Field type (int, float, str)
        position: Column position in the card
        width: Column width
        default: Default value for the field
        help: Documentation/help text
        used: Whether the field is actively used
        property_name: Python property name (may differ from field name)
        property_type: Python type annotation string
        readonly: Whether field is read-only
        options: Valid options for the field (for enums)
        redundant: Whether field is redundant (appears in multiple cards)
        card_indices: List of card indices where this field appears (for shared fields)
        link: Link ID to other keywords
        flag: Whether this is a boolean flag field
    """

    name: str
    type: str
    position: int
    width: int
    default: Any = None
    help: str = ""
    used: bool = True
    property_name: Optional[str] = None
    property_type: Optional[str] = None
    readonly: bool = False
    options: Optional[List[Any]] = None
    redundant: bool = False
    card_indices: Optional[List[int]] = None
    link: Optional[int] = None
    flag: bool = False

    def to_dict(self) -> Dict[str, Any]:
        """Convert Field to dictionary representation for backward compatibility."""
        result = {
            "name": self.name,
            "type": self.type,
            "position": self.position,
            "width": self.width,
            "default": self.default,
            "help": self.help,
            "used": self.used,
        }
        if self.property_name is not None:
            result["property_name"] = self.property_name
        if self.property_type is not None:
            result["property_type"] = self.property_type
        if self.readonly:
            result["readonly"] = self.readonly
        if self.options is not None:
            result["options"] = self.options
        if self.redundant:
            result["redundant"] = self.redundant
        if self.card_indices is not None:
            result["card_indices"] = self.card_indices
        if self.link is not None:
            result["link"] = self.link
        if self.flag:
            result["flag"] = self.flag
        return result

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Field":
        """Create Field from dictionary representation."""
        return cls(
            name=data["name"],
            type=data["type"],
            position=data["position"],
            width=data["width"],
            default=data.get("default"),
            help=data.get("help", ""),
            used=data.get("used", True),
            property_name=data.get("property_name"),
            property_type=data.get("property_type"),
            readonly=data.get("readonly", False),
            options=data.get("options"),
            redundant=data.get("redundant", False),
            card_indices=data.get("card_indices"),
            link=data.get("link"),
            flag=data.get("flag", False),
        )


@dataclass
class Card:
    """
    Represents a card within a keyword.

    Attributes:
        index: Position in the cards list
        fields: List of Field objects
        mark_for_removal: Card removal marker (used by handlers)
        func: Conditional function string (for conditional cards)
        duplicate: Table card metadata
        variable: Series card metadata
        set: Card set placeholder metadata
        duplicate_group: Table card group flag
        sub_cards: Sub-cards for card groups
        external: External card implementation metadata
        source_index: Original position (for card sets)
        target_index: Target position (for card sets)
        length_func: Function to determine card repetition count
        active_func: Function to determine if card is active
        overall_name: Name for card groups
    """

    index: int
    fields: List[Field] = field(default_factory=list)
    mark_for_removal: Optional[int] = None
    func: Optional[str] = None
    duplicate: Optional[Union[DuplicateCardMetadata, Dict[str, Any]]] = None
    variable: Optional[Union[VariableCardMetadata, Dict[str, Any]]] = None
    set: Optional[Dict[str, Any]] = None
    duplicate_group: bool = False
    sub_cards: Optional[List[Dict[str, Any]]] = None
    external: Optional[Union[ExternalCardMetadata, Dict[str, Any]]] = None
    source_index: Optional[int] = None
    target_index: Optional[int] = None
    length_func: Optional[str] = None
    active_func: Optional[str] = None
    overall_name: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert Card to dictionary representation for backward compatibility."""
        result = {
            "index": self.index,
            "fields": [f.to_dict() if isinstance(f, Field) else f for f in self.fields],
        }
        if self.mark_for_removal is not None:
            result["mark_for_removal"] = self.mark_for_removal
        if self.func is not None:
            result["func"] = self.func
        if self.duplicate is not None:
            result["duplicate"] = (
                self.duplicate.to_dict() if isinstance(self.duplicate, DuplicateCardMetadata) else self.duplicate
            )
        if self.variable is not None:
            result["variable"] = (
                self.variable.to_dict() if isinstance(self.variable, VariableCardMetadata) else self.variable
            )
        if self.set is not None:
            result["set"] = self.set
        if self.duplicate_group:
            result["duplicate_group"] = self.duplicate_group
        if self.sub_cards is not None:
            result["sub_cards"] = self.sub_cards
        if self.external is not None:
            result["external"] = (
                self.external.to_dict() if isinstance(self.external, ExternalCardMetadata) else self.external
            )
        if self.source_index is not None:
            result["source_index"] = self.source_index
        if self.target_index is not None:
            result["target_index"] = self.target_index
        if self.length_func is not None:
            result["length_func"] = self.length_func
        if self.active_func is not None:
            result["active_func"] = self.active_func
        if self.overall_name is not None:
            result["overall_name"] = self.overall_name
        return result

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Card":
        """Create Card from dictionary representation."""
        fields_raw = [
            Field.from_dict(f) if isinstance(f, dict) and "name" in f and "type" in f else f
            for f in data.get("fields", [])
        ]
        fields: List[Field] = typing.cast(List[Field], fields_raw)
        # Convert metadata dicts to typed objects
        duplicate_data = data.get("duplicate")
        duplicate = (
            DuplicateCardMetadata.from_dict(duplicate_data) if isinstance(duplicate_data, dict) else duplicate_data
        )
        variable_data = data.get("variable")
        variable = VariableCardMetadata.from_dict(variable_data) if isinstance(variable_data, dict) else variable_data
        external_data = data.get("external")
        external = ExternalCardMetadata.from_dict(external_data) if isinstance(external_data, dict) else external_data

        return cls(
            index=data["index"],
            fields=fields,
            mark_for_removal=data.get("mark_for_removal"),
            func=data.get("func"),
            duplicate=duplicate,
            variable=variable,
            set=data.get("set"),
            duplicate_group=data.get("duplicate_group", False),
            sub_cards=data.get("sub_cards"),
            external=external,
            source_index=data.get("source_index"),
            target_index=data.get("target_index"),
            length_func=data.get("length_func"),
            active_func=data.get("active_func"),
            overall_name=data.get("overall_name"),
        )


@dataclass
class KeywordData:
    """
    Represents the complete data structure for a keyword during code generation.

    This is the primary data structure that flows through the handler pipeline.
    Handlers read from and write to this structure to transform keyword definitions
    into generated Python code.

    Attributes:
        keyword: Base keyword name (e.g., "SECTION")
        subkeyword: Subkeyword variant (e.g., "SHELL")
        title: Full keyword title
        classname: Generated Python class name
        cards: List of Card objects
        options: Optional card groups (added by AddOptionHandler)
        card_sets: Card set metadata (added by CardSetHandler)
        duplicate: Table card flag (added by TableCardHandler)
        duplicate_group: Table card group flag (added by TableCardGroupHandler)
        variable: Series card flag (added by SeriesCardHandler)
        dataclasses: Custom dataclass definitions (added by SeriesCardHandler)
        mixins: Mixin class names (added by ExternalCardHandler)
        mixin_imports: Mixin import specifications (added by ExternalCardHandler)
        links: Linked keyword relationships
        negative_shared_fields: Shared fields in option cards (used by SharedFieldHandler)
        card_insertions: Pending card insertions (used by InsertCardHandler and others)
    """

    keyword: str
    subkeyword: str
    title: str
    classname: str
    cards: List[Card] = field(default_factory=list)
    options: Optional[Union[List[OptionGroup], List[Dict[str, Any]]]] = None
    card_sets: Optional[Union[CardSetsContainer, Dict[str, Any]]] = None
    duplicate: bool = False
    duplicate_group: bool = False
    variable: bool = False
    dataclasses: Optional[Union[List[DataclassDefinition], List[Dict[str, Any]]]] = None
    mixins: Optional[List[str]] = None
    mixin_imports: Optional[Union[List[MixinImport], List[Dict[str, Any]]]] = None
    links: Optional[Union[List[LinkData], List[Dict[str, Any]]]] = None
    negative_shared_fields: Optional[List[Any]] = None
    card_insertions: List[Any] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert KeywordData to dictionary representation.

        This maintains backward compatibility with code that expects dict-based structures.
        Handlers and templates can continue to work with dicts during the transition period.

        Returns:
            Dictionary representation of the keyword data
        """
        result = {
            "keyword": self.keyword,
            "subkeyword": self.subkeyword,
            "title": self.title,
            "classname": self.classname,
            "cards": [c.to_dict() if isinstance(c, Card) else c for c in self.cards],
            "card_insertions": self.card_insertions,
        }
        if self.options is not None:
            result["options"] = (
                [o.to_dict() if isinstance(o, OptionGroup) else o for o in self.options]
                if isinstance(self.options, list)
                else self.options
            )
        if self.card_sets is not None:
            result["card_sets"] = (
                self.card_sets.to_dict() if isinstance(self.card_sets, CardSetsContainer) else self.card_sets
            )
        if self.duplicate:
            result["duplicate"] = self.duplicate
        if self.duplicate_group:
            result["duplicate_group"] = self.duplicate_group
        if self.variable:
            result["variable"] = self.variable
        if self.dataclasses is not None:
            result["dataclasses"] = (
                [d.to_dict() if isinstance(d, DataclassDefinition) else d for d in self.dataclasses]
                if isinstance(self.dataclasses, list)
                else self.dataclasses
            )
        if self.mixins is not None:
            result["mixins"] = self.mixins
        if self.mixin_imports is not None:
            result["mixin_imports"] = (
                [m.to_dict() if isinstance(m, MixinImport) else m for m in self.mixin_imports]
                if isinstance(self.mixin_imports, list)
                else self.mixin_imports
            )
        if self.links is not None:
            result["links"] = (
                [link.to_dict() if isinstance(link, LinkData) else link for link in self.links]
                if isinstance(self.links, list)
                else self.links
            )
        if self.negative_shared_fields is not None:
            result["negative_shared_fields"] = self.negative_shared_fields
        return result

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "KeywordData":
        """
        Create KeywordData from dictionary representation.

        This enables gradual migration from dict-based to typed structures.

        Args:
            data: Dictionary containing keyword data

        Returns:
            KeywordData instance
        """
        logger.debug(f"Creating KeywordData for {data.get('keyword')}.{data.get('subkeyword')}")
        cards_raw = [Card.from_dict(c) if isinstance(c, dict) and "index" in c else c for c in data.get("cards", [])]
        cards: typing.List[Card] = typing.cast(typing.List[Card], cards_raw)

        # Convert metadata dicts to typed objects
        options_data = data.get("options")
        options = (
            [OptionGroup.from_dict(o) if isinstance(o, dict) else o for o in options_data]
            if isinstance(options_data, list)
            else options_data
        )

        card_sets_data = data.get("card_sets")
        card_sets = CardSetsContainer.from_dict(card_sets_data) if isinstance(card_sets_data, dict) else card_sets_data

        dataclasses_data = data.get("dataclasses")
        dataclasses = (
            [DataclassDefinition.from_dict(d) if isinstance(d, dict) else d for d in dataclasses_data]
            if isinstance(dataclasses_data, list)
            else dataclasses_data
        )

        mixin_imports_data = data.get("mixin_imports")
        mixin_imports = (
            [MixinImport.from_dict(m) if isinstance(m, dict) else m for m in mixin_imports_data]
            if isinstance(mixin_imports_data, list)
            else mixin_imports_data
        )

        links_data = data.get("links")
        links = (
            [LinkData.from_dict(link) if isinstance(link, dict) else link for link in links_data]
            if isinstance(links_data, list)
            else links_data
        )

        return cls(
            keyword=data["keyword"],
            subkeyword=data["subkeyword"],
            title=data["title"],
            classname=data["classname"],
            cards=cards,
            options=options,
            card_sets=card_sets,
            duplicate=data.get("duplicate", False),
            duplicate_group=data.get("duplicate_group", False),
            variable=data.get("variable", False),
            dataclasses=dataclasses,
            mixins=data.get("mixins"),
            mixin_imports=mixin_imports,
            links=links,
            negative_shared_fields=data.get("negative_shared_fields"),
            card_insertions=data.get("card_insertions", []),
        )
