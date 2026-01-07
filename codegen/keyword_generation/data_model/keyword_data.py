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
Typed data models for keyword generation.

This module defines strongly-typed dataclasses that represent the structure of keyword data,
cards, and fields used throughout the code generation process. These models replace the
dictionary-based structures for improved type safety and IDE support.
"""

from dataclasses import dataclass, field
import logging
import typing
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Union

from .metadata import (
    CardSetsContainer,
    DataclassDefinition,
    ExternalCardMetadata,
    LinkData,
    MixinImport,
    OptionGroup,
    TableCardMetadata,
    VariableCardMetadata,
)

if TYPE_CHECKING:
    from .label_registry import LabelRegistry

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
    options: List[Any] = field(default_factory=list)  # Empty list for templates instead of None
    redundant: bool = False
    card_indices: Optional[List[int]] = None
    link: Optional[int] = None
    flag: bool = False
    on: Optional[str] = None  # Value when flag is True
    off: Optional[str] = None  # Value when flag is False

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
            options=data.get("options", []),  # Default to empty list
            redundant=data.get("redundant", False),
            card_indices=data.get("card_indices"),
            link=data.get("link"),
            flag=data.get("flag", False),
            on=data.get("on"),
            off=data.get("off"),
        )

    def __getitem__(self, key: str) -> Any:
        """Dict-like access for backward compatibility."""
        return getattr(self, key)

    def __setitem__(self, key: str, value: Any) -> None:
        """Dict-like assignment for backward compatibility."""
        setattr(self, key, value)

    def __contains__(self, key: str) -> bool:
        """Dict-like 'in' operator for backward compatibility."""
        return hasattr(self, key)

    def get(self, key: str, default: Any = None) -> Any:
        """Dict-like get() method for backward compatibility."""
        return getattr(self, key, default)

    def normalize(self) -> None:
        """Normalize field data for Python code generation.

        Performs:
        - Type mapping (integer->int, real->float, string->str)
        - Field name fixing (uppercase->lowercase, special chars, reserved words)
        - Property name generation
        - Default value conversion (string wrapping, int conversion)
        - Flag type conversion (flag fields become bool)
        - Help text cleanup
        """
        # Set default 'used' flag if not present
        if not hasattr(self, "used") or self.used is None:
            self.used = True

        # Type mapping - MUST happen before checking 'used' flag
        type_mapping = {"integer": "int", "real": "float", "string": "str", "real-integer": "float"}
        if self.type in type_mapping:
            self.type = type_mapping[self.type]

        # Handle unused fields - set name to "unused" and skip rest of processing
        if not self.used:
            self.default = None
            self.help = ""
            self.name = "unused"
            return

        # Handle flag fields - convert to bool type
        if self.flag:
            self.type = "bool"

        # Fix field name to be Python-friendly
        # Original logic: field["name"] gets lowercased original, property_name gets fixed version
        field_name = self.name  # Keep original for fixing

        # Create fixed version with character replacements for property_name
        fixed_name = field_name
        # Deal with bad characters
        for bad_char in ["/", "-", " ", "(", ")", ",", ".", "'", "*", "|", "+"]:
            fixed_name = fixed_name.replace(bad_char, "_")
        # Deal with reserved keywords
        if fixed_name.lower() in ["global", "as", "int", "lambda", "for"]:
            fixed_name = fixed_name + "_"
        # Deal with names starting with digits
        if fixed_name and fixed_name[0].isdigit():
            fixed_name = "_" + fixed_name

        fixed_field_name = fixed_name.lower()
        self.name = field_name.lower()  # Use original name lowercased, not the fixed version

        # Set property_name if not already set
        if not self.property_name:
            self.property_name = fixed_field_name

        # Set property_type if not already set
        if not self.property_type:
            self.property_type = self.type

        # Type-specific default handling
        if self.type == "str":
            # Wrap string options in quotes
            if self.options:
                self.options = [f'"{option}"' for option in self.options]
            # Wrap string default in quotes
            if self.default is not None:
                self.default = f'"{self.default}"'
        elif self.type == "int":
            # Convert int defaults from strings (might be floats)
            if self.default is not None:
                try:
                    self.default = int(float(self.default))
                except (ValueError, TypeError):
                    # If conversion fails, leave as None
                    self.default = None

        # Clean up help text (remove leading whitespace from each line)
        if self.help:
            lines = self.help.split("\n")
            cleaned_lines = [line.strip() for line in lines]
            self.help = "\n".join(cleaned_lines)
            # Add trailing space if help ends with quote
            if self.help.endswith('"'):
                self.help = self.help + " "


@dataclass
class Card:
    """
    Represents a card within a keyword.

    Attributes:
        index: Position in the cards list
        fields: List of Field objects
        mark_for_removal: Card removal marker (used by handlers)
        func: Conditional function string (for conditional cards)
        table: Table card metadata
        variable: Series card metadata
        set: Card set placeholder metadata
        table_group: Table card group flag
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
    table: Optional[Union[TableCardMetadata, Dict[str, Any]]] = None
    variable: Optional[Union[VariableCardMetadata, Dict[str, Any]]] = None
    set: Optional[Dict[str, Any]] = None
    table_group: bool = False
    sub_cards: Optional[List[Dict[str, Any]]] = None
    external: Optional[Union[ExternalCardMetadata, Dict[str, Any]]] = None
    source_index: Optional[int] = None
    target_index: Optional[int] = None
    length_func: Optional[str] = None
    active_func: Optional[str] = None
    overall_name: Optional[str] = None
    active: Optional[str] = None  # Activation condition for option cards

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Card":
        """Create Card from dictionary representation."""
        fields_raw = [
            Field.from_dict(f) if isinstance(f, dict) and "name" in f and "type" in f else f
            for f in data.get("fields", [])
        ]
        fields: List[Field] = typing.cast(List[Field], fields_raw)
        # Convert metadata dicts to typed objects
        table_data = data.get("table")
        table = TableCardMetadata.from_dict(table_data) if isinstance(table_data, dict) else table_data
        variable_data = data.get("variable")
        variable = VariableCardMetadata.from_dict(variable_data) if isinstance(variable_data, dict) else variable_data
        external_data = data.get("external")
        external = ExternalCardMetadata.from_dict(external_data) if isinstance(external_data, dict) else external_data

        return cls(
            index=data.get("index", -1),  # Default to -1, will be set by class_generator
            fields=fields,
            mark_for_removal=data.get("mark_for_removal"),
            func=data.get("func"),
            table=table,
            variable=variable,
            set=data.get("set"),
            table_group=data.get("table_group", False),
            sub_cards=data.get("sub_cards"),
            external=external,
            source_index=data.get("source_index"),
            target_index=data.get("target_index"),
            length_func=data.get("length_func"),
            active_func=data.get("active_func"),
            overall_name=data.get("overall_name"),
            active=data.get("active"),
        )

    def __getitem__(self, key: str) -> Any:
        """Dict-like access for backward compatibility during migration."""
        return getattr(self, key)

    def __setitem__(self, key: str, value: Any) -> None:
        """Dict-like assignment for backward compatibility during migration."""
        setattr(self, key, value)

    def __contains__(self, key: str) -> bool:
        """Dict-like 'in' operator for backward compatibility."""
        return hasattr(self, key)

    def get(self, key: str, default: Any = None) -> Any:
        """Dict-like get() method for backward compatibility."""
        return getattr(self, key, default)

    def get_all_fields(self) -> List[Field]:
        """Get all fields for this card, handling table_group aggregation.

        For regular cards, returns self.fields directly.
        For table_group cards, aggregates fields from all sub_cards.

        Returns:
            List of Field instances
        """
        if self.table_group and self.sub_cards:
            all_fields = []
            for sub_card in self.sub_cards:
                # sub_card might be dict or Card during transition
                if isinstance(sub_card, dict):
                    all_fields.extend(sub_card.get("fields", []))
                else:
                    all_fields.extend(sub_card.fields)
            return all_fields
        return self.fields


@dataclass
class RenamedProperty:
    """Tracks a property that was renamed from its original field name.

    Used for generating documentation about field name to property name mappings.
    """

    field_name: str  # Original field name (e.g., "R")
    property_name: str  # New property name (e.g., "gas_constant")
    card_index: int  # 0-based card index
    description: str = ""  # Description from help text (e.g., "gas constant")


@dataclass
class KeywordData:
    """
    Represents the complete data structure for a keyword during code generation.

    This is the primary data structure that flows through the handler pipeline.
    Handlers read from and write to this structure using attribute access
    (e.g., kwd_data.cards) to transform keyword definitions into generated Python code.

    Design Rationale:
    - KeywordData uses dataclass attributes for type safety and IDE support
    - Cards are now List[Card] instances with dict-like access (__getitem__/__setitem__)
      for backward compatibility during the migration from dict-based code
    - Options may be List[OptionGroup] or List[Dict] during transition period
    - The from_dict/to_dict methods enable conversion at pipeline boundaries

    Attributes:
        keyword: Base keyword name (e.g., "SECTION")
        subkeyword: Subkeyword variant (e.g., "SHELL")
        title: Full keyword title
        classname: Generated Python class name
        cards: List of Card objects
        options: Optional card groups (added by AddOptionHandler)
        card_sets: Card set metadata (added by CardSetHandler)
        table: Table card flag (added by TableCardHandler)
        table_group: Table card group flag (added by TableCardGroupHandler)
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
    classname: str = ""  # Set later by _get_base_variable
    cards: List[Card] = field(default_factory=list)  # Now using Card dataclass instances
    options: Union[List[OptionGroup], List[Dict[str, Any]]] = field(default_factory=list)  # Empty list for templates
    card_sets: Optional[Union[CardSetsContainer, Dict[str, Any]]] = None
    table: bool = False
    table_group: bool = False
    variable: bool = False
    dataclasses: Union[List[DataclassDefinition], List[Dict[str, Any]]] = field(
        default_factory=list
    )  # Empty list for templates
    mixins: List[str] = field(default_factory=list)  # Empty list for templates
    mixin_imports: Union[List[MixinImport], List[Dict[str, Any]]] = field(
        default_factory=list
    )  # Empty list for templates
    links: Union[List[LinkData], List[Dict[str, Any]]] = field(default_factory=list)  # Empty list for templates
    negative_shared_fields: List[Any] = field(default_factory=list)  # Empty list for templates
    card_insertions: List[Any] = field(default_factory=list)
    label_registry: Optional["LabelRegistry"] = None  # Initialized before handlers run
    renamed_properties: List["RenamedProperty"] = field(default_factory=list)  # Tracks renamed fields for docs
    property_collisions: Dict[str, str] = field(default_factory=dict)  # Maps property_name -> collision note

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
        # Convert cards from dicts to Card instances
        cards_data = data.get("cards", [])
        cards = [Card.from_dict(c) if isinstance(c, dict) else c for c in cards_data]

        # Convert metadata dicts to typed objects
        options_data = data.get("options")
        options = (
            [OptionGroup.from_dict(o) if isinstance(o, dict) else o for o in options_data]
            if isinstance(options_data, list)
            else []
        )

        card_sets_data = data.get("card_sets")
        card_sets = CardSetsContainer.from_dict(card_sets_data) if isinstance(card_sets_data, dict) else card_sets_data

        dataclasses_data = data.get("dataclasses")
        dataclasses = (
            [DataclassDefinition.from_dict(d) if isinstance(d, dict) else d for d in dataclasses_data]
            if isinstance(dataclasses_data, list)
            else []
        )

        mixin_imports_data = data.get("mixin_imports")
        mixin_imports = (
            [MixinImport.from_dict(m) if isinstance(m, dict) else m for m in mixin_imports_data]
            if isinstance(mixin_imports_data, list)
            else []
        )

        links_data = data.get("links")
        links = (
            [LinkData.from_dict(link) if isinstance(link, dict) else link for link in links_data]
            if isinstance(links_data, list)
            else []  # Default to empty list if None
        )

        return cls(
            keyword=data["keyword"],
            subkeyword=data["subkeyword"],
            title=data["title"],
            classname=data.get("classname", ""),  # Optional, set later by generator
            cards=cards,
            options=options,
            card_sets=card_sets,
            table=data.get("table", False),
            table_group=data.get("table_group", False),
            variable=data.get("variable", False),
            dataclasses=dataclasses,
            mixins=data.get("mixins", []),
            mixin_imports=mixin_imports,
            links=links,
            negative_shared_fields=data.get("negative_shared_fields", []),
            card_insertions=data.get("card_insertions", []),
            renamed_properties=data.get("renamed_properties", []),
        )

    def get_all_cards(self) -> Union[List[Card], List[Dict[str, Any]], List[Union[Card, Dict[str, Any]]]]:
        """Get all cards that need transformation, from all nested structures.

        This collects cards from:
        - Main cards list
        - Card sets (source_cards and their options)
        - Top-level options (option cards)

        Returns:
            Flat list of all Card instances or dicts requiring field transformation.
            During the transition period, may contain both Card instances and dicts.
        """
        all_cards = []

        # Main cards
        all_cards.extend(self.cards)

        # Cards from card_sets
        if self.card_sets:
            # card_sets may be CardSetsContainer or dict during transition
            if hasattr(self.card_sets, "sets"):
                sets = self.card_sets.sets
            else:
                sets = self.card_sets.get("sets", [])  # type: ignore

            for card_set in sets:
                # card_set may be CardSet instance or dict
                if hasattr(card_set, "get_all_cards"):
                    all_cards.extend(card_set.get_all_cards())
                else:
                    # Fallback for dict-based card_set
                    source_cards = card_set.get("source_cards", [])  # type: ignore
                    all_cards.extend(source_cards)
                    options = card_set.get("options", [])  # type: ignore
                    for option in options:
                        all_cards.extend(option.get("cards", []))  # type: ignore

        # Top-level options
        for option in self.options or []:
            # option may be OptionGroup instance or dict
            if hasattr(option, "get_all_cards"):
                all_cards.extend(option.get_all_cards())
            else:
                # Fallback for dict-based option
                all_cards.extend(option.get("cards", []))  # type: ignore

        return all_cards
