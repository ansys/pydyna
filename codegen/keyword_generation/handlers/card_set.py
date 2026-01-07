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
Card Set Handler: Creates reusable card collections with dynamic sizing.

This handler enables keywords to define card sets - groups of cards that can be
repeated with variable length, optionally including their own sub-options.
"""

import copy
from dataclasses import dataclass
import typing
from typing import Any, Dict, List, Optional

import keyword_generation.data_model as gen
from keyword_generation.data_model.keyword_data import Field, KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class DiscriminatorConfig:
    """Configuration for a discriminator field that determines which card variant to use.

    A discriminator is a field whose value determines which of multiple mutually-exclusive
    cards should be active. This is used for patterns like MAT_295 fiber families where
    FTYPE=1 means use card format A, FTYPE=2 means use card format B.

    The field position, width, and default value are obtained from the Field object at
    runtime, so only the field name and which cards contain it need to be specified.
    """

    field: str  # Field name (e.g., "ftype")
    cards_with_field: List[int] = None  # Card indices containing this field

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "DiscriminatorConfig":
        return cls(
            field=data["field"],
            cards_with_field=data.get("cards-with-field"),
        )


@dataclass
class InternalConditional:
    """Configuration for a conditional on a card within a CardSet.

    Unlike regular conditional-card which references the parent keyword's fields,
    internal conditionals reference fields within the CardSet item itself (self.ftype, etc).
    """

    index: int  # Card index within the card set (0-based after mapping)
    func: str  # Conditional function (e.g., "self.ftype == 1")

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "InternalConditional":
        return cls(
            index=data["index"],
            func=data["func"],
        )


@dataclass
class SharedFieldConfig:
    """Configuration for a field shared across multiple mutually-exclusive cards.

    When the same field (like ftype) appears in multiple cards, we need
    property accessors that read from the active card and set on all cards.
    This is distinct from the shared_field handler which handles fields
    shared across base/option cards.
    """

    name: str  # Field name (e.g., "ftype")
    card_indices: List[int]  # Card indices containing this field
    source_index: Optional[int] = None  # Card index to use for metadata (type, help); defaults to first

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SharedFieldConfig":
        card_indices = data["card-indices"]
        source_index = data.get("source-index", card_indices[0] if card_indices else 0)
        return cls(
            name=data["name"],
            card_indices=card_indices,
            source_index=source_index,
        )


@dataclass
class CardSetSettings:
    """Configuration for grouping cards into repeatable sets."""

    name: str
    source_indices: List[int]
    length_func: Optional[str] = None
    active_func: Optional[str] = None
    options: Optional[List[Dict[str, Any]]] = None
    discriminator: Optional[DiscriminatorConfig] = None
    internal_conditionals: Optional[List[InternalConditional]] = None
    shared_fields: Optional[List[SharedFieldConfig]] = None
    item_name: Optional[str] = None  # Singular name for items (e.g., "fiber_family")
    items_name: Optional[str] = None  # Plural name for items (e.g., "fiber_families")

    @property
    def bounded(self) -> bool:
        """CardSet is bounded if length_func is defined (size controlled externally)."""
        return self.length_func is not None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "CardSetSettings":
        discriminator = None
        if "discriminator" in data:
            discriminator = DiscriminatorConfig.from_dict(data["discriminator"])

        internal_conditionals = None
        if "internal-conditionals" in data:
            internal_conditionals = [InternalConditional.from_dict(c) for c in data["internal-conditionals"]]

        shared_fields = None
        if "shared-fields" in data:
            shared_fields = [SharedFieldConfig.from_dict(f) for f in data["shared-fields"]]

        return cls(
            name=data["name"],
            source_indices=data["source-indices"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
            options=data.get("options"),
            discriminator=discriminator,
            internal_conditionals=internal_conditionals,
            shared_fields=shared_fields,
            item_name=data.get("item-name"),
            items_name=data.get("items-name"),
        )


@handler(
    name="card-set",
    dependencies=["reorder-card", "add-option", "series-card", "table-card", "conditional-card"],
    description="Creates reusable card sets with dynamic sizing and optional sub-options",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Name of the card set"},
                "source-indices": {
                    "type": "array",
                    "items": {"type": "integer"},
                    "description": "Card indices to include in the set",
                },
                "source-options": {
                    "type": "array",
                    "items": {"type": "integer"},
                    "description": "Option indices to include in the set",
                },
                "target-index": {"type": "integer", "description": "Where to insert the set card"},
                "target-name": {"type": "string", "description": "Target option name (if inserting in option)"},
                "length-func": {"type": "string", "description": "Function to compute set length"},
                "active-func": {"type": "string", "description": "Function to determine if set is active"},
            },
            "required": ["name", "source-indices", "target-index"],
        },
    },
    output_description="Adds 'card_sets' dict and card insertions, marks source cards for removal",
)
class CardSetHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Creates reusable card sets with dynamic sizing.

    This handler enables complex card structures by grouping multiple cards into
    a reusable set that can be repeated. Card sets can include their own options
    and are dynamically sized at runtime.

    CRITICAL IMPLEMENTATION NOTES:
    1. **Reference Semantics**: This handler MUST append card references (not copies)
       to source_cards. Later handlers (e.g., conditional-card) modify these same
       card objects, so the modifications appear in both the main cards list AND
       the card-set's source_cards list. Using deepcopy breaks this behavior.

    2. **Handler Ordering**: This handler traditionally runs BEFORE conditional-card,
       series-card, and table-card. The source-indices refer to positions AFTER
       reorder-card has run but BEFORE transformations are applied. Later handlers
       modify the cards in-place via shared references.

    3. **Index Rewriting**: Cards store their original index in 'source_index' and
       get a new sequential 'index' within the card-set (0, 1, 2, ...). The
       'mark_for_removal' flag prevents them from appearing in the main cards list.

    Input Settings Example:
        [
            {
                "name": "LoadSet",
                "source-indices": [1, 2, 3],
                "source-options": [0],  # Include option 0 in the set
                "target-index": 1,
                "target-name": "",  # Empty = base keyword
                "length-func": "len(self.load_sets)",
                "active-func": "self.nsets > 0"
            }
        ]

    Output Modification:
        - Creates kwd_data["card_sets"] = {"sets": [...], "options": bool}
        - Each set contains: name, source_cards (with updated indices)
        - Optionally includes set-specific options
        - Adds insertion to kwd_data["card_insertions"]
        - Marks source cards/options with "mark_for_removal" = 1
    """

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[typing.Dict[str, typing.Any]]:
        """Keep dict settings for card-set due to complex optional fields not in CardSetSettings."""
        return settings

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Create card sets from source cards and options.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of card set definitions

        Raises:
            Exception: If more than one default target (empty target-name) is specified
        """
        typed_settings = self._parse_settings(settings)
        card_sets = []
        has_options = False
        default_target = 0

        for card_settings in typed_settings:
            card_set = {"name": card_settings["name"], "source_cards": []}
            target_name = card_settings.get("target-name", "")
            if target_name == "":
                default_target = default_target + 1
                if default_target > 1:
                    raise Exception("Currently only one card set on the base keyword is supported!")
            card_index = -1  # Initialize to handle empty source-indices case
            for card_index, source_index in enumerate(card_settings["source-indices"]):
                source_card = kwd_data.cards[source_index]
                source_card["source_index"] = source_card["index"]
                source_card["index"] = card_index
                source_card["mark_for_removal"] = 1
                card_set["source_cards"].append(source_card)

            if "source-options" in card_settings:
                has_options = True
                for option_index in card_settings["source-options"]:
                    source_option = kwd_data.options[int(option_index)]
                    option = copy.deepcopy(source_option)
                    for card in option["cards"]:
                        card_index += 1
                        card["index"] = card_index
                    if "options" not in card_set:
                        card_set["options"] = [option]
                    else:
                        card_set["options"].extend([option])
                    source_option["mark_for_removal"] = 1

            # Add discriminator configuration if present
            if "discriminator" in card_settings:
                disc_config = DiscriminatorConfig.from_dict(card_settings["discriminator"])
                card_set["discriminator"] = {
                    "field": disc_config.field,
                    "cards_with_field": disc_config.cards_with_field,
                }

            # Add internal conditionals if present
            if "internal-conditionals" in card_settings:
                internal_conds = [InternalConditional.from_dict(c) for c in card_settings["internal-conditionals"]]
                card_set["internal_conditionals"] = [{"index": c.index, "func": c.func} for c in internal_conds]
                # Apply conditionals to the source cards
                for cond in internal_conds:
                    if cond.index < len(card_set["source_cards"]):
                        card_set["source_cards"][cond.index]["func"] = cond.func

            # Add shared fields if present (fields shared across mutually-exclusive cards)
            if "shared-fields" in card_settings:
                shared = [SharedFieldConfig.from_dict(f) for f in card_settings["shared-fields"]]
                shared_fields_data = []
                for shared_field in shared:
                    # Get metadata from source_index card, mark all as redundant
                    field_type = "int"
                    field_help = shared_field.name
                    for card_idx in shared_field.card_indices:
                        if card_idx < len(card_set["source_cards"]):
                            source_card = card_set["source_cards"][card_idx]
                            for field in source_card.get("fields", []):
                                if field.get("name", "").lower() == shared_field.name.lower():
                                    # TODO: Type normalization should happen earlier in pipeline
                                    if isinstance(field, Field):
                                        field.normalize()
                                        if card_idx == shared_field.source_index:
                                            field_type = field.type
                                            field_help = field.help or shared_field.name
                                        field.redundant = True
                                    else:
                                        from keyword_generation.generators.class_generator import (
                                            _normalize_field_dict,
                                        )

                                        _normalize_field_dict(field)
                                        if card_idx == shared_field.source_index:
                                            field_type = field.get("type", "int")
                                            field_help = field.get("help", shared_field.name)
                                        field["redundant"] = True
                    shared_fields_data.append(
                        {
                            "name": shared_field.name,
                            "card_indices": shared_field.card_indices,
                            "type": field_type,
                            "help": field_help,
                        }
                    )
                card_set["shared_fields"] = shared_fields_data

            # Bounded is inferred from length-func: if set, CardSet size is controlled externally
            is_bounded = bool(card_settings.get("length-func"))
            card_set["bounded"] = is_bounded

            # Set item names with defaults
            item_name = card_settings.get("item-name", "set")
            items_name = card_settings.get("items-name", "sets")
            card_set["item_name"] = item_name
            card_set["items_name"] = items_name

            length_func = card_settings.get("length-func", "")
            card = {
                "set": {"name": card_settings["name"]},
                "fields": [],
                "index": card_settings["target-index"],
                "target_index": card_settings["target-index"],
                "length_func": length_func,
                "active_func": card_settings.get("active-func", ""),
                "bounded": is_bounded,
                "item_name": item_name,
                "items_name": items_name,
            }
            target_name = card_settings.get("target-name", "")
            target_index = card_settings["target-index"]
            insertion = gen.Insertion(target_index, target_name, card)
            kwd_data.card_insertions.append(insertion)
            card_sets.append(card_set)
        kwd_data.card_sets = {"sets": card_sets, "options": has_options}

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
