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

"""Card Set Handler: Creates reusable card collections with dynamic sizing.

This handler enables keywords to define card sets - groups of cards that can be
repeated with variable length, optionally including their own sub-options.

Uses label-based references (source-refs, target-ref) for all card addressing.
"""

import copy
from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict, List, Optional

import keyword_generation.data_model as gen
from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


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
    """Configuration for grouping cards into repeatable sets.

    Uses label-based addressing (source-refs, target-ref) for all references.
    """

    name: str
    source_refs: List[str] = None  # Label references for source cards
    target_ref: str = None  # Label reference for target insertion point
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

    def resolve_source_indices(self, registry: LabelRegistry, cards: List[Any]) -> List[int]:
        """Resolve source references to indices."""
        if not self.source_refs:
            raise ValueError(f"CardSetSettings '{self.name}' must have source-refs")
        return [registry.resolve_index(ref, cards) for ref in self.source_refs]

    def resolve_target_index(self, registry: LabelRegistry, cards: List[Any]) -> int:
        """Resolve target reference to index."""
        if not self.target_ref:
            raise ValueError(f"CardSetSettings '{self.name}' must have target-ref")
        return registry.resolve_index(self.target_ref, cards)

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
            source_refs=data.get("source-refs"),
            target_ref=data.get("target-ref"),
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
    description="Creates reusable card sets with dynamic sizing and optional sub-options",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Name of the card set"},
                "source-refs": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "Label references for cards to include",
                },
                "source-options": {
                    "type": "array",
                    "items": {"type": "integer"},
                    "description": "Option indices to include in the set",
                },
                "target-ref": {"type": "string", "description": "Label reference for insertion point"},
                "target-name": {"type": "string", "description": "Target option name (if inserting in option)"},
                "length-func": {"type": "string", "description": "Function to compute set length"},
                "active-func": {"type": "string", "description": "Function to determine if set is active"},
            },
            "required": ["name"],
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

    Uses label-based references (source-refs, target-ref) for all card addressing.

    CRITICAL IMPLEMENTATION NOTES:
    1. **Reference Semantics**: This handler MUST append card references (not copies)
       to source_cards. Later handlers (e.g., conditional-card) modify these same
       card objects, so the modifications appear in both the main cards list AND
       the card-set's source_cards list. Using deepcopy breaks this behavior.

    2. **Handler Ordering**: This handler traditionally runs BEFORE conditional-card,
       series-card, and table-card. The source refs/indices refer to positions AFTER
       reorder-card has run but BEFORE transformations are applied. Later handlers
       modify the cards in-place via shared references.

    3. **Index Rewriting**: Cards store their original index in 'source_index' and
       get a new sequential 'index' within the card-set (0, 1, 2, ...). The
       'mark_for_removal' flag prevents them from appearing in the main cards list.

    Input Settings Example:
        [
            {
                "name": "LoadSet",
                "source-refs": ["load_card_1", "load_card_2", "load_card_3"],
                "source-options": [0],  # Include option 0 in the set
                "target-ref": "load_card_1",
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
            kwd_data: Complete keyword data
            settings: List of card set definitions

        Raises:
            Exception: If more than one default target (empty target-name) is specified
            ValueError: If label registry is not available when using refs
        """
        # Get registry for label resolution
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError("card-set handler requires a label registry. Ensure labels are defined in manifest.")

        card_sets = []
        has_options = False
        default_target = 0

        for card_settings_dict in settings:
            # Parse into typed settings
            card_settings = CardSetSettings.from_dict(card_settings_dict)

            card_set = {"name": card_settings.name, "source_cards": []}
            target_name = card_settings_dict.get("target-name", "")
            if target_name == "":
                default_target = default_target + 1
                if default_target > 1:
                    raise Exception("Currently only one card set on the base keyword is supported!")

            # Resolve source indices from refs
            source_indices = card_settings.resolve_source_indices(registry, kwd_data.cards)
            logger.debug(f"CardSet '{card_settings.name}': resolved source indices = {source_indices}")

            card_index = -1  # Initialize to handle empty source case
            for card_index, source_index in enumerate(source_indices):
                source_card = kwd_data.cards[source_index]
                source_card["source_index"] = source_card["index"]
                source_card["index"] = card_index
                source_card["mark_for_removal"] = 1
                card_set["source_cards"].append(source_card)

            if "source-options" in card_settings_dict:
                has_options = True
                for option_index in card_settings_dict["source-options"]:
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
            if card_settings.discriminator:
                card_set["discriminator"] = {
                    "field": card_settings.discriminator.field,
                    "cards_with_field": card_settings.discriminator.cards_with_field,
                }

            # Add internal conditionals if present
            if card_settings.internal_conditionals:
                card_set["internal_conditionals"] = [
                    {"index": c.index, "func": c.func} for c in card_settings.internal_conditionals
                ]
                # Apply conditionals to the source cards
                for cond in card_settings.internal_conditionals:
                    if cond.index < len(card_set["source_cards"]):
                        card_set["source_cards"][cond.index]["func"] = cond.func

            # Add shared fields if present (fields shared across mutually-exclusive cards)
            if card_settings.shared_fields:
                shared_fields_data = []
                for shared_field in card_settings.shared_fields:
                    # Get metadata from source_index card, mark all as redundant
                    field_type = "int"
                    field_help = shared_field.name
                    for card_idx in shared_field.card_indices:
                        if card_idx < len(card_set["source_cards"]):
                            source_card = card_set["source_cards"][card_idx]
                            for field in source_card.get("fields", []):
                                if field.get("name", "").lower() == shared_field.name.lower():
                                    field.normalize()
                                    if card_idx == shared_field.source_index:
                                        field_type = field.type
                                        field_help = field.help or shared_field.name
                                    field.redundant = True
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
            is_bounded = card_settings.bounded
            card_set["bounded"] = is_bounded

            # Set item names with defaults
            item_name = card_settings.item_name or "set"
            items_name = card_settings.items_name or "sets"
            card_set["item_name"] = item_name
            card_set["items_name"] = items_name

            # Resolve target index from ref
            target_index = card_settings.resolve_target_index(registry, kwd_data.cards)
            logger.debug(f"CardSet '{card_settings.name}': resolved target index = {target_index}")

            length_func = card_settings.length_func or ""
            card = {
                "set": {"name": card_settings.name},
                "fields": [],
                "index": target_index,
                "target_index": target_index,
                "length_func": length_func,
                "active_func": card_settings.active_func or "",
                "bounded": is_bounded,
                "item_name": item_name,
                "items_name": items_name,
            }
            target_name = card_settings_dict.get("target-name", "")
            insertion = gen.Insertion(target_index, target_name, card)
            kwd_data.card_insertions.append(insertion)
            card_sets.append(card_set)
        kwd_data.card_sets = {"sets": card_sets, "options": has_options}
