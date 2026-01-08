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
Shared Field Handler: Marks fields that appear across multiple cards.

This handler identifies fields with the same name across different cards and marks
them as shared, preventing duplication and enabling cross-card field references.
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class SharedFieldSettings:
    """Configuration for field sharing across cards."""

    field_name: str
    card_indices: List[int]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SharedFieldSettings":
        return cls(field_name=data["field-name"], card_indices=data["card-indices"])


def do_negative_shared_fields(kwd_data: typing.Any):
    """
    Process shared fields with negative indices (option cards).

    Negative indices are used to reference cards in optional groups, which are not
    available during the main handle() phase. This function runs in post_process
    after all options are fully defined.

    Critical: Searches option cards FIRST (regardless of index value) because option
    cards can have indices that overlap with base card ranges. For example, a keyword
    with 3 base cards and an option card at index=2 would incorrectly trigger the
    assertion if we checked `index >= num_cards` first.

    Args:
        kwd_data: KeywordData instance with options populated (may be OptionGroup
                  instances or dicts during transition)
    """
    negative_shared_fields = kwd_data.negative_shared_fields
    num_cards = len(kwd_data.cards)
    options = kwd_data.options or []

    option_cards = []
    for option in options:
        # option may be OptionGroup instance or dict (transitional)
        cards = option.cards if hasattr(option, "cards") else option["cards"]
        option_cards.extend(cards)
    for setting in negative_shared_fields:
        indices = [-i for i in setting["cards"]]
        fields = []
        for index in indices:
            # CRITICAL: Search option cards first, regardless of index value.
            # Option cards may have ANY index (not necessarily >= num_cards).
            # Example: keyword with 3 base cards + option card at index=2
            found_in_options = False
            for option in options:
                # option may be OptionGroup instance or dict (transitional)
                cards = option.cards if hasattr(option, "cards") else option["cards"]
                for card in cards:
                    if card["index"] == index:
                        for field in card["fields"]:
                            if field["name"] == setting["name"]:
                                fields.append(field)
                                found_in_options = True

            # If not found in options, check base cards
            if not found_in_options and index < num_cards:
                assert False, "TODO - support negative indices for shared fields for non-options"
        if len(fields) <= 1:
            import logging

            logging.warning(
                f"Shared field skipped (insufficient occurrences): "
                f"keyword={kwd_data.keyword}.{kwd_data.subkeyword}, field={setting['name']}, "
                f"found {len(fields)} fields, expected >= 2"
            )
            continue  # Skip this shared field configuration
        if not setting["applied_card_indices"]:
            fields[0]["card_indices"] = indices
        for field in fields[1:]:
            field["redundant"] = True


def handle_shared_field(kwd_data, settings):
    """
    Mark fields as shared across multiple cards.

    Processes positive indices immediately (base keyword cards) and defers
    negative indices (option cards) to post_process phase.

    Args:
        kwd_data: Complete keyword data dictionary
        settings: List of shared field configurations
    """
    # positive card indices are applied in handler
    # negative card indices are marked and handled after transformations (after_handle)
    for setting in settings:
        setting["applied_card_indices"] = False
        cards = setting["cards"]
        num_positive = len([c for c in cards if c > 0])

        # either or - we cannot support some positive some negative in the same setting now
        assert num_positive == 0 or num_positive == len(cards)
        if num_positive > 0:
            fields = []
            for card in kwd_data.cards:
                for field in card["fields"]:
                    if field["name"] == setting["name"]:
                        fields.append(field)
            assert len(fields) > 1
            fields[0]["card_indices"] = cards
            setting["applied_card_indices"] = True
            for field in fields[1:]:
                field["redundant"] = True
        else:
            if len(kwd_data.negative_shared_fields) == 0:
                kwd_data.negative_shared_fields = []
            kwd_data.negative_shared_fields.append(setting)


@handler(
    name="shared-field",
    dependencies=["reorder-card"],
    description="Marks fields shared across multiple cards to prevent duplication",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "name": {"type": "string", "description": "Name of the shared field"},
                "cards": {
                    "type": "array",
                    "items": {"type": "integer"},
                    "description": "Card indices where field appears (negative for option cards)",
                },
            },
            "required": ["name", "cards"],
        },
    },
    output_description=(
        "Adds 'card_indices' to first field, marks duplicates as 'redundant', " "may add 'negative_shared_fields'"
    ),
)
class SharedFieldHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Marks fields that appear in multiple cards as shared.

    This handler identifies fields with the same name across different cards and
    marks them appropriately to avoid duplication. The first occurrence gets a
    'card_indices' list, and subsequent occurrences are marked 'redundant'.

    Supports both positive indices (base keyword cards, processed immediately) and
    negative indices (option cards, processed in post_process after options exist).

    Input Settings Example:
        [
            {
                "name": "pid",
                "cards": [0, 1, 2]  # Positive: base cards
            },
            {
                "name": "sid",
                "cards": [-1, -2]  # Negative: option cards
            }
        ]

    Output Modification:
        For positive indices (immediate):
        - First field with name gets: field["card_indices"] = [0, 1, 2]
        - Other fields get: field["redundant"] = True

        For negative indices (deferred to post_process):
        - Stored in kwd_data["negative_shared_fields"] for later processing
        - Processed after options are available
    """

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[typing.Dict[str, typing.Any]]:
        """Keep dict settings for shared-field - uses 'name' and 'cards' directly."""
        return settings

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Mark shared fields, handling positive indices immediately.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of shared field configurations
        """
        typed_settings = self._parse_settings(settings)
        return handle_shared_field(kwd_data, typed_settings)

    def post_process(self, kwd_data: KeywordData) -> None:
        """
        Process deferred negative-index shared fields.

        Runs after all handlers complete to ensure option cards are fully defined.
        """
        return do_negative_shared_fields(kwd_data)
