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
Skip Card Handler: Marks cards for deletion from generated code.

Cards marked for deletion are removed from the final keyword structure after
all handlers have processed.

Uses label-based card references:
    {"ref": "id_title_card"}
"""

from dataclasses import dataclass
import logging
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class SkipCardSettings:
    """Configuration for marking a card for removal."""

    ref: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SkipCardSettings":
        return cls(ref=data["ref"])

    def resolve_index(self, registry: LabelRegistry, cards: List[Any]) -> int:
        """Resolve label to a concrete card index.

        Args:
            registry: LabelRegistry for resolving label references.
            cards: The cards list to search for the card object.

        Returns:
            Integer index into kwd_data.cards

        Raises:
            UndefinedLabelError: If ref label is not found
        """
        return registry.resolve_index(self.ref, cards)


@handler(
    name="skip-card",
    dependencies=[],
    description="Marks cards for removal from the generated keyword class",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Label reference to card to skip"},
            },
            "required": ["ref"],
        },
    },
    output_description="Sets 'mark_for_removal' flag on specified cards",
)
class SkipCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """Marks cards for removal from the generated keyword class."""

    @classmethod
    def _parse_settings(cls, settings: List[Dict[str, Any]]) -> List[SkipCardSettings]:
        """Convert dict settings to typed SkipCardSettings instances."""
        return [SkipCardSettings.from_dict(s) for s in settings]

    def handle(self, kwd_data: KeywordData, settings: List[Dict[str, Any]]) -> None:
        """
        Mark specified cards for removal.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts with 'ref' key for label reference
        """
        typed_settings = self._parse_settings(settings)
        registry = kwd_data.label_registry

        if registry is None:
            raise ValueError("skip-card handler requires label_registry to be initialized")

        for setting in typed_settings:
            index = setting.resolve_index(registry, kwd_data.cards)
            logger.debug(f"Marking card {index} for removal (ref={setting.ref})")
            kwd_data.cards[index]["mark_for_removal"] = 1

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
