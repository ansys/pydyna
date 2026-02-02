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
from keyword_generation.handlers.base_settings import LabelRefSettings, parse_settings_list
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class SkipCardSettings(LabelRefSettings):
    """Configuration for marking a card for removal.

    Inherits ref and resolve_index from LabelRefSettings.
    """

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SkipCardSettings":
        return cls(ref=data["ref"])


@handler(
    name="skip-card",
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

    def handle(self, kwd_data: KeywordData, settings: List[Dict[str, Any]]) -> None:
        """
        Mark specified cards for removal.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts with 'ref' key for label reference
        """
        typed_settings = parse_settings_list(SkipCardSettings, settings)
        registry = kwd_data.label_registry

        if registry is None:
            raise ValueError("skip-card handler requires label_registry to be initialized")

        for setting in typed_settings:
            index = setting.resolve_index(registry, kwd_data.cards)
            logger.debug(f"Marking card {index} for removal (ref={setting.ref})")
            kwd_data.cards[index]["mark_for_removal"] = 1
