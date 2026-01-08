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
Conditional Card Handler: Adds conditional rendering logic to cards.

This handler enables cards to be rendered only when specific conditions are met,
supporting dynamic card structures based on field values.

Uses label-based card references:
    {"ref": "thickness_card", "func": "self.elform == 1"}

Labels must be defined in the keyword's labels section or auto-generated.
"""

from dataclasses import dataclass
import logging
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.base_settings import LabelRefSettings, parse_settings_list
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class ConditionalCardSettings(LabelRefSettings):
    """Configuration for conditional card inclusion.

    Attributes:
        ref: Label-based reference (inherited from LabelRefSettings)
        func: Python expression that determines if the card should be rendered
    """

    func: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ConditionalCardSettings":
        """Create settings from dict.

        Args:
            data: Dict with 'ref' and 'func' keys

        Returns:
            ConditionalCardSettings instance

        Raises:
            KeyError: If 'ref' or 'func' is missing
        """
        return cls(ref=data["ref"], func=data["func"])


@handler(
    name="conditional-card",
    description="Adds conditional rendering logic to cards based on field values",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Card label reference"},
                "func": {"type": "string", "description": "Python expression for condition"},
            },
            "required": ["ref", "func"],
        },
    },
    output_description="Adds 'func' property to card dict containing conditional expression",
)
class ConditionalCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Makes cards conditional based on field values.

    This handler adds a 'func' property to cards that contains a Python expression.
    The template uses this to generate conditional logic that determines whether
    the card should be rendered in the output.

    Input Settings Example:
        [{"ref": "thickness_card", "func": "self.iauto == 3"}]

    Output Modification:
        Adds 'func' key to card dict:
        card["func"] = "self.iauto == 3"

    Requires:
        - LabelRegistry must be available on kwd_data.label_registry
        - Labels must be defined in the manifest 'labels' section
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: List[Dict[str, Any]],
    ) -> None:
        """
        Add conditional logic to specified cards.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts with 'ref' and 'func' keys

        Raises:
            ValueError: If label_registry is not available on kwd_data
            UndefinedLabelError: If a referenced label is not defined
        """
        typed_settings = parse_settings_list(ConditionalCardSettings, settings)
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "ConditionalCardHandler requires LabelRegistry on kwd_data.label_registry. "
                "Ensure labels are defined in the manifest."
            )

        for setting in typed_settings:
            index = setting.resolve_index(registry, kwd_data.cards)
            logger.debug(f"Adding conditional func to card {index} (ref='{setting.ref}'): {setting.func}")
            card = kwd_data.cards[index]
            card["func"] = setting.func
