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
"""

from dataclasses import dataclass
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import KeywordData
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class SkipCardSettings:
    """Configuration for marking a card for removal."""

    index: int

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SkipCardSettings":
        return cls(index=data["index"])


@handler(
    name="skip-card",
    dependencies=[],
    description="Marks cards for removal from the generated keyword class",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {"index": {"type": "integer"}},
            "required": ["index"],
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
            kwd_data: KeywordData instance (or dict during transition)
            settings: List of dicts, each containing a single card index to skip
        """
        # Parse settings into typed instances
        typed_settings = self._parse_settings(settings)

        # Use attribute access on typed settings
        for setting in typed_settings:
            kwd_data.cards[setting.index]["mark_for_removal"] = 1

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
