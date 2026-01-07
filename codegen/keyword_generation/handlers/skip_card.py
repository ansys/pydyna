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

Supports both legacy index-based and new label-based card references:
- Legacy: {"index": 6}
- Label-based: {"ref": "discrete_beam_dof_card"}
"""

from dataclasses import dataclass
import logging
from typing import Any, Dict, List, Optional

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class SkipCardSettings:
    """Configuration for marking a card for removal.

    Supports hybrid referencing:
    - index: Legacy integer index (direct position in cards list)
    - ref: Label-based reference (resolved via LabelRegistry)

    Only one of index or ref should be provided.
    """

    index: Optional[int] = None
    ref: Optional[str] = None

    def __post_init__(self):
        """Validate that exactly one reference type is provided."""
        if self.index is None and self.ref is None:
            raise ValueError("SkipCardSettings requires either 'index' or 'ref'")
        if self.index is not None and self.ref is not None:
            raise ValueError("SkipCardSettings cannot have both 'index' and 'ref'")

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SkipCardSettings":
        return cls(index=data.get("index"), ref=data.get("ref"))

    def resolve_index(self, registry: Optional[LabelRegistry]) -> int:
        """Resolve to a concrete card index.

        Args:
            registry: LabelRegistry for resolving label references.
                      Required if using ref-based settings.

        Returns:
            Integer index into kwd_data.cards

        Raises:
            ValueError: If ref is used but registry is None
            UndefinedLabelError: If ref label is not found
        """
        if self.index is not None:
            return self.index
        if self.ref is not None:
            if registry is None:
                raise ValueError(f"Cannot resolve ref '{self.ref}' without LabelRegistry")
            return registry.resolve_index(self.ref)
        raise ValueError("SkipCardSettings has neither index nor ref")


@handler(
    name="skip-card",
    dependencies=[],
    description="Marks cards for removal from the generated keyword class",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer"},
                "ref": {"type": "string"},
            },
            "oneOf": [{"required": ["index"]}, {"required": ["ref"]}],
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

        Supports both legacy index-based and label-based references:
        - {"index": 6} - direct index reference
        - {"ref": "discrete_beam_card"} - label reference (resolved via kwd_data.label_registry)

        Args:
            kwd_data: KeywordData instance containing cards and label_registry
            settings: List of dicts, each containing either 'index' or 'ref'
        """
        typed_settings = self._parse_settings(settings)
        registry = kwd_data.label_registry

        for setting in typed_settings:
            index = setting.resolve_index(registry)
            logger.debug(f"Marking card {index} for removal " f"(ref={setting.ref}, index={setting.index})")
            kwd_data.cards[index]["mark_for_removal"] = 1

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
