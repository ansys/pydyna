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
Table Card Handler: Marks cards that represent 2D tabular data.

Indicates that a card repeats to form a table structure (e.g., integration points).
The card repeats based on a length function.
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict, Optional

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.metadata import TableCardMetadata
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class TableCardSettings:
    """Configuration for marking a card as a repeating table structure."""

    index: int
    property_name: str
    length_func: Optional[str] = None
    active_func: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "TableCardSettings":
        return cls(
            index=data["index"],
            property_name=data["property-name"],
            length_func=data.get("length-func"),
            active_func=data.get("active-func"),
        )


@handler(
    name="table-card",
    dependencies=["reorder-card"],
    description="Marks cards as repeating table structures with dynamic row count",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer"},
                "property-name": {"type": "string"},
                "length-func": {"type": "string"},
                "active-func": {"type": "string"},
            },
            "required": ["index", "property-name"],
        },
    },
    output_description="Sets kwd_data['table']=True and adds 'table' dict to card",
)
class TableCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """Marks cards as repeating table structures with dynamic row count."""

    @classmethod
    def _parse_settings(cls, settings: typing.List[typing.Dict[str, typing.Any]]) -> typing.List[TableCardSettings]:
        """Convert dict settings to typed TableCardSettings instances."""
        return [TableCardSettings.from_dict(s) for s in settings]

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Mark cards as table structures.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of table card specifications
        """
        typed_settings = self._parse_settings(settings)
        kwd_data.table = True
        for card_settings in typed_settings:
            table_card = kwd_data.cards[card_settings.index]
            table_card["table"] = TableCardMetadata(
                name=card_settings.property_name,
                length_func=card_settings.length_func or "",
                active_func=card_settings.active_func or "",
            )

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
