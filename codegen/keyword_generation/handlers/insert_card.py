# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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
Insert Card Handler: Inserts new cards at specified positions.

Queues cards for insertion during the post-handler phase. Actual insertion
happens after all handlers run via _do_insertions().
"""

import typing

import keyword_generation.data_model as gen
from keyword_generation.data_model import get_card
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="insert-card",
    dependencies=["reorder-card"],
    description="Queues new cards for insertion at specified indices",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer"},
                "card": {"type": "object"},
            },
            "required": ["index", "card"],
        },
    },
    output_description="Appends Insertion objects to kwd_data['card_insertions'] list",
)
class InsertCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Queues cards for insertion.

    Does not insert cards immediately. Instead, creates Insertion objects
    that are processed later by _do_insertions() in the generation pipeline.

    Input Settings Example:
        [
            {
                "index": 0,
                "card": {
                    "source": "additional-cards",
                    "card-name": "CONTACT_CARD_1"
                }
            }
        ]

    Output Modification:
        Appends Insertion(target_index, "", card) to kwd_data["card_insertions"]
    """

    def handle(self, kwd_data: typing.Any, settings: typing.List[typing.Dict[str, typing.Any]]) -> None:
        """
        Queue cards for insertion.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of {"index", "card"} dicts
        """
        for card_settings in settings:
            index = card_settings["index"]
            card = get_card(card_settings["card"])
            insertion = gen.Insertion(index, "", card)
            kwd_data.card_insertions.append(insertion)

    def post_process(self, kwd_data: typing.Any) -> None:
        """No post-processing required."""
        pass
