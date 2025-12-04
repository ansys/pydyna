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
Skip Card Handler: Marks cards for deletion from generated code.

Cards marked for deletion are removed from the final keyword structure after
all handlers have processed.
"""

from typing import Any, Dict

import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="skip-card",
    dependencies=[],
    description="Marks cards for removal from the generated keyword class",
    input_schema={
        "oneOf": [
            {"type": "integer", "description": "Single card index to skip"},
            {"type": "array", "items": {"type": "integer"}, "description": "List of card indices to skip"},
        ]
    },
    output_description="Sets 'mark_for_removal' flag on specified cards",
)
class SkipCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Marks cards to be excluded from code generation.

    This handler sets a 'mark_for_removal' flag on cards. Actual deletion
    happens later in the generation pipeline via _delete_marked_indices().

    Input Settings Example:
        [0, 1, 2]  # Skip cards at indices 0, 1, and 2
        or
        1  # Skip single card at index 1

    Output Modification:
        Sets card["mark_for_removal"] = 1 for each specified index
    """

    def handle(self, kwd_data: Any, settings: Dict[str, Any]) -> None:
        """
        Mark specified cards for removal.

        Args:
            kwd_data: KeywordData instance (or dict during transition)
            settings: Dict from manifest.json with handler config (may contain card indices)
        """
        # Extract the actual card indices from settings
        # Settings may be passed differently depending on manifest structure
        if isinstance(settings, int):
            skipped_card_indices = [settings]
        elif isinstance(settings, list):
            skipped_card_indices = settings
        else:
            # If settings is a dict, look for a key that contains the indices
            # This handles various manifest.json structures
            skipped_card_indices = settings.get("indices", [])

        for index in skipped_card_indices:
            kwd_data.cards[index]["mark_for_removal"] = 1

    def post_process(self, kwd_data: Any) -> None:
        """No post-processing required."""
        pass
