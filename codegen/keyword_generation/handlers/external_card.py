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
External Card Handler: Integrates externally-defined card implementations.

This handler enables keywords to reuse card implementations from other modules,
supporting code reuse through mixins and external card references.
"""

import typing

import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@handler(
    name="external-card-implementation",
    dependencies=["reorder-card"],
    description="Integrates externally-defined cards and mixins into keyword classes",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "index": {"type": "integer", "description": "Card index to replace with external card"},
                "card": {
                    "type": "object",
                    "properties": {
                        "source": {"type": "string", "description": "Module to import from"},
                        "card-name": {"type": "string", "description": "External card class name"},
                    },
                    "required": ["source", "card-name"],
                },
                "mixin": {"type": "string", "description": "Mixin class name to add to keyword"},
            },
            "required": ["index", "card", "mixin"],
        },
    },
    output_description="Adds 'mixins' and 'mixin_imports' lists, adds 'external' dict to cards",
)
class ExternalCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Integrates externally-defined card implementations into keywords.

    This handler enables code reuse by allowing keywords to use card implementations
    from other modules. It sets up the necessary imports and mixins to integrate
    external cards into the generated keyword class.

    Input Settings Example:
        [
            {
                "index": 0,
                "card": {
                    "source": "include_card",
                    "card-name": "IncludeCard"
                },
                "mixin": "IncludeCardMixin"
            },
            {
                "index": 1,
                "card": {
                    "source": "common_cards",
                    "card-name": "TitleCard"
                },
                "mixin": "TitleCardMixin"
            }
        ]

    Output Modification:
        - Initializes kwd_data["mixins"] = ["IncludeCardMixin", "TitleCardMixin", ...]
        - Initializes kwd_data["mixin_imports"] = [
              {"source": "include_card", "names": ["IncludeCard", "IncludeCardMixin"]},
              ...
          ]
        - Adds card["external"] = {"name": "IncludeCard"} to each external card
    """

    def handle(self, kwd_data: typing.Any, settings: typing.Any) -> None:
        """
        Configure external card imports and mixins.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of external card configurations
        """
        kwd_data.mixins = []
        kwd_data.mixin_imports = []
        settings_list = typing.cast(typing.List[typing.Dict[str, typing.Any]], settings)
        for setting in settings_list:
            card_name = setting["card"]["card-name"]
            card_index = setting["index"]
            card_source = setting["card"]["source"]
            mixin_name = setting["mixin"]
            kwd_data.mixins.append(mixin_name)
            kwd_data.mixin_imports.append({"source": card_source, "names": [card_name, mixin_name]})
            external_card = kwd_data.cards[card_index]
            external_card["external"] = {"name": card_name}

    def post_process(self, kwd_data: typing.Any) -> None:
        """No post-processing required."""
        pass
