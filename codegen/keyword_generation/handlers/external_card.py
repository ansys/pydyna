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
External Card Handler: Integrates externally-defined card implementations.

This handler enables keywords to reuse card implementations from other modules,
supporting code reuse through mixins and external card references.
"""

from dataclasses import dataclass
import typing
from typing import Any, Dict

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.metadata import ExternalCardMetadata, MixinImport
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler


@dataclass
class ExternalCardSettings:
    """Configuration for external card implementation."""

    index: int
    name: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ExternalCardSettings":
        return cls(index=data["index"], name=data["name"])


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

    @classmethod
    def _parse_settings(
        cls, settings: typing.List[typing.Dict[str, typing.Any]]
    ) -> typing.List[typing.Dict[str, typing.Any]]:
        """Keep dict settings for external-card - nested card structure in manifest."""
        return settings

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Configure external card imports and mixins.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of external card configurations
        """
        typed_settings = self._parse_settings(settings)
        kwd_data.mixins = []
        kwd_data.mixin_imports = []
        for setting in typed_settings:
            card_name = setting["card"]["card-name"]
            card_index = setting["index"]
            card_source = setting["card"]["source"]
            mixin_name = setting["mixin"]
            kwd_data.mixins.append(mixin_name)
            kwd_data.mixin_imports.append(MixinImport(source=card_source, names=[card_name, mixin_name]))
            external_card = kwd_data.cards[card_index]
            external_card["external"] = ExternalCardMetadata(name=card_name)

    def post_process(self, kwd_data: KeywordData) -> None:
        """No post-processing required."""
        pass
