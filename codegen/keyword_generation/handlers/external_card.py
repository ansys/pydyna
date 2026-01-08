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

Uses label-based references (ref) for card addressing.
"""

from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.metadata import ExternalCardMetadata, MixinImport
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.base_settings import LabelRefSettings
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class ExternalCardSettings(LabelRefSettings):
    """Configuration for external card implementation.

    Uses label-based addressing (ref) for card references.
    """

    card_source: str  # Module to import from
    card_name: str  # External card class name
    mixin_name: str  # Mixin class name to add to keyword

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ExternalCardSettings":
        return cls(
            ref=data["ref"],
            card_source=data["card"]["source"],
            card_name=data["card"]["card-name"],
            mixin_name=data["mixin"],
        )


@handler(
    name="external-card-implementation",
    description="Integrates externally-defined cards and mixins into keyword classes",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Label reference for the card"},
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
            "required": ["ref", "card", "mixin"],
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

    Uses label-based references (ref) for card addressing.

    Input Settings Example:
        [
            {
                "ref": "include_card",
                "card": {
                    "source": "include_card",
                    "card-name": "IncludeCard"
                },
                "mixin": "IncludeCardMixin"
            },
            {
                "ref": "title_card",
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
    def _parse_settings(cls, settings: typing.List[typing.Dict[str, typing.Any]]) -> typing.List[ExternalCardSettings]:
        """Parse dict settings into typed ExternalCardSettings objects."""
        return [ExternalCardSettings.from_dict(s) for s in settings]

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

        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "ExternalCardHandler requires LabelRegistry on kwd_data.label_registry. "
                "Ensure 'labels' are defined in manifest for this keyword."
            )

        kwd_data.mixins = []
        kwd_data.mixin_imports = []
        for setting in typed_settings:
            card_index = setting.resolve_index(registry, kwd_data.cards)
            logger.debug(
                "Processing external card: ref=%s -> index=%d, card=%s, mixin=%s",
                setting.ref,
                card_index,
                setting.card_name,
                setting.mixin_name,
            )
            kwd_data.mixins.append(setting.mixin_name)
            kwd_data.mixin_imports.append(
                MixinImport(source=setting.card_source, names=[setting.card_name, setting.mixin_name])
            )
            external_card = kwd_data.cards[card_index]
            external_card["external"] = ExternalCardMetadata(name=setting.card_name)
