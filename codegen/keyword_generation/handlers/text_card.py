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
Text Card Handler: Marks cards that hold unbounded free-form text.

Keywords whose content is read until the next ``*`` line (e.g. ``*COMMENT``,
``*DEFINE_FUNCTION``) need a single ``TextCard`` rather than fixed-width
``Card`` fields.  This handler wires that card type into the codegen pipeline.

Uses label-based card references:
    {"ref": "comment_card", "name": "comment"}

Labels must be defined in the keyword's labels section or auto-generated.
"""

from dataclasses import dataclass
import logging
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.metadata import TextCardMetadata
from keyword_generation.handlers.base_settings import LabelRefSettings, parse_settings_list
import keyword_generation.handlers.handler_base
from keyword_generation.handlers.handler_base import handler

logger = logging.getLogger(__name__)


@dataclass
class TextCardSettings(LabelRefSettings):
    """Configuration for a free-form text card.

    Parameters 
    ----------
    ref:
        Label-based reference (resolved via LabelRegistry)
    name: 
        Property name for the text content in the generated class
    """

    name: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "TextCardSettings":
        """Create settings from dict.

        Parameters
        ----------
        data: Dict[str, Any]
            Input settings dictionary with keys 'ref' and 'name'.

        Returns
        -------
        TextCardSettings instance

        Raises
        ------
        KeyError: If required keys are missing
        """
        return cls(
            ref=data["ref"],
            name=data["name"],
        )


@handler(
    name="text-card",
    description="Marks a card as a free-form TextCard for unbounded text content",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "ref": {"type": "string", "description": "Card label reference"},
                "name": {"type": "string", "description": "Property name for the text content"},
            },
            "required": ["ref", "name"],
        },
    },
    output_description="Sets kwd_data.text_card=True and adds TextCardMetadata to card['text']",
)
class TextCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """Marks a card as a free-form TextCard.

    Input Settings Example::

        [{"ref": "comment_card", "name": "comment"}]

    Output Modification:
        - Sets ``kwd_data.text_card = True``
        - Adds ``TextCardMetadata(name=...)`` to the card's ``text`` attribute

    Requires:
        - ``LabelRegistry`` must be available on ``kwd_data.label_registry``
        - Label must be defined in the manifest ``labels`` section
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: List[Dict[str, Any]],
    ) -> None:
        """Mark cards as TextCard instances.

        Args:
            kwd_data: KeywordData instance containing cards and label_registry.
            settings: List of dicts with 'ref' and 'name'.

        Raises
        ------
            ValueError: If label_registry is not available on kwd_data
            UndefinedLabelError: If a referenced label is not defined
        """
        typed_settings = parse_settings_list(TextCardSettings, settings)
        registry = kwd_data.label_registry
        if registry is None:
            raise ValueError(
                "TextCardHandler requires LabelRegistry on kwd_data.label_registry. "
                "Ensure labels are defined in the manifest."
            )

        kwd_data.text_card = True
        for card_settings in typed_settings:
            index = card_settings.resolve_index(registry, kwd_data.cards)
            logger.debug(
                f"Marking card {index} (ref='{card_settings.ref}') as text card "
                f"with property '{card_settings.name}'"
            )
            kwd_data.cards[index]["text"] = TextCardMetadata(name=card_settings.name)
