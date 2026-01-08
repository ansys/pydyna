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

import copy
import json
import logging
import os
import typing

from keyword_generation.data_model.insertion import Insertion
from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import (
    CardNotFoundError,
    DuplicateLabelError,
    LabelError,
    LabelRegistry,
    UndefinedLabelError,
)

logger = logging.getLogger(__name__)

# Global instances - type annotations added for type checking
KWDM_INSTANCE: typing.Optional[typing.Any] = None  # KWDM type
MANIFEST: typing.Optional[typing.Dict[str, typing.Any]] = None
ADDITIONAL_CARDS: typing.Optional[typing.Any] = None  # AdditionalCards type

KWD_TO_ALIAS: typing.Dict[str, str] = {}
ALIAS_TO_KWD: typing.Dict[str, str] = {}


def get_card(setting: typing.Dict[str, str]) -> Card:
    """
    Get a card from either kwd-data or additional-cards sources.

    Returns a Card dataclass instance.
    """
    assert KWDM_INSTANCE is not None, "KWDM_INSTANCE not initialized"
    assert ADDITIONAL_CARDS is not None, "ADDITIONAL_CARDS not initialized"
    source = setting["source"]
    if source == "kwd-data":
        data = KWDM_INSTANCE.get_keyword_data_dict(setting["keyword-name"])
        card_dict = data[setting["card-index"]]
        return Card.from_dict(card_dict)

    if source == "additional-cards":
        card_dict = ADDITIONAL_CARDS[setting["card-name"]]
        return Card.from_dict(card_dict)

    raise Exception(f"Unknown card source: {source}")


def add_alias(keyword: str, alias: str) -> None:
    KWD_TO_ALIAS[keyword] = alias
    ALIAS_TO_KWD[alias] = keyword


def get_alias(keyword: str) -> typing.Optional[str]:
    return KWD_TO_ALIAS.get(keyword, None)


def get_aliased_by(keyword: str) -> typing.Optional[str]:
    return ALIAS_TO_KWD.get(keyword, None)


def is_aliased(keyword: str) -> bool:
    return keyword in ALIAS_TO_KWD.keys()


def _load_manifest(filename: str) -> typing.Dict[str, typing.Any]:
    with open(filename) as f:
        manifest = json.load(f)
    return manifest


class AdditionalCards:
    """
    Manages additional card definitions loaded from additional-cards.json.

    Keeps cards as dicts for now to avoid breaking template generation logic.
    """

    def __init__(self, filename: str) -> None:
        with open(filename) as f:
            self._cards: typing.Dict[str, typing.Dict[str, typing.Any]] = json.load(f)

    def __getitem__(self, name: str) -> typing.Dict[str, typing.Any]:
        """Return a copy of the additional card as a dict."""
        return copy.deepcopy(self._cards[name])


class KWDM:
    def __init__(self, filename: str) -> None:
        with open(filename, encoding="utf-8") as f:
            self._data: typing.Dict[str, typing.Any] = json.load(f)

    def get_keywords_list(self) -> typing.List[str]:
        return list(self._data.keys())

    def get_keyword_data_dict(self, name: str) -> typing.List[typing.Dict[str, typing.Any]]:
        return copy.deepcopy(self[name])

    def __getitem__(self, name: str) -> typing.List[typing.Dict[str, typing.Any]]:
        return self._data[name]


def load(this_folder: str, kwd_file: str, manifest: str, additional_cards: str) -> None:
    global KWDM_INSTANCE, MANIFEST, ADDITIONAL_CARDS
    if kwd_file == "":
        kwd_file = os.path.join(this_folder, "kwd.json")
        KWDM_INSTANCE = KWDM(kwd_file)
    else:
        KWDM_INSTANCE = KWDM(kwd_file)
    logger.info(f"Loaded keyword data from: {kwd_file}")

    if manifest == "":
        manifest = os.path.join(this_folder, "manifest.json")
        MANIFEST = _load_manifest(manifest)
    else:
        MANIFEST = _load_manifest(manifest)
    logger.info(f"Loaded manifest from: {manifest}")

    if additional_cards == "":
        additional_cards = os.path.join(this_folder, "additional-cards.json")
        ADDITIONAL_CARDS = AdditionalCards(additional_cards)
    else:
        ADDITIONAL_CARDS = AdditionalCards(additional_cards)
    logger.info(f"Loaded additional cards from: {additional_cards}")
