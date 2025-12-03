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

import copy
import json
import logging
import os
import typing

from keyword_generation.data_model.insertion import Insertion
from keyword_generation.data_model.keyword_data import Card, Field, KeywordData

logger = logging.getLogger(__name__)

KWDM_INSTANCE = None
MANIFEST = None
ADDITIONAL_CARDS = None

KWD_TO_ALIAS: typing.Dict[str, str] = {}
ALIAS_TO_KWD: typing.Dict[str, str] = {}

from .insertion import Insertion


def get_card(setting: typing.Dict[str, str]):
    source = setting["source"]
    if source == "kwd-data":
        data = KWDM_INSTANCE.get_keyword_data_dict(setting["keyword-name"])
        card = data[setting["card-index"]]
        return card

    if source == "additional-cards":
        return ADDITIONAL_CARDS[setting["card-name"]]

    raise Exception()


def add_alias(keyword: str, alias: str):
    KWD_TO_ALIAS[keyword] = alias
    ALIAS_TO_KWD[alias] = keyword


def get_alias(keyword: str) -> typing.Optional[str]:
    return KWD_TO_ALIAS.get(keyword, None)


def get_aliased_by(keyword: str):
    return ALIAS_TO_KWD.get(keyword, None)


def is_aliased(keyword: str):
    return keyword in ALIAS_TO_KWD.keys()


def _load_manifest(filename) -> typing.Dict:
    with open(filename) as f:
        manifest = json.load(f)
    return manifest


class AdditionalCards:
    def __init__(self, filename):
        with open(filename) as f:
            self._cards = json.load(f)

    def __getitem__(self, name):
        """return a copy of the additional card, since the client may mutate it."""
        return copy.deepcopy(self._cards[name])


class KWDM:
    def __init__(self, filename):
        with open(filename, encoding="utf-8") as f:
            self._data: typing.Dict = json.load(f)

    def get_keywords_list(self) -> typing.List[str]:
        return list(self._data.keys())

    def get_keyword_data_dict(self, name: str) -> typing.Dict:
        return copy.deepcopy(self[name])

    def __getitem__(self, name: str) -> typing.Dict:
        return self._data[name]


def load(this_folder: str, kwd_file: str, manifest: str, additional_cards: str):
    global KWDM_INSTANCE, MANIFEST, ADDITIONAL_CARDS
    if kwd_file == "":
        kwd_file = os.path.join(this_folder, "kwd.json")
        KWDM_INSTANCE = KWDM(kwd_file)
    else:
        KWDM_INSTANCE = KWDM(kwd_file)
    logger.info(f"Loaded keyword data from: {kwd_file}")

    if manifest == "":
        manifest = this_folder / "manifest.json"
        MANIFEST = _load_manifest(manifest)
    else:
        MANIFEST = _load_manifest(manifest)
    logger.info(f"Loaded manifest from: {manifest}")

    if additional_cards == "":
        additional_cards = this_folder / "additional-cards.json"
        ADDITIONAL_CARDS = AdditionalCards(additional_cards)
    else:
        ADDITIONAL_CARDS = AdditionalCards(additional_cards)
    logger.info(f"Loaded additional cards from: {additional_cards}")
