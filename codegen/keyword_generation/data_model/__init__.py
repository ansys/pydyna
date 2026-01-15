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

"""Data model module for keyword generation.

Provides access to keyword metadata, manifest configuration, and additional cards
through the KeywordDataModel class.
"""

import logging
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
from keyword_generation.keyword_data_model import KeywordDataModel

logger = logging.getLogger(__name__)

# Global keyword data model instance
_MODEL: typing.Optional[KeywordDataModel] = None


def get_config() -> KeywordDataModel:
    """Get the current keyword data model instance.

    Returns
    -------
        KeywordDataModel instance

    Raises
    ------
        RuntimeError: If configuration has not been loaded yet
    """
    if _MODEL is None:
        raise RuntimeError("Configuration not loaded. Call load() first.")
    return _MODEL


def get_card(setting: typing.Dict[str, str]) -> Card:
    """Get a card from either kwd-data or additional-cards sources."""
    return get_config().get_card(setting)


def add_alias(keyword: str, alias: str) -> None:
    """Register a keyword alias."""
    get_config().add_alias(keyword, alias)


def get_alias(keyword: str) -> typing.Optional[str]:
    """Get alias for a keyword."""
    return get_config().get_alias(keyword)


def get_aliased_by(keyword: str) -> typing.Optional[str]:
    """Get original keyword name for alias."""
    return get_config().get_aliased_by(keyword)


def is_aliased(keyword: str) -> bool:
    """Check if keyword is an alias."""
    return get_config().is_aliased(keyword)


def load(this_folder: str, kwd_file: str, manifest: str, additional_cards: str) -> None:
    """Load all configuration files.

    Args:
        this_folder: Base directory containing config files
        kwd_file: Path to kwd.json (or empty string for default)
        manifest: Path to manifest.json (or empty string for default)
        additional_cards: Path to additional-cards.json (or empty string for default)
    """
    global _MODEL

    _MODEL = KeywordDataModel(
        codegen_dir=this_folder,
        kwd_file=kwd_file,
        manifest_file=manifest,
        additional_cards_file=additional_cards,
    )
