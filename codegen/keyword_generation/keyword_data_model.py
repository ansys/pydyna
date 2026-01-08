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
Configuration loader for codegen system.

Centralizes loading of kwd.json, manifest.json, and additional-cards.json
without relying on global state.
"""

import copy
import json
import logging
import os
from pathlib import Path
from typing import Any, Dict, List, Optional

from keyword_generation.data_model.keyword_data import Card

logger = logging.getLogger(__name__)


class KeywordDataLoader:
    """Loads and provides access to keyword definitions from kwd.json."""

    def __init__(self, filepath: str) -> None:
        """
        Load keyword data from kwd.json.

        Args:
            filepath: Path to kwd.json file
        """
        self._filepath = filepath
        with open(filepath, encoding="utf-8") as f:
            self._data: Dict[str, Any] = json.load(f)
        logger.info(f"Loaded keyword data from: {filepath}")

    def get_keywords_list(self) -> List[str]:
        """Get list of all keyword names."""
        return list(self._data.keys())

    def get_keyword_data_dict(self, name: str) -> List[Dict[str, Any]]:
        """Get card data for a keyword as a list of dicts (deep copy)."""
        return copy.deepcopy(self._data[name])

    def __getitem__(self, name: str) -> List[Dict[str, Any]]:
        """Direct access to keyword data (returns reference, not copy)."""
        return self._data[name]


class AdditionalCardsLoader:
    """Loads and provides access to additional card definitions."""

    def __init__(self, filepath: str) -> None:
        """
        Load additional card definitions from additional-cards.json.

        Args:
            filepath: Path to additional-cards.json file
        """
        self._filepath = filepath
        with open(filepath) as f:
            self._cards: Dict[str, Dict[str, Any]] = json.load(f)
        logger.info(f"Loaded additional cards from: {filepath}")

    def __getitem__(self, name: str) -> Dict[str, Any]:
        """Return a copy of the additional card as a dict."""
        return copy.deepcopy(self._cards[name])

    def get_card(self, name: str) -> Card:
        """Return additional card as a Card dataclass instance."""
        return Card.from_dict(self._cards[name])


class ManifestLoader:
    """Loads and provides access to manifest configuration."""

    def __init__(self, filepath: str) -> None:
        """
        Load manifest from manifest.json.

        Args:
            filepath: Path to manifest.json file
        """
        self._filepath = filepath
        with open(filepath) as f:
            self._data: Dict[str, Any] = json.load(f)
        logger.info(f"Loaded manifest from: {filepath}")

    def get_keyword_options(self, keyword: str) -> Dict[str, Any]:
        """Get generation options for a specific keyword."""
        return self._data.get(keyword, {})

    def get_wildcards(self) -> List[Dict[str, Any]]:
        """Get wildcard configuration."""
        return self._data.get("WILDCARDS", [])

    def __getitem__(self, key: str) -> Any:
        """Direct access to manifest data."""
        return self._data[key]

    def get(self, key: str, default: Any = None) -> Any:
        """Get manifest value with optional default."""
        return self._data.get(key, default)


class KeywordDataModel:
    """
    Centralized data model for the keyword generation system.

    Encapsulates all keyword metadata including kwd.json, manifest.json,
    additional-cards.json and provides unified access without global state.
    """

    def __init__(
        self,
        codegen_dir: str,
        kwd_file: str = "",
        manifest_file: str = "",
        additional_cards_file: str = "",
    ) -> None:
        """
        Initialize keyword data model.

        Args:
            codegen_dir: Base directory containing config files
            kwd_file: Path to kwd.json (empty string = use default)
            manifest_file: Path to manifest.json (empty string = use default)
            additional_cards_file: Path to additional-cards.json (empty string = use default)
        """
        self.codegen_dir = Path(codegen_dir)

        # Resolve file paths
        kwd_path = kwd_file if kwd_file else str(self.codegen_dir / "kwd.json")
        manifest_path = manifest_file if manifest_file else str(self.codegen_dir / "manifest.json")
        additional_cards_path = (
            additional_cards_file if additional_cards_file else str(self.codegen_dir / "additional-cards.json")
        )

        # Load all configuration files
        self.keyword_data = KeywordDataLoader(kwd_path)
        self.manifest = ManifestLoader(manifest_path)
        self.additional_cards = AdditionalCardsLoader(additional_cards_path)

        # Alias tracking (migrated from global state)
        self._kwd_to_alias: Dict[str, str] = {}
        self._alias_to_kwd: Dict[str, str] = {}

    def get_card(self, setting: Dict[str, str]) -> Card:
        """
        Get a card from either kwd-data or additional-cards sources.

        Args:
            setting: Dict with 'source' and either 'keyword-name'/'card-index'
                    or 'card-name' keys

        Returns:
            Card dataclass instance
        """
        source = setting["source"]
        if source == "kwd-data":
            data = self.keyword_data.get_keyword_data_dict(setting["keyword-name"])
            card_dict = data[setting["card-index"]]
            return Card.from_dict(card_dict)

        if source == "additional-cards":
            return self.additional_cards.get_card(setting["card-name"])

        raise ValueError(f"Unknown card source: {source}")

    def add_alias(self, keyword: str, alias: str) -> None:
        """Register a keyword alias."""
        self._kwd_to_alias[keyword] = alias
        self._alias_to_kwd[alias] = keyword

    def get_alias(self, keyword: str) -> Optional[str]:
        """Get alias for a keyword, or None if no alias exists."""
        return self._kwd_to_alias.get(keyword, None)

    def get_aliased_by(self, keyword: str) -> Optional[str]:
        """Get the original keyword name that this is an alias for."""
        return self._alias_to_kwd.get(keyword, None)

    def is_aliased(self, keyword: str) -> bool:
        """Check if a keyword is an alias of another keyword."""
        return keyword in self._alias_to_kwd

    def get_aliases(self) -> Dict[str, str]:
        """Get all registered aliases as {alias: original_keyword} mapping."""
        return dict(self._alias_to_kwd)
