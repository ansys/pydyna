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

"""Base classes and utilities for the keywords backend."""

from dataclasses import dataclass, field
import logging
import os
from typing import Any, Dict, List, Optional

from ansys.dyna.core.lib.deck import Deck

logger = logging.getLogger(__name__)


@dataclass
class IdGenerator:
    """Simple ID generator for keywords that need unique IDs."""

    _counters: Dict[str, int] = field(default_factory=dict)

    def next_id(self, category: str = "default") -> int:
        """Get the next available ID for a category.

        Parameters
        ----------
        category : str
            The category of ID (e.g., "curve", "part", "material").

        Returns
        -------
        int
            The next available ID.
        """
        if category not in self._counters:
            self._counters[category] = 0
        self._counters[category] += 1
        return self._counters[category]

    def reset(self, category: Optional[str] = None) -> None:
        """Reset ID counters.

        Parameters
        ----------
        category : str, optional
            The category to reset. If None, resets all categories.
        """
        if category is None:
            self._counters.clear()
        elif category in self._counters:
            del self._counters[category]


class KeywordsBackendBase:
    """Base class for the keywords backend.

    Provides core functionality for file management, ID generation, and deck access.
    """

    def __init__(self):
        """Initialize the keywords backend base."""
        logger.debug("Initializing KeywordsBackendBase")
        self._deck = Deck()
        self._id_generator = IdGenerator()
        self._working_dir = os.getcwd()
        self._main_filename = "output.k"
        self._loaded_files: List[str] = []

        # Termination settings
        self._termination_time = 0.0

        # Database settings
        self._db_binary_settings: Dict[str, Any] = {}
        self._db_ascii_settings: List[Dict[str, Any]] = []

    @property
    def deck(self) -> Deck:
        """Get the underlying Deck instance."""
        return self._deck

    def reset(self) -> None:
        """Reset the backend to initial state."""
        logger.debug("Resetting KeywordsBackend")
        self._deck = Deck()
        self._id_generator.reset()
        self._loaded_files.clear()
        self._termination_time = 0.0
        self._db_binary_settings.clear()
        self._db_ascii_settings.clear()

    def set_working_dir(self, path: str) -> None:
        """Set the working directory.

        Parameters
        ----------
        path : str
            The working directory path.
        """
        self._working_dir = path
        logger.debug(f"Working directory set to: {path}")

    def get_working_dir(self) -> str:
        """Get the current working directory.

        Returns
        -------
        str
            The working directory path.
        """
        return self._working_dir

    def load_file(self, filepath: str) -> bool:
        """Load a keyword file into the deck.

        Parameters
        ----------
        filepath : str
            Path to the keyword file.

        Returns
        -------
        bool
            True if successful, False otherwise.
        """
        try:
            logger.info(f"Loading file: {filepath}")
            self._deck.import_file(filepath)
            self._loaded_files.append(filepath)
            return True
        except Exception as e:
            logger.error(f"Failed to load file {filepath}: {e}")
            return False

    def save_file(self, filename: Optional[str] = None) -> str:
        """Save the deck to a keyword file.

        Parameters
        ----------
        filename : str, optional
            The output filename. If None, uses the main filename.

        Returns
        -------
        str
            The path where the file was saved.
        """
        if filename is None or filename == "":
            filename = self._main_filename

        # Ensure we have a valid filename
        if not filename:
            filename = "output.k"

        output_path = os.path.join(self._working_dir, filename)
        logger.info(f"Saving deck to: {output_path}")

        # Ensure the working directory exists
        os.makedirs(self._working_dir, exist_ok=True)

        self._deck.export_file(output_path)
        return self._working_dir

    def set_main_filename(self, filename: str) -> None:
        """Set the main output filename.

        Parameters
        ----------
        filename : str
            The main filename.
        """
        self._main_filename = filename
        logger.debug(f"Main filename set to: {filename}")

    def get_main_filename(self) -> str:
        """Get the main output filename.

        Returns
        -------
        str
            The main filename.
        """
        return self._main_filename

    def next_id(self, category: str = "default") -> int:
        """Get the next available ID for a category.

        Parameters
        ----------
        category : str
            The category of ID.

        Returns
        -------
        int
            The next available ID.
        """
        return self._id_generator.next_id(category)
