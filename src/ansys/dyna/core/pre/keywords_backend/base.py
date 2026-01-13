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
    """Simple ID generator for keywords that need unique IDs.

    Also provides an optional registry to track objects by their assigned IDs,
    enabling lookups and validation of ID references.
    """

    _counters: Dict[str, int] = field(default_factory=dict)
    _registry: Dict[str, Dict[int, Any]] = field(default_factory=dict)

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

    def register(self, category: str, id_value: int, obj: Any) -> None:
        """Register an object with its assigned ID for later lookup.

        Parameters
        ----------
        category : str
            The category of ID (e.g., "curve", "part", "material").
        id_value : int
            The ID value assigned to the object.
        obj : Any
            The object (keyword instance or high-level API object) to register.
        """
        if category not in self._registry:
            self._registry[category] = {}
        self._registry[category][id_value] = obj
        logger.debug(f"Registered {category} id={id_value}: {type(obj).__name__}")

    def get(self, category: str, id_value: int) -> Optional[Any]:
        """Get a registered object by category and ID.

        Parameters
        ----------
        category : str
            The category of ID.
        id_value : int
            The ID value to look up.

        Returns
        -------
        Any or None
            The registered object, or None if not found.
        """
        return self._registry.get(category, {}).get(id_value)

    def get_all(self, category: str) -> Dict[int, Any]:
        """Get all registered objects for a category.

        Parameters
        ----------
        category : str
            The category of ID.

        Returns
        -------
        Dict[int, Any]
            Dictionary mapping IDs to registered objects.
        """
        return self._registry.get(category, {}).copy()

    def ensure_min_id(self, category: str, min_id: int) -> None:
        """Ensure the counter is at least the given value.

        Parameters
        ----------
        category : str
            The category of ID.
        min_id : int
            The minimum value the counter should be at.
        """
        if category not in self._counters:
            self._counters[category] = min_id
        else:
            self._counters[category] = max(self._counters[category], min_id)

    def reset(self, category: Optional[str] = None) -> None:
        """Reset ID counters and registry.

        Parameters
        ----------
        category : str, optional
            The category to reset. If None, resets all categories.
        """
        if category is None:
            self._counters.clear()
            self._registry.clear()
        else:
            if category in self._counters:
                del self._counters[category]
            if category in self._registry:
                del self._registry[category]


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

    def register_id(self, category: str, id_value: int, obj: Any) -> None:
        """Register an object with its assigned ID for later lookup.

        Parameters
        ----------
        category : str
            The category of ID (e.g., "curve", "part", "material").
        id_value : int
            The ID value assigned to the object.
        obj : Any
            The object (keyword instance or high-level API object) to register.
        """
        self._id_generator.register(category, id_value, obj)

    def get_registered(self, category: str, id_value: int) -> Optional[Any]:
        """Get a registered object by category and ID.

        Parameters
        ----------
        category : str
            The category of ID.
        id_value : int
            The ID value to look up.

        Returns
        -------
        Any or None
            The registered object, or None if not found.
        """
        return self._id_generator.get(category, id_value)

    def get_all_registered(self, category: str) -> Dict[int, Any]:
        """Get all registered objects for a category.

        Parameters
        ----------
        category : str
            The category of ID.

        Returns
        -------
        Dict[int, Any]
            Dictionary mapping IDs to registered objects.
        """
        return self._id_generator.get_all(category)

    def ensure_min_id(self, category: str, min_id: int) -> None:
        """Ensure the ID counter is at least the given value.

        Parameters
        ----------
        category : str
            The category of ID.
        min_id : int
            The minimum value the counter should be at.
        """
        self._id_generator.ensure_min_id(category, min_id)
