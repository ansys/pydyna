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

"""Provides a chainable collection wrapper for filtering keywords."""

import logging
import typing
from typing import Callable, Iterator, List, Optional, Union

if typing.TYPE_CHECKING:
    from ansys.dyna.core.lib.keyword_base import KeywordBase

logger = logging.getLogger(__name__)


class KeywordCollection:
    """A lazy, chainable wrapper around keyword iterables.

    This class provides a fluent interface for filtering and accessing keywords
    without materializing intermediate results until explicitly requested.

    Examples
    --------
    >>> collection = KeywordCollection(deck.keywords)
    >>> shells = collection.by_subtype("SHELL")
    >>> for kwd in shells:
    ...     print(kwd.secid)
    >>> first_shell = shells.first()
    >>> all_shells = shells.to_list()
    """

    def __init__(self, keywords: Union[Iterator["KeywordBase"], List["KeywordBase"]]):
        """Initialize the collection.

        Parameters
        ----------
        keywords : Union[Iterator[KeywordBase], List[KeywordBase]]
            An iterable of keywords to wrap.
        """
        # Materialize iterators to avoid consumption issues
        if hasattr(keywords, "__iter__") and not isinstance(keywords, (list, tuple)):
            self._keywords = list(keywords)
        else:
            self._keywords = keywords
        logger.debug("KeywordCollection created")

    def by_subtype(self, subkeyword: str) -> "KeywordCollection":
        """Filter keywords by subtype.

        Parameters
        ----------
        subkeyword : str
            The subkeyword to filter by (e.g., "SHELL", "SOLID").

        Returns
        -------
        KeywordCollection
            A new collection containing only keywords with the specified subtype.

        Examples
        --------
        >>> sections = deck.sections.by_subtype("SHELL")
        """
        logger.debug(f"Filtering by subtype: {subkeyword}")
        return KeywordCollection(kwd for kwd in self._keywords if kwd.subkeyword == subkeyword)

    def where(self, predicate: Callable[["KeywordBase"], bool]) -> "KeywordCollection":
        """Filter keywords using a custom predicate.

        Parameters
        ----------
        predicate : Callable[[KeywordBase], bool]
            A function that takes a keyword and returns True if it should be included.

        Returns
        -------
        KeywordCollection
            A new collection containing only keywords that satisfy the predicate.

        Examples
        --------
        >>> sections = deck.sections.where(lambda k: k.secid > 10)
        """
        logger.debug("Filtering with custom predicate")
        return KeywordCollection(kwd for kwd in self._keywords if predicate(kwd))

    def first(self) -> Optional["KeywordBase"]:
        """Get the first keyword in the collection.

        Returns
        -------
        Optional[KeywordBase]
            The first keyword, or None if the collection is empty.

        Examples
        --------
        >>> first_section = deck.sections.first()
        """
        logger.debug("Getting first keyword")
        for kwd in self._keywords:
            return kwd
        return None

    def to_list(self) -> List["KeywordBase"]:
        """Materialize the collection as a list.

        Returns
        -------
        List[KeywordBase]
            A list of all keywords in the collection.

        Examples
        --------
        >>> all_sections = deck.sections.to_list()
        """
        logger.debug("Materializing collection to list")
        result = list(self._keywords)
        logger.debug(f"Materialized {len(result)} keywords")
        return result

    def __iter__(self) -> Iterator["KeywordBase"]:
        """Iterate over the keywords in the collection.

        Returns
        -------
        Iterator[KeywordBase]
            An iterator over the keywords.

        Examples
        --------
        >>> for section in deck.sections:
        ...     print(section.secid)
        """
        return iter(self._keywords)

    def __len__(self) -> int:
        """Get the number of keywords in the collection.

        Note: This materializes the collection.

        Returns
        -------
        int
            The number of keywords in the collection.

        Examples
        --------
        >>> num_sections = len(deck.sections)
        """
        return len(self.to_list())

    def __getitem__(self, index: Union[int, slice]) -> Union["KeywordBase", List["KeywordBase"]]:
        """Get a keyword by index or slice.

        Note: This materializes the collection.

        Parameters
        ----------
        index : Union[int, slice]
            The index or slice to retrieve.

        Returns
        -------
        Union[KeywordBase, List[KeywordBase]]
            The keyword at the index, or a list of keywords for a slice.

        Examples
        --------
        >>> first_section = deck.sections[0]
        >>> first_three = deck.sections[:3]
        """
        return self.to_list()[index]

    def __eq__(self, other) -> bool:
        """Check equality with another collection or list.

        Parameters
        ----------
        other : Union[KeywordCollection, List]
            The object to compare with.

        Returns
        -------
        bool
            True if the collections contain the same keywords.

        Examples
        --------
        >>> deck.keywords == [kwd1, kwd2]
        True
        """
        if isinstance(other, KeywordCollection):
            return self.to_list() == other.to_list()
        elif isinstance(other, list):
            return self.to_list() == other
        return NotImplemented

    def __repr__(self) -> str:
        """Get a string representation of the collection.

        Returns
        -------
        str
            A string representation showing the number of keywords.
        """
        try:
            count = len(self)
            return f"<KeywordCollection: {count} keyword(s)>"
        except Exception:
            return "<KeywordCollection: (unevaluated)>"
