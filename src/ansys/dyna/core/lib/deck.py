# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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
"""Module provides a collection of keywords that can read and write to a keyword file."""

import collections
import os
import typing
from typing import Union
import warnings

from ansys.dyna.core.lib.encrypted_keyword import EncryptedKeyword
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.import_handler import ImportContext, ImportHandler
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.parameters import ParameterHandler, ParameterSet
from ansys.dyna.core.lib.transform import TransformHandler


class Deck:
    """Provides a collection of keywords that can read and write to a keyword file."""

    def __init__(self, title: str = None, **kwargs):
        """Initialize the deck."""
        self._keywords: typing.List = kwargs.get("keywords", [])
        self._parameter_set = ParameterSet()
        self.comment_header: str = None
        self.title: str = title
        self.format: format_type = kwargs.get("format", format_type.default)
        self._import_handlers: typing.List[ImportHandler] = [ParameterHandler()]
        self._transform_handler = TransformHandler()

    def __add__(self, other):
        """Add two decks together."""
        sum_keyword = self._keywords + other._keywords
        sum_deck = Deck()
        sum_deck.extend(sum_keyword)
        return sum_deck

    def clear(self):
        """Clear all keywords from the deck."""
        for keyword in self._keywords:
            if isinstance(keyword, KeywordBase):
                keyword.deck = None
        self._keywords = []
        self.comment_header = None
        self.title = None
        self.format = format_type.default

    @property
    def transform_handler(self) -> TransformHandler:
        return self._transform_handler

    def register_import_handler(self, import_handler: ImportHandler) -> None:
        """Registers an ImportHandler object"""
        self._import_handlers.append(import_handler)

    @property
    def parameters(self) -> ParameterSet:
        return self._parameter_set

    @property
    def format(self) -> format_type:
        """Format type of the deck."""
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        """Set the format type of the deck."""
        self._format_type = value

    def append(self, keyword: Union[KeywordBase, str], check=False) -> None:
        """Add a keyword to the collection.

        Parameters
        ----------
        keyword : Union[KeywordBase, EncryptedKeyword, str]
            Keyword. The keyword can be ``KeywordBase``, ``EncryptedKeyword``,
            or a string.
        check : bool, optional
            The default is ``False``.
        """
        assert (
            isinstance(keyword, KeywordBase) or isinstance(keyword, str) or isinstance(keyword, EncryptedKeyword)
        ), "Only keywords, encrypted keywords, or strings can be included in a deck."
        if isinstance(keyword, str):
            self._keywords.append(self._formatstring(keyword, check))
        elif isinstance(keyword, KeywordBase):
            keyword.deck = self
            self._keywords.append(keyword)
        else:
            self._keywords.append(keyword)

    def _remove_at(self, index: int) -> None:
        kwd = self._keywords[index]
        if isinstance(kwd, KeywordBase):
            kwd.deck = None
        del self._keywords[index]

    def remove(self, index: int | list[int]) -> None:
        """Remove a keyword from the collection by index.

        Parameters
        ----------
        index : int or list of int, mandatory
        """
        try:
            if isinstance(index, int):
                self._remove_at(index)
            elif isinstance(index, list):
                for i in sorted(index, reverse=True):
                    self._remove_at(i)
            else:
                raise TypeError("Input must be an integer or a list of integers.")
        except IndexError:
            raise IndexError(f"One or more indices {index} are out of range for the keywords list.")

    def _formatstring(self, string, check=False):
        """Format a string to be appended to the deck."""
        linelist = string.split("\n")
        if check:
            assert linelist[0][0] == "*", "Appended string must begin with a keyword."
            kwcount = 0
            for idx, line in enumerate(linelist):
                if len(line) > 0:
                    if line[0] == "*":
                        kwcount += 1
                assert kwcount == 1, "Appended string must contain only one keyword."
                width = 80
                if self.format == format_type.long:
                    width = 200
                if len(line) > width and check:
                    linelist[idx] = line[0:width]
                    print(f"truncated line {idx} to {width} characters")
            newstring = "\n".join(linelist)
            return newstring
        return string

    @property
    def all_keywords(self) -> typing.List[typing.Union[str, KeywordBase, EncryptedKeyword]]:
        """List of all keywords."""
        return self._keywords

    @property
    def string_keywords(self) -> typing.List[str]:
        """List of keywords as a raw string."""
        return [kw for kw in self._keywords if isinstance(kw, str)]

    @property
    def encrypted_keywords(self) -> typing.List[str]:
        """List of keywords as a raw string."""
        return [kw for kw in self._keywords if isinstance(kw, EncryptedKeyword)]

    @property
    def keywords(self):
        """List of processed keywords."""
        return [kw for kw in self._keywords if isinstance(kw, KeywordBase)]

    def extend(self, kwlist: list) -> None:
        """Add a list of keywords to the deck.

        Parameters
        ----------
        kwlist : list
            List of keywords.
        """
        for kw in kwlist:
            self.append(kw)

    def _expand_helper(self, search_paths: typing.List[str], recurse: bool) -> typing.List[KeywordBase]:
        """Recursively outputs a list of keywords within Includes."""
        keywords = []
        for keyword in self.all_keywords:
            if not isinstance(keyword, KeywordBase):
                keywords.append(keyword)
                continue
            if keyword.keyword != "INCLUDE":
                keywords.append(keyword)
                continue
            if keyword.subkeyword == "PATH":
                search_paths.append(keyword.filename)
                keywords.append(keyword)
                continue
            success = False
            for search_path in search_paths:
                include_file = os.path.join(search_path, keyword.filename)
                include_deck = Deck(format=keyword.format)
                for import_handler in self._import_handlers:
                    include_deck.register_import_handler(import_handler)
                try:
                    xform = None
                    if keyword.subkeyword == "TRANSFORM":
                        xform = keyword
                        include_deck.register_import_handler(self.transform_handler)
                    context = ImportContext(xform, self, include_file)
                    encoding = "utf-8"  # TODO - how to control encoding in expand?
                    include_deck._import_file(include_file, "utf-8", context)
                    success = True
                    break
                except FileNotFoundError:
                    pass
            if success:
                if recurse:
                    # TODO: merge the parameters if the "LOCAL" option is not used!
                    expanded = include_deck._expand_helper(search_paths, True)
                    keywords.extend(expanded)
                else:
                    keywords.extend(include_deck.all_keywords)
            else:
                keywords.append(keyword)
        for keyword in keywords:
            if isinstance(keyword, KeywordBase):
                keyword.deck = None
        return keywords

    def expand(self, cwd=None, recurse=True):
        """Get a new deck that is flattened copy of `self`.

        A flattened deck is one where the *INCLUDE keywords are replaced
        by the contents of the file that is included.
        `cwd` is a working directory used to resolve the filename
        If `recurse` is true, *INCLUDE keywords within included decks
        are expanded, recursively.
        """
        cwd = cwd or os.getcwd()
        new_deck = Deck(title=self.title)
        new_deck.comment_header = self.comment_header
        search_paths = [cwd]
        new_deck.extend(self._expand_helper(search_paths, recurse))
        return new_deck

    def _get_title_lines(self) -> typing.List[str]:
        """Get the title lines."""
        if self.title is None:
            return []
        return ["*TITLE", self.title]

    def _get_comment_header_lines(self) -> typing.List[str]:
        """Get the comment header lines."""
        comment_header = self.comment_header
        if comment_header is None:
            return []
        split_lines = comment_header.split("\n")
        line_start = "$"
        return [f"{line_start}{line}" for line in split_lines]

    def _get_keyword_line(self, format: format_type) -> str:
        """Get the keyword line."""
        keyword_line = "*KEYWORD"
        if format == format_type.long:
            keyword_line += " LONG=Y"
        elif format == format_type.standard:
            keyword_line += " LONG=S"
        return keyword_line

    def _get_header(self, format: format_type) -> str:
        """Get the header of the keyword file."""
        comment_lines = self._get_comment_header_lines()
        keyword_lines = [self._get_keyword_line(format)]
        title_lines = self._get_title_lines()
        header_lines = comment_lines + keyword_lines + title_lines
        return "\n".join(header_lines)

    def dumps(self) -> str:
        """Get the keyword file representation of all keywords as a string.

        Returns
        -------
        str
            Keyword file representation of all keywords as a string.
        """
        warnings.warn("The 'dumps()' method is deprecated. Use the 'write()' method instead.")
        return self.write()

    def _write_keyword(
        self, buf: typing.TextIO, kwd: typing.Union[str, KeywordBase, EncryptedKeyword], format: format_type
    ) -> None:
        """Write a keyword to the buffer."""
        if isinstance(kwd, KeywordBase):
            kwd.write(buf, None, format)
        elif isinstance(kwd, str):
            buf.write(kwd)
        elif isinstance(kwd, EncryptedKeyword):
            buf.write("-----BEGIN PGP MESSAGE-----\n")
            buf.write(kwd.data)
            buf.write("\n-----END PGP MESSAGE-----\n")

    def _remove_trailing_newline(self, buf: typing.TextIO) -> None:
        """If the last character is a newline, seek back so that it can be overwritten.

        Otherwise, leave the buffer unmodified.
        """
        pos = buf.tell()
        buf.seek(pos - 1)
        last_char = buf.read(1)
        if last_char == "\n":
            buf.seek(pos - 1)

    def write(
        self,
        buf: typing.Optional[typing.TextIO] = None,
        format: typing.Optional[format_type] = None,
    ):
        """Write the card in the dyna keyword format.

        Parameters
        ----------
        buf : optional
            Buffer to write to. The default is ``None``,
            in which case the output is returned as a string.
        format : optional
            Format to write in. The default is ``None``.
        """
        if format is None:
            format = self._format_type

        def _write(buf):
            buf.write(self._get_header(format))
            for kwd in self._keywords:
                self._remove_trailing_newline(buf)
                buf.write("\n")
                self._write_keyword(buf, kwd, format)
            buf.write("\n*END")

        return write_or_return(buf, _write)

    def loads(
        self, value: str, context: typing.Optional[ImportContext] = None
    ) -> "ansys.dyna.keywords.lib.deck_loader.DeckLoaderResult":  # noqa: F821
        """Load all keywords from the keyword file as a string.

        When adding all keywords from the file, this method
        overwrites the title and user comment, if any.

        Parameters
        ----------
        value : str
        context: ImportContext
            the context

        """
        # import deck_loader only when loading to avoid circular imports

        # ansys.dyna.keywords imports deck, deck imports deck_loader
        # deck_loader imports ansys.dyna.keywords
        from ansys.dyna.core.lib.deck_loader import load_deck

        if context is None:
            context = ImportContext(None, self, None)
        result = load_deck(self, value, context, self._import_handlers)
        return result

    def _check_unique(self, type: str, field: str) -> None:
        """Check that all keywords of a given type have a unique field value."""
        ids = []
        for kwd in self.get_kwds_by_type(type):
            if not hasattr(kwd, field):
                raise Exception(f"kwd of type {type} does not have field {field}.")
            ids.append(getattr(kwd, field))
        duplicates = [id for id, count in collections.Counter(ids).items() if count > 1]
        if len(duplicates) > 0:
            raise Exception(f"kwds of type {type} have the following duplicate {field} values: {duplicates}")

    def _check_valid(self) -> None:
        """Check that all keywords are valid."""
        for kwd in self._keywords:
            is_valid, msg = kwd._is_valid()
            if not is_valid:
                raise Exception(f"{kwd} is not valid due to {msg}")

    def validate(self) -> None:
        """Validate the collection of keywords."""
        # TODO - globally unique keywords (like CONTROL_TIME_STEP) are unique
        self._check_unique("SECTION", "secid")
        self._check_valid()

    def get_kwds_by_type(self, str_type: str) -> typing.Iterator[KeywordBase]:
        """Get all keywords for a given type.

        Parameters
        ----------
        str_type : str
            Keyword type.

        Returns
        -------
        typing.Iterator[KeywordBase]

        Examples
        --------
        Get all *SECTION_* keywords in the deck.

        >>>deck.get_kwds_by_type("SECTION")
        """
        return filter(lambda kwd: isinstance(kwd, KeywordBase) and kwd.keyword == str_type, self._keywords)

    def get_kwds_by_full_type(self, str_type: str, str_subtype: str) -> typing.Iterator[KeywordBase]:
        """Get all keywords for a given full type.

        Parameters
        ----------
        str_type : str
            Keyword type.
        str_subtype : str
            Keyword subtype.

        Returns
        -------
        typing.Iterator[KeywordBase]

        Examples
        --------
        Get all *SECTION_SHELL keyword instances in the deck.

        >>>deck.get_kwds_by_full_type("SECTION", "SHELL")
        """
        return filter(
            lambda kwd: isinstance(kwd, KeywordBase) and kwd.keyword == str_type and kwd.subkeyword == str_subtype,
            self._keywords,
        )

    def get_section_by_id(self, id: int) -> typing.Optional[KeywordBase]:
        """Get the SECTION keyword in the collection for a given section ID.

        Parameters
        ----------
        id : int
            Section ID.

        Returns
        -------
        SECTION keyword or ``None`` if there is no SECTION keyword that matches the section ID.

        Raises
        ------
        Exception
            If multiple SECTION keywords use the given section ID.
        """
        sections = self.get(type="SECTION", filter=lambda kwd: kwd.secid == id)
        if len(sections) == 0:
            return None
        assert (
            len(sections) == 1
        ), f"Failure in `deck.get_section_by_id() method`. Multiple SECTION keywords use matid {id}."  # noqa: E501
        return sections[0]

    def get(self, **kwargs) -> typing.List[KeywordBase]:
        """Get a list of keywords.

        Parameters
        ----------
        :Keyword Arguments:
        * *type* (``str``) --
            The type of keyword to get. For example, "SECTION" returns all section keywords.
        * *filter* (``callable``) --
            The filter to apply to the result. Only keywords which pass the filter will be returned.

        """
        if "type" in kwargs:
            kwds = list(self.get_kwds_by_type(kwargs["type"]))
        else:
            kwds = self.keywords
        if "filter" in kwargs:
            return [kwd for kwd in kwds if kwargs["filter"](kwd)]
        return kwds

    def _import_file(self, path: str, encoding: str, context: ImportContext):
        from ansys.dyna.core.lib.deck_loader import load_deck_from_buffer

        with open(path, encoding=encoding) as f:
            return load_deck_from_buffer(self, f, context, self._import_handlers)

    def import_file(
        self, path: str, encoding: str = "utf-8"
    ) -> "ansys.dyna.keywords.lib.deck_loader.DeckLoaderResult":  # noqa: F821
        """Import a keyword file.

        Parameters
        ----------
        path : str
            Full path for the keyword file.
        encoding: str
            String encoding used to read the keyword file.
        """
        context = ImportContext(None, self, path)
        self._import_file(path, encoding, context)

    def export_file(self, path: str, encoding="utf-8") -> None:
        """Export the keyword file to a new keyword file.

        Parameters
        ----------
        path : str
            Full path for the new keyword file.
        """
        with open(path, "w+", encoding=encoding) as f:
            if os.name == "nt":
                self.write(f)
            else:
                # TODO - on linux writing to the buffer can insert a spurious newline
                #        this is less performant but more correct until that is fixed
                contents = self.write()
                f.write(contents)

    @property
    def comment_header(self) -> typing.Optional[str]:
        """Comment header of the keyword database."""
        return self._comment_header

    @comment_header.setter
    def comment_header(self, value: str) -> None:
        self._comment_header = value

    @property
    def title(self) -> typing.Optional[str]:
        """Title of the keyword database."""
        return self._title

    @title.setter
    def title(self, value: str) -> None:
        self._title = value

    @property
    def keyword_names(self) -> typing.List[str]:
        names = []
        for kw in self.all_keywords:
            if isinstance(kw, KeywordBase):
                names.append(f"{kw.get_title()}")
            elif isinstance(kw, str):
                try_title = kw.split("\n")[0]
                names.append(f"str({try_title}...)")
            elif isinstance(kw, EncryptedKeyword):
                names.append(f"Encrypted")
        return names

    def __repr__(self) -> str:
        """Get a console-friendly representation of a list of all keywords in the deck."""
        kwlist = self.all_keywords
        if len(kwlist) == 0:
            content_lines = ["Empty"]
        else:
            content_lines = []
            for kw in kwlist:
                if isinstance(kw, KeywordBase):
                    content_lines.append(f"kwd: {kw.get_title()}")
                elif isinstance(kw, str):
                    content_lines.append("str: " + kw.split("\n")[0] + "...")
                elif isinstance(kw, EncryptedKeyword):
                    content_lines.append("encrypted keyword...")

        output = "\n".join(content_lines)
        return output

    def plot(self, **args):
        """Plot the node and element of the mesh using PyVista.

        Parameters
        ----------
        **args :
            Keyword arguments. Use * *cwd* (``int``) if the deck and include files are in
            a separate directory.
        """
        from ansys.dyna.core.lib.deck_plotter import plot_deck

        plot_deck(self, **args)
