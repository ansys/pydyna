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
"""Module provides a collection of keywords that can read and write to a keyword file."""

import os
import typing
from typing import Union
import warnings

from ansys.dyna.core.lib.encrypted_keyword import EncryptedKeyword
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.import_handler import ImportContext, ImportHandler
from ansys.dyna.core.lib.import_handlers.define_table_processor import DefineTableProcessor
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_collection import KeywordCollection
from ansys.dyna.core.lib.parameters import ParameterHandler, ParameterSet
from ansys.dyna.core.lib.transform import TransformHandler
from ansys.dyna.core.lib.validation_mixin import ValidationMixin


class Deck(ValidationMixin):
    """Provides a collection of keywords that can read and write to a keyword file."""

    def __init__(self, title: str = None, **kwargs):
        """Initialize the deck."""
        self._keywords: typing.List = kwargs.get("keywords", [])
        self._parameter_set = ParameterSet()
        self.comment_header: str = None
        self.title: str = title
        self.format: format_type = kwargs.get("format", format_type.default)
        self._import_handlers: typing.List[ImportHandler] = [ParameterHandler()]
        # Add the DEFINE_TABLE processor so curves following tables are
        # automatically linked during import.
        self._import_handlers.append(DefineTableProcessor())
        self._transform_handler = TransformHandler()
        self._init_validation()  # Initialize validation mixin

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
        """Deck parameters."""
        return self._parameter_set

    @parameters.setter
    def parameters(self, value: ParameterSet) -> None:
        import copy

        self._parameter_set = copy.copy(value)

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
        if not (isinstance(keyword, KeywordBase) or isinstance(keyword, str) or isinstance(keyword, EncryptedKeyword)):
            raise TypeError("Only keywords, encrypted keywords, or strings can be included in a deck.")

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
            if linelist[0][0] != "*":
                raise ValueError("Appended string must begin with a keyword.")
            kwcount = 0
            for idx, line in enumerate(linelist):
                if len(line) > 0:
                    if line[0] == "*":
                        kwcount += 1

                if kwcount != 1:
                    raise ValueError("Appended string must contain only one keyword.")
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
    def keywords(self) -> KeywordCollection:
        """Get all processed keywords as a KeywordCollection.

        This provides access to the fluent filtering API for all keywords
        in the deck. The collection supports indexing, iteration, and
        comparison with lists for backward compatibility.

        Returns
        -------
        KeywordCollection
            A collection of all KeywordBase instances in the deck.

        Examples
        --------
        >>> # Access by index (backward compatible)
        >>> first = deck.keywords[0]
        >>> # Iterate (backward compatible)
        >>> for kwd in deck.keywords:
        ...     print(kwd.keyword)
        >>> # Filter with fluent API
        >>> high_id_sections = deck.keywords.where(
        ...     lambda k: k.keyword == "SECTION" and k.secid > 100
        ... )
        """
        return KeywordCollection([kw for kw in self._keywords if isinstance(kw, KeywordBase)])

    def extend(self, kwlist: list) -> None:
        """Add a list of keywords to the deck.

        Parameters
        ----------
        kwlist : list
            List of keywords.
        """
        for kw in kwlist:
            self.append(kw)

    def _detect_encoding(self, path: str) -> str:
        import chardet

        try:
            with open(path, "rb") as f:
                return chardet.detect(f.read())["encoding"]
        except Exception as e:
            raise Exception("Failed to detect encoding of deck in `expand`: " + str(e))

    def _prepare_deck_for_expand(self, keyword: KeywordBase):
        """Prepare deck for expansion by adding import handlers."""
        include_deck = Deck(format=keyword.format)
        # Create child scope for include to isolate PARAMETER_LOCAL
        include_deck.parameters = self.parameters.copy_with_child_scope()
        for import_handler in self._import_handlers:
            include_deck.register_import_handler(import_handler)
        if keyword.subkeyword == "TRANSFORM":
            include_deck.register_import_handler(self.transform_handler)
        return include_deck

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
            expand_include_file = None
            for search_path in search_paths:
                include_file = os.path.join(search_path, keyword.filename)
                if os.path.isfile(include_file):
                    expand_include_file = include_file
                    break
            if expand_include_file is None:
                keywords.append(keyword)
                continue
            xform = None
            if keyword.subkeyword == "TRANSFORM":
                xform = keyword
            include_deck = self._prepare_deck_for_expand(keyword)
            context = ImportContext(xform, include_deck, expand_include_file)
            try:
                include_deck._import_file(expand_include_file, "utf-8", context)
            except UnicodeDecodeError as e:
                encoding = self._detect_encoding(expand_include_file)
                include_deck = self._prepare_deck_for_expand(keyword)
                context = ImportContext(xform, include_deck, expand_include_file)
                include_deck._import_file(expand_include_file, encoding, context)
            if recurse:
                expanded = include_deck._expand_helper(search_paths, True)
                keywords.extend(expanded)
            else:
                keywords.extend(include_deck.all_keywords)
        for keyword in keywords:
            if isinstance(keyword, KeywordBase):
                keyword.deck = None
        return keywords

    def expand(self, cwd=None, recurse=True):
        """Get a new deck that is flattened copy of `self`.

        A flattened deck is one where the ``*INCLUDE`` keywords are replaced
        by the contents of the file that is included.
        `cwd` is a working directory used to resolve the filename
        If `recurse` is true, ``*INCLUDE`` keywords within included decks
        are expanded, recursively.
        """
        cwd = cwd or os.getcwd()
        new_deck = Deck(title=self.title)
        new_deck.comment_header = self.comment_header
        new_deck.parameters = self.parameters
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
        validate: bool = False,
        trailing_newline: bool = False,
    ):
        """Write the card in the dyna keyword format.

        Parameters
        ----------
        buf : optional
            Buffer to write to. The default is ``None``,
            in which case the output is returned as a string.
        format : optional
            Format to write in. The default is ``None``.
        validate : bool, optional
            If True, validate the deck before writing. The default is False.
            Validation uses registered validators and raises ValidationError if errors are found.
        trailing_newline : bool, optional
            If True, add a trailing newline after *END. The default is False.
        """
        if validate:
            result = self.validate()
            result.raise_if_errors()

        if format is None:
            format = self._format_type

        def _write(buf):
            buf.write(self._get_header(format))
            for kwd in self._keywords:
                self._remove_trailing_newline(buf)
                buf.write("\n")
                self._write_keyword(buf, kwd, format)
            buf.write("\n*END")
            if trailing_newline:
                buf.write("\n")

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

    # Validation methods are provided by ValidationMixin

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
        Get all ``*SECTION_*`` keywords in the deck.

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
        Get all ``*SECTION_SHELL`` keyword instances in the deck.

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

        if len(sections) != 1:
            raise Exception(
                f"Failure in `deck.get_section_by_id() method`. Multiple SECTION keywords use secid {id}."  # noqa: E501
            )
        return sections[0]

    def get(self, **kwargs) -> typing.List[KeywordBase]:
        """Get a list of keywords.

        Parameters
        ----------
        - *kwargs* (``dict``) --
            Keyword arguments.
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
            loader_result = load_deck_from_buffer(self, f, context, self._import_handlers)
        for keyword in self.keywords:
            keyword.included_from = path
        return loader_result

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

    def export_file(self, path: str, encoding="utf-8", validate: bool = False, trailing_newline: bool = False) -> None:
        """Export the keyword file to a new keyword file.

        Parameters
        ----------
        path : str
            Full path for the new keyword file.
        encoding : str, optional
            String encoding for the file. The default is "utf-8".
        validate : bool, optional
            If True, validate the deck before export. The default is False.
            Validation uses registered validators and raises ValidationError if errors are found.
        trailing_newline : bool, optional
            If True, add a trailing newline after *END. The default is False.

        Examples
        --------
        >>> deck.export_file("output.k", validate=True)  # Validate before export
        """
        with open(path, "w+", encoding=encoding) as f:
            if os.name == "nt":
                self.write(f, validate=validate, trailing_newline=trailing_newline)
            else:
                # TODO - on linux writing to the buffer can insert a spurious newline
                #        this is less performant but more correct until that is fixed
                contents = self.write(validate=validate, trailing_newline=trailing_newline)
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

    def __getitem__(self, key: typing.Union[str, typing.Tuple[str, str]]) -> KeywordCollection:
        """Get keywords by type using dict-like access.

        Parameters
        ----------
        key : Union[str, Tuple[str, str]]
            Either a keyword type (e.g., "SECTION") or a tuple of (type, subtype)
            (e.g., ("SECTION", "SHELL")).

        Returns
        -------
        KeywordCollection
            A collection of matching keywords.

        Examples
        --------
        >>> sections = deck["SECTION"]
        >>> shells = deck["SECTION", "SHELL"]
        """
        if isinstance(key, tuple):
            if len(key) != 2:
                raise ValueError("Tuple key must have exactly 2 elements (type, subtype)")
            keyword_type, subkeyword = key
            return KeywordCollection(self.get_kwds_by_full_type(keyword_type, subkeyword))
        else:
            return KeywordCollection(self.get_kwds_by_type(key))

    def __iter__(self) -> typing.Iterator[typing.Union[KeywordBase, str, EncryptedKeyword]]:
        """Iterate over all keywords in the deck.

        Returns
        -------
        Iterator[Union[KeywordBase, str, EncryptedKeyword]]
            An iterator over all keywords.

        Examples
        --------
        >>> for kwd in deck:
        ...     if isinstance(kwd, KeywordBase):
        ...         print(kwd.keyword)
        """
        return iter(self._keywords)

    def __len__(self) -> int:
        """Get the number of keywords in the deck.

        Returns
        -------
        int
            The total number of keywords (including strings and encrypted).

        Examples
        --------
        >>> num_keywords = len(deck)
        """
        return len(self._keywords)

    @property
    def sections(self) -> KeywordCollection:
        """Get all SECTION_* keywords.

        Returns
        -------
        KeywordCollection
            A collection of all section keywords.

        Examples
        --------
        >>> shells = deck.sections.by_subtype("SHELL")
        """
        return self["SECTION"]

    @property
    def materials(self) -> KeywordCollection:
        """Get all MAT_* keywords.

        Returns
        -------
        KeywordCollection
            A collection of all material keywords.

        Examples
        --------
        >>> elastic_mats = deck.materials.by_subtype("ELASTIC")
        """
        return self["MAT"]

    @property
    def elements(self) -> KeywordCollection:
        """Get all ELEMENT_* keywords.

        Returns
        -------
        KeywordCollection
            A collection of all element keywords.

        Examples
        --------
        >>> solid_elements = deck.elements.by_subtype("SOLID")
        """
        return self["ELEMENT"]

    @property
    def nodes(self) -> KeywordCollection:
        """Get all NODE keywords.

        Returns
        -------
        KeywordCollection
            A collection of all node keywords.

        Examples
        --------
        >>> all_nodes = deck.nodes.to_list()
        """
        return self["NODE"]

    @property
    def parts(self) -> KeywordCollection:
        """Get all PART* keywords.

        Returns
        -------
        KeywordCollection
            A collection of all part keywords.

        Examples
        --------
        >>> parts = deck.parts.to_list()
        """
        return self["PART"]

    @property
    def sets(self) -> KeywordCollection:
        """Get all SET_* keywords.

        Returns
        -------
        KeywordCollection
            A collection of all set keywords.

        Examples
        --------
        >>> node_sets = deck.sets.by_subtype("NODE")
        """
        return self["SET"]

    @property
    def defines(self) -> KeywordCollection:
        """Get all DEFINE_* keywords.

        Returns
        -------
        KeywordCollection
            A collection of all define keywords.

        Examples
        --------
        >>> curves = deck.defines.by_subtype("CURVE")
        """
        return self["DEFINE"]

    def split(
        self, key_func: typing.Callable[[typing.Union[KeywordBase, str, EncryptedKeyword]], typing.Any]
    ) -> typing.Dict[typing.Any, "Deck"]:
        """Split the deck into multiple decks based on a key function.

        The key function is called on each keyword and should return a value
        that will be used to group keywords. Keywords with the same return value
        will be placed in the same deck.

        Parameters
        ----------
        key_func : Callable[[Union[KeywordBase, str, EncryptedKeyword]], Any]
            A function that takes a keyword and returns a grouping key.

        Returns
        -------
        Dict[Any, Deck]
            A dictionary mapping keys to Deck instances.

        Examples
        --------
        >>> # Split by keyword type
        >>> decks_by_type = deck.split(lambda k: k.keyword if isinstance(k, KeywordBase) else "string")
        >>> section_deck = decks_by_type["SECTION"]

        >>> # Split by domain using the domain mapper
        >>> from ansys.dyna.core.lib.domain_mapper import by_domain
        >>> decks_by_domain = deck.split(by_domain)
        >>> mat_deck = decks_by_domain["mat"]

        >>> # Custom splitting logic
        >>> def by_id_range(kwd):
        ...     if isinstance(kwd, KeywordBase) and hasattr(kwd, 'secid'):
        ...         if kwd.secid < 100:
        ...             return "low"
        ...         else:
        ...             return "high"
        ...     return "other"
        >>> decks = deck.split(by_id_range)
        """
        result: typing.Dict[typing.Any, Deck] = {}
        for keyword in self._keywords:
            key = key_func(keyword)
            if key not in result:
                result[key] = Deck(format=self.format)
                result[key].parameters = self.parameters
            # Clear deck association if it's a KeywordBase
            if isinstance(keyword, KeywordBase):
                keyword.deck = None
            result[key].append(keyword)
        return result

    def split_by_domain(self) -> typing.Dict[str, "Deck"]:
        """Split the deck by keyword domain (mat, section, control, etc.).

        This is a convenience method that uses the domain mapper to categorize
        keywords. For custom splitting logic, use the `split()` method directly.

        Returns
        -------
        Dict[str, Deck]
            A dictionary mapping domain names to Deck instances.

        Examples
        --------
        >>> decks = deck.split_by_domain()
        >>> mat_deck = decks["mat"]
        >>> control_deck = decks["control"]
        >>> section_deck = decks["section"]

        See Also
        --------
        split : Generic splitting method with custom key function.
        """
        from ansys.dyna.core.lib.domain_mapper import by_domain

        return self.split(by_domain)

    def plot(self, **args):
        """Plot the node and element of the mesh using PyVista.

        Automatically detects Jupyter notebook environments and configures
        appropriate rendering backend for display.

        Parameters
        ----------
        cwd : str, optional
            Current working directory if the deck and include files are in
            a separate directory.
        jupyter_backend : str, optional
            Jupyter backend to use. Options are:

            - ``'static'`` - Static image (default for notebooks)
            - ``'server'`` - Interactive with server backend
            - ``'trame'`` - Interactive with Trame (requires pyvista[trame])
            - ``None`` - Disable Jupyter mode, use default windowing
            - ``'auto'`` - Automatically detect (default)
        color : str, optional
            Color of the mesh.
        scalars : str, optional
            Name of scalars to color by (e.g., 'part_ids', 'element_ids').
        **args :
            Additional keyword arguments passed to PyVista's plot method.

        Returns
        -------
        Depends on backend
            Camera position or plot object depending on the backend used.

        Examples
        --------
        Plot a deck in a regular Python script:

        >>> deck.plot()

        Plot with a specific working directory:

        >>> deck.plot(cwd='/path/to/includes')

        Plot in a Jupyter notebook with interactive backend:

        >>> deck.plot(jupyter_backend='server')

        Color by part IDs:

        >>> deck.plot(scalars='part_ids')
        """
        from ansys.dyna.core.lib.deck_plotter import plot_deck

        plot_deck(self, **args)
