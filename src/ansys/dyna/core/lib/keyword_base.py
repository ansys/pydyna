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
"""Keyword base class and related functionality."""

import enum
import io
import typing
import warnings

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.parameters import ParameterSet

# protected due to circular import
if typing.TYPE_CHECKING:
    from ansys.dyna.core.lib.deck import Deck


class LinkType(enum.Enum):
    """Enumeration of link types for keyword references.

    Link types identify which kind of keyword a field references.
    Use with :meth:`KeywordBase.get_links` to filter by link type.
    """

    ALL = 0
    """All link types."""

    NODE = 1
    """Reference to a NODE keyword."""

    ELEMENT_BEAM = 3
    """Reference to an ELEMENT_BEAM keyword."""

    ELEMENT_SHELL = 4
    """Reference to an ELEMENT_SHELL keyword."""

    ELEMENT_SOLID = 5
    """Reference to an ELEMENT_SOLID keyword."""

    MAT = 14
    """Reference to a MAT_* material keyword."""

    SECTION = 15
    """Reference to a SECTION_* keyword."""

    HOURGLASS = 17
    """Reference to a HOURGLASS keyword."""

    DEFINE_CURVE = 19
    """Reference to a DEFINE_CURVE keyword."""

    DEFINE_BOX = 20
    """Reference to a DEFINE_BOX keyword."""

    DEFINE_COORDINATE_SYSTEM = 21
    """Reference to a DEFINE_COORDINATE_SYSTEM keyword."""

    DEFINE_VECTOR = 22
    """Reference to a DEFINE_VECTOR keyword."""

    DEFINE_TRANSFORMATION = 40
    """Reference to a DEFINE_TRANSFORMATION keyword."""

    SET_BEAM = 25
    """Reference to a SET_BEAM keyword."""

    SET_DISCRETE = 26
    """Reference to a SET_DISCRETE keyword."""

    SET_NODE = 27
    """Reference to a SET_NODE keyword."""

    SET_PART = 28
    """Reference to a SET_PART keyword."""

    SET_SEGMENT = 29
    """Reference to a SET_SEGMENT keyword."""

    SET_SOLID = 31
    """Reference to a SET_SOLID keyword."""

    PART = 13
    """Reference to a PART keyword."""

    DEFINE_CURVE_OR_TABLE = 86
    """Reference to either DEFINE_CURVE or DEFINE_TABLE (polymorphic)."""


class KeywordBase(Cards):
    """Base class for all keywords.

    Derived class must provide::
        - _cards
        - keyword
        - subkeyword
    """

    def __init__(self, **kwargs):
        super().__init__(self)
        self.user_comment = kwargs.get("user_comment", "")
        self._format_type: format_type = kwargs.get("format", format_type.default)
        self._deck = None
        self._included_from = None
        self._parameter_set: typing.Optional[ParameterSet] = None  # Stored for write-time ref lookup

    @property
    def deck(self) -> typing.Optional["Deck"]:
        """Get the deck that this keyword is associated to."""
        return self._deck

    @deck.setter
    def deck(self, deck: "Deck") -> None:
        """Get the deck that this keyword is associated to."""
        if deck is None:
            self._deck = None
            return

        if self._deck is not None:
            raise Exception("This keyword is already associated with a deck!")
        self._deck = deck

    @property
    def format(self) -> format_type:
        """Get or set the format for this keyword."""
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        self._format_type = value
        for card in self._cards:
            card.format = value

    def _get_base_title(self) -> str:
        kwd: str = self.keyword
        subkwd: str = self.subkeyword
        if kwd == subkwd:
            return f"{kwd}"
        return f"{kwd}_{subkwd}"

    def get_title(self, format_symbol: str = "") -> str:
        """Get the title of this keyword."""
        base_title = self._get_base_title()
        titles = [base_title]
        if self.options != None:
            options_specs = self.option_specs
            title_suffix_options = [o for o in options_specs if self.is_option_active(o.name) and o.title_order > 0]
            title_suffix_options.sort(key=lambda option: option.title_order)
            suffix_names = [op.name for op in title_suffix_options]
            titles = titles + suffix_names
        return f"*{'_'.join(titles)}{format_symbol}"

    @property
    def user_comment(self) -> str:
        """Get or set the "user comment" for this keyword."""
        return self._user_comment

    @user_comment.setter
    def user_comment(self, value: str) -> None:
        self._user_comment = value

    @property
    def cards(self) -> typing.List[CardInterface]:
        """Gets the cards of the keyword"""
        return self._get_all_cards()

    @property
    def included_from(self) -> str:
        """Get the filename this was included from.

        If the keyword was not read from a file,
        return None.
        """
        return self._included_from

    @included_from.setter
    def included_from(self, value: str):
        self._included_from = value

    def _get_user_comment_lines(self) -> typing.List[str]:
        user_comment = self.user_comment
        if user_comment == "":
            return []
        split_lines = user_comment.split("\n")
        line_start = "$"
        return [f"{line_start}{line}" for line in split_lines]

    def _is_valid(self) -> typing.Tuple[bool, str]:
        return True, ""

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the keyword data as it would appear in the .k file"""
        max_rows = 60  # TODO - make these configurable somewhere

        class TruncatedStringException(Exception):  # noqa: N818
            pass

        class TruncatedStringIO(io.IOBase):
            def __init__(self, max_lines: int) -> None:
                self._io = io.StringIO()
                self._num_lines: int = 0
                self._max_lines: int = max_lines

            def getvalue(self) -> str:
                return self._io.getvalue()

            def seek(self, whence):
                return self._io.seek(whence)

            def tell(self):
                return self._io.tell()

            def truncate(self, size=None):
                return self._io.truncate(size)

            def write(self, value: str) -> None:
                if "\n" not in value:
                    self._io.write(value)
                    return

                value_lines = value.splitlines()
                num_lines = len(value_lines)
                can_write_all_lines = self._num_lines + num_lines < self._max_lines
                if can_write_all_lines:
                    self._num_lines += num_lines
                    self._io.write(value)
                    return

                self._num_lines += len(value.splitlines())
                if self._num_lines > self._max_lines:
                    num_lines_to_write = self._max_lines - num_lines - 1
                    lines_to_write = value_lines[0:num_lines_to_write]
                    self._io.write("\n".join(lines_to_write))
                    self._io.write(f"\n...console output truncated at {self._max_lines} rows")
                    raise TruncatedStringException()
                else:
                    self._io.write(value)

        try:
            buf = TruncatedStringIO(max_rows)
            self.write(buf)
        except TruncatedStringException:
            pass

        return buf.getvalue()

    def _format_to_symbol(self, format: format_type):
        if format == format_type.long:
            return "+"
        if format == format_type.standard:
            return "-"
        if format == format_type.default:
            return ""
        raise RuntimeError("Unexpected format!")

    def _get_write_format(self, format: format_type, deck_format: typing.Optional[format_type] = None) -> format_type:
        """Gets the write format."""
        if format == format_type.default:
            return deck_format

        return format

    def _get_symbol(self, format: format_type, deck_format: typing.Optional[format_type] = None) -> str:
        """Gets the format symbol (+ or -) used when writing the keyword. Depends on the deck format, if any."""
        if format == format_type.default:
            return ""

        if deck_format == format_type.default:
            return self._format_to_symbol(format)

        if deck_format == format:
            # deck uses the same format as the keyword, no need to use a format symbol
            return ""
        else:
            return self._format_to_symbol(format)

    def _write_header(self, buf: typing.TextIO, symbol: str) -> None:
        buf.write(self.get_title(symbol) + "\n")
        for comment_line in self._get_user_comment_lines():
            buf.write(comment_line + "\n")

    def write(
        self,
        buf: typing.Optional[typing.TextIO] = None,
        format: typing.Optional[format_type] = None,
        deck_format: format_type = format_type.default,
        retain_parameters: bool = False,
    ) -> str:
        """Renders the keyword in the dyna keyword format.

        Parameters
        ----------
        buf: IO
            Optional - buffer to write to.
        format: format_type, optional
            Format to use for writing.
        deck_format: format_type, optional
            The deck's format (used to determine if format symbol needed).
        retain_parameters: bool, optional
            If True, write original parameter references (e.g., &myvar) instead of
            substituted values for fields that were read from parameters. Default is False.

        Returns
        -------
        _______
        If `buf` is None, the output is returned as a string
        """
        if format == None:
            format = self.format
        will_return = buf == None
        if will_return:
            buf = io.StringIO()
        self._write_header(buf, self._get_symbol(format, deck_format))
        format = self._get_write_format(format, deck_format)
        Cards.write(self, buf, format, retain_parameters=retain_parameters)
        if will_return:
            keyword_string = buf.getvalue()[: buf.tell()]
            return keyword_string

    def dumps(self) -> str:
        """Return the string representation of the keyword."""
        warnings.warn("dumps is deprecated - use write instead")
        return self.write()

    def before_read(self, buf: typing.TextIO) -> None:
        """Run any pre-processing before reading the keyword."""
        # subclasses can do custom logic before reading.
        return

    def _process_title(self, title_line: str) -> None:
        # Verify the title line and set the format, remove trailing +/- if set
        title_line = title_line.strip()

        # the options are not activated yet, therefore get_title only returns title_prime
        if self.get_title().strip("*") not in title_line:
            raise ValueError(
                f"Title line '{title_line}' does not match expected title '{self.get_title().strip('*')}'."
            )

        if title_line.endswith("-"):
            self.format = format_type.standard
            return title_line[:-1]
        if title_line.endswith("+"):
            self.format = format_type.long
            return title_line[:-1]
        return title_line

    def read(self, buf: typing.TextIO, parameters: ParameterSet = None) -> None:
        """Read the keyword from a buffer."""
        title_line = buf.readline()
        title_line = self._process_title(title_line)
        self.before_read(buf)
        if title_line != self.get_title():
            self._activate_options(title_line.strip("*"))
        # TODO: self.user_comment should come from somewhere.
        # maybe after the keyword but before any $#

        # Store parameter set for write-time reference lookup
        self._parameter_set = parameters

        # Scope the parameter references by this keyword's identity
        if parameters is not None:
            with parameters.scope(str(id(self))):
                self._read_data(buf, parameters)
        else:
            self._read_data(buf, parameters)

    def loads(self, value: str, parameters: ParameterSet = None) -> typing.Any:
        """Load the keyword from string.

        Return `self` to support chaining
        """
        # TODO - add a method to load from a buffer.
        s = io.StringIO()
        s.write(value)
        s.seek(0)
        self.read(s, parameters)
        return self

    # Class attribute to be overridden by subclasses with link field metadata.
    # Maps field names to their LinkType values.
    # Example: {"lcsr": LinkType.DEFINE_CURVE, "tranid": LinkType.DEFINE_TRANSFORMATION}
    _link_fields: typing.ClassVar[typing.Dict[str, "LinkType"]] = {}

    def _get_set_link(
        self,
        subtype_prefix: str,
        value: typing.Any,
    ) -> typing.Optional["KeywordBase"]:
        """Get a SET_* keyword by matching sid with subtype prefix filtering.

        Searches SET keywords where the subkeyword starts with the given prefix
        and the sid matches the given value.

        Parameters
        ----------
        subtype_prefix : str
            The subtype prefix to match (e.g., "NODE", "PART", "BEAM").
        value : Any
            The sid value to search for.

        Returns
        -------
        KeywordBase or None
            The matching SET keyword, or None if not found.
        """
        if self.deck is None or value is None:
            return None
        for kwd in self.deck.get_kwds_by_type("SET"):
            if kwd.subkeyword.startswith(subtype_prefix) and kwd.sid == value:
                return kwd
        return None

    def _get_link_by_attr(
        self,
        keyword_type: str,
        attr_name: str,
        value: typing.Any,
        table_attr: typing.Optional[str] = None,
    ) -> typing.Optional["KeywordBase"]:
        """Get a keyword by matching an attribute value.

        Searches keywords of the specified type for one where ``attr_name``
        matches ``value``. If ``table_attr`` is provided, searches within
        that DataFrame attribute instead of a scalar attribute.

        Parameters
        ----------
        keyword_type : str
            The keyword type to search (e.g., "PART", "MAT", "SECTION").
        attr_name : str
            The attribute/column name to match against.
        value : Any
            The value to search for.
        table_attr : str, optional
            If provided, search within this DataFrame attribute's column
            instead of a scalar attribute.

        Returns
        -------
        KeywordBase or None
            The matching keyword, or None if not found.
        """
        if self.deck is None or value is None:
            return None
        for kwd in self.deck.get_kwds_by_type(keyword_type):
            if table_attr is not None:
                table = getattr(kwd, table_attr, None)
                if table is not None and not table[table[attr_name] == value].empty:
                    return kwd
            elif getattr(kwd, attr_name, None) == value:
                return kwd
        return None

    def _get_links_from_table(
        self,
        keyword_type: str,
        id_attr: str,
        table_name: str,
        id_column: str,
        target_table_attr: typing.Optional[str] = None,
    ) -> typing.Dict[int, "KeywordBase"]:
        """Get keywords for IDs in a table, keyed by ID value.

        Builds a mapping from each ID value in the specified table to the
        keyword that contains that ID.

        Parameters
        ----------
        keyword_type : str
            The keyword type to search (e.g., "PART", "MAT").
        id_attr : str
            The attribute/column name on target keywords to match.
        table_name : str
            Name of the property on self containing the DataFrame with IDs.
        id_column : str
            Name of the column containing ID values in the table.
        target_table_attr : str, optional
            If provided, search within this DataFrame attribute on target
            keywords instead of a scalar attribute.

        Returns
        -------
        Dict[int, KeywordBase]
            Mapping of ID values to keywords.
        """
        if self.deck is None:
            return {}
        table = getattr(self, table_name, None)
        if table is None:
            return {}
        # Build id -> keyword map
        id_to_kwd: typing.Dict[int, "KeywordBase"] = {}
        for kwd in self.deck.get_kwds_by_type(keyword_type):
            if target_table_attr is not None:
                target_table = getattr(kwd, target_table_attr, None)
                if target_table is not None:
                    for id_val in target_table[id_attr].values:
                        id_to_kwd[id_val] = kwd
            else:
                id_val = getattr(kwd, id_attr, None)
                if id_val is not None:
                    id_to_kwd[id_val] = kwd
        # Map ids from our table
        result: typing.Dict[int, "KeywordBase"] = {}
        for id_val in table[id_column].values:
            if id_val in id_to_kwd:
                result[id_val] = id_to_kwd[id_val]
        return result

    def _get_table_group_links(
        self,
        keyword_type: str,
        linkid: str,
        table_name: str,
        field_name: str,
        key_field: str,
    ) -> typing.Dict[int, "KeywordBase"]:
        """Get keywords for IDs in a table-group field, keyed by key_field.

        Used for TableCardGroup patterns where a DataFrame column contains
        references to other keywords.

        Parameters
        ----------
        keyword_type : str
            The keyword type to search (e.g., "HOURGLASS", "MAT").
        linkid : str
            The attribute name on target keywords to match (e.g., "hgid", "mid").
        table_name : str
            Name of the property on self containing the DataFrame.
        field_name : str
            Name of the column containing the link ID values.
        key_field : str
            Name of the column to use as keys in the result dict.

        Returns
        -------
        Dict[int, KeywordBase]
            Mapping of key_field values to keywords.
        """
        if self.deck is None:
            return {}
        table = getattr(self, table_name, None)
        if table is None or field_name not in table.columns:
            return {}
        result = {}
        kwd_map = {getattr(kwd, linkid): kwd for kwd in self.deck.get_kwds_by_type(keyword_type)}
        for _, row in table.iterrows():
            key = row[key_field]
            link_id = row[field_name]
            if link_id in kwd_map:
                result[key] = kwd_map[link_id]
        return result

    def _get_table_group_link(
        self,
        keyword_type: str,
        linkid: str,
        table_name: str,
        field_name: str,
        key_field: str,
        key_value: int,
    ) -> typing.Optional["KeywordBase"]:
        """Get a single keyword for a specific key in a table-group field.

        Parameters
        ----------
        keyword_type : str
            The keyword type to search (e.g., "HOURGLASS", "MAT").
        linkid : str
            The attribute name on target keywords to match.
        table_name : str
            Name of the property on self containing the DataFrame.
        field_name : str
            Name of the column containing the link ID values.
        key_field : str
            Name of the column used as keys.
        key_value : int
            The specific key value to look up.

        Returns
        -------
        KeywordBase or None
            The matching keyword, or None if not found.
        """
        if self.deck is None:
            return None
        table = getattr(self, table_name, None)
        if table is None or field_name not in table.columns:
            return None
        row = table[table[key_field] == key_value]
        if row.empty:
            return None
        link_id = row.iloc[0][field_name]
        for kwd in self.deck.get_kwds_by_type(keyword_type):
            if getattr(kwd, linkid) == link_id:
                return kwd
        return None

    def get_links(
        self,
        link_type: "LinkType" = None,
        level: int = 1,
        _seen: typing.Optional[typing.Set] = None,
    ) -> typing.List["KeywordBase"]:
        """Get all keywords referenced by this keyword.

        Traverses the fields of this keyword that reference other keywords
        and returns the linked keyword objects from the parent deck.

        Parameters
        ----------
        link_type : LinkType, optional
            Filter results by link type. If ``None`` or ``LinkType.ALL``,
            returns all linked keywords. Otherwise, returns only keywords
            matching the specified link type.
        level : int, default=1
            Traversal depth. ``1`` returns only direct links (default),
            ``-1`` traverses recursively without limit, ``0`` returns empty list,
            and any positive integer ``n`` traverses up to ``n`` levels deep.
        _seen : set, optional
            Internal parameter for cycle prevention. Do not pass explicitly.

        Returns
        -------
        List[KeywordBase]
            List of referenced keyword objects. Returns an empty list if
            the keyword is not associated with a deck or if no links are found.

        Examples
        --------
        >>> mat = deck.keywords[0]  # A material keyword with curve references
        >>> curves = mat.get_links(LinkType.DEFINE_CURVE)
        >>> all_links = mat.get_links()  # Get all referenced keywords
        >>> # Get full dependency tree
        >>> all_deps = mat.get_links(level=-1)
        """
        if level == 0:
            return []

        if link_type is None:
            link_type = LinkType.ALL

        if self.deck is None:
            return []

        if _seen is None:
            _seen = set()

        results = []
        for field_name, field_link_type in self._link_fields.items():
            # Filter by link type if specified
            if link_type != LinkType.ALL and field_link_type != link_type:
                continue

            # Collect linked keywords - check both singular and plural patterns
            linked_kwds = []

            # Try singular *_link property first (scalar fields)
            link_property_name = f"{field_name}_link"
            if hasattr(self, link_property_name):
                linked_kwd = getattr(self, link_property_name)
                if linked_kwd is not None:
                    linked_kwds.append(linked_kwd)

            # Also try plural *_links property (TableCard fields return dict)
            links_property_name = f"{field_name}_links"
            if hasattr(self, links_property_name):
                links_dict = getattr(self, links_property_name)
                if links_dict:
                    linked_kwds.extend(links_dict.values())

            # Process all collected linked keywords
            for linked_kwd in linked_kwds:
                # Dedup using (keyword, subkeyword, object_id) to prevent cycles
                kwd_identity = (linked_kwd.keyword, linked_kwd.subkeyword, id(linked_kwd))
                if kwd_identity in _seen:
                    continue
                _seen.add(kwd_identity)
                results.append(linked_kwd)

                # Recurse if level allows
                if level != 1:  # -1 (unlimited) or >1
                    next_level = level - 1 if level > 1 else -1
                    results.extend(linked_kwd.get_links(link_type, next_level, _seen))

        return results
