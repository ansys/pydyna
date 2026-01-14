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
"""Module for card handling."""

import io
import typing
import warnings

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.field import Field, Flag, to_long  # noqa: F401
from ansys.dyna.core.lib.field_schema import CardSchema, FieldSchema
from ansys.dyna.core.lib.field_writer import write_comment_line, write_fields, write_fields_csv
from ansys.dyna.core.lib.format_type import card_format, format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import FormatSpec, load_dataline_with_format, read_line
from ansys.dyna.core.lib.parameters import ParameterSet

# Module-level cache for FormatSpec objects, keyed by (signature, format_type)
_format_spec_cache: typing.Dict[typing.Tuple, FormatSpec] = {}

# Module-level cache for from_field_schemas: id(tuple) -> (CardSchema, signature)
_field_schemas_cache: typing.Dict[int, typing.Tuple[CardSchema, tuple]] = {}


def _get_cached_format_spec(signature: tuple, schema: CardSchema, fmt: format_type) -> FormatSpec:
    """Get or create a cached FormatSpec from schema and format type.

    Parameters
    ----------
    signature : tuple
        The hashable signature for this schema (from _get_cached_schema).
    schema : CardSchema
        The card schema.
    fmt : format_type
        The format type (standard or long).

    Returns
    -------
    FormatSpec
        Cached or newly created FormatSpec.
    """
    cache_key = (signature, fmt)
    cached = _format_spec_cache.get(cache_key)
    if cached is not None:
        return cached

    # Build format spec items from schema
    if fmt == format_type.long:
        # Convert to long format widths
        items = []
        offset = 0
        for fs in schema.fields:
            width = max(fs.width, 20)
            if fs.is_flag():
                item_type = fs.default  # The Flag instance
            else:
                item_type = fs.type
            items.append((offset, width, item_type))
            offset += width
    else:
        # Standard format
        items = []
        for fs in schema.fields:
            if fs.is_flag():
                item_type = fs.default  # The Flag instance
            else:
                item_type = fs.type
            items.append((fs.offset, fs.width, item_type))

    format_spec = FormatSpec.from_list(items)
    _format_spec_cache[cache_key] = format_spec
    return format_spec


class Card(CardInterface):
    """A Card represents a single line of data in a keyword.

    This implementation separates schema (shared) from values (per-instance)
    for memory efficiency and faster loading of keywords with many cards.
    """

    def __init__(
        self,
        fields: typing.List[Field],
        active_func: typing.Callable = None,
        format: format_type = format_type.default,
    ):
        """Initialize a Card from Field objects.

        .. deprecated::
            This constructor is deprecated. Prefer using
            `Card.from_field_schemas()` or `Card.from_field_schemas_with_defaults()`
            which provide better performance by avoiding Field object creation.

        Parameters
        ----------
        fields : List[Field]
            List of Field objects defining the card structure and values.
        active_func : callable, optional
            A function returning bool to determine if card is active.
        format : format_type, optional
            The format type (default, standard, or long).
        """
        warnings.warn(
            "Card(fields=[...]) is deprecated. Use Card.from_field_schemas() or "
            "Card.from_field_schemas_with_defaults() for better performance. "
            "This constructor is maintained for backward compatibility.",
            DeprecationWarning,
            stacklevel=2,
        )
        # Build schema from fields
        field_schemas = tuple(FieldSchema.from_field(f) for f in fields)
        name_to_index = {f.name: i for i, f in enumerate(fields)}
        self._schema = CardSchema(field_schemas, name_to_index)
        # Use object id as signature - no cross-instance caching for legacy path
        self._signature = id(self._schema)
        self._values = [f.value for f in fields]
        self._fields_set: bool = False  # Track whether fields were ever explicitly set
        self._active_func = active_func
        self._format_type = format
        self._card_format = card_format.fixed

    @classmethod
    def from_field_schemas(
        cls,
        field_schemas: typing.Tuple[FieldSchema, ...],
        values: typing.Optional[typing.List[typing.Any]] = None,
        active_func=None,
        format=format_type.default,
    ) -> "Card":
        """Create a Card directly from FieldSchema tuples (no Field objects).

        This is the fast path for creating Cards when the schema is already
        defined at the class level. It avoids creating Field objects entirely.

        Parameters
        ----------
        field_schemas : Tuple[FieldSchema, ...]
            Tuple of FieldSchema (should be defined at class level for reuse).
        values : List[Any], optional
            Initial values for each field. If None, uses defaults from schema.
        active_func : callable, optional
            A function returning bool to determine if card is active.
        format : format_type, optional
            The format type (default, standard, or long).

        Returns
        -------
        Card
            A new Card instance.
        """
        instance = cls.__new__(cls)

        # Build CardSchema and signature from field_schemas tuple
        # Use the tuple's id as a cache key for the derived structures
        cache_key = id(field_schemas)
        cached = _field_schemas_cache.get(cache_key)
        if cached is not None:
            schema, signature = cached
        else:
            name_to_index = {fs.name: i for i, fs in enumerate(field_schemas)}
            schema = CardSchema(field_schemas, name_to_index)
            signature = tuple(
                (
                    (fs.name, fs.type, fs.offset, fs.width, fs.default.true_value, fs.default.false_value)
                    if fs.is_flag()
                    else (fs.name, fs.type, fs.offset, fs.width)
                )
                for fs in field_schemas
            )
            _field_schemas_cache[cache_key] = (schema, signature)

        instance._schema = schema
        instance._signature = signature

        # Initialize values from provided list or from schema defaults
        if values is not None:
            instance._values = list(values)
        else:
            instance._values = [fs.default for fs in field_schemas]

        instance._fields_set: bool = False
        instance._active_func = active_func
        instance._format_type = format
        instance._card_format = card_format.fixed
        return instance

    @classmethod
    def from_field_schemas_with_defaults(
        cls,
        field_schemas: typing.Tuple[FieldSchema, ...],
        active_func=None,
        format=format_type.default,
        **kwargs,
    ) -> "Card":
        """Create a Card from FieldSchema tuples with kwargs defaults (lspp_defaults).

        This is like from_field_schemas but applies lspp_defaults logic:
        - If use_lspp_defaults() is True: look up value in kwargs by field name,
          falling back to the schema default.
        - If use_lspp_defaults() is False: all fields are None.

        Parameters
        ----------
        field_schemas : Tuple[FieldSchema, ...]
            Tuple of FieldSchema (should be defined at class level for reuse).
        active_func : callable, optional
            A function returning bool to determine if card is active.
        format : format_type, optional
            The format type (default, standard, or long).
        **kwargs
            Field values by name (lspp_defaults style).

        Returns
        -------
        Card
            A new Card instance.
        """
        from ansys.dyna.core.lib.config import use_lspp_defaults

        # Build values list: respect use_lspp_defaults() setting
        # Always apply kwargs if provided, but only fall back to schema defaults
        # when use_lspp_defaults() is True
        if use_lspp_defaults():
            values = [kwargs.get(fs.name, fs.default) for fs in field_schemas]
        else:
            values = [kwargs.get(fs.name, None) for fs in field_schemas]
        return cls.from_field_schemas(field_schemas, values, active_func, format)

    @property
    def format(self):
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        self._format_type = value

    @property
    def _fields(self) -> typing.List[Field]:
        """Lazily create Field objects from schema + values.

        This property creates Field objects on-demand for operations
        that need them (like write). For read operations, we use
        schema + values directly.
        """
        return self._schema.to_fields(self._values)

    def _convert_fields_to_long_format(self) -> typing.List[Field]:
        """Convert fields to long format (20-char minimum width)."""
        fields = []
        offset = 0
        for fs, value in zip(self._schema.fields, self._values):
            new_width = max(fs.width, 20)
            new_field = Field(fs.name, fs.type, offset, new_width, value)
            if fs.is_flag():
                # Reconstruct flag field
                flag = Flag(
                    value=value,
                    true_value=fs.default.true_value,
                    false_value=fs.default.false_value,
                )
                new_field = Field(fs.name, fs.type, offset, new_width, flag)
            offset += new_width
            fields.append(new_field)
        return fields

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> bool:
        if not self.active:
            return False
        line, to_exit = read_line(buf)
        if to_exit:
            return True
        self._load(line, parameter_set)
        return False

    def _load(self, data_line: str, parameter_set: ParameterSet) -> None:
        """Load card data from a string line.

        Uses cached FormatSpec for efficient parsing.
        """
        current_format = self.format
        format_spec = _get_cached_format_spec(self._signature, self._schema, current_format)

        values, detected_format = load_dataline_with_format(format_spec, data_line, parameter_set)
        self._card_format = detected_format

        # Update values directly (no Field objects needed)
        for i in range(len(self._schema)):
            self._values[i] = values[i]

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
        output_format: typing.Optional[str] = None,
    ) -> typing.Union[str, None]:
        """Write the card to a buffer or return as string.

        Parameters
        ----------
        format : format_type, optional
            Field width format (default, standard, or long).
        buf : TextIO, optional
            Buffer to write to. If None, returns the output as a string.
        comment : bool, optional
            Whether to include the comment line with field names. Default is True.
        output_format : str, optional
            Card serialization format: card_format.fixed or card_format.csv.
            If None (default), uses fixed unless the card was originally read as csv.

        Returns
        -------
        str or None
            The card as a string if buf is None, otherwise None.
        """
        if format is None:
            format = self._format_type
        if output_format is None:
            output_format = self._card_format

        def _write(buf: typing.TextIO):
            if self.active:
                # Create fields lazily for write
                fields = self._fields
                if format == format_type.long:
                    fields = self._convert_fields_to_long_format()

                if output_format == card_format.csv:
                    # CSV format: no comment line, comma-separated values
                    write_fields_csv(buf, fields)
                else:
                    # Fixed-width format (default)
                    if comment:
                        write_comment_line(buf, fields, format)
                        buf.write("\n")
                    write_fields(buf, fields, None, format)

        return write_or_return(buf, _write)

    @property
    def active(self) -> bool:
        if self._active_func is None:
            return True
        return True if self._active_func() else False

    # only used by tests, TODO move to conftest
    def _get_comment(self, format: format_type) -> str:
        s = io.StringIO()
        write_comment_line(s, self._fields, format)
        return s.getvalue()

    def _get_field_by_name(self, prop: str) -> Field:
        """Get a Field object by name. Creates Field lazily."""
        idx = self._schema.get_index(prop)
        fs = self._schema.fields[idx]
        return fs.to_field(self._values[idx])

    def get_value(self, prop: str) -> typing.Any:
        """Get the value of a field by name.

        Parameters
        ----------
        prop : str
            The field name.

        Returns
        -------
        Any
            The field value.
        """
        idx = self._schema.get_index(prop)
        return self._values[idx]

    def set_value(self, prop: str, value: typing.Any) -> None:
        """Set the value of a field by name.

        Parameters
        ----------
        prop : str
            The field name.
        value : Any
            The value to set.
        """
        idx = self._schema.get_index(prop)
        self._values[idx] = value
        self._fields_set = True  # Track that this field was explicitly set

    def has_nondefault_values(self) -> bool:
        """Check if any field in this card has been explicitly set.

        This method returns True if any field has been set via set_value(),
        which is typically called by property setters on keyword classes.
        It is used for cascading card activation, where optional cards
        become active when any of their fields are explicitly set.

        Returns
        -------
        bool
            True if at least one field has been explicitly set, False otherwise.
        """
        return self._fields_set

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the card."""
        content_lines = []
        content_lines.append(self._get_comment(self._format_type))
        output = "\n".join(content_lines)
        return "StandardCard:" + output
