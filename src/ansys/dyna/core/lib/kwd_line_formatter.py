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

"""Keyword line formatter."""

import dataclasses
import logging
import typing
import warnings

from ansys.dyna.core.lib import config
from ansys.dyna.core.lib.field import Flag
from ansys.dyna.core.lib.parameters import ParameterSet

logger = logging.getLogger(__name__)


def read_line(buf: typing.TextIO, skip_comment=True) -> typing.Tuple[str, bool]:
    """Read and return the line, and a flag on whether to stop reading."""
    while True:
        line = buf.readline()
        len_line = len(line)
        if len_line == 0:
            return None, True
        if skip_comment and line.startswith("$"):
            continue
        if line.startswith("*"):
            # walk back to the start of the line
            buf.seek(buf.tell() - len_line)
            return None, True
        if line.endswith("\n"):
            line = line[:-1]
        return line, False


def at_end_of_keyword(buf: typing.TextIO) -> bool:
    """Return whether the buffer is at the end of the keyword"""
    pos = buf.tell()
    _, end_of_keyword = read_line(buf, True)
    if end_of_keyword:
        return True
    buf.seek(pos)
    return False


def buffer_to_lines(buf: typing.TextIO, max_num_lines: int = -1) -> typing.List[str]:
    """Read from the buffer into a list of string.
    buf: buffer to read from
    max_num_lines: number of lines to read. -1 means no limit
    """
    # used by tabular cards (duplicate card, duplicate card group)
    # store all lines until one that starts with * into an array and then call load with it.
    # perhaps pandas will support a iteration system that allows exiting
    # on a certain line, but that doesn't appear to be possible yet.
    # alternatively we could wrap the buffer in a class that does this, that might
    # end up being better for performance
    # https://pandas.pydata.org/docs/user_guide/io.html#iterating-through-files-chunk-by-chunk
    data_lines: typing.List[str] = []
    if max_num_lines == 0:
        return data_lines

    index = -1
    while True:
        index += 1
        if max_num_lines > 0 and index == max_num_lines:
            break
        line, exit_loop = read_line(buf)
        if exit_loop:
            break
        data_lines.append(line)
    return data_lines


def _is_flag(item_type: typing.Union[type, Flag]):
    if isinstance(item_type, Flag):
        return True
    return False


class FormatSpec:
    """Immutable, hashable specification for parsing fixed-width data lines.

    This class wraps the format specification (list of offset, width, type tuples)
    and caches computed properties like expanded spec and dataclass detection.

    Using a class instead of raw tuples allows functools.lru_cache to work
    efficiently on functions that take specs as arguments.
    """

    __slots__ = ("_items", "_hash", "_expanded", "_has_dataclass")

    def __init__(
        self,
        items: typing.Tuple[typing.Tuple[int, int, type], ...],
        _precomputed_hash: typing.Optional[int] = None,
    ):
        """Initialize FormatSpec from a tuple of (offset, width, type) tuples.

        Parameters
        ----------
        items : tuple of tuples
            Each inner tuple is (offset, width, type) where type can be
            int, float, str, Flag, or a dataclass.
        _precomputed_hash : int, optional
            Pre-computed hash value to avoid recomputation. Internal use only.
        """
        self._items = items
        # Use pre-computed hash if provided, otherwise compute
        self._hash = _precomputed_hash if _precomputed_hash is not None else self._compute_hash(items)
        # Lazily computed
        self._expanded: typing.Optional[typing.Tuple] = None
        self._has_dataclass: typing.Optional[bool] = None

    @staticmethod
    def _compute_hash(items: tuple) -> int:
        """Compute hash for items, handling unhashable Flag objects."""
        hashable_items = []
        for offset, width, item_type in items:
            if isinstance(item_type, Flag):
                # Convert Flag to a hashable tuple representation
                hashable_type = ("Flag", item_type.value, item_type.true_value, item_type.false_value)
            else:
                hashable_type = item_type
            hashable_items.append((offset, width, hashable_type))
        return hash(tuple(hashable_items))

    # Class-level cache for FormatSpec instances
    _cache: typing.ClassVar[typing.Dict[int, "FormatSpec"]] = {}

    @classmethod
    def from_list(cls, spec_list: typing.List[tuple]) -> "FormatSpec":
        """Create FormatSpec from a list of (offset, width, type) tuples.

        Results are cached by hash to avoid creating duplicate FormatSpec objects.
        """
        items = tuple(spec_list)
        cache_key = cls._compute_hash(items)
        cached = cls._cache.get(cache_key)
        if cached is not None and cached._items == items:
            return cached
        # Pass pre-computed hash to avoid recomputing in __init__
        instance = cls(items, _precomputed_hash=cache_key)
        cls._cache[cache_key] = instance
        return instance

    @classmethod
    def from_fields(cls, fields) -> "FormatSpec":
        """Create FormatSpec from a list of Field objects."""
        items = []
        for field in fields:
            if field._is_flag():
                field_type = field._value
            else:
                field_type = field.type
            items.append((field.offset, field.width, field_type))
        return cls.from_list(items)

    def __hash__(self) -> int:
        return self._hash

    def __eq__(self, other) -> bool:
        if not isinstance(other, FormatSpec):
            return False
        return self._items == other._items

    def __iter__(self):
        return iter(self._items)

    def __len__(self) -> int:
        return len(self._items)

    @property
    def expanded(self) -> typing.Tuple:
        """Get expanded spec, unpacking any dataclass fields."""
        if self._expanded is None:
            self._compute_expanded()
        return self._expanded

    @property
    def has_dataclass(self) -> bool:
        """Check if any field type is a dataclass."""
        if self._has_dataclass is None:
            self._compute_expanded()
        return self._has_dataclass

    def _compute_expanded(self) -> None:
        """Compute expanded spec and has_dataclass flag."""
        specs = []
        has_dc = False
        for position, width, item_type in self._items:
            if _is_flag(item_type):
                specs.append((position, width, item_type))
            elif dataclasses.is_dataclass(item_type):
                has_dc = True
                offset = position
                for field in dataclasses.fields(item_type):
                    specs.append((offset, width, field.type))
                    offset = offset + width
            else:
                specs.append((position, width, item_type))
        self._expanded = tuple(specs)
        self._has_dataclass = has_dc

    def contract_data(self, data: typing.List) -> typing.Tuple:
        """Contract expanded data back, packing dataclass fields into instances."""
        if not self.has_dataclass:
            # Fast path: no dataclasses, just return as tuple
            return tuple(data)

        result = []
        iterdata = iter(data)
        for _, _, item_type in self._items:
            if _is_flag(item_type):
                result.append(next(iterdata))
            elif dataclasses.is_dataclass(item_type):
                args = [next(iterdata) for _ in dataclasses.fields(item_type)]
                result.append(item_type(*args))
            else:
                result.append(next(iterdata))
        return tuple(result)


# Legacy functions for backward compatibility - delegate to FormatSpec
def _expand_spec(spec: typing.List[tuple]) -> typing.List[tuple]:
    """Expand spec, handling dataclass types by unpacking their fields.

    DEPRECATED: Use FormatSpec.expanded instead for better performance.
    """
    return list(FormatSpec.from_list(spec).expanded)


def _spec_has_dataclass(spec: typing.List[tuple]) -> bool:
    """Check if spec contains any dataclass types.

    DEPRECATED: Use FormatSpec.has_dataclass instead for better performance.
    """
    return FormatSpec.from_list(spec).has_dataclass


def _convert_type(raw_value, item_type):
    if item_type is int and isinstance(raw_value, float):
        # ensure that float raw_values are convertible to int
        if raw_value.is_integer():
            return int(raw_value)
        raise ValueError(f"Cannot convert non-integer float: {raw_value}")
    return item_type(raw_value)


def _contract_data(spec: typing.List[tuple], data: typing.List) -> typing.Iterable:
    """Contract expanded data back, packing dataclass fields.

    DEPRECATED: Use FormatSpec.contract_data() instead for better performance.
    """
    return FormatSpec.from_list(spec).contract_data(data)


def _is_comma_delimited(line_data: str, num_fields: int = None) -> bool:
    """Detect if a line uses comma-delimited (free) format.

    LS-DYNA allows comma-delimited format as an alternative to fixed-width.
    Detection is based on these rules:
    1. Line must contain commas outside of parentheses (not function calls)
    2. If expected field count is known, CSV field count should match
    3. Otherwise, use heuristics based on structure

    Parameters
    ----------
    line_data : str
        The line to check.
    num_fields : int, optional
        Expected number of fields if known. Helps disambiguate.

    Returns
    -------
    bool
        True if the line appears to be comma-delimited.
    """
    if not config.csv_autodetect_enabled():
        return False

    if "," not in line_data:
        return False

    # Check if commas are inside parentheses (function calls like min(1,2,3))
    # If so, this is NOT CSV format - it's an expression
    paren_depth = 0
    has_top_level_comma = False
    for char in line_data:
        if char == "(":
            paren_depth += 1
        elif char == ")":
            paren_depth = max(0, paren_depth - 1)
        elif char == "," and paren_depth == 0:
            has_top_level_comma = True
            break

    if not has_top_level_comma:
        return False

    # Count comma-separated fields (at top level)
    csv_fields = line_data.split(",")
    num_csv_fields = len(csv_fields)

    # If we know expected field count, check if CSV count matches
    if num_fields is not None:
        # If CSV field count matches expected (within ±1), likely CSV
        if num_csv_fields >= num_fields - 1 and num_csv_fields <= num_fields + 2:
            # Additional check: first field shouldn't have many leading spaces
            # (which would indicate fixed-width format with embedded commas)
            first_field = csv_fields[0]
            leading_spaces = len(first_field) - len(first_field.lstrip())
            if leading_spaces < 6:
                return True

    # Without field count hint, be more conservative
    # Only trigger for lines that look clearly like CSV (3+ short fields)
    if num_csv_fields >= 3:
        first_field = csv_fields[0]
        leading_spaces = len(first_field) - len(first_field.lstrip())
        if leading_spaces < 6:
            return True

    return False


def _parse_csv_value(
    text_block: str,
    item_type: type,
    parameter_set: typing.Optional[ParameterSet],
    get_none_value: typing.Callable,
    get_parameter: typing.Callable,
    field_index: int = 0,
) -> typing.Any:
    """Parse a single CSV field value.

    Parameters
    ----------
    text_block : str
        The text value from the CSV field (already stripped of surrounding whitespace).
    item_type : type
        The expected type for this field (int, float, str, or Flag).
    parameter_set : ParameterSet, optional
        Parameter set for resolving parameter references.
    get_none_value : callable
        Function to get the appropriate None/default value for the type.
    get_parameter : callable
        Function to resolve parameter references. Signature: (text_block, item_type, field_index) -> value.
    field_index : int, optional
        The index of this field in the spec, used for recording parameter references.

    Returns
    -------
    Any
        The parsed value.
    """
    # Check for empty/blank value
    if text_block == "" or text_block.isspace():
        return get_none_value(item_type)

    text_block = text_block.strip()

    # Check for parameter reference
    if "&" in text_block:
        if parameter_set is None:
            raise ValueError("Parameter set must be provided when using parameters in keyword data.")
        return get_parameter(text_block, item_type, field_index)

    # Handle Flag type
    if _is_flag(item_type):
        flag: Flag = item_type
        if flag.true_value and flag.true_value in text_block:
            return True
        if flag.false_value and flag.false_value in text_block:
            return False
        if len(flag.false_value) == 0:
            warnings.warn("value detected in field where false value was an empty string")
            return False
        raise Exception("Failed to find true or false value in flag")

    # Parse by type
    if item_type is int:
        return int(float(text_block))
    elif item_type is str:
        return text_block
    elif item_type is float:
        return float(text_block)
    else:
        raise Exception(f"Unexpected type in CSV field: {item_type}")


def _load_dataline_csv(
    spec: typing.List[tuple], line_data: str, parameter_set: typing.Optional[ParameterSet] = None
) -> typing.Tuple:
    """Load a comma-delimited (free format) keyword card line.

    LS-DYNA allows comma-separated values as an alternative to fixed-width format.
    Field positions/widths from the spec are ignored; fields are matched by order.

    Parameters
    ----------
    spec : list of tuple
        List of tuples representing the (offset, width, type) of each field.
    line_data : str
        Comma-separated string with keyword data.
    parameter_set : ParameterSet, optional
        Optional parameter set for resolving parameter references.

    Returns
    -------
    tuple
        Parsed values from the keyword card line.
    """
    logger.debug("Parsing comma-delimited line: %r", line_data)

    def get_none_value(item_type):
        if item_type is float:
            return float("nan")
        if _is_flag(item_type):
            if len(item_type.false_value) == 0:
                return False
            if len(item_type.true_value) == 0:
                return True
            raise Exception(
                "No input data for flag. Expected true or false value because neither uses `no input` as a value!"
            )
        return None

    def get_parameter(text_block: str, item_type: type, field_index: int) -> typing.Any:
        original_ref = text_block.strip()  # Preserve original reference string
        text_block = original_ref
        negative = False
        if text_block.startswith("-&"):
            negative = True
            text_block = text_block[1:]

        if not text_block.startswith("&"):
            raise ValueError(f"Expected parameter to start with '&', got '{text_block}' instead.")
        param_name = text_block[1:]
        raw_value = parameter_set.get(param_name)

        try:
            value = _convert_type(raw_value, item_type)
        except Exception:
            raise TypeError(
                f"Expected parameter '{param_name}' with value {raw_value} not convertible to type {item_type}."
            )
        if negative:
            value *= -1.0

        # Record the parameter reference for potential write-back
        parameter_set.record_ref(str(field_index), original_ref)

        return value

    expanded_spec = _expand_spec(spec)
    csv_fields = line_data.split(",")
    num_spec_fields = len(expanded_spec)
    num_csv_fields = len(csv_fields)

    if num_csv_fields > num_spec_fields:
        logger.warning(
            "CSV line has %d fields but spec expects %d; extra fields will be ignored",
            num_csv_fields,
            num_spec_fields,
        )
    elif num_csv_fields < num_spec_fields:
        logger.debug(
            "CSV line has %d fields but spec expects %d; missing fields will use defaults",
            num_csv_fields,
            num_spec_fields,
        )

    data = []
    for i, item_spec in enumerate(expanded_spec):
        _, _, item_type = item_spec
        if i < num_csv_fields:
            text_block = csv_fields[i]
            value = _parse_csv_value(text_block, item_type, parameter_set, get_none_value, get_parameter, i)
        else:
            value = get_none_value(item_type)
        data.append(value)

    data = list(_contract_data(spec, data))
    logger.debug("Parsed CSV values: %r", data)
    return tuple(data)


# Module-level helper functions for load_dataline (moved out of function for performance)
def seek_text_block(line_data: str, position: int, width: int) -> typing.Tuple[int, str]:
    """Returns the text block from the line at the given position and width.

    If the position is past the end, it will return an empty string.
    """
    end_position = position + width
    text_block = line_data[position:end_position]
    return end_position, text_block


def has_value(text_block: str) -> bool:
    """Given a text block - determine if a keyword value exists.

    If its an empty string (i.e. the seek_text_block returned an empty
    string) then there is no value. If its just whitespace, then
    the keyword file did not include data for that field.
    """
    if text_block == "":
        return False
    if text_block.isspace():
        return False
    return True


def _get_none_value(item_type):
    """Get the appropriate None/default value for a field type."""
    if item_type is float:
        return float("nan")
    if _is_flag(item_type):
        if len(item_type.false_value) == 0:
            return False
        if len(item_type.true_value) == 0:
            return True
        raise Exception(
            "No input data for flag. Expected true or false value because neither uses `no input` as a value!"
        )
    return None


def _has_parameter(text_block: str) -> bool:
    """Check if a text block contains a parameter reference."""
    return "&" in text_block


def load_dataline(spec: typing.List[tuple], line_data: str, parameter_set: ParameterSet = None) -> typing.List:
    """
    Loads a keyword card line from string, auto-detecting fixed-width or comma-delimited format.

    LS-DYNA supports both fixed-width format and comma-delimited (free) format. This function
    automatically detects which format is used based on the presence of commas in the line.
    Per the LS-DYNA manual, formats can be mixed within a deck but not within a single card.

    Parameters
    ----------
    spec : list of tuple
        List of tuples representing the (offset, width, type) of each field.
        Type can be a Flag which represents the True and False value.
    line_data : str
        String with keyword data (fixed-width or comma-delimited).
    parameter_set : ParameterSet, optional
        Optional parameter set.

    Returns
    -------
    list
        Parsed values from the keyword card line.

    Examples
    --------
    >>> load_dataline([(0,10, int),(10,10, str)], '         1     hello')
    (1, 'hello')
    >>> load_dataline([(0,10, int),(10,10, str)], '1,hello')
    (1, 'hello')
    """
    # Auto-detect comma-delimited format, passing field count as hint
    num_fields = len(spec)
    if _is_comma_delimited(line_data, num_fields):
        logger.debug("Detected comma-delimited format for line")
        return _load_dataline_csv(spec, line_data, parameter_set)

    # Fixed-width format parsing (original implementation)
    logger.debug("Using fixed-width format for line")

    def get_parameter(text_block: str, item_type: type, field_index: int) -> typing.Any:
        original_ref = text_block.strip()  # Preserve original reference string
        text_block = original_ref
        negative = False
        if text_block.startswith("-&"):
            negative = True
            text_block = text_block[1:]

        if not text_block.startswith("&"):
            raise ValueError(f"Expected parameter to start with '&', got '{text_block}' instead.")
        param_name = text_block[1:]
        raw_value = parameter_set.get(param_name)

        try:
            value = _convert_type(raw_value, item_type)
        except:
            raise TypeError(
                f"Expected parameter '{param_name}' with value {raw_value} not convertible to type {item_type}."
            )
        if negative:
            value *= -1.0

        # Record the parameter reference for potential write-back
        parameter_set.record_ref(str(field_index), original_ref)

        return value

    expanded_spec = _expand_spec(spec)
    data = []
    end_position = 0
    for field_index, item_spec in enumerate(expanded_spec):
        position, width, item_type = item_spec
        end_position, text_block = seek_text_block(line_data, position, width)
        if not has_value(text_block):
            value = _get_none_value(item_type)
        elif _is_flag(item_type):
            flag: Flag = item_type
            true_value = flag.true_value
            false_value = flag.false_value
            # check  the true value first, empty text may be false but not true
            if true_value in text_block:
                value = True
            else:
                if len(false_value) == 0:
                    warnings.warn("value detected in field where false value was an empty string")
                    value = False
                else:
                    if false_value in text_block:
                        value = False
                    raise Exception("Failed to find true or false value in flag")
        elif _has_parameter(text_block):
            if parameter_set is None:
                raise ValueError("Parameter set must be provided when using parameters in keyword data.")
            value = get_parameter(text_block, item_type, field_index)
        elif item_type is int:
            value = int(float(text_block))
        elif item_type is str:
            value = text_block.strip()
        elif item_type is float:
            value = float(text_block)
        else:
            raise Exception(f"Unexpected type in load_dataline spec: {item_type}")
        data.append(value)

    if end_position < len(line_data):
        warning_message = f'Detected out of bound card characters:\n"{line_data[end_position:]}"\n"Ignoring.'
        warnings.warn(warning_message)
    data = list(_contract_data(spec, data))
    return tuple(data)


def load_dataline_with_format(
    spec: typing.List[tuple], line_data: str, parameter_set: ParameterSet = None
) -> typing.Tuple[typing.List, str]:
    """
    Like load_dataline, but also returns the detected format.

    Returns
    -------
    tuple
        A tuple of (values, format) where format is 'csv' or 'fixed'.
    """
    from ansys.dyna.core.lib.format_type import card_format

    num_fields = len(spec)
    is_csv = _is_comma_delimited(line_data, num_fields)
    values = load_dataline(spec, line_data, parameter_set)
    detected_format = card_format.csv if is_csv else card_format.fixed
    return values, detected_format
