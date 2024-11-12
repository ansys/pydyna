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

import io
import typing
import warnings

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.option_card import OptionsAPI


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
        self._options_api: OptionsAPI = OptionsAPI(self)
        self._format_type: format_type = kwargs.get("format", format_type.default)
        self._active_options: typing.Set[str] = set()

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
            options_specs = self.options.option_specs
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

    def _get_user_comment_lines(self) -> typing.List[str]:
        user_comment = self.user_comment
        if user_comment == "":
            return []
        split_lines = user_comment.split("\n")
        line_start = "$"
        return [f"{line_start}{line}" for line in split_lines]

    def _is_valid(self) -> typing.Tuple[bool, str]:
        return True, ""

    def is_option_active(self, option: str) -> bool:
        return option in self._active_options

    def activate_option(self, option: str) -> None:
        self._active_options.add(option)

    def deactivate_option(self, option: str) -> None:
        if option in self._active_options:
            self._active_options.remove(option)

    def _try_activate_options(self, names: typing.List[str]) -> None:
        for option in self.options.option_specs:
            if option.name in names:
                self.activate_option(option.name)

    def _activate_options(self, title: str) -> None:
        if self.options is None:
            return
        title_list = title.split("_")
        self._try_activate_options(title_list)

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the keyword data as it would appear in the .k file"""

        max_rows = 60  # TODO - make these configurable somewhere

        class TruncatedStringException(Exception):
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
    ) -> str:
        """Renders the keyword in the dyna keyword format.

        Parameters
        ----------
        buf: IO
            Optional - buffer to write to.

        Returns
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
        superfluous_newline = Cards.write(self, buf, format)
        if will_return:
            keyword_string = buf.getvalue()
            if superfluous_newline:  # remove last character before returning
                return keyword_string[:-1]
            return keyword_string
        else:
            if superfluous_newline:
                buf.seek(buf.tell() - 1)

    def dumps(self) -> str:
        """Return the string representation of the keyword."""
        warnings.warn("dumps is deprecated - use write instead")
        return self.write()

    def before_read(self, buf: typing.TextIO) -> None:
        # subclasses can do custom logic before reading.
        return

    def _process_title(self, title_line: str) -> None:
        # Verify the title line and set the format, remove trailing +/- if set
        title_line = title_line.strip()

        # the options are not activated yet, therefore get_title only returns title_prime
        assert self.get_title().strip("*") in title_line, "first line in loads must contain the keyword title"

        if title_line.endswith("-"):
            self.format = format_type.standard
            return title_line[:-1]
        if title_line.endswith("+"):
            self.format = format_type.long
            return title_line[:-1]
        return title_line

    def read(self, buf: typing.TextIO) -> None:
        title_line = buf.readline()
        title_line = self._process_title(title_line)
        self.before_read(buf)
        if title_line != self.get_title():
            self._activate_options(title_line.strip("*"))
        # TODO: self.user_comment should come from somewhere.
        # maybe after the keyword but before any $#
        self._read_data(buf)

    def loads(self, value: str) -> None:
        """Load the keyword from string."""
        # TODO - add a method to load from a buffer.
        s = io.StringIO()
        s.write(value)
        s.seek(0)
        self.read(s)
