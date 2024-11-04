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

import typing


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


def load_dataline(spec: typing.List[tuple], line_data: str) -> tuple:
    """loads a keyword card line with fixed column offsets and width from string
    spec: list of tuples representing the (offset, width, type) of each field
    line_data: string with keyword data
    example:
    >>> load_dataline([(0,10, int),(10,10, str)], '         1     hello')
    (1, 'hello')
    """

    def seek_text_block(line_data: str, position: int, width: int) -> str:
        """Returns the text block from the line at the given position and width
        If the position is past the end, it will return an empty string"""
        end_position = position + width
        text_block = line_data[position:end_position]
        return end_position, text_block

    def has_value(text_block: str) -> bool:
        """Given a text block - determine if a keyword value exists.
        if its an empty string (i.e. the seek_text_block returned an empty
        string) then there is no value.  If its just whitespace, then
        the keyword file did not include data for that field.
        """
        if text_block == "":
            return False
        if text_block.isspace():
            return False
        return True

    def get_none_value(item_type):
        if item_type is float:
            return float("nan")
        return None

    data = []
    end_position = 0
    for item_spec in spec:
        position, width, item_type = item_spec
        end_position, text_block = seek_text_block(line_data, position, width)
        if not has_value(text_block):
            value = get_none_value(item_type)
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
        raise Exception("Data line is too long!")

    return tuple(data)
