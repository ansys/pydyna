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

"""Function to write cards."""

import typing

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.format_type import format_type


def write_cards(
    cards: typing.List[CardInterface],
    buf: typing.TextIO,
    write_format: format_type,
    comment: typing.Optional[bool] = True,
) -> bool:
    """Write the cards. Return whether a superfluous trailing newline was added."""

    # this code tries its best to avoid adding superfluous trailing newlines, but
    # is not always successful. If one or more empty cards exist at the end of the
    # keyword, a single newline will be added before them. Streams are typically
    # write-only, and it is hard to remove the trailing newline from the stream.
    # HOWEVER - if no buffer is passed in, this scenario can be detected and the
    # trailing newline can be removed from the return value. In addition, if the
    # keywords are written as part of a deck, the deck can detect if any keyword
    # added a trailing newline and seek back one character to continue writing
    # more keywords.

    pos = buf.tell()  # record the position of the last newline
    for card in cards:
        if buf.tell() != pos:
            # if we have written since the last newline, we need to prepend a new line
            # (unless this is the last newline to write?)
            buf.write("\n")
            pos = buf.tell()
        card.write(write_format, buf, comment)
    return pos == buf.tell()
