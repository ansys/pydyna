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

from ansys.dyna.core.keywords.keyword_classes.auto.mat_295 import Mat295 as Parent
from ansys.dyna.core.lib.kwd_line_formatter import read_line


class Mat295(Parent):
    """DYNA MAT_295 keyword"""

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._ftype = None

    def before_read(self, buf: typing.TextIO) -> None:
        """Abstract method to peek into the buffer before reading.

        Normally, the number of rows is calculated by the card, and is based on whether
        the card is active or not. In most cases, a card will know whether it is active
        or not before it is read, because the `active_func` is based on information loaded
        into previous cards. In some cases, `active_func` is based on information from the
        current card, which might default to inactive, and so the card will never be read.
        """
        pos = buf.tell()
        while True:
            line, end = read_line(buf)
            if end:
                break
            if line.startswith("ANISO"):
                # set atype to 1 so that the ANSIO card is active
                self.atype = 1
                # set ftype to 1 so that the second row of the anisotropic_settings card is read
                self.ftype = 1
            elif line.startswith("ACTIVE"):
                # set actype to 1 so that the ACTIVE card is active
                self.actype = 1
        buf.seek(pos)

    @property
    def ftype(self) -> typing.Optional[int]:
        return self._ftype

    @ftype.setter
    def ftype(self, value: int) -> None:
        self._ftype = value


class MatAnisotropicHyperelastic(Mat295):
    subkeyword = "ANISOTROPIC_HYPERELASTIC"
