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
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ChemistryControlBlast1D(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_BLAST1D keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_BLAST1D"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "blastid",
                        int,
                        0,
                        10,
                        kwargs.get("blastid")
                    ),
                    Field(
                        "x0",
                        float,
                        10,
                        10,
                        kwargs.get("x0")
                    ),
                    Field(
                        "y0",
                        float,
                        20,
                        10,
                        kwargs.get("y0")
                    ),
                    Field(
                        "z0",
                        float,
                        30,
                        10,
                        kwargs.get("z0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "file",
                        str,
                        0,
                        256,
                        kwargs.get("file")
                    ),
                ],
            ),
        ]

    @property
    def blastid(self) -> typing.Optional[int]:
        """Get or set the Identifier for this one-dimensional detonation solution.
        """ # nopep8
        return self._cards[0].get_value("blastid")

    @blastid.setter
    def blastid(self, value: int) -> None:
        self._cards[0].set_value("blastid", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Origin of the detonation
        """ # nopep8
        return self._cards[0].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[0].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Origin of the detonation
        """ # nopep8
        return self._cards[0].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        self._cards[0].set_value("y0", value)

    @property
    def z0(self) -> typing.Optional[float]:
        """Get or set the Origin of the detonation
        """ # nopep8
        return self._cards[0].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        self._cards[0].set_value("z0", value)

    @property
    def file(self) -> typing.Optional[str]:
        """Get or set the Name of the LSDA file containing the one-dimensional solution.
        """ # nopep8
        return self._cards[1].get_value("file")

    @file.setter
    def file(self, value: str) -> None:
        self._cards[1].set_value("file", value)

