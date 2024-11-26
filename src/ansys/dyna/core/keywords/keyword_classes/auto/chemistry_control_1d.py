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

class ChemistryControl1D(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_1D keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_1D"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "xyzd",
                        float,
                        10,
                        10,
                        kwargs.get("xyzd")
                    ),
                    Field(
                        "detdir",
                        int,
                        20,
                        10,
                        kwargs.get("detdir")
                    ),
                    Field(
                        "csp_sel",
                        int,
                        30,
                        10,
                        kwargs.get("csp_sel", 0)
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
            Card(
                [
                    Field(
                        "ampl",
                        float,
                        0,
                        10,
                        kwargs.get("ampl")
                    ),
                    Field(
                        "ycut",
                        float,
                        10,
                        10,
                        kwargs.get("ycut")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Identifier for this one-dimensional detonation solution.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def xyzd(self) -> typing.Optional[float]:
        """Get or set the Position of the detonation front in the DETDIR direction.
        """ # nopep8
        return self._cards[0].get_value("xyzd")

    @xyzd.setter
    def xyzd(self, value: float) -> None:
        self._cards[0].set_value("xyzd", value)

    @property
    def detdir(self) -> typing.Optional[int]:
        """Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)
        """ # nopep8
        return self._cards[0].get_value("detdir")

    @detdir.setter
    def detdir(self, value: int) -> None:
        self._cards[0].set_value("detdir", value)

    @property
    def csp_sel(self) -> int:
        """Get or set the CSP solver option:
        EQ.0: Do not use the CSP solver, and ignore the AMPL and YCUT parameters (default).
        GT.0: Use the CSP solver, with the AMPL and YCUT parameters.
        """ # nopep8
        return self._cards[0].get_value("csp_sel")

    @csp_sel.setter
    def csp_sel(self, value: int) -> None:
        self._cards[0].set_value("csp_sel", value)

    @property
    def file(self) -> typing.Optional[str]:
        """Get or set the Name of the LSDA file containing the one-dimensional solution.
        """ # nopep8
        return self._cards[1].get_value("file")

    @file.setter
    def file(self, value: str) -> None:
        self._cards[1].set_value("file", value)

    @property
    def ampl(self) -> typing.Optional[float]:
        """Get or set the Relative accuracy for the mass fraction of a chemical species in the Chemkin input file.
        """ # nopep8
        return self._cards[2].get_value("ampl")

    @ampl.setter
    def ampl(self, value: float) -> None:
        self._cards[2].set_value("ampl", value)

    @property
    def ycut(self) -> typing.Optional[float]:
        """Get or set the Absolute accuracy for the mass fraction of a chemical species in the Chemkin input file.
        """ # nopep8
        return self._cards[2].get_value("ycut")

    @ycut.setter
    def ycut(self, value: float) -> None:
        self._cards[2].set_value("ycut", value)

