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

class ChemistryModel(KeywordBase):
    """DYNA CHEMISTRY_MODEL keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "MODEL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "modelid",
                        int,
                        0,
                        10,
                        kwargs.get("modelid")
                    ),
                    Field(
                        "jacsel",
                        int,
                        10,
                        10,
                        kwargs.get("jacsel", 1)
                    ),
                    Field(
                        "errlim",
                        float,
                        20,
                        10,
                        kwargs.get("errlim", 1.0e-3)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "file1",
                        str,
                        0,
                        256,
                        kwargs.get("file1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "file2",
                        str,
                        0,
                        256,
                        kwargs.get("file2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "file3",
                        str,
                        0,
                        256,
                        kwargs.get("file3")
                    ),
                ],
            ),
        ]

    @property
    def modelid(self) -> typing.Optional[int]:
        """Get or set the Identifier for this chemkin-based chemistry model.
        """ # nopep8
        return self._cards[0].get_value("modelid")

    @modelid.setter
    def modelid(self, value: int) -> None:
        self._cards[0].set_value("modelid", value)

    @property
    def jacsel(self) -> int:
        """Get or set the Selects the form of the Jacobian matrix for use in the source term.
        EQ.1:	Fully implicit(default)
        EQ.2 : Simplified implicit
        """ # nopep8
        return self._cards[0].get_value("jacsel")

    @jacsel.setter
    def jacsel(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""jacsel must be one of {1,2}""")
        self._cards[0].set_value("jacsel", value)

    @property
    def errlim(self) -> float:
        """Get or set the Allowed error in element balance in a chemical reaction.
        """ # nopep8
        return self._cards[0].get_value("errlim")

    @errlim.setter
    def errlim(self, value: float) -> None:
        self._cards[0].set_value("errlim", value)

    @property
    def file1(self) -> typing.Optional[str]:
        """Get or set the Name of the file containing the Chemkin-compatible input.
        """ # nopep8
        return self._cards[1].get_value("file1")

    @file1.setter
    def file1(self, value: str) -> None:
        self._cards[1].set_value("file1", value)

    @property
    def file2(self) -> typing.Optional[str]:
        """Get or set the Name of the file containing the chemistry thermodynamics database.
        """ # nopep8
        return self._cards[2].get_value("file2")

    @file2.setter
    def file2(self, value: str) -> None:
        self._cards[2].set_value("file2", value)

    @property
    def file3(self) -> typing.Optional[str]:
        """Get or set the Name of the file containing the chemistry transport properties database.
        """ # nopep8
        return self._cards[3].get_value("file3")

    @file3.setter
    def file3(self, value: str) -> None:
        self._cards[3].set_value("file3", value)

