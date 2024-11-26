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

class DatabasePapOutput(KeywordBase):
    """DYNA DATABASE_PAP_OUTPUT keyword"""

    keyword = "DATABASE"
    subkeyword = "PAP_OUTPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ivel",
                        int,
                        0,
                        10,
                        kwargs.get("ivel", 0)
                    ),
                    Field(
                        "iaccx",
                        int,
                        10,
                        10,
                        kwargs.get("iaccx", 0)
                    ),
                    Field(
                        "iaccy",
                        int,
                        20,
                        10,
                        kwargs.get("iaccy", 0)
                    ),
                    Field(
                        "iaccz",
                        int,
                        30,
                        10,
                        kwargs.get("iaccz", 0)
                    ),
                    Field(
                        "ncyout",
                        int,
                        40,
                        10,
                        kwargs.get("ncyout", 100)
                    ),
                ],
            ),
        ]

    @property
    def ivel(self) -> int:
        """Get or set the Meaning of "velocity" in d3plot and d3thdt output files
        0:  Nodal velocity vector
        1:  Seepage velocity vector
        """ # nopep8
        return self._cards[0].get_value("ivel")

    @ivel.setter
    def ivel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ivel must be one of {0,1}""")
        self._cards[0].set_value("ivel", value)

    @property
    def iaccx(self) -> int:
        """Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
        0:  Not written
        21: Nodal air density
        22: Nodal pore air pressure
        24: Nodal air mass
        25: Nodal air mass flow rate
        """ # nopep8
        return self._cards[0].get_value("iaccx")

    @iaccx.setter
    def iaccx(self, value: int) -> None:
        if value not in [0, 21, 22, 24, 25]:
            raise Exception("""iaccx must be one of {0,21,22,24,25}""")
        self._cards[0].set_value("iaccx", value)

    @property
    def iaccy(self) -> int:
        """Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
        0:  Not written
        21: Nodal air density
        22: Nodal pore air pressure
        24: Nodal air mass
        25: Nodal air mass flow rate
        """ # nopep8
        return self._cards[0].get_value("iaccy")

    @iaccy.setter
    def iaccy(self, value: int) -> None:
        if value not in [0, 21, 22, 24, 25]:
            raise Exception("""iaccy must be one of {0,21,22,24,25}""")
        self._cards[0].set_value("iaccy", value)

    @property
    def iaccz(self) -> int:
        """Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
        0:  Not written
        21: Nodal air density
        22: Nodal pore air pressure
        24: Nodal air mass
        25: Nodal air mass flow rate
        """ # nopep8
        return self._cards[0].get_value("iaccz")

    @iaccz.setter
    def iaccz(self, value: int) -> None:
        if value not in [0, 21, 22, 24, 25]:
            raise Exception("""iaccz must be one of {0,21,22,24,25}""")
        self._cards[0].set_value("iaccz", value)

    @property
    def ncyout(self) -> int:
        """Get or set the Number of cycles between outputs of calculation status to d3hsp and log file
        """ # nopep8
        return self._cards[0].get_value("ncyout")

    @ncyout.setter
    def ncyout(self, value: int) -> None:
        self._cards[0].set_value("ncyout", value)

