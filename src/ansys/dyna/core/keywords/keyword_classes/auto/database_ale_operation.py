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

class DatabaseAleOperation(KeywordBase):
    """DYNA DATABASE_ALE_OPERATION keyword"""

    keyword = "DATABASE"
    subkeyword = "ALE_OPERATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "fct",
                        int,
                        0,
                        10,
                        kwargs.get("fct")
                    ),
                    Field(
                        "hisvn",
                        int,
                        10,
                        10,
                        kwargs.get("hisvn")
                    ),
                    Field(
                        "wrt",
                        int,
                        20,
                        10,
                        kwargs.get("wrt", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dt",
                        int,
                        0,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "setid",
                        int,
                        10,
                        10,
                        kwargs.get("setid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "var",
                        int,
                        0,
                        10,
                        kwargs.get("var", 0)
                    ),
                    Field(
                        "var",
                        int,
                        10,
                        10,
                        kwargs.get("var", 0)
                    ),
                    Field(
                        "var",
                        int,
                        20,
                        10,
                        kwargs.get("var", 0)
                    ),
                    Field(
                        "var",
                        int,
                        30,
                        10,
                        kwargs.get("var", 0)
                    ),
                    Field(
                        "var",
                        int,
                        40,
                        10,
                        kwargs.get("var", 0)
                    ),
                    Field(
                        "var",
                        int,
                        50,
                        10,
                        kwargs.get("var", 0)
                    ),
                    Field(
                        "var",
                        int,
                        60,
                        10,
                        kwargs.get("var", 0)
                    ),
                    Field(
                        "var",
                        int,
                        70,
                        10,
                        kwargs.get("var", 0)
                    ),
                ],
            ),
        ]

    @property
    def fct(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID;
        """ # nopep8
        return self._cards[0].get_value("fct")

    @fct.setter
    def fct(self, value: int) -> None:
        self._cards[0].set_value("fct", value)

    @property
    def hisvn(self) -> typing.Optional[int]:
        """Get or set the Number of the history variable replaced in d3plot
        """ # nopep8
        return self._cards[0].get_value("hisvn")

    @hisvn.setter
    def hisvn(self, value: int) -> None:
        self._cards[0].set_value("hisvn", value)

    @property
    def wrt(self) -> int:
        """Get or set the File output flag. WRT must be a two digit number:WRT = L + M×10
        The 1 digit controls the replacement of the history variable number HISVN in d3plot :
        L.EQ.1 : For each ALE element in the mesh, replace the values of the history variable with values computed by the function FCT
        L.EQ.0 : Do not modify d3plot.
        The 10s digit controls the history output of values computed by the function FCT :
        M.EQ.1 : For each ALE element in the set SETID, write.xy file that stores values computed by FCT at a frequency DT. (See Remarks 3 and 4.)
        M.EQ.0 : Do not output this history file.
        """ # nopep8
        return self._cards[0].get_value("wrt")

    @wrt.setter
    def wrt(self, value: int) -> None:
        self._cards[0].set_value("wrt", value)

    @property
    def dt(self) -> typing.Optional[int]:
        """Get or set the Time interval between computed function values included in the .xy file
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: int) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the ALE element set ID.See Remarks 3 and 4. If the model is 2D(*SECTION_ALE2D), the set should be a shell set(see* SET_SHELL).If the model is 3D(*SECTION_SOLID), the set should be a solid set(see* SET_SOLID
        """ # nopep8
        return self._cards[1].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[1].set_value("setid", value)

    @property
    def var(self) -> int:
        """Get or set the Arguments that can be included in function FCT (see Remark 1):
        EQ.1:	xx - stress
        EQ.2 : yy - stress
        EQ.3 : zz - stress
        EQ.4 : xy - stress
        EQ.5 : yz - stress
        EQ.6 : zx - stress
        EQ.7 : Plastic strain
        EQ.8 : Internal energy
        EQ.9 : Bulk viscosity
        EQ.10 : Previous volume
        EQ.11 : Mass
        EQ.12 : Volume
        EQ.13 : Nodal x - positions
        EQ.14 : Nodal y - positions
        EQ.15 : Nodal z - positions
        EQ.16 : Nodal x - velocities
        EQ.17 : Nodal y - velocities
        EQ.18 : Nodal z - velocities
        EQ.19 : Nodal x - accelerations
        EQ.20 : Nodal y - accelerations
        EQ.21 : Nodal z - accelerations
        """ # nopep8
        return self._cards[2].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        self._cards[2].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Arguments that can be included in function FCT (see Remark 1):
        EQ.1:	xx - stress
        EQ.2 : yy - stress
        EQ.3 : zz - stress
        EQ.4 : xy - stress
        EQ.5 : yz - stress
        EQ.6 : zx - stress
        EQ.7 : Plastic strain
        EQ.8 : Internal energy
        EQ.9 : Bulk viscosity
        EQ.10 : Previous volume
        EQ.11 : Mass
        EQ.12 : Volume
        EQ.13 : Nodal x - positions
        EQ.14 : Nodal y - positions
        EQ.15 : Nodal z - positions
        EQ.16 : Nodal x - velocities
        EQ.17 : Nodal y - velocities
        EQ.18 : Nodal z - velocities
        EQ.19 : Nodal x - accelerations
        EQ.20 : Nodal y - accelerations
        EQ.21 : Nodal z - accelerations
        """ # nopep8
        return self._cards[2].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        self._cards[2].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Arguments that can be included in function FCT (see Remark 1):
        EQ.1:	xx - stress
        EQ.2 : yy - stress
        EQ.3 : zz - stress
        EQ.4 : xy - stress
        EQ.5 : yz - stress
        EQ.6 : zx - stress
        EQ.7 : Plastic strain
        EQ.8 : Internal energy
        EQ.9 : Bulk viscosity
        EQ.10 : Previous volume
        EQ.11 : Mass
        EQ.12 : Volume
        EQ.13 : Nodal x - positions
        EQ.14 : Nodal y - positions
        EQ.15 : Nodal z - positions
        EQ.16 : Nodal x - velocities
        EQ.17 : Nodal y - velocities
        EQ.18 : Nodal z - velocities
        EQ.19 : Nodal x - accelerations
        EQ.20 : Nodal y - accelerations
        EQ.21 : Nodal z - accelerations
        """ # nopep8
        return self._cards[2].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        self._cards[2].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Arguments that can be included in function FCT (see Remark 1):
        EQ.1:	xx - stress
        EQ.2 : yy - stress
        EQ.3 : zz - stress
        EQ.4 : xy - stress
        EQ.5 : yz - stress
        EQ.6 : zx - stress
        EQ.7 : Plastic strain
        EQ.8 : Internal energy
        EQ.9 : Bulk viscosity
        EQ.10 : Previous volume
        EQ.11 : Mass
        EQ.12 : Volume
        EQ.13 : Nodal x - positions
        EQ.14 : Nodal y - positions
        EQ.15 : Nodal z - positions
        EQ.16 : Nodal x - velocities
        EQ.17 : Nodal y - velocities
        EQ.18 : Nodal z - velocities
        EQ.19 : Nodal x - accelerations
        EQ.20 : Nodal y - accelerations
        EQ.21 : Nodal z - accelerations
        """ # nopep8
        return self._cards[2].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        self._cards[2].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Arguments that can be included in function FCT (see Remark 1):
        EQ.1:	xx - stress
        EQ.2 : yy - stress
        EQ.3 : zz - stress
        EQ.4 : xy - stress
        EQ.5 : yz - stress
        EQ.6 : zx - stress
        EQ.7 : Plastic strain
        EQ.8 : Internal energy
        EQ.9 : Bulk viscosity
        EQ.10 : Previous volume
        EQ.11 : Mass
        EQ.12 : Volume
        EQ.13 : Nodal x - positions
        EQ.14 : Nodal y - positions
        EQ.15 : Nodal z - positions
        EQ.16 : Nodal x - velocities
        EQ.17 : Nodal y - velocities
        EQ.18 : Nodal z - velocities
        EQ.19 : Nodal x - accelerations
        EQ.20 : Nodal y - accelerations
        EQ.21 : Nodal z - accelerations
        """ # nopep8
        return self._cards[2].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        self._cards[2].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Arguments that can be included in function FCT (see Remark 1):
        EQ.1:	xx - stress
        EQ.2 : yy - stress
        EQ.3 : zz - stress
        EQ.4 : xy - stress
        EQ.5 : yz - stress
        EQ.6 : zx - stress
        EQ.7 : Plastic strain
        EQ.8 : Internal energy
        EQ.9 : Bulk viscosity
        EQ.10 : Previous volume
        EQ.11 : Mass
        EQ.12 : Volume
        EQ.13 : Nodal x - positions
        EQ.14 : Nodal y - positions
        EQ.15 : Nodal z - positions
        EQ.16 : Nodal x - velocities
        EQ.17 : Nodal y - velocities
        EQ.18 : Nodal z - velocities
        EQ.19 : Nodal x - accelerations
        EQ.20 : Nodal y - accelerations
        EQ.21 : Nodal z - accelerations
        """ # nopep8
        return self._cards[2].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        self._cards[2].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Arguments that can be included in function FCT (see Remark 1):
        EQ.1:	xx - stress
        EQ.2 : yy - stress
        EQ.3 : zz - stress
        EQ.4 : xy - stress
        EQ.5 : yz - stress
        EQ.6 : zx - stress
        EQ.7 : Plastic strain
        EQ.8 : Internal energy
        EQ.9 : Bulk viscosity
        EQ.10 : Previous volume
        EQ.11 : Mass
        EQ.12 : Volume
        EQ.13 : Nodal x - positions
        EQ.14 : Nodal y - positions
        EQ.15 : Nodal z - positions
        EQ.16 : Nodal x - velocities
        EQ.17 : Nodal y - velocities
        EQ.18 : Nodal z - velocities
        EQ.19 : Nodal x - accelerations
        EQ.20 : Nodal y - accelerations
        EQ.21 : Nodal z - accelerations
        """ # nopep8
        return self._cards[2].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        self._cards[2].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Arguments that can be included in function FCT (see Remark 1):
        EQ.1:	xx - stress
        EQ.2 : yy - stress
        EQ.3 : zz - stress
        EQ.4 : xy - stress
        EQ.5 : yz - stress
        EQ.6 : zx - stress
        EQ.7 : Plastic strain
        EQ.8 : Internal energy
        EQ.9 : Bulk viscosity
        EQ.10 : Previous volume
        EQ.11 : Mass
        EQ.12 : Volume
        EQ.13 : Nodal x - positions
        EQ.14 : Nodal y - positions
        EQ.15 : Nodal z - positions
        EQ.16 : Nodal x - velocities
        EQ.17 : Nodal y - velocities
        EQ.18 : Nodal z - velocities
        EQ.19 : Nodal x - accelerations
        EQ.20 : Nodal y - accelerations
        EQ.21 : Nodal z - accelerations
        """ # nopep8
        return self._cards[2].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        self._cards[2].set_value("var", value)

