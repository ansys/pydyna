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

class DatabaseTracerGenerate(KeywordBase):
    """DYNA DATABASE_TRACER_GENERATE keyword"""

    keyword = "DATABASE"
    subkeyword = "TRACER_GENERATE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dt",
                        float,
                        0,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "valow",
                        float,
                        10,
                        10,
                        kwargs.get("valow", 0.0)
                    ),
                    Field(
                        "valup",
                        float,
                        20,
                        10,
                        kwargs.get("valup", 0.0)
                    ),
                    Field(
                        "valtype1",
                        int,
                        30,
                        10,
                        kwargs.get("valtype1")
                    ),
                    Field(
                        "set",
                        int,
                        40,
                        10,
                        kwargs.get("set")
                    ),
                    Field(
                        "setype",
                        int,
                        50,
                        10,
                        kwargs.get("setype", 0)
                    ),
                    Field(
                        "mmgset",
                        int,
                        60,
                        10,
                        kwargs.get("mmgset")
                    ),
                    Field(
                        "updt",
                        float,
                        70,
                        10,
                        kwargs.get("updt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "varloc",
                        int,
                        0,
                        10,
                        kwargs.get("varloc", 0)
                    ),
                    Field(
                        "valtype2",
                        int,
                        10,
                        10,
                        kwargs.get("valtype2", 0)
                    ),
                    Field(
                        "mmgset",
                        int,
                        20,
                        10,
                        kwargs.get("mmgset", 0)
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Interval time between each tracer generation and position update (See Remark 1).
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def valow(self) -> float:
        """Get or set the Range of values between which the isosurface is defined. VALOW is the lower bound while VALUP is the upper bound.  See Remark 2.
        The value at the isosurface is 0.5(VALOW+VALUP).
        The variable with this value is defined by VALTYPE.
        """ # nopep8
        return self._cards[0].get_value("valow")

    @valow.setter
    def valow(self, value: float) -> None:
        self._cards[0].set_value("valow", value)

    @property
    def valup(self) -> float:
        """Get or set the Range of values between which the isosurface is defined. VALOW is the lower bound while VALUP is the upper bound.  See Remark 2.
        The value at the isosurface is 0.5(VALOW+VALUP).
        The variable with this value is defined by VALTYPE.
        """ # nopep8
        return self._cards[0].get_value("valup")

    @valup.setter
    def valup(self, value: float) -> None:
        self._cards[0].set_value("valup", value)

    @property
    def valtype1(self) -> typing.Optional[int]:
        """Get or set the The variable that will be used to generate the isosurfaces.  See VALTYPE2 for enumeration of values.
        """ # nopep8
        return self._cards[0].get_value("valtype1")

    @valtype1.setter
    def valtype1(self, value: int) -> None:
        self._cards[0].set_value("valtype1", value)

    @property
    def set(self) -> typing.Optional[int]:
        """Get or set the Set ID (See Remark 2)
        """ # nopep8
        return self._cards[0].get_value("set")

    @set.setter
    def set(self, value: int) -> None:
        self._cards[0].set_value("set", value)

    @property
    def setype(self) -> int:
        """Get or set the Type of set (See Remark 2):
        EQ.0:	solid set
        EQ.1:	segment set
        EQ.2:	node set
        """ # nopep8
        return self._cards[0].get_value("setype")

    @setype.setter
    def setype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""setype must be one of {0,1,2}""")
        self._cards[0].set_value("setype", value)

    @property
    def mmgset(self) -> typing.Optional[int]:
        """Get or set the Multi-material group set (See Remark 3).
        """ # nopep8
        return self._cards[0].get_value("mmgset")

    @mmgset.setter
    def mmgset(self, value: int) -> None:
        self._cards[0].set_value("mmgset", value)

    @property
    def updt(self) -> typing.Optional[float]:
        """Get or set the Time interval between tracer position update (See Remark 1).
        """ # nopep8
        return self._cards[0].get_value("updt")

    @updt.setter
    def updt(self, value: float) -> None:
        self._cards[0].set_value("updt", value)

    @property
    def varloc(self) -> int:
        """Get or set the Variable location in trcrgen_binout to be replaced with the variable specified in the VALTYPE2 field:
        EQ.4:	-velocity
        EQ.5:	-velocity
        EQ.6:	-velocity
        EQ.7:	-stress
        EQ.8:	-stress
        EQ.9:	-stress
        EQ.10:	-stress
        EQ.11:	-stress
        EQ.12:	-stress
        EQ.13:	plastic strain
        EQ.14:	density
        EQ.15:	relative volume
        """ # nopep8
        return self._cards[1].get_value("varloc")

    @varloc.setter
    def varloc(self, value: int) -> None:
        self._cards[1].set_value("varloc", value)

    @property
    def valtype2(self) -> int:
        """Get or set the Data to be output to the trcrgen_binout file.  The interpretation of VALTYPE1 and VALTYPE2 is enumerated in the following list:
        EQ.1:	-stress
        EQ.2:	-stress
        EQ.3:	-stress
        EQ.4:	-stress
        EQ.5:	-stress
        EQ.6:	-stress
        EQ.7:	plastic strain
        EQ.8:	internal energy
        EQ.9:	bulk viscosity
        EQ.10:	relative volume
        GE.11 and LE.19:	other auxiliary variables
        EQ.20:	pressure
        EQ.21:	density
        EQ.22:	material volume
        EQ.23:	compression ratio
        EQ.24:	element volume fraction.
        EQ.25:	nodal volume fraction
        EQ.26:	-position
        EQ.27:	-position
        EQ.28:	-position
        EQ.29:	-velocity
        EQ.30:	-velocity
        EQ.31:	-velocity
        EQ.31:	velocity
        EQ.33:	-acceleration
        EQ.34:	- acceleration
        EQ.35:	- acceleration
        EQ.36:	acceleration
        EQ.37:	nodal mass
        EQ.38:	nodal temperature.
        """ # nopep8
        return self._cards[1].get_value("valtype2")

    @valtype2.setter
    def valtype2(self, value: int) -> None:
        self._cards[1].set_value("valtype2", value)

    @property
    def mmgset(self) -> int:
        """Get or set the Multi-material group set (See Remark 3)
        """ # nopep8
        return self._cards[1].get_value("mmgset")

    @mmgset.setter
    def mmgset(self, value: int) -> None:
        self._cards[1].set_value("mmgset", value)

