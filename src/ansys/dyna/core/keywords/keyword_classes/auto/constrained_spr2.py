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

class ConstrainedSpr2(KeywordBase):
    """DYNA CONSTRAINED_SPR2 keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "SPR2"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "upid",
                        int,
                        0,
                        10,
                        kwargs.get("upid")
                    ),
                    Field(
                        "lpid",
                        int,
                        10,
                        10,
                        kwargs.get("lpid")
                    ),
                    Field(
                        "nsid",
                        int,
                        20,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "thick",
                        float,
                        30,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "d",
                        float,
                        40,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "fn",
                        float,
                        50,
                        10,
                        kwargs.get("fn")
                    ),
                    Field(
                        "ft",
                        float,
                        60,
                        10,
                        kwargs.get("ft")
                    ),
                    Field(
                        "dn",
                        float,
                        70,
                        10,
                        kwargs.get("dn")
                    ),
                ],
            ),
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
                        "xin",
                        float,
                        10,
                        10,
                        kwargs.get("xin")
                    ),
                    Field(
                        "xit",
                        float,
                        20,
                        10,
                        kwargs.get("xit")
                    ),
                    Field(
                        "alpha1",
                        float,
                        30,
                        10,
                        kwargs.get("alpha1")
                    ),
                    Field(
                        "alpha2",
                        float,
                        40,
                        10,
                        kwargs.get("alpha2")
                    ),
                    Field(
                        "alpha3",
                        float,
                        50,
                        10,
                        kwargs.get("alpha3")
                    ),
                    Field(
                        "dens",
                        float,
                        60,
                        10,
                        kwargs.get("dens")
                    ),
                    Field(
                        "intp",
                        int,
                        70,
                        10,
                        kwargs.get("intp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "expn",
                        float,
                        0,
                        10,
                        kwargs.get("expn", 8.0)
                    ),
                    Field(
                        "expt",
                        float,
                        10,
                        10,
                        kwargs.get("expt", 8.0)
                    ),
                    Field(
                        "pidvb",
                        int,
                        20,
                        10,
                        kwargs.get("pidvb")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xpid1",
                        int,
                        0,
                        10,
                        kwargs.get("xpid1")
                    ),
                    Field(
                        "xpid2",
                        int,
                        10,
                        10,
                        kwargs.get("xpid2")
                    ),
                    Field(
                        "xpid3",
                        int,
                        20,
                        10,
                        kwargs.get("xpid3")
                    ),
                    Field(
                        "xpid4",
                        int,
                        30,
                        10,
                        kwargs.get("xpid4")
                    ),
                ],
            ),
        ]

    @property
    def upid(self) -> typing.Optional[int]:
        """Get or set the Upper sheet part ID
        """ # nopep8
        return self._cards[0].get_value("upid")

    @upid.setter
    def upid(self, value: int) -> None:
        self._cards[0].set_value("upid", value)

    @property
    def lpid(self) -> typing.Optional[int]:
        """Get or set the Lower sheet part ID
        """ # nopep8
        return self._cards[0].get_value("lpid")

    @lpid.setter
    def lpid(self, value: int) -> None:
        self._cards[0].set_value("lpid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of rivet location nodes.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Total thickness of upper and lower sheets.
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[0].set_value("thick", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Rivet diameter.
        """ # nopep8
        return self._cards[0].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[0].set_value("d", value)

    @property
    def fn(self) -> typing.Optional[float]:
        """Get or set the Rivet strength in tension (pull-out):
        GT.0: Constant value
        LT.0 : Material data from instantiation of * MAT_CONSTRAINED_SPR2(*MAT_265) with MID of absolutevalue | FN |
        """ # nopep8
        return self._cards[0].get_value("fn")

    @fn.setter
    def fn(self, value: float) -> None:
        self._cards[0].set_value("fn", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Rivet strength in pure shear.
        """ # nopep8
        return self._cards[0].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        self._cards[0].set_value("ft", value)

    @property
    def dn(self) -> typing.Optional[float]:
        """Get or set the Failure displacement in normal direction.
        """ # nopep8
        return self._cards[0].get_value("dn")

    @dn.setter
    def dn(self, value: float) -> None:
        self._cards[0].set_value("dn", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Failure displacement in tangential direction.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def xin(self) -> typing.Optional[float]:
        """Get or set the Fraction of failure displacement at maximum normal force.
        """ # nopep8
        return self._cards[1].get_value("xin")

    @xin.setter
    def xin(self, value: float) -> None:
        self._cards[1].set_value("xin", value)

    @property
    def xit(self) -> typing.Optional[float]:
        """Get or set the Fraction of failure displacement at maximum tangential force.
        """ # nopep8
        return self._cards[1].get_value("xit")

    @xit.setter
    def xit(self, value: float) -> None:
        self._cards[1].set_value("xit", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Dimensionless parameter scaling the effective displacement.
        """ # nopep8
        return self._cards[1].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        self._cards[1].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Dimensionless parameter scaling the effective displacement
        """ # nopep8
        return self._cards[1].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        self._cards[1].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Dimensionless parameter scaling the effective displacement.The sign of ALPHA3 can be used to choose the normal update procedure :
        GT.0 : Incremental update(default)
        LT.0 : Total update(recommended)
        """ # nopep8
        return self._cards[1].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        self._cards[1].set_value("alpha3", value)

    @property
    def dens(self) -> typing.Optional[float]:
        """Get or set the Rivet density (necessary for time step calculation).
        """ # nopep8
        return self._cards[1].get_value("dens")

    @dens.setter
    def dens(self, value: float) -> None:
        self._cards[1].set_value("dens", value)

    @property
    def intp(self) -> int:
        """Get or set the Flag for interpolation.
        EQ.0: Linear (default),
        EQ.1: Uniform,
        EQ.2 : Inverse distance weighting.
        """ # nopep8
        return self._cards[1].get_value("intp")

    @intp.setter
    def intp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""intp must be one of {0,1,2}""")
        self._cards[1].set_value("intp", value)

    @property
    def expn(self) -> float:
        """Get or set the Exponent value for load function in normal direction.
        """ # nopep8
        return self._cards[2].get_value("expn")

    @expn.setter
    def expn(self, value: float) -> None:
        self._cards[2].set_value("expn", value)

    @property
    def expt(self) -> float:
        """Get or set the Exponent value for load function in tangential direction.
        """ # nopep8
        return self._cards[2].get_value("expt")

    @expt.setter
    def expt(self, value: float) -> None:
        self._cards[2].set_value("expt", value)

    @property
    def pidvb(self) -> typing.Optional[int]:
        """Get or set the Part ID for visualization beams representing SPR2 in post-processing.
        EQ.0:	Part id automatically set (default),
        GT.0:	PIDVB defines part id .
        """ # nopep8
        return self._cards[2].get_value("pidvb")

    @pidvb.setter
    def pidvb(self, value: int) -> None:
        self._cards[2].set_value("pidvb", value)

    @property
    def xpid1(self) -> typing.Optional[int]:
        """Get or set the Extra part ID 1 for multi-sheet connection.
        """ # nopep8
        return self._cards[3].get_value("xpid1")

    @xpid1.setter
    def xpid1(self, value: int) -> None:
        self._cards[3].set_value("xpid1", value)

    @property
    def xpid2(self) -> typing.Optional[int]:
        """Get or set the Extra part ID 2 for multi-sheet connection.
        """ # nopep8
        return self._cards[3].get_value("xpid2")

    @xpid2.setter
    def xpid2(self, value: int) -> None:
        self._cards[3].set_value("xpid2", value)

    @property
    def xpid3(self) -> typing.Optional[int]:
        """Get or set the Extra part ID 3 for multi-sheet connection.
        """ # nopep8
        return self._cards[3].get_value("xpid3")

    @xpid3.setter
    def xpid3(self, value: int) -> None:
        self._cards[3].set_value("xpid3", value)

    @property
    def xpid4(self) -> typing.Optional[int]:
        """Get or set the Extra part ID 4 for multi-sheet connection.
        """ # nopep8
        return self._cards[3].get_value("xpid4")

    @xpid4.setter
    def xpid4(self, value: int) -> None:
        self._cards[3].set_value("xpid4", value)

