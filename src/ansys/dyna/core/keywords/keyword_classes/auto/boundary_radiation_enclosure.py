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

class BoundaryRadiationEnclosure(KeywordBase):
    """DYNA BOUNDARY_RADIATION_ENCLOSURE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "RADIATION_ENCLOSURE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "brencid",
                        int,
                        0,
                        10,
                        kwargs.get("brencid")
                    ),
                    Field(
                        "encname",
                        str,
                        10,
                        70,
                        kwargs.get("encname")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "calopt",
                        int,
                        0,
                        10,
                        kwargs.get("calopt", 0)
                    ),
                    Field(
                        "outopt",
                        int,
                        10,
                        10,
                        kwargs.get("outopt", 0)
                    ),
                    Field(
                        "conopt",
                        int,
                        20,
                        10,
                        kwargs.get("conopt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "encname",
                        str,
                        0,
                        80,
                        kwargs.get("encname")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "smflag",
                        int,
                        0,
                        10,
                        kwargs.get("smflag", 0)
                    ),
                    Field(
                        "smmaxi",
                        int,
                        10,
                        10,
                        kwargs.get("smmaxi", 500)
                    ),
                    Field(
                        "smabst",
                        float,
                        20,
                        10,
                        kwargs.get("smabst", 1.0E-10)
                    ),
                    Field(
                        "smrelt",
                        float,
                        30,
                        10,
                        kwargs.get("smrelt", 1.0E-6)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "stype",
                        int,
                        0,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "slmaxi",
                        int,
                        10,
                        10,
                        kwargs.get("slmaxi", 500)
                    ),
                    Field(
                        "slabst",
                        float,
                        20,
                        10,
                        kwargs.get("slabst", 1.0E-10)
                    ),
                    Field(
                        "slrelt",
                        float,
                        30,
                        10,
                        kwargs.get("slrelt", 1.0E-6)
                    ),
                    Field(
                        "slmlev",
                        int,
                        40,
                        10,
                        kwargs.get("slmlev", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nint",
                        int,
                        0,
                        10,
                        kwargs.get("nint")
                    ),
                    Field(
                        "block",
                        int,
                        10,
                        10,
                        kwargs.get("block", 0)
                    ),
                    Field(
                        "selcid",
                        int,
                        20,
                        10,
                        kwargs.get("selcid", 0)
                    ),
                    Field(
                        "semult",
                        float,
                        30,
                        10,
                        kwargs.get("semult", 0.0)
                    ),
                    Field(
                        "loc",
                        int,
                        40,
                        10,
                        kwargs.get("loc", 0)
                    ),
                ],
            ),
        ]

    @property
    def brencid(self) -> typing.Optional[int]:
        """Get or set the Boundary radiation ID for this enclosure
        """ # nopep8
        return self._cards[0].get_value("brencid")

    @brencid.setter
    def brencid(self, value: int) -> None:
        self._cards[0].set_value("brencid", value)

    @property
    def encname(self) -> typing.Optional[str]:
        """Get or set the Name of enclosure, used for output purposes
        """ # nopep8
        return self._cards[0].get_value("encname")

    @encname.setter
    def encname(self, value: str) -> None:
        self._cards[0].set_value("encname", value)

    @property
    def calopt(self) -> int:
        """Get or set the Calculation option:
        EQ.0:	view factors
        """ # nopep8
        return self._cards[1].get_value("calopt")

    @calopt.setter
    def calopt(self, value: int) -> None:
        self._cards[1].set_value("calopt", value)

    @property
    def outopt(self) -> int:
        """Get or set the Output option:
        EQ.0:	no output
        EQ.1 : output in LSDA format
        """ # nopep8
        return self._cards[1].get_value("outopt")

    @outopt.setter
    def outopt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""outopt must be one of {0,1}""")
        self._cards[1].set_value("outopt", value)

    @property
    def conopt(self) -> int:
        """Get or set the Control option:
        EQ.0:	calculate view factors matrix and preform thermal analysis
        """ # nopep8
        return self._cards[1].get_value("conopt")

    @conopt.setter
    def conopt(self, value: int) -> None:
        self._cards[1].set_value("conopt", value)

    @property
    def encname(self) -> typing.Optional[str]:
        """Get or set the Name of view factor output file
        """ # nopep8
        return self._cards[2].get_value("encname")

    @encname.setter
    def encname(self, value: str) -> None:
        self._cards[2].set_value("encname", value)

    @property
    def smflag(self) -> int:
        """Get or set the View factor matrix smoothing flag:
        EQ.0:	no smoothing
        EQ.1 : smoothing
        """ # nopep8
        return self._cards[3].get_value("smflag")

    @smflag.setter
    def smflag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""smflag must be one of {0,1}""")
        self._cards[3].set_value("smflag", value)

    @property
    def smmaxi(self) -> int:
        """Get or set the Maximum number of iterations for view factor matrix smoothing (default = 500)
        """ # nopep8
        return self._cards[3].get_value("smmaxi")

    @smmaxi.setter
    def smmaxi(self, value: int) -> None:
        self._cards[3].set_value("smmaxi", value)

    @property
    def smabst(self) -> float:
        """Get or set the Absolute convergence tolerance for view factor matrix smoothing (default = 10-10)
        """ # nopep8
        return self._cards[3].get_value("smabst")

    @smabst.setter
    def smabst(self, value: float) -> None:
        self._cards[3].set_value("smabst", value)

    @property
    def smrelt(self) -> float:
        """Get or set the Relative convergence tolerance for view factor matrix smoothing (default = 10-6)
        """ # nopep8
        return self._cards[3].get_value("smrelt")

    @smrelt.setter
    def smrelt(self, value: float) -> None:
        self._cards[3].set_value("smrelt", value)

    @property
    def stype(self) -> int:
        """Get or set the Solver type:
        EQ.0:	reverse conjugated gradient
        """ # nopep8
        return self._cards[4].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        self._cards[4].set_value("stype", value)

    @property
    def slmaxi(self) -> int:
        """Get or set the Maximum number of iterations for radiosity solver (default = 500)
        """ # nopep8
        return self._cards[4].get_value("slmaxi")

    @slmaxi.setter
    def slmaxi(self, value: int) -> None:
        self._cards[4].set_value("slmaxi", value)

    @property
    def slabst(self) -> float:
        """Get or set the Absolute convergence tolerance for radiosity solver (default is 10-10)
        """ # nopep8
        return self._cards[4].get_value("slabst")

    @slabst.setter
    def slabst(self, value: float) -> None:
        self._cards[4].set_value("slabst", value)

    @property
    def slrelt(self) -> float:
        """Get or set the Relative convergence tolerance for radiosity solver (default = 10-6)
        """ # nopep8
        return self._cards[4].get_value("slrelt")

    @slrelt.setter
    def slrelt(self, value: float) -> None:
        self._cards[4].set_value("slrelt", value)

    @property
    def slmlev(self) -> int:
        """Get or set the Radiosity solver message level:
        EQ.0:	no output
        EQ.1 : debug output level I
        EQ.2 : debug output level II
        EQ.3 : debug output level III
        """ # nopep8
        return self._cards[4].get_value("slmlev")

    @slmlev.setter
    def slmlev(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""slmlev must be one of {0,1,2,3}""")
        self._cards[4].set_value("slmlev", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the SSID specifies the ID for a set of segments that comprise a portion of, or possibly, the entire enclosure. See *SET_‌SEGMENT.
        """ # nopep8
        return self._cards[5].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[5].set_value("ssid", value)

    @property
    def nint(self) -> typing.Optional[int]:
        """Get or set the Number of integration points for view factor calculation, 1 ≤ NINT ≤ 10
        EQ.0:	LS - DYNA determines the number of integration points based on the segment size and separation distance.
        """ # nopep8
        return self._cards[6].get_value("nint")

    @nint.setter
    def nint(self, value: int) -> None:
        self._cards[6].set_value("nint", value)

    @property
    def block(self) -> int:
        """Get or set the Flag indicating if this surface blocks the view between any other 2 surfaces:
        EQ.0:	no blocking(default)
        EQ.1 : blocking
        """ # nopep8
        return self._cards[6].get_value("block")

    @block.setter
    def block(self, value: int) -> None:
        self._cards[6].set_value("block", value)

    @property
    def selcid(self) -> int:
        """Get or set the Load curve ID for surface emissivity (see *DEFINE_‌CURVE):
        GT.0:	surface emissivity as a function of time
        EQ.0 : use constant multiplier value, SEMULT
        LT.0 : surface emissivity as a function of temperature.The value of –SELCID must be an integer,and it is interpreted as a load curve ID.
        """ # nopep8
        return self._cards[6].get_value("selcid")

    @selcid.setter
    def selcid(self, value: int) -> None:
        self._cards[6].set_value("selcid", value)

    @property
    def semult(self) -> float:
        """Get or set the Curve multiplier for surface emissivity; see *DEFINE_‌CURVE
        """ # nopep8
        return self._cards[6].get_value("semult")

    @semult.setter
    def semult(self, value: float) -> None:
        self._cards[6].set_value("semult", value)

    @property
    def loc(self) -> int:
        """Get or set the Application of surface for thermal shell elements (see THSHEL in the *CONTROL_‌SHELL input):
        EQ. - 1:	lower surface of thermal shell element
        EQ.0 : middle surface of thermal shell element
        EQ.1 : upper surface of thermal shell element
        """ # nopep8
        return self._cards[6].get_value("loc")

    @loc.setter
    def loc(self, value: int) -> None:
        self._cards[6].set_value("loc", value)

