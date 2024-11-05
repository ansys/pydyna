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

class BoundaryPap(KeywordBase):
    """DYNA BOUNDARY_PAP keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PAP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "segid",
                        int,
                        0,
                        10,
                        kwargs.get("segid")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "cmult",
                        float,
                        20,
                        10,
                        kwargs.get("cmult")
                    ),
                    Field(
                        "cvmass",
                        float,
                        30,
                        10,
                        kwargs.get("cvmass")
                    ),
                    Field(
                        "block",
                        float,
                        40,
                        10,
                        kwargs.get("block", 0.0)
                    ),
                    Field(
                        "tbirth",
                        float,
                        50,
                        10,
                        kwargs.get("tbirth", 0.0)
                    ),
                    Field(
                        "tdeath",
                        float,
                        60,
                        10,
                        kwargs.get("tdeath", 1.0E20)
                    ),
                    Field(
                        "cvrper",
                        float,
                        70,
                        10,
                        kwargs.get("cvrper", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def segid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID.
        """ # nopep8
        return self._cards[0].get_value("segid")

    @segid.setter
    def segid(self, value: int) -> None:
        self._cards[0].set_value("segid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve giving pore air pressure vs. time. EQ.0: constant pressure assumed equal to CMULT
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def cmult(self) -> typing.Optional[float]:
        """Get or set the Factor on curve or constant pressure head if LCID=0
        """ # nopep8
        return self._cards[0].get_value("cmult")

    @cmult.setter
    def cmult(self, value: float) -> None:
        self._cards[0].set_value("cmult", value)

    @property
    def cvmass(self) -> typing.Optional[float]:
        """Get or set the Initial mass of a control volume next to the segment set SETID.
        """ # nopep8
        return self._cards[0].get_value("cvmass")

    @cvmass.setter
    def cvmass(self, value: float) -> None:
        self._cards[0].set_value("cvmass", value)

    @property
    def block(self) -> float:
        """Get or set the Contact blockage effect,
        EQ.0: When all segments in SEGID are subject to the pressure
        defined by LCID and CMULT;
        EQ.-1: When only elements in SEGID not involved in contact are
        subject to the pressure defined by LCID and CMULT
        """ # nopep8
        return self._cards[0].get_value("block")

    @block.setter
    def block(self, value: float) -> None:
        if value not in [0.0, -1]:
            raise Exception("""block must be one of {0.0,-1}""")
        self._cards[0].set_value("block", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Time at which boundary condition becomes active
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Time at which boundary condition becomes inactive
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def cvrper(self) -> float:
        """Get or set the Permeability factor of cover material, where cover refers to a shell layer coating the surface of the solid. Default value is 1.0 when it is not defined. See Remark 3 below. 0.0 <= CVRPER <= 1.0
        """ # nopep8
        return self._cards[0].get_value("cvrper")

    @cvrper.setter
    def cvrper(self, value: float) -> None:
        self._cards[0].set_value("cvrper", value)

