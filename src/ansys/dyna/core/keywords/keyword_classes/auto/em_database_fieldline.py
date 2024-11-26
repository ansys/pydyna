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

class EmDatabaseFieldline(KeywordBase):
    """DYNA EM_DATABASE_FIELDLINE keyword"""

    keyword = "EM"
    subkeyword = "DATABASE_FIELDLINE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "flid",
                        int,
                        0,
                        10,
                        kwargs.get("flid")
                    ),
                    Field(
                        "psid",
                        int,
                        10,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "dtout",
                        float,
                        20,
                        10,
                        kwargs.get("dtout")
                    ),
                    Field(
                        "npoint",
                        int,
                        30,
                        10,
                        kwargs.get("npoint", 100)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "integ",
                        int,
                        0,
                        10,
                        kwargs.get("integ", 2)
                    ),
                    Field(
                        "h",
                        float,
                        10,
                        10,
                        kwargs.get("h", 0.0)
                    ),
                    Field(
                        "hmin",
                        float,
                        20,
                        10,
                        kwargs.get("hmin", 0.0)
                    ),
                    Field(
                        "hmax",
                        float,
                        30,
                        10,
                        kwargs.get("hmax", 1e10)
                    ),
                    Field(
                        "tolabs",
                        float,
                        40,
                        10,
                        kwargs.get("tolabs", 1e-3)
                    ),
                    Field(
                        "tolrel",
                        float,
                        50,
                        10,
                        kwargs.get("tolrel", 1e-5)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "btype",
                        int,
                        0,
                        10,
                        kwargs.get("btype", 2)
                    ),
                ],
            ),
        ]

    @property
    def flid(self) -> typing.Optional[int]:
        """Get or set the Field line set ID.
        """ # nopep8
        return self._cards[0].get_value("flid")

    @flid.setter
    def flid(self, value: int) -> None:
        self._cards[0].set_value("flid", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Point Set ID associated to the field line set (See *EM_POINT_SET).
        The coordinates given by the different points will be the starting points of the field lines.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the EM timestep will be used.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def npoint(self) -> int:
        """Get or set the Number of points per field line. The points are regularly spaced.
        """ # nopep8
        return self._cards[0].get_value("npoint")

    @npoint.setter
    def npoint(self, value: int) -> None:
        self._cards[0].set_value("npoint", value)

    @property
    def integ(self) -> int:
        """Get or set the Type of numerical integrator used to compute the field lines:
        EQ.1: RK4 (Runge Kutta 4).
        EQ.2: DOP853 (Dormand Prince 8(5,3)).
        """ # nopep8
        return self._cards[1].get_value("integ")

    @integ.setter
    def integ(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""integ must be one of {2,1}""")
        self._cards[1].set_value("integ", value)

    @property
    def h(self) -> float:
        """Get or set the The value of the step size is equal to the maximum value between /n 1/|B| (evaluated at the starting point of the field line) and the value
        H given by the user. In case of an integrator with adaptive stepsize,
        it is the initial value of the stepsize.
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[1].set_value("h", value)

    @property
    def hmin(self) -> float:
        """Get or set the Minimal stepsize value. Only used in the case of an integrator with adaptive stepsize.
        """ # nopep8
        return self._cards[1].get_value("hmin")

    @hmin.setter
    def hmin(self, value: float) -> None:
        self._cards[1].set_value("hmin", value)

    @property
    def hmax(self) -> float:
        """Get or set the Maximal stepsize value. Only used in the case of an integrator with adaptive stepsize.
        """ # nopep8
        return self._cards[1].get_value("hmax")

    @hmax.setter
    def hmax(self, value: float) -> None:
        self._cards[1].set_value("hmax", value)

    @property
    def tolabs(self) -> float:
        """Get or set the Absolute tolerance of the integrator. Only used in the case of an integrator with adaptive stepsize.
        """ # nopep8
        return self._cards[1].get_value("tolabs")

    @tolabs.setter
    def tolabs(self, value: float) -> None:
        self._cards[1].set_value("tolabs", value)

    @property
    def tolrel(self) -> float:
        """Get or set the Relative tolerance of the integrator. Only used in the case of an integrator with adaptive stepsize.
        """ # nopep8
        return self._cards[1].get_value("tolrel")

    @tolrel.setter
    def tolrel(self, value: float) -> None:
        self._cards[1].set_value("tolrel", value)

    @property
    def btype(self) -> int:
        """Get or set the Method to compute the magnetic field:
        EQ.1: Direct method (every contribution is computed by the Biot Savart Law and summed up : very slow).
        EQ.2: Multipole method (approximation of the direct method using the multipole expansion).
        EQ.3: Multicenter method (approximation of the direct method using a weighted subset of points only in order to compute the magnetic field).
        """ # nopep8
        return self._cards[2].get_value("btype")

    @btype.setter
    def btype(self, value: int) -> None:
        if value not in [2, 1, 3]:
            raise Exception("""btype must be one of {2,1,3}""")
        self._cards[2].set_value("btype", value)

