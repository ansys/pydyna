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

class LoadSsa(KeywordBase):
    """DYNA LOAD_SSA keyword"""

    keyword = "LOAD"
    subkeyword = "SSA"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "vs",
                        float,
                        0,
                        10,
                        kwargs.get("vs")
                    ),
                    Field(
                        "ds",
                        float,
                        10,
                        10,
                        kwargs.get("ds")
                    ),
                    Field(
                        "refl",
                        float,
                        20,
                        10,
                        kwargs.get("refl", 0)
                    ),
                    Field(
                        "zb",
                        float,
                        30,
                        10,
                        kwargs.get("zb", 0.0)
                    ),
                    Field(
                        "zsurf",
                        float,
                        40,
                        10,
                        kwargs.get("zsurf", 0.0)
                    ),
                    Field(
                        "fpsid",
                        int,
                        50,
                        10,
                        kwargs.get("fpsid", 0)
                    ),
                    Field(
                        "psid",
                        int,
                        60,
                        10,
                        kwargs.get("psid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a",
                        float,
                        0,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "alpha",
                        float,
                        10,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "gamma",
                        float,
                        20,
                        10,
                        kwargs.get("gamma")
                    ),
                    Field(
                        "ktheta",
                        float,
                        30,
                        10,
                        kwargs.get("ktheta")
                    ),
                    Field(
                        "kappa",
                        float,
                        40,
                        10,
                        kwargs.get("kappa")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xs",
                        float,
                        0,
                        10,
                        kwargs.get("xs")
                    ),
                    Field(
                        "ys",
                        float,
                        10,
                        10,
                        kwargs.get("ys")
                    ),
                    Field(
                        "zs",
                        float,
                        20,
                        10,
                        kwargs.get("zs")
                    ),
                    Field(
                        "w",
                        float,
                        30,
                        10,
                        kwargs.get("w")
                    ),
                    Field(
                        "tdely",
                        float,
                        40,
                        10,
                        kwargs.get("tdely")
                    ),
                    Field(
                        "rad",
                        float,
                        50,
                        10,
                        kwargs.get("rad")
                    ),
                    Field(
                        "cz",
                        float,
                        60,
                        10,
                        kwargs.get("cz")
                    ),
                ],
            ),
        ]

    @property
    def vs(self) -> typing.Optional[float]:
        """Get or set the Sound speed in fluid.
        """ # nopep8
        return self._cards[0].get_value("vs")

    @vs.setter
    def vs(self, value: float) -> None:
        self._cards[0].set_value("vs", value)

    @property
    def ds(self) -> typing.Optional[float]:
        """Get or set the Density of fluid.
        """ # nopep8
        return self._cards[0].get_value("ds")

    @ds.setter
    def ds(self, value: float) -> None:
        self._cards[0].set_value("ds", value)

    @property
    def refl(self) -> float:
        """Get or set the Consider reflections from sea floor:
        EQ.0: off,
        EQ.1: on.
        """ # nopep8
        return self._cards[0].get_value("refl")

    @refl.setter
    def refl(self, value: float) -> None:
        if value not in [0, 1]:
            raise Exception("""refl must be one of {0,1}""")
        self._cards[0].set_value("refl", value)

    @property
    def zb(self) -> float:
        """Get or set the z-coordinate of sea floor. Define only if REFL=1.
        """ # nopep8
        return self._cards[0].get_value("zb")

    @zb.setter
    def zb(self, value: float) -> None:
        self._cards[0].set_value("zb", value)

    @property
    def zsurf(self) -> float:
        """Get or set the z-coordinate of sea surface.
        """ # nopep8
        return self._cards[0].get_value("zsurf")

    @zsurf.setter
    def zsurf(self, value: float) -> None:
        self._cards[0].set_value("zsurf", value)

    @property
    def fpsid(self) -> int:
        """Get or set the Part set ID of parts subject to flood control. Use the *PART_SET_COLUMN option where the parameters A1 and A2 must be defined as follows:
        Parameter A1: Flooding status:
        EQ.1.0: Fluid on both sides,
        EQ.2.0: Fluid outside, air inside,
        EQ.3.0: Air outside, fluid inside,
        EQ.4.0: Material or part is ignored.
        Parameter A2:
        Tubular outer diameter of beam elements. For shell elements this input must be greater than zero for loading.
        """ # nopep8
        return self._cards[0].get_value("fpsid")

    @fpsid.setter
    def fpsid(self, value: int) -> None:
        self._cards[0].set_value("fpsid", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID of parts defining the wet surface. The elements defining these parts must have their outward normals pointing into the fluid.
        EQ.0: all parts are included,
        GT.0: define n part ID's in the *SET_PART_COLUMN keyword.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Shock pressure parameter.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[1].set_value("a", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Shock pressure parameter.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[1].set_value("alpha", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Time constant parameter.
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[1].set_value("gamma", value)

    @property
    def ktheta(self) -> typing.Optional[float]:
        """Get or set the Time constant parameter.
        """ # nopep8
        return self._cards[1].get_value("ktheta")

    @ktheta.setter
    def ktheta(self, value: float) -> None:
        self._cards[1].set_value("ktheta", value)

    @property
    def kappa(self) -> typing.Optional[float]:
        """Get or set the Ratio of specific heat capacities.
        """ # nopep8
        return self._cards[1].get_value("kappa")

    @kappa.setter
    def kappa(self, value: float) -> None:
        self._cards[1].set_value("kappa", value)

    @property
    def xs(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of charge.
        """ # nopep8
        return self._cards[2].get_value("xs")

    @xs.setter
    def xs(self, value: float) -> None:
        self._cards[2].set_value("xs", value)

    @property
    def ys(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of charge.
        """ # nopep8
        return self._cards[2].get_value("ys")

    @ys.setter
    def ys(self, value: float) -> None:
        self._cards[2].set_value("ys", value)

    @property
    def zs(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of charge.
        """ # nopep8
        return self._cards[2].get_value("zs")

    @zs.setter
    def zs(self, value: float) -> None:
        self._cards[2].set_value("zs", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Weight of charge.
        """ # nopep8
        return self._cards[2].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[2].set_value("w", value)

    @property
    def tdely(self) -> typing.Optional[float]:
        """Get or set the Time delay before charge detonates.
        """ # nopep8
        return self._cards[2].get_value("tdely")

    @tdely.setter
    def tdely(self, value: float) -> None:
        self._cards[2].set_value("tdely", value)

    @property
    def rad(self) -> typing.Optional[float]:
        """Get or set the Charge radius.
        """ # nopep8
        return self._cards[2].get_value("rad")

    @rad.setter
    def rad(self, value: float) -> None:
        self._cards[2].set_value("rad", value)

    @property
    def cz(self) -> typing.Optional[float]:
        """Get or set the Water depth.
        """ # nopep8
        return self._cards[2].get_value("cz")

    @cz.setter
    def cz(self, value: float) -> None:
        self._cards[2].set_value("cz", value)

