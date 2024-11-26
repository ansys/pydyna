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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatSidDamperDiscreteBeam(KeywordBase):
    """DYNA MAT_SID_DAMPER_DISCRETE_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "SID_DAMPER_DISCRETE_BEAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "st",
                        float,
                        20,
                        10,
                        kwargs.get("st")
                    ),
                    Field(
                        "d",
                        float,
                        30,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "r",
                        float,
                        40,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "h",
                        float,
                        50,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "k",
                        float,
                        60,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "c",
                        float,
                        70,
                        10,
                        kwargs.get("c")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c3",
                        float,
                        0,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "stf",
                        float,
                        10,
                        10,
                        kwargs.get("stf")
                    ),
                    Field(
                        "rhof",
                        float,
                        20,
                        10,
                        kwargs.get("rhof")
                    ),
                    Field(
                        "c1",
                        float,
                        30,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        40,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "lcidf",
                        float,
                        50,
                        10,
                        kwargs.get("lcidf", 0)
                    ),
                    Field(
                        "lcidd",
                        float,
                        60,
                        10,
                        kwargs.get("lcidd", 0)
                    ),
                    Field(
                        "s0",
                        float,
                        70,
                        10,
                        kwargs.get("s0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "orfloc",
                        float,
                        0,
                        10,
                        kwargs.get("orfloc")
                    ),
                    Field(
                        "orfrad",
                        float,
                        10,
                        10,
                        kwargs.get("orfrad")
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "dc",
                        float,
                        30,
                        10,
                        kwargs.get("dc")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSidDamperDiscreteBeam.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume on *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Piston stroke. St must equal or exceed the length of the beam element.
        """ # nopep8
        return self._cards[0].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        self._cards[0].set_value("st", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Piston diameter.
        """ # nopep8
        return self._cards[0].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[0].set_value("d", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Default orifice radius.
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[0].set_value("r", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Orifice controller position.
        """ # nopep8
        return self._cards[0].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[0].set_value("h", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Damping constant.
        LT.0.0: |K| is the load curve number ID, see *DEFINE_CURVE, defining the damping coefficient as a function of the absolute value of the relative velocity.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[0].set_value("c", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Coefficient for fluid inertia term.
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[1].set_value("c3", value)

    @property
    def stf(self) -> typing.Optional[float]:
        """Get or set the k, stiffness coefficient if piston bottoms out.
        """ # nopep8
        return self._cards[1].get_value("stf")

    @stf.setter
    def stf(self, value: float) -> None:
        self._cards[1].set_value("stf", value)

    @property
    def rhof(self) -> typing.Optional[float]:
        """Get or set the Fluid density.
        """ # nopep8
        return self._cards[1].get_value("rhof")

    @rhof.setter
    def rhof(self, value: float) -> None:
        self._cards[1].set_value("rhof", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the C1, coefficient for linear velocity term.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the C2, coefficient for quadratic velocity term.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[1].set_value("c2", value)

    @property
    def lcidf(self) -> float:
        """Get or set the Load curve number ID defining force versus piston displacement. Compressive behavior is defined in the positive quadrant of the force displacement curve. Displacements falling outside of the defined force displacement curve are extrapolated. Care must be taken to ensure that extrapolated values are reasonable.
        """ # nopep8
        return self._cards[1].get_value("lcidf")

    @lcidf.setter
    def lcidf(self, value: float) -> None:
        self._cards[1].set_value("lcidf", value)

    @property
    def lcidd(self) -> float:
        """Get or set the Load curve number ID defining damping coefficient versus piston displacement. Displacements falling outside the defined curve are extrapolated. Care must be taken to ensure that extrapolated values are reasonable.
        """ # nopep8
        return self._cards[1].get_value("lcidd")

    @lcidd.setter
    def lcidd(self, value: float) -> None:
        self._cards[1].set_value("lcidd", value)

    @property
    def s0(self) -> typing.Optional[float]:
        """Get or set the Initial displacement s0 , typically set to zero. A positive displacement corresponds to compressive behavior.
        """ # nopep8
        return self._cards[1].get_value("s0")

    @s0.setter
    def s0(self, value: float) -> None:
        self._cards[1].set_value("s0", value)

    @property
    def orfloc(self) -> typing.Optional[float]:
        """Get or set the Orifice location of i'th orifice relative to the fixed end.
        """ # nopep8
        return self._cards[2].get_value("orfloc")

    @orfloc.setter
    def orfloc(self, value: float) -> None:
        self._cards[2].set_value("orfloc", value)

    @property
    def orfrad(self) -> typing.Optional[float]:
        """Get or set the Orifice radius of i'th orifice, if zero the default radius is used.
        """ # nopep8
        return self._cards[2].get_value("orfrad")

    @orfrad.setter
    def orfrad(self, value: float) -> None:
        self._cards[2].set_value("orfrad", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor on calculated force. (default = 1.0).
        """ # nopep8
        return self._cards[2].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[2].set_value("sf", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Linear viscous damping coefficient used after damper bottoms out either in tension or compression.
        """ # nopep8
        return self._cards[2].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[2].set_value("dc", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

