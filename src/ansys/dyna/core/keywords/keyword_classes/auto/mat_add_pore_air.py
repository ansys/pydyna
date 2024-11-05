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

class MatAddPoreAir(KeywordBase):
    """DYNA MAT_ADD_PORE_AIR keyword"""

    keyword = "MAT"
    subkeyword = "ADD_PORE_AIR"
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
                        "pa_rho",
                        float,
                        10,
                        10,
                        kwargs.get("pa_rho")
                    ),
                    Field(
                        "pa_pre",
                        float,
                        20,
                        10,
                        kwargs.get("pa_pre")
                    ),
                    Field(
                        "pore",
                        float,
                        30,
                        10,
                        kwargs.get("pore", 1.)
                    ),
                    Field(
                        "dvimin",
                        float,
                        40,
                        10,
                        kwargs.get("dvimin")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "perm1",
                        float,
                        0,
                        10,
                        kwargs.get("perm1", 0.)
                    ),
                    Field(
                        "perm2",
                        float,
                        10,
                        10,
                        kwargs.get("perm2")
                    ),
                    Field(
                        "perm3",
                        float,
                        20,
                        10,
                        kwargs.get("perm3")
                    ),
                    Field(
                        "cdarcy",
                        float,
                        30,
                        10,
                        kwargs.get("cdarcy", 1.)
                    ),
                    Field(
                        "cdf",
                        float,
                        40,
                        10,
                        kwargs.get("cdf", 0.)
                    ),
                    Field(
                        "lcpgd1",
                        int,
                        50,
                        10,
                        kwargs.get("lcpgd1", 0)
                    ),
                    Field(
                        "lcpgd2",
                        int,
                        60,
                        10,
                        kwargs.get("lcpgd2")
                    ),
                    Field(
                        "lcpgd3",
                        int,
                        70,
                        10,
                        kwargs.get("lcpgd3")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddPoreAir.option_specs[0],
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
        """Get or set the Material identification - must be same as the structural material.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def pa_rho(self) -> typing.Optional[float]:
        """Get or set the Initial density of pore air, default to atmospheric air density, AIR_RO, defined in *CONTROL_PORE_AIR
        """ # nopep8
        return self._cards[0].get_value("pa_rho")

    @pa_rho.setter
    def pa_rho(self, value: float) -> None:
        self._cards[0].set_value("pa_rho", value)

    @property
    def pa_pre(self) -> typing.Optional[float]:
        """Get or set the Initial pressure of pore air, default to atmospheric air pressure, AIR_P, defined in *CONTROL_PORE_AIR.
        """ # nopep8
        return self._cards[0].get_value("pa_pre")

    @pa_pre.setter
    def pa_pre(self, value: float) -> None:
        self._cards[0].set_value("pa_pre", value)

    @property
    def pore(self) -> float:
        """Get or set the Porosity, ratio of pores to total volume, default to 1.
        """ # nopep8
        return self._cards[0].get_value("pore")

    @pore.setter
    def pore(self, value: float) -> None:
        self._cards[0].set_value("pore", value)

    @property
    def dvimin(self) -> typing.Optional[float]:
        """Get or set the Optional parameters to trigger air flow analysis.  Pore air flow analysis is performed only for these nodes having incremental volume change ratio, abs(V(t)-V(t-dt))/V(t-dt), larger than DVMIN, where V(t) is the nodal volume at time=t.  This parameter may be needed for the material that has very high permeability.  Caution has to be exercised when using DVMIN, a reasonable starting value may equal the inverse of the total number of analysis time steps.  Another option to control pore air analysis can be found in *SENSOR_CONTROL.
        """ # nopep8
        return self._cards[0].get_value("dvimin")

    @dvimin.setter
    def dvimin(self, value: float) -> None:
        self._cards[0].set_value("dvimin", value)

    @property
    def perm1(self) -> float:
        """Get or set the Permeability of pore air along x-direction, <0 when ABS(PERMX) is the curve defining permeability coefficient as a function of volume ratio,	current-volume)/volume-at-stress-free-state.
        """ # nopep8
        return self._cards[1].get_value("perm1")

    @perm1.setter
    def perm1(self, value: float) -> None:
        self._cards[1].set_value("perm1", value)

    @property
    def perm2(self) -> typing.Optional[float]:
        """Get or set the Permeability of pore air along y-direction, <0 when ABS(PERMY) is the curve defining permeability coefficient as a function of volume ratio,	current-volume)/volume-at-stress-free-state
        """ # nopep8
        return self._cards[1].get_value("perm2")

    @perm2.setter
    def perm2(self, value: float) -> None:
        self._cards[1].set_value("perm2", value)

    @property
    def perm3(self) -> typing.Optional[float]:
        """Get or set the Permeability of pore air along z-direction, <0 when ABS(PERMZ) is the curve defining permeability coefficient as a function of volume ratio,	current-volume)/volume-at-stress-free-state.
        """ # nopep8
        return self._cards[1].get_value("perm3")

    @perm3.setter
    def perm3(self, value: float) -> None:
        self._cards[1].set_value("perm3", value)

    @property
    def cdarcy(self) -> float:
        """Get or set the Coefficient of Darcy's law
        """ # nopep8
        return self._cards[1].get_value("cdarcy")

    @cdarcy.setter
    def cdarcy(self, value: float) -> None:
        self._cards[1].set_value("cdarcy", value)

    @property
    def cdf(self) -> float:
        """Get or set the Coefficient of Dupuit-Forchheimer law
        """ # nopep8
        return self._cards[1].get_value("cdf")

    @cdf.setter
    def cdf(self, value: float) -> None:
        self._cards[1].set_value("cdf", value)

    @property
    def lcpgd1(self) -> int:
        """Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
        """ # nopep8
        return self._cards[1].get_value("lcpgd1")

    @lcpgd1.setter
    def lcpgd1(self, value: int) -> None:
        self._cards[1].set_value("lcpgd1", value)

    @property
    def lcpgd2(self) -> typing.Optional[int]:
        """Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
        """ # nopep8
        return self._cards[1].get_value("lcpgd2")

    @lcpgd2.setter
    def lcpgd2(self, value: int) -> None:
        self._cards[1].set_value("lcpgd2", value)

    @property
    def lcpgd3(self) -> typing.Optional[int]:
        """Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
        """ # nopep8
        return self._cards[1].get_value("lcpgd3")

    @lcpgd3.setter
    def lcpgd3(self, value: int) -> None:
        self._cards[1].set_value("lcpgd3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

