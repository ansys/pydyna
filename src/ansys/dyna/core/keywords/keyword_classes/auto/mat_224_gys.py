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

class Mat224Gys(KeywordBase):
    """DYNA MAT_224_GYS keyword"""

    keyword = "MAT"
    subkeyword = "224_GYS"
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
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "cp",
                        float,
                        40,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "tr",
                        float,
                        50,
                        10,
                        kwargs.get("tr", 0.0)
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta", 1.0)
                    ),
                    Field(
                        "numint",
                        float,
                        70,
                        10,
                        kwargs.get("numint", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lck1",
                        int,
                        0,
                        10,
                        kwargs.get("lck1", 0)
                    ),
                    Field(
                        "lckt",
                        int,
                        10,
                        10,
                        kwargs.get("lckt", 0)
                    ),
                    Field(
                        "lcf",
                        int,
                        20,
                        10,
                        kwargs.get("lcf", 0)
                    ),
                    Field(
                        "lcg",
                        int,
                        30,
                        10,
                        kwargs.get("lcg", 0)
                    ),
                    Field(
                        "lch",
                        int,
                        40,
                        10,
                        kwargs.get("lch", 0)
                    ),
                    Field(
                        "lci",
                        int,
                        50,
                        10,
                        kwargs.get("lci", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lccr",
                        int,
                        0,
                        10,
                        kwargs.get("lccr")
                    ),
                    Field(
                        "lcct",
                        int,
                        10,
                        10,
                        kwargs.get("lcct")
                    ),
                    Field(
                        "lcsr",
                        int,
                        20,
                        10,
                        kwargs.get("lcsr")
                    ),
                    Field(
                        "lcst",
                        int,
                        30,
                        10,
                        kwargs.get("lcst")
                    ),
                    Field(
                        "iflag",
                        int,
                        40,
                        10,
                        kwargs.get("iflag", 0)
                    ),
                    Field(
                        "sfiepm",
                        float,
                        50,
                        10,
                        kwargs.get("sfiepm", 1.0)
                    ),
                    Field(
                        "niter",
                        int,
                        60,
                        10,
                        kwargs.get("niter", 100)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat224Gys.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        GT.0.0:	Constant value is used.
        LT.0.0:	Temperature dependent Young’s modulus given by load curve ID = -E
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat.
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[0].set_value("cp", value)

    @property
    def tr(self) -> float:
        """Get or set the Room temperature.
        """ # nopep8
        return self._cards[0].get_value("tr")

    @tr.setter
    def tr(self, value: float) -> None:
        self._cards[0].set_value("tr", value)

    @property
    def beta(self) -> float:
        """Get or set the Fraction of plastic work converted into heat (superseded by FWORK in *CONTROL_THERMAL_SOLVER if a coupled thermal/structural analysis):
        GT.0.0:	Constant value is used
        LT.0.0 : -BETA gives a load curve ID for strain rate dependence, a table ID for strain rateand temperature dependence, or a 3 - dimensional table ID for temperature(TABLE_3D), strain rate(TABLE) and plastic strain(CURVE) dependence, or a 4 - dimensional table ID for triaxiality(TABLE_4D), temperature(TABLE_3D), strain rate(TABLE) and plastic strain(CURVE) dependence.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def numint(self) -> float:
        """Get or set the Number of integration points which must fail before the element is deleted. Available for shells and solids.
        LT.0.0: |NUMINT| is percentage of integration points/layers which must fail before element fails. For fully integrated shells, a methodology is used where a layer fails if one integrationpoint fails and then the given percentage of layers must fail before the element fails.
        """ # nopep8
        return self._cards[0].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        self._cards[0].set_value("numint", value)

    @property
    def lck1(self) -> int:
        """Get or set the Table ID defining for each plastic strain rate value a load curve ID giving the (isothermal) effective stress as a function of effective plastic strain for that rate.
        """ # nopep8
        return self._cards[1].get_value("lck1")

    @lck1.setter
    def lck1(self, value: int) -> None:
        self._cards[1].set_value("lck1", value)

    @property
    def lckt(self) -> int:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (qu asi-static) effective stress as a function of effective plastic strain for that temperature.
        """ # nopep8
        return self._cards[1].get_value("lckt")

    @lckt.setter
    def lckt(self, value: int) -> None:
        self._cards[1].set_value("lckt", value)

    @property
    def lcf(self) -> int:
        """Get or set the Load curve ID or table ID. The load curve specifies plastic failure strain as a function of triaxiality. The table specifies for each Lode parameter a load curve ID giving the plastic failure strain versus triaxiality for that Lode parameter. (Table option not yet generally supported).
        """ # nopep8
        return self._cards[1].get_value("lcf")

    @lcf.setter
    def lcf(self, value: int) -> None:
        self._cards[1].set_value("lcf", value)

    @property
    def lcg(self) -> int:
        """Get or set the Load curve ID for specifying plastic failure strain as a function of plastic strain rate.
        """ # nopep8
        return self._cards[1].get_value("lcg")

    @lcg.setter
    def lcg(self, value: int) -> None:
        self._cards[1].set_value("lcg", value)

    @property
    def lch(self) -> int:
        """Get or set the Load curve ID for specifying plastic failure strain as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("lch")

    @lch.setter
    def lch(self, value: int) -> None:
        self._cards[1].set_value("lch", value)

    @property
    def lci(self) -> int:
        """Get or set the Load curve ID or table ID. The load curve ID defines plastic failure strain as a function of element size. The table ID defines for each triaxiality a load curve ID giving the plastic failure strain versus element size for that triaxiality.
        """ # nopep8
        return self._cards[1].get_value("lci")

    @lci.setter
    def lci(self, value: int) -> None:
        self._cards[1].set_value("lci", value)

    @property
    def lccr(self) -> typing.Optional[int]:
        """Get or set the Table ID. The curves in this table define compressive yield stress as a function of plastic strain or effective plastic strain (see IFLAG). The table ID defines for each plastic strain rate value or effective plastic strain rate value a load curve ID giving the (isothermal) compressive yield stress as a function of plastic strain or effective plastic strain for that rate.
        """ # nopep8
        return self._cards[2].get_value("lccr")

    @lccr.setter
    def lccr(self, value: int) -> None:
        self._cards[2].set_value("lccr", value)

    @property
    def lcct(self) -> typing.Optional[int]:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) compressive yield stress as a function of strain for that temperature. The curves in this table define compressive yield stress as a function of plastic strain or effective plastic strain (see IFLAG).
        """ # nopep8
        return self._cards[2].get_value("lcct")

    @lcct.setter
    def lcct(self, value: int) -> None:
        self._cards[2].set_value("lcct", value)

    @property
    def lcsr(self) -> typing.Optional[int]:
        """Get or set the Table ID. The load curves define shear yield stress in function of plastic strain or effective plastic strain (see IFLAG). The table ID defines for each plastic strain rate value or effective plastic strain rate value a load curve ID giving the (isothermal) shear yield stress as a function of plastic strain or effective plastic strain for that rate.
        """ # nopep8
        return self._cards[2].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        self._cards[2].set_value("lcsr", value)

    @property
    def lcst(self) -> typing.Optional[int]:
        """Get or set the Table ID defining for each temperature value a load curve ID giving the (quasi-static) shear yield stress as a function of strain for that temperature. The load curves define shear yield stress as a function of plastic strain or effective plastic strain (see IFLAG).
        """ # nopep8
        return self._cards[2].get_value("lcst")

    @lcst.setter
    def lcst(self, value: int) -> None:
        self._cards[2].set_value("lcst", value)

    @property
    def iflag(self) -> int:
        """Get or set the Flag to specify abscissa for LCCR, LCCT, LCSR, LCST:
        EQ.0:	Compressive and shear yields are given as functions of plastic strain as defined in Remark 1 (default).
        EQ.1 : Compressive and shear yields are given as functions of effective plastic strain
        """ # nopep8
        return self._cards[2].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iflag must be one of {0,1}""")
        self._cards[2].set_value("iflag", value)

    @property
    def sfiepm(self) -> float:
        """Get or set the Scale factor on the initial estimate of the plastic multiplier.
        """ # nopep8
        return self._cards[2].get_value("sfiepm")

    @sfiepm.setter
    def sfiepm(self, value: float) -> None:
        self._cards[2].set_value("sfiepm", value)

    @property
    def niter(self) -> int:
        """Get or set the Number of secant iterations to be performed.
        """ # nopep8
        return self._cards[2].get_value("niter")

    @niter.setter
    def niter(self, value: int) -> None:
        self._cards[2].set_value("niter", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

