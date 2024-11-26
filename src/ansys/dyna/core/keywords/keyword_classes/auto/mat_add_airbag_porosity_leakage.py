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

class MatAddAirbagPorosityLeakage(KeywordBase):
    """DYNA MAT_ADD_AIRBAG_POROSITY_LEAKAGE keyword"""

    keyword = "MAT"
    subkeyword = "ADD_AIRBAG_POROSITY_LEAKAGE"
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
                        "x2/flc",
                        float,
                        10,
                        10,
                        kwargs.get("x2/flc")
                    ),
                    Field(
                        "x3/fac",
                        float,
                        20,
                        10,
                        kwargs.get("x3/fac", 1.0)
                    ),
                    Field(
                        "ela",
                        float,
                        30,
                        10,
                        kwargs.get("ela")
                    ),
                    Field(
                        "fvopt",
                        float,
                        40,
                        10,
                        kwargs.get("fvopt")
                    ),
                    Field(
                        "x0",
                        float,
                        50,
                        10,
                        kwargs.get("x0")
                    ),
                    Field(
                        "x1",
                        float,
                        60,
                        10,
                        kwargs.get("x1")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddAirbagPorosityLeakage.option_specs[0],
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
        """Get or set the Material ID for which the porosity leakage property applies
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def x2_flc(self) -> typing.Optional[float]:
        """Get or set the X2 is one of the coefficients of the porosity in the equation of Anagonye  and Wang [1999].  (Defined below in description for X0/X1)
        Optional fabric porous leakage flow coefficient:
        GE.0.0:	fabric porous leakage flow coefficient
        LT.0.0 : | FLC | is the load curve ID of the curve defining FLC as a function of time.
        """ # nopep8
        return self._cards[0].get_value("x2/flc")

    @x2_flc.setter
    def x2_flc(self, value: float) -> None:
        self._cards[0].set_value("x2/flc", value)

    @property
    def x3_fac(self) -> float:
        """Get or set the X3 is one of the coefficients of the porosity in the equation of Anagonye and Wang [1999].  (Defined below in description for X0/X1)
        Optional fabric characteristic parameter:
        GE.0.0:	optional fabric characteristic parameter
        LT.0.0 : | FAC | is the load curve ID of the curve defining FAC as a function of absolute pressure.
        """ # nopep8
        return self._cards[0].get_value("x3/fac")

    @x3_fac.setter
    def x3_fac(self, value: float) -> None:
        self._cards[0].set_value("x3/fac", value)

    @property
    def ela(self) -> typing.Optional[float]:
        """Get or set the Effective leakage area for blocked fabric, ELA.
        LT.0.0: | ELA | is the load curve ID of the curve defining ELA as a function of time.The default value of zero assumes that no leakage occurs.A value of .10 would assume that 10 % of the blocked fabric is leaking gas.
        """ # nopep8
        return self._cards[0].get_value("ela")

    @ela.setter
    def ela(self, value: float) -> None:
        self._cards[0].set_value("ela", value)

    @property
    def fvopt(self) -> typing.Optional[float]:
        """Get or set the Fabric venting option.
        EQ.1:	Wang - Nefske formulas for venting through an orifice are used.Blockage is not considered.
        EQ.2 : Wang - Nefske formulas for venting through an orifice are used.Blockage of venting area due to contact is considered.
        EQ.3 : Leakage formulas of Graefe, Krummheuer,and Siejak[1990] are used.Blockage is not considered.
        EQ.4 : Leakage formulas of Graefe, Krummheuer,and Siejak[1990] are used.Blockage of venting area due to contact is considered.
        EQ.5 : Leakage formulas based on flow through a porous media are used.Blockage is not considered.
        EQ.6 : Leakage formulas based on flow through a porous media are used.Blockage of venting area due to contact is considered.
        EQ.7 : Leakage is based on gas volume outflow as a function of pressure load curve[Lian, 2000].Blockage is not considered.Absolute pressure is used in the porous - velocity - versus - pressure load curve, given as FAC.
        EQ.8 : Leakage is based on gas volume outflow as a function of pressure load curve[Lian 2000].Blockage of venting or porous area due to contact is considered.Absolute pressure is used in the porous - velocity - versus - pressure load curve, given as FAC.
        """ # nopep8
        return self._cards[0].get_value("fvopt")

    @fvopt.setter
    def fvopt(self, value: float) -> None:
        self._cards[0].set_value("fvopt", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
        """ # nopep8
        return self._cards[0].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[0].set_value("x0", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
        """ # nopep8
        return self._cards[0].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[0].set_value("x1", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

