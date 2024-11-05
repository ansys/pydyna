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

class MatConcreteDamagePlasticModel(KeywordBase):
    """DYNA MAT_CONCRETE_DAMAGE_PLASTIC_MODEL keyword"""

    keyword = "MAT"
    subkeyword = "CONCRETE_DAMAGE_PLASTIC_MODEL"
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
                        kwargs.get("pr", 0.2)
                    ),
                    Field(
                        "ecc",
                        float,
                        40,
                        10,
                        kwargs.get("ecc")
                    ),
                    Field(
                        "qh0",
                        float,
                        50,
                        10,
                        kwargs.get("qh0", 0.3)
                    ),
                    Field(
                        "ft",
                        float,
                        60,
                        10,
                        kwargs.get("ft")
                    ),
                    Field(
                        "fc",
                        float,
                        70,
                        10,
                        kwargs.get("fc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hp",
                        float,
                        0,
                        10,
                        kwargs.get("hp", 0.5)
                    ),
                    Field(
                        "ah",
                        float,
                        10,
                        10,
                        kwargs.get("ah", 0.08)
                    ),
                    Field(
                        "bh",
                        float,
                        20,
                        10,
                        kwargs.get("bh", 0.003)
                    ),
                    Field(
                        "ch",
                        float,
                        30,
                        10,
                        kwargs.get("ch", 2.0)
                    ),
                    Field(
                        "dh",
                        float,
                        40,
                        10,
                        kwargs.get("dh", 1.0E-6)
                    ),
                    Field(
                        "as",
                        float,
                        50,
                        10,
                        kwargs.get("as", 15.0)
                    ),
                    Field(
                        "df",
                        float,
                        60,
                        10,
                        kwargs.get("df", 0.85)
                    ),
                    Field(
                        "fc0",
                        float,
                        70,
                        10,
                        kwargs.get("fc0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "type",
                        float,
                        0,
                        10,
                        kwargs.get("type", 0.0)
                    ),
                    Field(
                        "bs",
                        float,
                        10,
                        10,
                        kwargs.get("bs", 1.0)
                    ),
                    Field(
                        "wf",
                        float,
                        20,
                        10,
                        kwargs.get("wf")
                    ),
                    Field(
                        "wf1",
                        float,
                        30,
                        10,
                        kwargs.get("wf1")
                    ),
                    Field(
                        "ft1",
                        float,
                        40,
                        10,
                        kwargs.get("ft1")
                    ),
                    Field(
                        "strflg",
                        float,
                        50,
                        10,
                        kwargs.get("strflg", 0.0)
                    ),
                    Field(
                        "failflg",
                        float,
                        60,
                        10,
                        kwargs.get("failflg")
                    ),
                    Field(
                        "efc",
                        float,
                        70,
                        10,
                        kwargs.get("efc", 1.0E-4)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatConcreteDamagePlasticModel.option_specs[0],
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus. The sign determines if an anisotropic (E positive, referred to as ISOFLAG=0 in the remarks) or an isotropic (E negative, referred to as ISOFLAG=1 in the remarks) damage formulation is used. The Young's modulus is taken as the absolute value of this parameter.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> float:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def ecc(self) -> typing.Optional[float]:
        """Get or set the Eccentricity parameter.
        """ # nopep8
        return self._cards[0].get_value("ecc")

    @ecc.setter
    def ecc(self, value: float) -> None:
        self._cards[0].set_value("ecc", value)

    @property
    def qh0(self) -> float:
        """Get or set the Initial hardening defined as FC0/FC where FC0 is the compressive stress at which the initial yield surface is reached. Default = 0.3.
        """ # nopep8
        return self._cards[0].get_value("qh0")

    @qh0.setter
    def qh0(self, value: float) -> None:
        self._cards[0].set_value("qh0", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Uniaxial tensile strength (stress).
        """ # nopep8
        return self._cards[0].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        self._cards[0].set_value("ft", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Uniaxial compression strength (stress)
        """ # nopep8
        return self._cards[0].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        self._cards[0].set_value("fc", value)

    @property
    def hp(self) -> float:
        """Get or set the Hardening parameter. Default is HP=0.5 which is the value used in Grassl et al. (2011) for strain rate dependent material response (STRFLG = 1). For applications without strain rate effect  (STRFLG = 0) a value of HP = 0.01 is recommended, which has been used in Grassl et al. (2013)
        """ # nopep8
        return self._cards[1].get_value("hp")

    @hp.setter
    def hp(self, value: float) -> None:
        self._cards[1].set_value("hp", value)

    @property
    def ah(self) -> float:
        """Get or set the Hardening ductility parameter 1.
        """ # nopep8
        return self._cards[1].get_value("ah")

    @ah.setter
    def ah(self, value: float) -> None:
        self._cards[1].set_value("ah", value)

    @property
    def bh(self) -> float:
        """Get or set the Hardening ductility parameter 2.
        """ # nopep8
        return self._cards[1].get_value("bh")

    @bh.setter
    def bh(self, value: float) -> None:
        self._cards[1].set_value("bh", value)

    @property
    def ch(self) -> float:
        """Get or set the Hardening ductility parameter 3.
        """ # nopep8
        return self._cards[1].get_value("ch")

    @ch.setter
    def ch(self, value: float) -> None:
        self._cards[1].set_value("ch", value)

    @property
    def dh(self) -> float:
        """Get or set the Hardening ductility parameter 4.
        """ # nopep8
        return self._cards[1].get_value("dh")

    @dh.setter
    def dh(self, value: float) -> None:
        self._cards[1].set_value("dh", value)

    @property
    def as_(self) -> float:
        """Get or set the Ductility parameter during damage.
        """ # nopep8
        return self._cards[1].get_value("as")

    @as_.setter
    def as_(self, value: float) -> None:
        self._cards[1].set_value("as", value)

    @property
    def df(self) -> float:
        """Get or set the Flow rule parameter.
        """ # nopep8
        return self._cards[1].get_value("df")

    @df.setter
    def df(self, value: float) -> None:
        self._cards[1].set_value("df", value)

    @property
    def fc0(self) -> typing.Optional[float]:
        """Get or set the Rate dependent parameter.Only needed if STRFLG = 1. Recommended value is 10 MPa, which has to be entered consistently with the system of units used.
        """ # nopep8
        return self._cards[1].get_value("fc0")

    @fc0.setter
    def fc0(self, value: float) -> None:
        self._cards[1].set_value("fc0", value)

    @property
    def type(self) -> float:
        """Get or set the Flag for damage type.
        EQ.0.0: Linear damage formulation (Default)
        EQ.1.0: Bi-linear damage formulation
        EQ.2.0: Exponential damage formulation
        EQ.3.0: No damage The best results are obtained with the bi-linear formulation.
        """ # nopep8
        return self._cards[2].get_value("type")

    @type.setter
    def type(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0]:
            raise Exception("""type must be one of {0.0,1.0,2.0,3.0}""")
        self._cards[2].set_value("type", value)

    @property
    def bs(self) -> float:
        """Get or set the Damage ductility exponent during damage.	Default = 1.0.
        """ # nopep8
        return self._cards[2].get_value("bs")

    @bs.setter
    def bs(self, value: float) -> None:
        self._cards[2].set_value("bs", value)

    @property
    def wf(self) -> typing.Optional[float]:
        """Get or set the Tensile threshold value for linear damage formulation.Parameter controlling tensile softening branch for exponential tensile damage formulation.
        """ # nopep8
        return self._cards[2].get_value("wf")

    @wf.setter
    def wf(self, value: float) -> None:
        self._cards[2].set_value("wf", value)

    @property
    def wf1(self) -> typing.Optional[float]:
        """Get or set the Tensile threshold value for the second part of the bi-linear damage formulation. Default = 0.15*WF.
        """ # nopep8
        return self._cards[2].get_value("wf1")

    @wf1.setter
    def wf1(self, value: float) -> None:
        self._cards[2].set_value("wf1", value)

    @property
    def ft1(self) -> typing.Optional[float]:
        """Get or set the Tensile strength threshold value for bi-linear damage formulation. Default = 0.3*FT.
        """ # nopep8
        return self._cards[2].get_value("ft1")

    @ft1.setter
    def ft1(self, value: float) -> None:
        self._cards[2].set_value("ft1", value)

    @property
    def strflg(self) -> float:
        """Get or set the Strain rate flag.
        EQ.1.0: Strain rate dependent
        EQ.0.0: No strain rate dependency.
        """ # nopep8
        return self._cards[2].get_value("strflg")

    @strflg.setter
    def strflg(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""strflg must be one of {0.0,1.0}""")
        self._cards[2].set_value("strflg", value)

    @property
    def failflg(self) -> typing.Optional[float]:
        """Get or set the Failure flag.
        EQ.0.0: Not active. No erosion.
        EQ.X > 0.0: Active and element will erode if wt and wc is equal to 1 in
        X percent of the integration points. If X=0.60, 60% of all integration
        points must fail before erosion..
        """ # nopep8
        return self._cards[2].get_value("failflg")

    @failflg.setter
    def failflg(self, value: float) -> None:
        self._cards[2].set_value("failflg", value)

    @property
    def efc(self) -> float:
        """Get or set the Parameter controlling compressive damage softening branch in the exponential compressive damage formulation.Default = 1.0E-4
        """ # nopep8
        return self._cards[2].get_value("efc")

    @efc.setter
    def efc(self, value: float) -> None:
        self._cards[2].set_value("efc", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

