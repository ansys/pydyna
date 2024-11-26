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

class Mat153(KeywordBase):
    """DYNA MAT_153 keyword"""

    keyword = "MAT"
    subkeyword = "153"
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
                        "sigy",
                        float,
                        40,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "hardi",
                        float,
                        50,
                        10,
                        kwargs.get("hardi")
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "lcss",
                        int,
                        70,
                        10,
                        kwargs.get("lcss")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hardk1",
                        float,
                        0,
                        10,
                        kwargs.get("hardk1")
                    ),
                    Field(
                        "gamma1",
                        float,
                        10,
                        10,
                        kwargs.get("gamma1")
                    ),
                    Field(
                        "hardk2",
                        float,
                        20,
                        10,
                        kwargs.get("hardk2")
                    ),
                    Field(
                        "gamma2",
                        float,
                        30,
                        10,
                        kwargs.get("gamma2")
                    ),
                    Field(
                        "src",
                        float,
                        40,
                        10,
                        kwargs.get("src")
                    ),
                    Field(
                        "srp",
                        float,
                        50,
                        10,
                        kwargs.get("srp")
                    ),
                    Field(
                        "hardk3",
                        float,
                        60,
                        10,
                        kwargs.get("hardk3")
                    ),
                    Field(
                        "gamma3",
                        float,
                        70,
                        10,
                        kwargs.get("gamma3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idamage",
                        int,
                        0,
                        10,
                        kwargs.get("idamage", 0)
                    ),
                    Field(
                        "ids",
                        int,
                        10,
                        10,
                        kwargs.get("ids", 0)
                    ),
                    Field(
                        "idep",
                        int,
                        20,
                        10,
                        kwargs.get("idep", 0)
                    ),
                    Field(
                        "epsd",
                        float,
                        30,
                        10,
                        kwargs.get("epsd")
                    ),
                    Field(
                        "s",
                        float,
                        40,
                        10,
                        kwargs.get("s")
                    ),
                    Field(
                        "t",
                        float,
                        50,
                        10,
                        kwargs.get("t", 1)
                    ),
                    Field(
                        "dc",
                        float,
                        60,
                        10,
                        kwargs.get("dc", 0.5)
                    ),
                    Field(
                        "khflg",
                        int,
                        70,
                        10,
                        kwargs.get("khflg", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat153.option_specs[0],
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
        """Get or set the Young's modulus.
        LT.0:	-E gives the curve ID for E as a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        LT.0:	-v gives the curve ID for v as a function of temperature.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress, sigma-0.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def hardi(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening modulus, H.
        """ # nopep8
        return self._cards[0].get_value("hardi")

    @hardi.setter
    def hardi(self, value: float) -> None:
        self._cards[0].set_value("hardi", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter,  beta.  Set  beta=0 for linear isotropic hardening.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID defining effective stress as a function of effective plastic strain (and optionally temperature) for isotropic hardening.  The first abscissa value in each curve must be zero corresponding to the initial yield stress. The first ordinate value in each curve is the initial yield stress
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[0].set_value("lcss", value)

    @property
    def hardk1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening modulus C1.
        LT.0:  -C_j gives the curve ID for C_j as a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("hardk1")

    @hardk1.setter
    def hardk1(self, value: float) -> None:
        self._cards[1].set_value("hardk1", value)

    @property
    def gamma1(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter  Gamma1.γ_j.  Set γ_j = 0 for linear kinematic hardening. Ignored if HARDKj = 0.
        LT.0:  -γ_j gives the curve ID for γ_j as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("gamma1")

    @gamma1.setter
    def gamma1(self, value: float) -> None:
        self._cards[1].set_value("gamma1", value)

    @property
    def hardk2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening modulus C2.
        """ # nopep8
        return self._cards[1].get_value("hardk2")

    @hardk2.setter
    def hardk2(self, value: float) -> None:
        self._cards[1].set_value("hardk2", value)

    @property
    def gamma2(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter  Gamma2.
        """ # nopep8
        return self._cards[1].get_value("gamma2")

    @gamma2.setter
    def gamma2(self, value: float) -> None:
        self._cards[1].set_value("gamma2", value)

    @property
    def src(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, C, for Cowper Symonds strain rate model. If zero, rate effects are not considered.
        LT.0: -SRC gives the curve ID for C as a function of temperature.
        """ # nopep8
        return self._cards[1].get_value("src")

    @src.setter
    def src(self, value: float) -> None:
        self._cards[1].set_value("src", value)

    @property
    def srp(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, P, for Cowper Symonds strain rate model. If zero, rate effects are not considered.
        LT.0: -SRP gives the curve ID for p as a function of temperature
        """ # nopep8
        return self._cards[1].get_value("srp")

    @srp.setter
    def srp(self, value: float) -> None:
        self._cards[1].set_value("srp", value)

    @property
    def hardk3(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening modulus C3
        """ # nopep8
        return self._cards[1].get_value("hardk3")

    @hardk3.setter
    def hardk3(self, value: float) -> None:
        self._cards[1].set_value("hardk3", value)

    @property
    def gamma3(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter GAMMA3. Set GAMMA3 = 0 for linear kinematic hardening. Ignored if (HARDK3.EQ.0) is defined.
        """ # nopep8
        return self._cards[1].get_value("gamma3")

    @gamma3.setter
    def gamma3(self, value: float) -> None:
        self._cards[1].set_value("gamma3", value)

    @property
    def idamage(self) -> int:
        """Get or set the Isotropic damage flag
        EQ. 0: damage is inactivated
        EQ. 1: damage is activated.
        """ # nopep8
        return self._cards[2].get_value("idamage")

    @idamage.setter
    def idamage(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""idamage must be one of {0,1}""")
        self._cards[2].set_value("idamage", value)

    @property
    def ids(self) -> int:
        """Get or set the Output stress flag
        EQ. 0: undamaged stress output
        EQ. 1: damaged stress output.
        """ # nopep8
        return self._cards[2].get_value("ids")

    @ids.setter
    def ids(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ids must be one of {0,1}""")
        self._cards[2].set_value("ids", value)

    @property
    def idep(self) -> int:
        """Get or set the Damaged plastic strain
        EQ. 0: plastic strain is accumulated
        EQ. 1: damaged plastic strain is accumulated.
        """ # nopep8
        return self._cards[2].get_value("idep")

    @idep.setter
    def idep(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""idep must be one of {0,1}""")
        self._cards[2].set_value("idep", value)

    @property
    def epsd(self) -> typing.Optional[float]:
        """Get or set the Damage threshold Rd .  Damage accumulation begins when  R>Rd.
        """ # nopep8
        return self._cards[2].get_value("epsd")

    @epsd.setter
    def epsd(self, value: float) -> None:
        self._cards[2].set_value("epsd", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Damage material constant S.
        """ # nopep8
        return self._cards[2].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[2].set_value("s", value)

    @property
    def t(self) -> float:
        """Get or set the Damage material constant t.  Default = 1
        """ # nopep8
        return self._cards[2].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[2].set_value("t", value)

    @property
    def dc(self) -> float:
        """Get or set the Critical damage value Dc .  When damage value reaches critical, the element is deleted from calculation.  Default = 0.5.
        """ # nopep8
        return self._cards[2].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[2].set_value("dc", value)

    @property
    def khflg(self) -> int:
        """Get or set the Kinematic hardening flag
        EQ.0:	Use kinematic hardening parameters HARDKj and GAMMAj (default).
        EQ.1:	Kinematic hardening parameters(C_j,γ_j) given by load curve or table.NKH data points used(with a maximum of 10) in each curve.HARDKj and GAMMAj fields are ignored.
        EQ.2 : Fits NKH kinematic hardening parameters(C_j,γ_j) to uniaxial stress - strain data at constant temperature for a half - cycle, that is, it fits
        EQ.3 : Fits NKH kinematic hardening parameters(C_j, γ_j) to uniaxial stress - strain data for the tensile part of a stabilized cycle, that is, it fits,to N stress as a function of plastic strain data(ε_i ^ p, σ_i, T) given by the load curve or table LCHK.Here the first data point is chosen such that ε_1 ^ p = 0. HARDKj and GAMMAj fields are ignored.
        EQ.4 : Fits NKH kinematic hardening parameters(C_j, γ_j) to uniaxial stress - strain data for different stabilized cycles, that is, it fits,to max stress as a function of max plastic strain data(ε_i ^ p, σ_i, T) over N cycles, given by the load curve or table LCHK.HARDKj and GAMMAj fields are ignored.
        """ # nopep8
        return self._cards[2].get_value("khflg")

    @khflg.setter
    def khflg(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""khflg must be one of {0,1,2,3,4}""")
        self._cards[2].set_value("khflg", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

