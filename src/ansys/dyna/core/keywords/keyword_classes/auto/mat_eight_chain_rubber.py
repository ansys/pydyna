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

class MatEightChainRubber(KeywordBase):
    """DYNA MAT_EIGHT_CHAIN_RUBBER keyword"""

    keyword = "MAT"
    subkeyword = "EIGHT_CHAIN_RUBBER"
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
                        "k",
                        float,
                        20,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "mu",
                        float,
                        30,
                        10,
                        kwargs.get("mu")
                    ),
                    Field(
                        "n",
                        int,
                        40,
                        10,
                        kwargs.get("n", 0)
                    ),
                    Field(
                        "mull",
                        int,
                        50,
                        10,
                        kwargs.get("mull", 1)
                    ),
                    Field(
                        "vispl",
                        int,
                        60,
                        10,
                        kwargs.get("vispl", 0)
                    ),
                    Field(
                        "visel",
                        int,
                        70,
                        10,
                        kwargs.get("visel", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "yld0",
                        float,
                        0,
                        10,
                        kwargs.get("yld0")
                    ),
                    Field(
                        "fp",
                        float,
                        10,
                        10,
                        kwargs.get("fp")
                    ),
                    Field(
                        "gp",
                        float,
                        20,
                        10,
                        kwargs.get("gp")
                    ),
                    Field(
                        "hp",
                        float,
                        30,
                        10,
                        kwargs.get("hp")
                    ),
                    Field(
                        "lp",
                        float,
                        40,
                        10,
                        kwargs.get("lp")
                    ),
                    Field(
                        "mp",
                        float,
                        50,
                        10,
                        kwargs.get("mp")
                    ),
                    Field(
                        "np",
                        float,
                        60,
                        10,
                        kwargs.get("np")
                    ),
                    Field(
                        "pmu",
                        float,
                        70,
                        10,
                        kwargs.get("pmu")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "m1",
                        float,
                        0,
                        10,
                        kwargs.get("m1")
                    ),
                    Field(
                        "m2",
                        float,
                        10,
                        10,
                        kwargs.get("m2")
                    ),
                    Field(
                        "m3",
                        float,
                        20,
                        10,
                        kwargs.get("m3")
                    ),
                    Field(
                        "m4",
                        float,
                        30,
                        10,
                        kwargs.get("m4")
                    ),
                    Field(
                        "m5",
                        float,
                        40,
                        10,
                        kwargs.get("m5")
                    ),
                    Field(
                        "time",
                        float,
                        50,
                        10,
                        kwargs.get("time")
                    ),
                    Field(
                        "vcon",
                        float,
                        60,
                        10,
                        kwargs.get("vcon", 9.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "q1",
                        float,
                        0,
                        10,
                        kwargs.get("q1")
                    ),
                    Field(
                        "b1",
                        float,
                        10,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "q2",
                        float,
                        20,
                        10,
                        kwargs.get("q2")
                    ),
                    Field(
                        "b2",
                        float,
                        30,
                        10,
                        kwargs.get("b2")
                    ),
                    Field(
                        "q3",
                        float,
                        40,
                        10,
                        kwargs.get("q3")
                    ),
                    Field(
                        "b3",
                        float,
                        50,
                        10,
                        kwargs.get("b3")
                    ),
                    Field(
                        "q4",
                        float,
                        60,
                        10,
                        kwargs.get("q4")
                    ),
                    Field(
                        "b4",
                        float,
                        70,
                        10,
                        kwargs.get("b4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k1",
                        float,
                        0,
                        10,
                        kwargs.get("k1")
                    ),
                    Field(
                        "s1",
                        float,
                        10,
                        10,
                        kwargs.get("s1")
                    ),
                    Field(
                        "k2",
                        float,
                        20,
                        10,
                        kwargs.get("k2")
                    ),
                    Field(
                        "s2",
                        float,
                        30,
                        10,
                        kwargs.get("s2")
                    ),
                    Field(
                        "k3",
                        float,
                        40,
                        10,
                        kwargs.get("k3")
                    ),
                    Field(
                        "s3",
                        float,
                        50,
                        10,
                        kwargs.get("s3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        float,
                        0,
                        10,
                        kwargs.get("aopt", 0.0)
                    ),
                    Field(
                        "macf",
                        int,
                        10,
                        10,
                        kwargs.get("macf", 1)
                    ),
                    Field(
                        "xp",
                        float,
                        20,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        30,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        40,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        50,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        60,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        70,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        kwargs.get("v3")
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "taui",
                        float,
                        0,
                        10,
                        kwargs.get("taui")
                    ),
                    Field(
                        "betai/gammai",
                        float,
                        10,
                        10,
                        kwargs.get("betai/gammai")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "taui",
                        float,
                        0,
                        10,
                        kwargs.get("taui")
                    ),
                    Field(
                        "betai/gammai",
                        float,
                        10,
                        10,
                        kwargs.get("betai/gammai")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "taui",
                        float,
                        0,
                        10,
                        kwargs.get("taui")
                    ),
                    Field(
                        "betai/gammai",
                        float,
                        10,
                        10,
                        kwargs.get("betai/gammai")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "taui",
                        float,
                        0,
                        10,
                        kwargs.get("taui")
                    ),
                    Field(
                        "betai/gammai",
                        float,
                        10,
                        10,
                        kwargs.get("betai/gammai")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "taui",
                        float,
                        0,
                        10,
                        kwargs.get("taui")
                    ),
                    Field(
                        "betai/gammai",
                        float,
                        10,
                        10,
                        kwargs.get("betai/gammai")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "taui",
                        float,
                        0,
                        10,
                        kwargs.get("taui")
                    ),
                    Field(
                        "betai/gammai",
                        float,
                        10,
                        10,
                        kwargs.get("betai/gammai")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "taui",
                        float,
                        0,
                        10,
                        kwargs.get("taui")
                    ),
                    Field(
                        "betai/gammai",
                        float,
                        10,
                        10,
                        kwargs.get("betai/gammai")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatEightChainRubber.option_specs[0],
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus. To get almost incompressible behavior set this to one or two orders of magnitude higher than MU. Note that the Poisson's ratio should be kept at a realistic value.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.MU is the product of the number of molecular chains	per unit volume (n), Boltzmann's constant (k) and the absolute temperature (T). Thus MU=nkT
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[0].set_value("mu", value)

    @property
    def n(self) -> int:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        self._cards[0].set_value("n", value)

    @property
    def mull(self) -> int:
        """Get or set the Parameter describing which softening algorithm that shall be used.
        EQ.1: Strain based Mullins effect from Qi and Boyce, see theory section
        below for details
        M1 = A (Qi recommends 3.5)
        M2 = B (Qi recommends 18.0)
        M3 = Z (Qi recommends 0.7)
        M4 = vs (between 0 and 1 and less than vss)
        M5 = vss (between 0 and 1 and greater than vs)
        EQ.2: Energy based Mullins, a modified version of Roxburgh and Ogden model. M1 > 0, M2 > 0 and M3 > 0 must be set. See Theory section for details.
        """ # nopep8
        return self._cards[0].get_value("mull")

    @mull.setter
    def mull(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""mull must be one of {1,2}""")
        self._cards[0].set_value("mull", value)

    @property
    def vispl(self) -> int:
        """Get or set the Parameter describing which viscoplastic formulation that should be
        used, see the theory section for details.
        EQ.0: No viscoplasticity.
        EQ.1: 2 parameters standard model, K1 and S1 must be set.
        EQ.¦Â: 6 parameters G'Sells model, K1,K¦Â,K¦Ã,S1,S¦Â and S¦Ã must be set.
        EQ.3: 4 parameters Strain hardening model, K1,K2,S1,S2 must be set.
        """ # nopep8
        return self._cards[0].get_value("vispl")

    @vispl.setter
    def vispl(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""vispl must be one of {0,1,2,3}""")
        self._cards[0].set_value("vispl", value)

    @property
    def visel(self) -> int:
        """Get or set the Option for viscoelastic behavior, see the theory section for details.
        EQ.0: No viscoelasticity.
        EQ.1: Free energy formulation based on Holzapfel and Ogden.
        EQ.2: Formulation based on stiffness ratios from Simo et al.
        """ # nopep8
        return self._cards[0].get_value("visel")

    @visel.setter
    def visel(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""visel must be one of {0,1,2,3}""")
        self._cards[0].set_value("visel", value)

    @property
    def yld0(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        EQ.0.0: No plasticity
        GT.0.0: Initial yield stress. Hardening is defined seperataly.
        LT.0.0: -YLD0 is taken as the load curve ID for the yield stress versus
        effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("yld0")

    @yld0.setter
    def yld0(self, value: float) -> None:
        self._cards[1].set_value("yld0", value)

    @property
    def fp(self) -> typing.Optional[float]:
        """Get or set the Parameters for Hill's general yield surface. For von mises yield criteria set FP=GP=HP=0.5 and LP=MP=NP=1.5.
        """ # nopep8
        return self._cards[1].get_value("fp")

    @fp.setter
    def fp(self, value: float) -> None:
        self._cards[1].set_value("fp", value)

    @property
    def gp(self) -> typing.Optional[float]:
        """Get or set the Parameters for Hill's general yield surface. For von mises yield criteria set FP=GP=HP=0.5 and LP=MP=NP=1.5.
        """ # nopep8
        return self._cards[1].get_value("gp")

    @gp.setter
    def gp(self, value: float) -> None:
        self._cards[1].set_value("gp", value)

    @property
    def hp(self) -> typing.Optional[float]:
        """Get or set the Parameters for Hill's general yield surface. For von mises yield criteria set FP=GP=HP=0.5 and LP=MP=NP=1.5.
        """ # nopep8
        return self._cards[1].get_value("hp")

    @hp.setter
    def hp(self, value: float) -> None:
        self._cards[1].set_value("hp", value)

    @property
    def lp(self) -> typing.Optional[float]:
        """Get or set the Parameters for Hill's general yield surface. For von mises yield criteria set FP=GP=HP=0.5 and LP=MP=NP=1.5.
        """ # nopep8
        return self._cards[1].get_value("lp")

    @lp.setter
    def lp(self, value: float) -> None:
        self._cards[1].set_value("lp", value)

    @property
    def mp(self) -> typing.Optional[float]:
        """Get or set the Parameters for Hill's general yield surface. For von mises yield criteria set FP=GP=HP=0.5 and LP=MP=NP=1.5.
        """ # nopep8
        return self._cards[1].get_value("mp")

    @mp.setter
    def mp(self, value: float) -> None:
        self._cards[1].set_value("mp", value)

    @property
    def np(self) -> typing.Optional[float]:
        """Get or set the Parameters for Hill's general yield surface. For von mises yield criteria set FP=GP=HP=0.5 and LP=MP=NP=1.5.
        """ # nopep8
        return self._cards[1].get_value("np")

    @np.setter
    def np(self, value: float) -> None:
        self._cards[1].set_value("np", value)

    @property
    def pmu(self) -> typing.Optional[float]:
        """Get or set the Kinematic hardening parameter. It is usually equal to MU.
        """ # nopep8
        return self._cards[1].get_value("pmu")

    @pmu.setter
    def pmu(self, value: float) -> None:
        self._cards[1].set_value("pmu", value)

    @property
    def m1(self) -> typing.Optional[float]:
        """Get or set the Mullins parameters
        MULL.EQ.1: M1-M5 are used
        MULL.EQ.2: M1-M3 are used.
        """ # nopep8
        return self._cards[2].get_value("m1")

    @m1.setter
    def m1(self, value: float) -> None:
        self._cards[2].set_value("m1", value)

    @property
    def m2(self) -> typing.Optional[float]:
        """Get or set the Mullins parameters
        MULL.EQ.1: M1-M5 are used
        MULL.EQ.2: M1-M3 are used.
        """ # nopep8
        return self._cards[2].get_value("m2")

    @m2.setter
    def m2(self, value: float) -> None:
        self._cards[2].set_value("m2", value)

    @property
    def m3(self) -> typing.Optional[float]:
        """Get or set the Mullins parameters
        MULL.EQ.1: M1-M5 are used
        MULL.EQ.2: M1-M3 are used.
        """ # nopep8
        return self._cards[2].get_value("m3")

    @m3.setter
    def m3(self, value: float) -> None:
        self._cards[2].set_value("m3", value)

    @property
    def m4(self) -> typing.Optional[float]:
        """Get or set the Mullins parameters
        MULL.EQ.1: M1-M5 are used
        MULL.EQ.2: M1-M3 are used.
        """ # nopep8
        return self._cards[2].get_value("m4")

    @m4.setter
    def m4(self, value: float) -> None:
        self._cards[2].set_value("m4", value)

    @property
    def m5(self) -> typing.Optional[float]:
        """Get or set the Mullins parameters
        MULL.EQ.1: M1-M5 are used
        MULL.EQ.2: M1-M3 are used.
        """ # nopep8
        return self._cards[2].get_value("m5")

    @m5.setter
    def m5(self, value: float) -> None:
        self._cards[2].set_value("m5", value)

    @property
    def time(self) -> typing.Optional[float]:
        """Get or set the A time filter that is used to smoothen out the time derivate of the strain	invariant over a TIME interval. Default is no smoothening but a value	100*TIMESTEP is recommended.
        """ # nopep8
        return self._cards[2].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        self._cards[2].set_value("time", value)

    @property
    def vcon(self) -> float:
        """Get or set the A material constant for the volumetric part of the strain energy. Default 9.0 but any value can be used to tailor the volumetric response.
        """ # nopep8
        return self._cards[2].get_value("vcon")

    @vcon.setter
    def vcon(self, value: float) -> None:
        self._cards[2].set_value("vcon", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Voce hardening parameters.
        """ # nopep8
        return self._cards[3].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        self._cards[3].set_value("q1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Voce hardening parameters.
        """ # nopep8
        return self._cards[3].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[3].set_value("b1", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Voce hardening parameters.
        """ # nopep8
        return self._cards[3].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        self._cards[3].set_value("q2", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Voce hardening parameters.
        """ # nopep8
        return self._cards[3].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[3].set_value("b2", value)

    @property
    def q3(self) -> typing.Optional[float]:
        """Get or set the Voce hardening parameters.
        """ # nopep8
        return self._cards[3].get_value("q3")

    @q3.setter
    def q3(self, value: float) -> None:
        self._cards[3].set_value("q3", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the Voce hardening parameters.
        """ # nopep8
        return self._cards[3].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        self._cards[3].set_value("b3", value)

    @property
    def q4(self) -> typing.Optional[float]:
        """Get or set the Voce hardening parameters.
        """ # nopep8
        return self._cards[3].get_value("q4")

    @q4.setter
    def q4(self, value: float) -> None:
        self._cards[3].set_value("q4", value)

    @property
    def b4(self) -> typing.Optional[float]:
        """Get or set the Voce hardening parameters.
        """ # nopep8
        return self._cards[3].get_value("b4")

    @b4.setter
    def b4(self, value: float) -> None:
        self._cards[3].set_value("b4", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Viscoplastic parameters.
        VISPL.EQ.1: K1 and S1 are used.
        VISPL.EQ.2: K1, S1, K2, S2, K3 and S3 are used.
        VISPL.EQ.3: K1, S1 and K2 are used.
        """ # nopep8
        return self._cards[4].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        self._cards[4].set_value("k1", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the Viscoplastic parameters.
        VISPL.EQ.1: K1 and S1 are used.
        VISPL.EQ.2: K1, S1, K2, S2, K3 and S3 are used.
        VISPL.EQ.3: K1, S1 and K2 are used.
        """ # nopep8
        return self._cards[4].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        self._cards[4].set_value("s1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Viscoplastic parameters.
        VISPL.EQ.1: K1 and S1 are used.
        VISPL.EQ.2: K1, S1, K2, S2, K3 and S3 are used.
        VISPL.EQ.3: K1, S1 and K2 are used.
        """ # nopep8
        return self._cards[4].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        self._cards[4].set_value("k2", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the Viscoplastic parameters.
        VISPL.EQ.1: K1 and S1 are used.
        VISPL.EQ.2: K1, S1, K2, S2, K3 and S3 are used.
        VISPL.EQ.3: K1, S1 and K2 are used.
        """ # nopep8
        return self._cards[4].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        self._cards[4].set_value("s2", value)

    @property
    def k3(self) -> typing.Optional[float]:
        """Get or set the Viscoplastic parameters.
        VISPL.EQ.1: K1 and S1 are used.
        VISPL.EQ.2: K1, S1, K2, S2, K3 and S3 are used.
        VISPL.EQ.3: K1, S1 and K2 are used.
        """ # nopep8
        return self._cards[4].get_value("k3")

    @k3.setter
    def k3(self, value: float) -> None:
        self._cards[4].set_value("k3", value)

    @property
    def s3(self) -> typing.Optional[float]:
        """Get or set the Viscoplastic parameters.
        VISPL.EQ.1: K1 and S1 are used.
        VISPL.EQ.2: K1, S1, K2, S2, K3 and S3 are used.
        VISPL.EQ.3: K1, S1 and K2 are used.
        """ # nopep8
        return self._cards[4].get_value("s3")

    @s3.setter
    def s3(self, value: float) -> None:
        self._cards[4].set_value("s3", value)

    @property
    def aopt(self) -> float:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        """ # nopep8
        return self._cards[5].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0, 4.0]:
            raise Exception("""aopt must be one of {0.0,1.0,2.0,3.0,4.0}""")
        self._cards[5].set_value("aopt", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ. - 4:	Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 7 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
        """ # nopep8
        return self._cards[5].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        if value not in [1, 2, 3, 4, -2, -3, -4]:
            raise Exception("""macf must be one of {1,2,3,4,-2,-3,-4}""")
        self._cards[5].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates for point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates for point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinates for point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[5].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[5].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[5].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[5].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[6].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[6].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[6].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[6].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[6].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[6].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3. It may be overridden on the element card; see *ELEMENT_‌SOLID_‌ORTHO
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[6].set_value("beta", value)

    @property
    def taui(self) -> typing.Optional[float]:
        """Get or set the Relaxation time. A maximum of 6 values can be used.
        """ # nopep8
        return self._cards[7].get_value("taui")

    @taui.setter
    def taui(self, value: float) -> None:
        self._cards[7].set_value("taui", value)

    @property
    def betai_gammai(self) -> typing.Optional[float]:
        """Get or set the VISEL.EQ.1: Dissipating energy factors.(see Holzapfel)
        VISEL.EQ.2: Gamma factors (see Simo).
        """ # nopep8
        return self._cards[7].get_value("betai/gammai")

    @betai_gammai.setter
    def betai_gammai(self, value: float) -> None:
        self._cards[7].set_value("betai/gammai", value)

    @property
    def taui(self) -> typing.Optional[float]:
        """Get or set the Relaxation time. A maximum of 6 values can be used.
        """ # nopep8
        return self._cards[8].get_value("taui")

    @taui.setter
    def taui(self, value: float) -> None:
        self._cards[8].set_value("taui", value)

    @property
    def betai_gammai(self) -> typing.Optional[float]:
        """Get or set the VISEL.EQ.1: Dissipating energy factors.(see Holzapfel)
        VISEL.EQ.2: Gamma factors (see Simo).
        """ # nopep8
        return self._cards[8].get_value("betai/gammai")

    @betai_gammai.setter
    def betai_gammai(self, value: float) -> None:
        self._cards[8].set_value("betai/gammai", value)

    @property
    def taui(self) -> typing.Optional[float]:
        """Get or set the Relaxation time. A maximum of 6 values can be used.
        """ # nopep8
        return self._cards[9].get_value("taui")

    @taui.setter
    def taui(self, value: float) -> None:
        self._cards[9].set_value("taui", value)

    @property
    def betai_gammai(self) -> typing.Optional[float]:
        """Get or set the VISEL.EQ.1: Dissipating energy factors.(see Holzapfel)
        VISEL.EQ.2: Gamma factors (see Simo).
        """ # nopep8
        return self._cards[9].get_value("betai/gammai")

    @betai_gammai.setter
    def betai_gammai(self, value: float) -> None:
        self._cards[9].set_value("betai/gammai", value)

    @property
    def taui(self) -> typing.Optional[float]:
        """Get or set the Relaxation time. A maximum of 6 values can be used.
        """ # nopep8
        return self._cards[10].get_value("taui")

    @taui.setter
    def taui(self, value: float) -> None:
        self._cards[10].set_value("taui", value)

    @property
    def betai_gammai(self) -> typing.Optional[float]:
        """Get or set the VISEL.EQ.1: Dissipating energy factors.(see Holzapfel)
        VISEL.EQ.2: Gamma factors (see Simo).
        """ # nopep8
        return self._cards[10].get_value("betai/gammai")

    @betai_gammai.setter
    def betai_gammai(self, value: float) -> None:
        self._cards[10].set_value("betai/gammai", value)

    @property
    def taui(self) -> typing.Optional[float]:
        """Get or set the Relaxation time. A maximum of 6 values can be used.
        """ # nopep8
        return self._cards[11].get_value("taui")

    @taui.setter
    def taui(self, value: float) -> None:
        self._cards[11].set_value("taui", value)

    @property
    def betai_gammai(self) -> typing.Optional[float]:
        """Get or set the VISEL.EQ.1: Dissipating energy factors.(see Holzapfel)
        VISEL.EQ.2: Gamma factors (see Simo).
        """ # nopep8
        return self._cards[11].get_value("betai/gammai")

    @betai_gammai.setter
    def betai_gammai(self, value: float) -> None:
        self._cards[11].set_value("betai/gammai", value)

    @property
    def taui(self) -> typing.Optional[float]:
        """Get or set the Relaxation time. A maximum of 6 values can be used.
        """ # nopep8
        return self._cards[12].get_value("taui")

    @taui.setter
    def taui(self, value: float) -> None:
        self._cards[12].set_value("taui", value)

    @property
    def betai_gammai(self) -> typing.Optional[float]:
        """Get or set the VISEL.EQ.1: Dissipating energy factors.(see Holzapfel)
        VISEL.EQ.2: Gamma factors (see Simo).
        """ # nopep8
        return self._cards[12].get_value("betai/gammai")

    @betai_gammai.setter
    def betai_gammai(self, value: float) -> None:
        self._cards[12].set_value("betai/gammai", value)

    @property
    def taui(self) -> typing.Optional[float]:
        """Get or set the Relaxation time. A maximum of 6 values can be used.
        """ # nopep8
        return self._cards[13].get_value("taui")

    @taui.setter
    def taui(self, value: float) -> None:
        self._cards[13].set_value("taui", value)

    @property
    def betai_gammai(self) -> typing.Optional[float]:
        """Get or set the VISEL.EQ.1: Dissipating energy factors.(see Holzapfel)
        VISEL.EQ.2: Gamma factors (see Simo).
        """ # nopep8
        return self._cards[13].get_value("betai/gammai")

    @betai_gammai.setter
    def betai_gammai(self, value: float) -> None:
        self._cards[13].set_value("betai/gammai", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[14].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[14].cards[0].set_value("title", value)

