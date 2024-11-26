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

class MatConcreteEc2(KeywordBase):
    """DYNA MAT_CONCRETE_EC2 keyword"""

    keyword = "MAT"
    subkeyword = "CONCRETE_EC2"
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
                        "fc",
                        float,
                        20,
                        10,
                        kwargs.get("fc")
                    ),
                    Field(
                        "ft",
                        float,
                        30,
                        10,
                        kwargs.get("ft")
                    ),
                    Field(
                        "typec",
                        float,
                        40,
                        10,
                        kwargs.get("typec", 1.0)
                    ),
                    Field(
                        "unitc",
                        float,
                        50,
                        10,
                        kwargs.get("unitc", 1.0)
                    ),
                    Field(
                        "ecuten",
                        float,
                        60,
                        10,
                        kwargs.get("ecuten", 0.0025)
                    ),
                    Field(
                        "fcc",
                        float,
                        70,
                        10,
                        kwargs.get("fcc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "esoft",
                        float,
                        0,
                        10,
                        kwargs.get("esoft")
                    ),
                    Field(
                        "lchar",
                        float,
                        10,
                        10,
                        kwargs.get("lchar")
                    ),
                    Field(
                        "mu",
                        float,
                        20,
                        10,
                        kwargs.get("mu", 0.4)
                    ),
                    Field(
                        "taumxf",
                        float,
                        30,
                        10,
                        kwargs.get("taumxf", 1.E20)
                    ),
                    Field(
                        "taumxc",
                        float,
                        40,
                        10,
                        kwargs.get("taumxc", 1.161)
                    ),
                    Field(
                        "ecragg",
                        float,
                        50,
                        10,
                        kwargs.get("ecragg", 0.001)
                    ),
                    Field(
                        "aggsz",
                        float,
                        60,
                        10,
                        kwargs.get("aggsz")
                    ),
                    Field(
                        "unitl",
                        float,
                        70,
                        10,
                        kwargs.get("unitl", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ymreinf",
                        float,
                        0,
                        10,
                        kwargs.get("ymreinf")
                    ),
                    Field(
                        "prrinf",
                        float,
                        10,
                        10,
                        kwargs.get("prrinf")
                    ),
                    Field(
                        "sureinf",
                        float,
                        20,
                        10,
                        kwargs.get("sureinf")
                    ),
                    Field(
                        "typer",
                        float,
                        30,
                        10,
                        kwargs.get("typer", 1.0)
                    ),
                    Field(
                        "fracrx",
                        float,
                        40,
                        10,
                        kwargs.get("fracrx")
                    ),
                    Field(
                        "fracry",
                        float,
                        50,
                        10,
                        kwargs.get("fracry")
                    ),
                    Field(
                        "lcrsu",
                        int,
                        60,
                        10,
                        kwargs.get("lcrsu")
                    ),
                    Field(
                        "lcalps",
                        int,
                        70,
                        10,
                        kwargs.get("lcalps")
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
                        kwargs.get("aopt")
                    ),
                    Field(
                        "et36",
                        float,
                        10,
                        10,
                        kwargs.get("et36")
                    ),
                    Field(
                        "prt36 ",
                        float,
                        20,
                        10,
                        kwargs.get("prt36 ", 0.25)
                    ),
                    Field(
                        "ecut36",
                        float,
                        30,
                        10,
                        kwargs.get("ecut36")
                    ),
                    Field(
                        "lcalpc",
                        int,
                        40,
                        10,
                        kwargs.get("lcalpc")
                    ),
                    Field(
                        "degrad",
                        float,
                        50,
                        10,
                        kwargs.get("degrad")
                    ),
                    Field(
                        "ishchk",
                        int,
                        60,
                        10,
                        kwargs.get("ishchk", 0)
                    ),
                    Field(
                        "unlfac",
                        float,
                        70,
                        10,
                        kwargs.get("unlfac", 0.5)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp ",
                        float,
                        20,
                        10,
                        kwargs.get("zp ")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1", 1.E20)
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        50,
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
                        "v3 ",
                        float,
                        20,
                        10,
                        kwargs.get("v3 ")
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
                        "typesc",
                        float,
                        0,
                        10,
                        kwargs.get("typesc", 1)
                    ),
                    Field(
                        "p_or_f",
                        float,
                        10,
                        10,
                        kwargs.get("p_or_f")
                    ),
                    Field(
                        "effd ",
                        float,
                        20,
                        10,
                        kwargs.get("effd ")
                    ),
                    Field(
                        "gamsc",
                        float,
                        30,
                        10,
                        kwargs.get("gamsc")
                    ),
                    Field(
                        "erodet",
                        float,
                        40,
                        10,
                        kwargs.get("erodet", 2.0)
                    ),
                    Field(
                        "erodec",
                        float,
                        50,
                        10,
                        kwargs.get("erodec", 0.01)
                    ),
                    Field(
                        "eroder",
                        float,
                        60,
                        10,
                        kwargs.get("eroder", 0.05)
                    ),
                    Field(
                        "tmpoff",
                        float,
                        70,
                        10,
                        kwargs.get("tmpoff")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ec1_6",
                        float,
                        0,
                        10,
                        kwargs.get("ec1_6")
                    ),
                    Field(
                        "ecsp69",
                        float,
                        10,
                        10,
                        kwargs.get("ecsp69")
                    ),
                    Field(
                        "gamce9",
                        float,
                        20,
                        10,
                        kwargs.get("gamce9")
                    ),
                    Field(
                        "phief9",
                        float,
                        30,
                        10,
                        kwargs.get("phief9")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ft2",
                        float,
                        0,
                        10,
                        kwargs.get("ft2")
                    ),
                    Field(
                        "ftshr",
                        float,
                        10,
                        10,
                        kwargs.get("ftshr")
                    ),
                    Field(
                        "lcftt",
                        int,
                        20,
                        10,
                        kwargs.get("lcftt")
                    ),
                    Field(
                        "wro_g",
                        float,
                        30,
                        10,
                        kwargs.get("wro_g")
                    ),
                    Field(
                        "zsurf",
                        float,
                        40,
                        10,
                        kwargs.get("zsurf")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatConcreteEc2.option_specs[0],
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
    def fc(self) -> typing.Optional[float]:
        """Get or set the Compressive strength of concrete (stress units)
        """ # nopep8
        return self._cards[0].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        self._cards[0].set_value("fc", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Tensile stress to cause cracking
        """ # nopep8
        return self._cards[0].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        self._cards[0].set_value("ft", value)

    @property
    def typec(self) -> float:
        """Get or set the EQ.1.0: Siliceous (default), Draft EC2 Annex (fire engineering)
        EQ.2.0: Calcareous, Draft EC2 Annex (fire engineering)
        EQ.3.0: Non-thermally-sensitive using ET3, ECU3
        EQ.4.0: Lightweight
        EQ.5.0: Fiber-reinforced
        EQ.6.0: Non-thermally-sensitive, Mander algorithm
        EQ.7.0: Siliceous, EC2 1-2:2004 (fire engineering)
        EQ.8.0: Calcareous, EC2 1-2:2004 (fire engineering)
        EQ.9.0: EC2 1-1:2004 (general and buildings)
        """ # nopep8
        return self._cards[0].get_value("typec")

    @typec.setter
    def typec(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]:
            raise Exception("""typec must be one of {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0}""")
        self._cards[0].set_value("typec", value)

    @property
    def unitc(self) -> float:
        """Get or set the Factor to convert stress units to MPa (see Remarks)
        """ # nopep8
        return self._cards[0].get_value("unitc")

    @unitc.setter
    def unitc(self, value: float) -> None:
        self._cards[0].set_value("unitc", value)

    @property
    def ecuten(self) -> float:
        """Get or set the Strain to fully open a crack
        """ # nopep8
        return self._cards[0].get_value("ecuten")

    @ecuten.setter
    def ecuten(self, value: float) -> None:
        self._cards[0].set_value("ecuten", value)

    @property
    def fcc(self) -> typing.Optional[float]:
        """Get or set the Relevant only if TYPEC = 6 or 9.
        TYPEC.EQ.6:	FCC is the compressive strength of confined concrete used in Mander equations. Default: unconfined properties are assumed (FCC‌ = FC).
        TYPEC.EQ.9:	FCC is the actual compressive strength. If blank, this will be set equal to
        the mean compressive strength (fcm in EC2 1-1) as required for serviceability calculations (8MPa greater than FC).
        For ultimate load calculations the user may set FCC to a factored characteristic compressive strength. See remarks below
        """ # nopep8
        return self._cards[0].get_value("fcc")

    @fcc.setter
    def fcc(self, value: float) -> None:
        self._cards[0].set_value("fcc", value)

    @property
    def esoft(self) -> typing.Optional[float]:
        """Get or set the Tension stiffening (Slope of stress-strain curve post-cracking in tension)
        """ # nopep8
        return self._cards[1].get_value("esoft")

    @esoft.setter
    def esoft(self, value: float) -> None:
        self._cards[1].set_value("esoft", value)

    @property
    def lchar(self) -> typing.Optional[float]:
        """Get or set the Characteristic length at which ESOFT applies; also used as crack spacing in aggregate-interlock calculation
        """ # nopep8
        return self._cards[1].get_value("lchar")

    @lchar.setter
    def lchar(self, value: float) -> None:
        self._cards[1].set_value("lchar", value)

    @property
    def mu(self) -> float:
        """Get or set the Friction on crack planes (ignored if AGGSZ>0 - see notes)
        """ # nopep8
        return self._cards[1].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[1].set_value("mu", value)

    @property
    def taumxf(self) -> float:
        """Get or set the Maximum friction shear stress on crack planes (ignored if AGGSZ>0 - see notes)
        """ # nopep8
        return self._cards[1].get_value("taumxf")

    @taumxf.setter
    def taumxf(self, value: float) -> None:
        self._cards[1].set_value("taumxf", value)

    @property
    def taumxc(self) -> float:
        """Get or set the Maximum through-thickness shear stress after cracking (see notes)
        """ # nopep8
        return self._cards[1].get_value("taumxc")

    @taumxc.setter
    def taumxc(self, value: float) -> None:
        self._cards[1].set_value("taumxc", value)

    @property
    def ecragg(self) -> float:
        """Get or set the Strain parameter for aggregate interlock (ignored if AGGSZ>0 - see notes)
        """ # nopep8
        return self._cards[1].get_value("ecragg")

    @ecragg.setter
    def ecragg(self, value: float) -> None:
        self._cards[1].set_value("ecragg", value)

    @property
    def aggsz(self) -> typing.Optional[float]:
        """Get or set the Aggregate size (length units)  (used in NS3473 aggregate interlock formula - see notes)
        """ # nopep8
        return self._cards[1].get_value("aggsz")

    @aggsz.setter
    def aggsz(self, value: float) -> None:
        self._cards[1].set_value("aggsz", value)

    @property
    def unitl(self) -> float:
        """Get or set the Factor to convert length units to millimetres (used only if AGGSZ>0  - see notes)
        """ # nopep8
        return self._cards[1].get_value("unitl")

    @unitl.setter
    def unitl(self, value: float) -> None:
        self._cards[1].set_value("unitl", value)

    @property
    def ymreinf(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus of reinforcement
        """ # nopep8
        return self._cards[2].get_value("ymreinf")

    @ymreinf.setter
    def ymreinf(self, value: float) -> None:
        self._cards[2].set_value("ymreinf", value)

    @property
    def prrinf(self) -> typing.Optional[float]:
        """Get or set the Poisson's Ratio of reinforcement
        """ # nopep8
        return self._cards[2].get_value("prrinf")

    @prrinf.setter
    def prrinf(self, value: float) -> None:
        self._cards[2].set_value("prrinf", value)

    @property
    def sureinf(self) -> typing.Optional[float]:
        """Get or set the Ultimate stress of reinforcement
        """ # nopep8
        return self._cards[2].get_value("sureinf")

    @sureinf.setter
    def sureinf(self, value: float) -> None:
        self._cards[2].set_value("sureinf", value)

    @property
    def typer(self) -> float:
        """Get or set the Type of reinforcement for stress-strain-temperature relationships
        EQ.1.0: Hot rolled reinforcing steel
        EQ.2.0: Cold worked reinforcing steel (default)
        EQ.3.0: Quenched and tempered prestressing steel
        EQ.4.0: Cold worked prestressing steel
        5.0  Non-thermally-sensitive using loadcurve LCRSU
        """ # nopep8
        return self._cards[2].get_value("typer")

    @typer.setter
    def typer(self, value: float) -> None:
        if value not in [1.0, 2.0, 3.0, 4.0, 5.0]:
            raise Exception("""typer must be one of {1.0,2.0,3.0,4.0,5.0}""")
        self._cards[2].set_value("typer", value)

    @property
    def fracrx(self) -> typing.Optional[float]:
        """Get or set the Fraction of reinforcement (x-axis) (e.g. for 1% reinforcement FRACR=0.01)
        """ # nopep8
        return self._cards[2].get_value("fracrx")

    @fracrx.setter
    def fracrx(self, value: float) -> None:
        self._cards[2].set_value("fracrx", value)

    @property
    def fracry(self) -> typing.Optional[float]:
        """Get or set the Fraction of reinforcement (y-axis) (e.g. for 1% reinforcement FRACR=0.01)
        """ # nopep8
        return self._cards[2].get_value("fracry")

    @fracry.setter
    def fracry(self, value: float) -> None:
        self._cards[2].set_value("fracry", value)

    @property
    def lcrsu(self) -> typing.Optional[int]:
        """Get or set the Load curve for TYPER=5, giving non-dimensional factor on SUREINF versus plastic strain (overrides stress-strain relationships from EC2)
        """ # nopep8
        return self._cards[2].get_value("lcrsu")

    @lcrsu.setter
    def lcrsu(self, value: int) -> None:
        self._cards[2].set_value("lcrsu", value)

    @property
    def lcalps(self) -> typing.Optional[int]:
        """Get or set the Optional loadcurve giving thermal expansion coefficient of reinforcement vs temperature - overrides relationship from EC2
        """ # nopep8
        return self._cards[2].get_value("lcalps")

    @lcalps.setter
    def lcalps(self, value: int) -> None:
        self._cards[2].set_value("lcalps", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def et36(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus of concrete (TYPEC=3 and 6)
        """ # nopep8
        return self._cards[3].get_value("et36")

    @et36.setter
    def et36(self, value: float) -> None:
        self._cards[3].set_value("et36", value)

    @property
    def prt36_(self) -> float:
        """Get or set the Poisson's Ratio of concrete (TYPEC=3 and 6)
        """ # nopep8
        return self._cards[3].get_value("prt36 ")

    @prt36_.setter
    def prt36_(self, value: float) -> None:
        self._cards[3].set_value("prt36 ", value)

    @property
    def ecut36(self) -> typing.Optional[float]:
        """Get or set the Strain to failure of concrete in compression cu (TYPEC=3 and 6).See under “Compressive response…” in in section Material Behavior of Concretethe below. Default is 0.02 for TYPEC = 3 and 1.1×EC1_6  for TYPEC = 6..
        """ # nopep8
        return self._cards[3].get_value("ecut36")

    @ecut36.setter
    def ecut36(self, value: float) -> None:
        self._cards[3].set_value("ecut36", value)

    @property
    def lcalpc(self) -> typing.Optional[int]:
        """Get or set the Optional load curve giving thermal expansion coefficient of concrete vs temperature - overrides relationship from EC2
        """ # nopep8
        return self._cards[3].get_value("lcalpc")

    @lcalpc.setter
    def lcalpc(self, value: int) -> None:
        self._cards[3].set_value("lcalpc", value)

    @property
    def degrad(self) -> typing.Optional[float]:
        """Get or set the If non-zero, the compressive strength of concrete parallel to an open crack will be reduced (see notes)
        """ # nopep8
        return self._cards[3].get_value("degrad")

    @degrad.setter
    def degrad(self, value: float) -> None:
        self._cards[3].set_value("degrad", value)

    @property
    def ishchk(self) -> int:
        """Get or set the Flag = 1 to input data for shear capacity check
        """ # nopep8
        return self._cards[3].get_value("ishchk")

    @ishchk.setter
    def ishchk(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ishchk must be one of {0,1}""")
        self._cards[3].set_value("ishchk", value)

    @property
    def unlfac(self) -> float:
        """Get or set the Stiffness degradation factor after crushing (0.0 to 1.0 ¨C see notes)
        """ # nopep8
        return self._cards[3].get_value("unlfac")

    @unlfac.setter
    def unlfac(self, value: float) -> None:
        self._cards[3].set_value("unlfac", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4 (see Mat type 2)
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4 (see Mat type 2)
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[4].set_value("yp", value)

    @property
    def zp_(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4 (see Mat type 2)
        """ # nopep8
        return self._cards[4].get_value("zp ")

    @zp_.setter
    def zp_(self, value: float) -> None:
        self._cards[4].set_value("zp ", value)

    @property
    def a1(self) -> float:
        """Get or set the Components of vector a for AOPT = 2 (see Mat type 2)
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2 (see Mat type 2)
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2 (see Mat type 2)
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4 (see Mat type 2)
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4 (see Mat type 2)
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[5].set_value("v2", value)

    @property
    def v3_(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4 (see Mat type 2)
        """ # nopep8
        return self._cards[5].get_value("v3 ")

    @v3_.setter
    def v3_(self, value: float) -> None:
        self._cards[5].set_value("v3 ", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2 (see Mat type 2)
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2 (see Mat type 2)
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2 (see Mat type 2)
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[5].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[5].set_value("beta", value)

    @property
    def typesc(self) -> float:
        """Get or set the Type of shear capacity check
        EQ.1.0:	BS 8110, no failure even if capacity is exceeded
        EQ.2.0:	ACI 318 - 05M, no failure even if capacity is exceeded
        EQ.11 : BS 8110, failure occurs if capacity is exceeded
        EQ.12 : ACI 318 - 05M, failure occurs if capacity is exceeded
        """ # nopep8
        return self._cards[6].get_value("typesc")

    @typesc.setter
    def typesc(self, value: float) -> None:
        if value not in [1, 2, 11, 12]:
            raise Exception("""typesc must be one of {1,2,11,12}""")
        self._cards[6].set_value("typesc", value)

    @property
    def p_or_f(self) -> typing.Optional[float]:
        """Get or set the If BS8110 shear check, percent reinforcement - e.g. if 0.5%, input 0.5. If ACI shear check, ratio (cylinder strength/FC) - defaults to 1
        """ # nopep8
        return self._cards[6].get_value("p_or_f")

    @p_or_f.setter
    def p_or_f(self, value: float) -> None:
        self._cards[6].set_value("p_or_f", value)

    @property
    def effd_(self) -> typing.Optional[float]:
        """Get or set the Effective section depth (length units), used in shear capacity check. This is usually the section depth excluding the cover concrete
        """ # nopep8
        return self._cards[6].get_value("effd ")

    @effd_.setter
    def effd_(self, value: float) -> None:
        self._cards[6].set_value("effd ", value)

    @property
    def gamsc(self) -> typing.Optional[float]:
        """Get or set the Load factor used in BS8110 shear capacity check
        """ # nopep8
        return self._cards[6].get_value("gamsc")

    @gamsc.setter
    def gamsc(self, value: float) -> None:
        self._cards[6].set_value("gamsc", value)

    @property
    def erodet(self) -> float:
        """Get or set the Crack-opening strain at which element is deleted; see Remark 7
        """ # nopep8
        return self._cards[6].get_value("erodet")

    @erodet.setter
    def erodet(self, value: float) -> None:
        self._cards[6].set_value("erodet", value)

    @property
    def erodec(self) -> float:
        """Get or set the Compressive strain used in erosion criteria; see Remark 7.
        """ # nopep8
        return self._cards[6].get_value("erodec")

    @erodec.setter
    def erodec(self, value: float) -> None:
        self._cards[6].set_value("erodec", value)

    @property
    def eroder(self) -> float:
        """Get or set the Reinforcement plastic strain used in erosion criteria; see Remark 7.
        """ # nopep8
        return self._cards[6].get_value("eroder")

    @eroder.setter
    def eroder(self, value: float) -> None:
        self._cards[6].set_value("eroder", value)

    @property
    def tmpoff(self) -> typing.Optional[float]:
        """Get or set the Constant to be added to the model's temperature unit to convert into degrees Celsius, e.g., if the model's temperature unit is degrees Kelvin, set TMPOFF to -273.  Degrees Celsius temperatures are then used throughout the material model, e.g., for LCALPC as well as for the default thermally-sensitive properties.
        """ # nopep8
        return self._cards[6].get_value("tmpoff")

    @tmpoff.setter
    def tmpoff(self, value: float) -> None:
        self._cards[6].set_value("tmpoff", value)

    @property
    def ec1_6(self) -> typing.Optional[float]:
        """Get or set the Strain at maximum compressive stress for Type 6 concrete
        """ # nopep8
        return self._cards[7].get_value("ec1_6")

    @ec1_6.setter
    def ec1_6(self, value: float) -> None:
        self._cards[7].set_value("ec1_6", value)

    @property
    def ecsp69(self) -> typing.Optional[float]:
        """Get or set the Spalling strain in compression for Type 6 concrete
        """ # nopep8
        return self._cards[7].get_value("ecsp69")

    @ecsp69.setter
    def ecsp69(self, value: float) -> None:
        self._cards[7].set_value("ecsp69", value)

    @property
    def gamce9(self) -> typing.Optional[float]:
        """Get or set the Material factor that divides the Youngs Modulus (TYPEC = 9).
        """ # nopep8
        return self._cards[7].get_value("gamce9")

    @gamce9.setter
    def gamce9(self, value: float) -> None:
        self._cards[7].set_value("gamce9", value)

    @property
    def phief9(self) -> typing.Optional[float]:
        """Get or set the Effective creep ratio (TYPEC = 9).
        """ # nopep8
        return self._cards[7].get_value("phief9")

    @phief9.setter
    def phief9(self, value: float) -> None:
        self._cards[7].set_value("phief9", value)

    @property
    def ft2(self) -> typing.Optional[float]:
        """Get or set the Tensile strength used for calculating tensile response.
        """ # nopep8
        return self._cards[8].get_value("ft2")

    @ft2.setter
    def ft2(self, value: float) -> None:
        self._cards[8].set_value("ft2", value)

    @property
    def ftshr(self) -> typing.Optional[float]:
        """Get or set the Tensile strength used for calculating post-crack shear response
        """ # nopep8
        return self._cards[8].get_value("ftshr")

    @ftshr.setter
    def ftshr(self, value: float) -> None:
        self._cards[8].set_value("ftshr", value)

    @property
    def lcftt(self) -> typing.Optional[int]:
        """Get or set the Load curve defining factor on tensile strength versus time.
        """ # nopep8
        return self._cards[8].get_value("lcftt")

    @lcftt.setter
    def lcftt(self, value: int) -> None:
        self._cards[8].set_value("lcftt", value)

    @property
    def wro_g(self) -> typing.Optional[float]:
        """Get or set the Density times gravity for water pressure in cracks.
        """ # nopep8
        return self._cards[8].get_value("wro_g")

    @wro_g.setter
    def wro_g(self, value: float) -> None:
        self._cards[8].set_value("wro_g", value)

    @property
    def zsurf(self) -> typing.Optional[float]:
        """Get or set the -coordinate of water surface (for water pressure in cracks)
        """ # nopep8
        return self._cards[8].get_value("zsurf")

    @zsurf.setter
    def zsurf(self, value: float) -> None:
        self._cards[8].set_value("zsurf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[9].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[9].cards[0].set_value("title", value)

