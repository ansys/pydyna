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

class Mat058Solid(KeywordBase):
    """DYNA MAT_058_SOLID keyword"""

    keyword = "MAT"
    subkeyword = "058_SOLID"
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
                        "ea",
                        float,
                        20,
                        10,
                        kwargs.get("ea")
                    ),
                    Field(
                        "eb",
                        float,
                        30,
                        10,
                        kwargs.get("eb")
                    ),
                    Field(
                        "ec",
                        float,
                        40,
                        10,
                        kwargs.get("ec")
                    ),
                    Field(
                        "prba",
                        float,
                        50,
                        10,
                        kwargs.get("prba")
                    ),
                    Field(
                        "tau1",
                        float,
                        60,
                        10,
                        kwargs.get("tau1")
                    ),
                    Field(
                        "gamma1",
                        float,
                        70,
                        10,
                        kwargs.get("gamma1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gab",
                        float,
                        0,
                        10,
                        kwargs.get("gab")
                    ),
                    Field(
                        "gbc",
                        float,
                        10,
                        10,
                        kwargs.get("gbc")
                    ),
                    Field(
                        "gca",
                        float,
                        20,
                        10,
                        kwargs.get("gca")
                    ),
                    Field(
                        "slimt1",
                        float,
                        30,
                        10,
                        kwargs.get("slimt1")
                    ),
                    Field(
                        "slimc1",
                        float,
                        40,
                        10,
                        kwargs.get("slimc1")
                    ),
                    Field(
                        "slimt2",
                        float,
                        50,
                        10,
                        kwargs.get("slimt2")
                    ),
                    Field(
                        "slimc2",
                        float,
                        60,
                        10,
                        kwargs.get("slimc2")
                    ),
                    Field(
                        "slims",
                        float,
                        70,
                        10,
                        kwargs.get("slims")
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
                        "tsize",
                        float,
                        10,
                        10,
                        kwargs.get("tsize")
                    ),
                    Field(
                        "erods",
                        float,
                        20,
                        10,
                        kwargs.get("erods")
                    ),
                    Field(
                        "soft",
                        float,
                        30,
                        10,
                        kwargs.get("soft")
                    ),
                    Field(
                        "fs",
                        float,
                        40,
                        10,
                        kwargs.get("fs", 0.0)
                    ),
                    Field(
                        "epsf",
                        float,
                        50,
                        10,
                        kwargs.get("epsf")
                    ),
                    Field(
                        "epsr",
                        float,
                        60,
                        10,
                        kwargs.get("epsr")
                    ),
                    Field(
                        "tsmd",
                        float,
                        70,
                        10,
                        kwargs.get("tsmd", 0.9)
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
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1")
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
                    Field(
                        "prca",
                        float,
                        60,
                        10,
                        kwargs.get("prca")
                    ),
                    Field(
                        "prcb",
                        float,
                        70,
                        10,
                        kwargs.get("prcb")
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
                    Field(
                        "lcdfail",
                        int,
                        70,
                        10,
                        kwargs.get("lcdfail")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e11c",
                        float,
                        0,
                        10,
                        kwargs.get("e11c")
                    ),
                    Field(
                        "e11t",
                        float,
                        10,
                        10,
                        kwargs.get("e11t")
                    ),
                    Field(
                        "e22c",
                        float,
                        20,
                        10,
                        kwargs.get("e22c")
                    ),
                    Field(
                        "e22t",
                        float,
                        30,
                        10,
                        kwargs.get("e22t")
                    ),
                    Field(
                        "gms",
                        float,
                        40,
                        10,
                        kwargs.get("gms")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xc",
                        float,
                        0,
                        10,
                        kwargs.get("xc")
                    ),
                    Field(
                        "xt",
                        float,
                        10,
                        10,
                        kwargs.get("xt")
                    ),
                    Field(
                        "yc",
                        float,
                        20,
                        10,
                        kwargs.get("yc")
                    ),
                    Field(
                        "yt",
                        float,
                        30,
                        10,
                        kwargs.get("yt")
                    ),
                    Field(
                        "sc",
                        float,
                        40,
                        10,
                        kwargs.get("sc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "e33c",
                        float,
                        0,
                        10,
                        kwargs.get("e33c")
                    ),
                    Field(
                        "e33t",
                        float,
                        10,
                        10,
                        kwargs.get("e33t")
                    ),
                    Field(
                        "gm23",
                        float,
                        20,
                        10,
                        kwargs.get("gm23")
                    ),
                    Field(
                        "gm31",
                        float,
                        30,
                        10,
                        kwargs.get("gm31")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "zc",
                        float,
                        0,
                        10,
                        kwargs.get("zc")
                    ),
                    Field(
                        "zt",
                        float,
                        10,
                        10,
                        kwargs.get("zt")
                    ),
                    Field(
                        "sc23",
                        float,
                        20,
                        10,
                        kwargs.get("sc23")
                    ),
                    Field(
                        "sc31",
                        float,
                        30,
                        10,
                        kwargs.get("sc31")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "slimt3",
                        float,
                        0,
                        10,
                        kwargs.get("slimt3")
                    ),
                    Field(
                        "slimc3",
                        float,
                        10,
                        10,
                        kwargs.get("slimc3")
                    ),
                    Field(
                        "slims23",
                        float,
                        20,
                        10,
                        kwargs.get("slims23")
                    ),
                    Field(
                        "lsims31",
                        float,
                        30,
                        10,
                        kwargs.get("lsims31")
                    ),
                    Field(
                        "tau2",
                        float,
                        40,
                        10,
                        kwargs.get("tau2")
                    ),
                    Field(
                        "gamma2",
                        float,
                        50,
                        10,
                        kwargs.get("gamma2")
                    ),
                    Field(
                        "tau3",
                        float,
                        60,
                        10,
                        kwargs.get("tau3")
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
                        "lcxc",
                        int,
                        0,
                        10,
                        kwargs.get("lcxc")
                    ),
                    Field(
                        "lcxt",
                        int,
                        10,
                        10,
                        kwargs.get("lcxt")
                    ),
                    Field(
                        "lcyc",
                        int,
                        20,
                        10,
                        kwargs.get("lcyc")
                    ),
                    Field(
                        "lcyt",
                        int,
                        30,
                        10,
                        kwargs.get("lcyt")
                    ),
                    Field(
                        "lcsc",
                        int,
                        40,
                        10,
                        kwargs.get("lcsc")
                    ),
                    Field(
                        "lctau",
                        int,
                        50,
                        10,
                        kwargs.get("lctau")
                    ),
                    Field(
                        "lcgam",
                        int,
                        60,
                        10,
                        kwargs.get("lcgam")
                    ),
                    Field(
                        "dt",
                        float,
                        70,
                        10,
                        kwargs.get("dt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lce11c",
                        int,
                        0,
                        10,
                        kwargs.get("lce11c", 0)
                    ),
                    Field(
                        "lce11t",
                        int,
                        10,
                        10,
                        kwargs.get("lce11t", 0)
                    ),
                    Field(
                        "lce22c",
                        int,
                        20,
                        10,
                        kwargs.get("lce22c", 0)
                    ),
                    Field(
                        "lce22t",
                        int,
                        30,
                        10,
                        kwargs.get("lce22t", 0)
                    ),
                    Field(
                        "lcgms",
                        int,
                        40,
                        10,
                        kwargs.get("lcgms", 0)
                    ),
                    Field(
                        "lcefs",
                        int,
                        50,
                        10,
                        kwargs.get("lcefs", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lczc",
                        int,
                        0,
                        10,
                        kwargs.get("lczc")
                    ),
                    Field(
                        "lczt",
                        int,
                        10,
                        10,
                        kwargs.get("lczt")
                    ),
                    Field(
                        "lcsc23",
                        int,
                        20,
                        10,
                        kwargs.get("lcsc23")
                    ),
                    Field(
                        "lcsc31",
                        int,
                        30,
                        10,
                        kwargs.get("lcsc31")
                    ),
                    Field(
                        "lctau2",
                        int,
                        40,
                        10,
                        kwargs.get("lctau2")
                    ),
                    Field(
                        "lcgam2",
                        int,
                        50,
                        10,
                        kwargs.get("lcgam2")
                    ),
                    Field(
                        "lctau3",
                        int,
                        60,
                        10,
                        kwargs.get("lctau3")
                    ),
                    Field(
                        "lcgam3",
                        int,
                        70,
                        10,
                        kwargs.get("lcgam3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lce33c",
                        int,
                        0,
                        10,
                        kwargs.get("lce33c")
                    ),
                    Field(
                        "lce33t",
                        int,
                        10,
                        10,
                        kwargs.get("lce33t")
                    ),
                    Field(
                        "lcgms23",
                        int,
                        20,
                        10,
                        kwargs.get("lcgms23")
                    ),
                    Field(
                        "lcgms31",
                        int,
                        30,
                        10,
                        kwargs.get("lcgms31")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat058Solid.option_specs[0],
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
    def ea(self) -> typing.Optional[float]:
        """Get or set the GT.0.0:	E_a, Young’s modulus - longitudinal direction
        LT.0.0:	Load Curve ID or Table ID = (-EA).See Remark 8.
        Load Curve.When - EA is equal to a load curve ID, it is taken as defining the uniaxial elastic stress as a function of strain behavior in the longitudinal direction.Negative data
        points correspond to compression and positive values to tension.
        Tabular Data.When - EA is equal to a table ID, it defines for each strain rate value a load curve ID giving the uniaxial elastic stress as a function of strain behavior in the longitudinal direction.
        Logarithmically Defined Tables.If the first uniaxial elastic stress as a function of strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all stress - strain curves.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the GT.0.0:	E_b, Young’s modulus - transverse direction
        LT.0.0:	Load Curve ID or Table ID = (-EB).See Remark 8.
        Load Curve.When - EB is equal to a load curve ID, it is taken as defining the uniaxial elastic stress as a function of strain behavior in the transverse direction.Negative data points correspond to compression and positive values to tension.
        Tabular Data.When - EB is equal to a table ID, it defines for each strain rate value a load curve ID giving the uniaxial elastic stress as a function of strain behavior in the transverse direction.
        Logarithmically Defined Tables.If the first uniaxial elastic stress as a function of strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all stress - strain curves.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the E_c, Young’s modulus - normal direction (used only by thick shells and solids).  See Remark 6.
        GT.0.0:	E_c, Young’s modulus - normal direction
        LT.0.0 : Load Curve ID or Table ID = (-EC) (solids only).See Remark 8.
        Load Curve.When - EC is equal to a load curve ID, it is taken as defining the uniaxial elastic stress as a function of strain behavior in the transverse direction.Negative data points correspond to compression and positive values to tension.
        Tabular Data.When - EC is equal to a table ID, it defines for each strain rate value a load curve ID giving the uniaxial elastic stress as a function of strain behavior in the transverse direction.
        Logarithmically Defined Tables.If the first uniaxial elastic stress as a function of strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all stress - strain curves.
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ba.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        self._cards[0].set_value("prba", value)

    @property
    def tau1(self) -> typing.Optional[float]:
        """Get or set the tau-1, stress limit of the first slightly nonlinear part of the of the shear stress versus shear strain curve. The values tau-1 and gamma-1 are used to define a curve of shear stress versus shear strain. These values are input if FS, defined below, is set to a value of -1.
        """ # nopep8
        return self._cards[0].get_value("tau1")

    @tau1.setter
    def tau1(self, value: float) -> None:
        self._cards[0].set_value("tau1", value)

    @property
    def gamma1(self) -> typing.Optional[float]:
        """Get or set the gamma-1, strain limit of the first slightly nonlinear part of the of the shear stress versus shear strain curve.
        """ # nopep8
        return self._cards[0].get_value("gamma1")

    @gamma1.setter
    def gamma1(self, value: float) -> None:
        self._cards[0].set_value("gamma1", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the GT.0.0:	G_ab, shear modulus in the ab-direction
        LT.0.0:	Load Curve ID or Table ID = (-GAB)
        Load Curve.When - GAB is equal to a load curve ID, it is taken as defining the elastic shear stress as a function of she strain behavior in the ab - direction.
        Tabular Data.When - GAB is equal to a table ID, it defines for each strain rate value a load curve ID giving the elastic shear stress as a function of shear strain behavior in the ab - direction.
        Logarithmically Defined Tables.If the first elastic shear stress as a function of shear strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all shear stress - shear strain curves.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the GT.0.0:	G_bc, shear modulus in the cb-direction
        LT.0.0:	Load Curve ID or Table ID = (-GBC) (solids only)
        Load Curve.When - GBC is equal to a load curve ID, it is taken as defining the elastic shear stress as a function of shear strain behavior in the bc - direction.
        Tabular Data.When - GBC is equal to a table ID, it defines for each strain rate value a load curve ID giving the elastic shear stress as a function of shear strain behavior in the bc - direction.
        Logarithmically Defined Tables.If the first elastic shear stress as a function of shear strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all shear stress - shear strain curves.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the GT.0.0:	G_ca, shear modulus in the ca-direction
        LT.0.0:	Load Curve ID or Table ID = (-GCA) (solids only)
        Load Curve.When - GCA is equal to a load curve ID, it is taken as defining the elastic shear stress as a function of shear strain behavior in the ca - direction.
        Tabular Data.When - GCA is equal to a table ID, it defines for each strain rate value a load curve ID giving the elastic shear stress as a function of shear strain behavior in the ca - direction.
        Logarithmically Defined Tables.If the first elastic shear stress as a function of shear strain curve in the table corresponds to a negative strain rate, LS - DYNA assumes that the natural logarithm of the strain rate value is used for all shear stress - shear strain curves.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        self._cards[1].set_value("gca", value)

    @property
    def slimt1(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (fiber tension).
        """ # nopep8
        return self._cards[1].get_value("slimt1")

    @slimt1.setter
    def slimt1(self, value: float) -> None:
        self._cards[1].set_value("slimt1", value)

    @property
    def slimc1(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (fiber compression).
        """ # nopep8
        return self._cards[1].get_value("slimc1")

    @slimc1.setter
    def slimc1(self, value: float) -> None:
        self._cards[1].set_value("slimc1", value)

    @property
    def slimt2(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (matrix tension).
        """ # nopep8
        return self._cards[1].get_value("slimt2")

    @slimt2.setter
    def slimt2(self, value: float) -> None:
        self._cards[1].set_value("slimt2", value)

    @property
    def slimc2(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (matrix compression).
        """ # nopep8
        return self._cards[1].get_value("slimc2")

    @slimc2.setter
    def slimc2(self, value: float) -> None:
        self._cards[1].set_value("slimc2", value)

    @property
    def slims(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (shear).
        """ # nopep8
        return self._cards[1].get_value("slims")

    @slims.setter
    def slims(self, value: float) -> None:
        self._cards[1].set_value("slims", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

    @property
    def tsize(self) -> typing.Optional[float]:
        """Get or set the Time step for automatic element deletion.
        """ # nopep8
        return self._cards[2].get_value("tsize")

    @tsize.setter
    def tsize(self, value: float) -> None:
        self._cards[2].set_value("tsize", value)

    @property
    def erods(self) -> typing.Optional[float]:
        """Get or set the Maximum effective strain for element layer failure. A value of unity would equal 100% strain.
        """ # nopep8
        return self._cards[2].get_value("erods")

    @erods.setter
    def erods(self, value: float) -> None:
        self._cards[2].set_value("erods", value)

    @property
    def soft(self) -> typing.Optional[float]:
        """Get or set the Softening reduction factor for strength in the crashfront.
        """ # nopep8
        return self._cards[2].get_value("soft")

    @soft.setter
    def soft(self, value: float) -> None:
        self._cards[2].set_value("soft", value)

    @property
    def fs(self) -> float:
        """Get or set the Failure surface type:
        EQ.1.0:smooth failure surface with a quadratic criterion for both the fiber (a) and transverse (b) directions. This option can be used with complete laminates and fabrics,
        EQ.0.0:smooth failure surface in the transverse (b) direction with a limiting value in the fiber (a) direction. This model is appropiate for unidirectional (UD) layered composites only (default),
        EQ.-1.:faceted failure surface. When the strength values are reached then damage evolves in tension and compression for both the fiber and transverse direction. Shear behavior is also considered. This option can be used with complete laminates and fabrics.
        """ # nopep8
        return self._cards[2].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        if value not in [0.0, -1.0, 1.0]:
            raise Exception("""fs must be one of {0.0,-1.0,1.0}""")
        self._cards[2].set_value("fs", value)

    @property
    def epsf(self) -> typing.Optional[float]:
        """Get or set the Damage initiation transverse shear strain
        """ # nopep8
        return self._cards[2].get_value("epsf")

    @epsf.setter
    def epsf(self, value: float) -> None:
        self._cards[2].set_value("epsf", value)

    @property
    def epsr(self) -> typing.Optional[float]:
        """Get or set the Final rupture transverse shear strain
        """ # nopep8
        return self._cards[2].get_value("epsr")

    @epsr.setter
    def epsr(self, value: float) -> None:
        self._cards[2].set_value("epsr", value)

    @property
    def tsmd(self) -> float:
        """Get or set the Transverse shear maximum damage, default = 0.90.
        """ # nopep8
        return self._cards[2].get_value("tsmd")

    @tsmd.setter
    def tsmd(self, value: float) -> None:
        self._cards[2].set_value("tsmd", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[3].set_value("a3", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ca
        """ # nopep8
        return self._cards[3].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        self._cards[3].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio cb
        """ # nopep8
        return self._cards[3].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        self._cards[3].set_value("prcb", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[4].set_value("beta", value)

    @property
    def lcdfail(self) -> typing.Optional[int]:
        """Get or set the Load-Curve ID, that defines orientation dependent failure strains.
        The ordinate values in the load-curve define the various failure strains, in the following order:
        1. EF_11T: tensile failure strain in longitudinal a-direction
        2. EF_11C: compressive failure strain in longitudinal a-direction
        3. EF_22T: tensile failure strain in transverse b-direction
        4. EF_22C: compressive failure strain in transverse b-direction
        5. EF_12: in-plane shear failure strain in ab-plane
        6. EF_33T: tensile failure strain in transverse c-direction
        7. EF_33C: compressive failure strain in transverse c-direction
        8. EF_23: out-of plane shear failure strain in bc-plane
        9. EF_31: out-of plane shear failure strain in ca-plane
        Thus, the load-curve to define these values has to have either 5 (shells) or 9 (solids) entries in its definition.
        A load-curve definition with 9 entries may be used for shells, ignoring the last 4 entries. The abscissa values are ignored.
        """ # nopep8
        return self._cards[4].get_value("lcdfail")

    @lcdfail.setter
    def lcdfail(self, value: int) -> None:
        self._cards[4].set_value("lcdfail", value)

    @property
    def e11c(self) -> typing.Optional[float]:
        """Get or set the Strain at longitudinal compressive strength, a-axis.
        """ # nopep8
        return self._cards[5].get_value("e11c")

    @e11c.setter
    def e11c(self, value: float) -> None:
        self._cards[5].set_value("e11c", value)

    @property
    def e11t(self) -> typing.Optional[float]:
        """Get or set the Strain at longitudinal tensile strength, a-axis.
        """ # nopep8
        return self._cards[5].get_value("e11t")

    @e11t.setter
    def e11t(self, value: float) -> None:
        self._cards[5].set_value("e11t", value)

    @property
    def e22c(self) -> typing.Optional[float]:
        """Get or set the Strain at transverse compressive strength, b-axis.
        """ # nopep8
        return self._cards[5].get_value("e22c")

    @e22c.setter
    def e22c(self, value: float) -> None:
        self._cards[5].set_value("e22c", value)

    @property
    def e22t(self) -> typing.Optional[float]:
        """Get or set the Strain at transverse tensile strength, b-axis.
        """ # nopep8
        return self._cards[5].get_value("e22t")

    @e22t.setter
    def e22t(self, value: float) -> None:
        self._cards[5].set_value("e22t", value)

    @property
    def gms(self) -> typing.Optional[float]:
        """Get or set the Strain at shear strength, ab plane.
        """ # nopep8
        return self._cards[5].get_value("gms")

    @gms.setter
    def gms(self, value: float) -> None:
        self._cards[5].set_value("gms", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the Longitudinal compressive strength.
        """ # nopep8
        return self._cards[6].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[6].set_value("xc", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Longitudinal tensile strength.
        """ # nopep8
        return self._cards[6].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        self._cards[6].set_value("xt", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the Transverse compressive strength, b-axis.
        """ # nopep8
        return self._cards[6].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[6].set_value("yc", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the Transverse tensile strength, b-axis.
        """ # nopep8
        return self._cards[6].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        self._cards[6].set_value("yt", value)

    @property
    def sc(self) -> typing.Optional[float]:
        """Get or set the Shear strength, ab plane.
        """ # nopep8
        return self._cards[6].get_value("sc")

    @sc.setter
    def sc(self, value: float) -> None:
        self._cards[6].set_value("sc", value)

    @property
    def e33c(self) -> typing.Optional[float]:
        """Get or set the Strain at transverse compressive strength, c-axis.
        """ # nopep8
        return self._cards[7].get_value("e33c")

    @e33c.setter
    def e33c(self, value: float) -> None:
        self._cards[7].set_value("e33c", value)

    @property
    def e33t(self) -> typing.Optional[float]:
        """Get or set the Strain at transverse tensile strength, c-axis.
        """ # nopep8
        return self._cards[7].get_value("e33t")

    @e33t.setter
    def e33t(self, value: float) -> None:
        self._cards[7].set_value("e33t", value)

    @property
    def gm23(self) -> typing.Optional[float]:
        """Get or set the Engineering shear strain at shear strength, bc-plane.
        """ # nopep8
        return self._cards[7].get_value("gm23")

    @gm23.setter
    def gm23(self, value: float) -> None:
        self._cards[7].set_value("gm23", value)

    @property
    def gm31(self) -> typing.Optional[float]:
        """Get or set the Engineering shear strain at shear strength, ca-plane.
        """ # nopep8
        return self._cards[7].get_value("gm31")

    @gm31.setter
    def gm31(self, value: float) -> None:
        self._cards[7].set_value("gm31", value)

    @property
    def zc(self) -> typing.Optional[float]:
        """Get or set the Transverse compressive strength, c-axis (positive value).
        """ # nopep8
        return self._cards[8].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[8].set_value("zc", value)

    @property
    def zt(self) -> typing.Optional[float]:
        """Get or set the Transverse tensile strength, c-axis.
        """ # nopep8
        return self._cards[8].get_value("zt")

    @zt.setter
    def zt(self, value: float) -> None:
        self._cards[8].set_value("zt", value)

    @property
    def sc23(self) -> typing.Optional[float]:
        """Get or set the Shear strength, bc-plane.
        """ # nopep8
        return self._cards[8].get_value("sc23")

    @sc23.setter
    def sc23(self, value: float) -> None:
        self._cards[8].set_value("sc23", value)

    @property
    def sc31(self) -> typing.Optional[float]:
        """Get or set the Shear strength, ca-plane.
        """ # nopep8
        return self._cards[8].get_value("sc31")

    @sc31.setter
    def sc31(self, value: float) -> None:
        self._cards[8].set_value("sc31", value)

    @property
    def slimt3(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (matrix tension, c-axis).
        """ # nopep8
        return self._cards[9].get_value("slimt3")

    @slimt3.setter
    def slimt3(self, value: float) -> None:
        self._cards[9].set_value("slimt3", value)

    @property
    def slimc3(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (matrix compression, c-axis).
        """ # nopep8
        return self._cards[9].get_value("slimc3")

    @slimc3.setter
    def slimc3(self, value: float) -> None:
        self._cards[9].set_value("slimc3", value)

    @property
    def slims23(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (shear, bc-plane
        """ # nopep8
        return self._cards[9].get_value("slims23")

    @slims23.setter
    def slims23(self, value: float) -> None:
        self._cards[9].set_value("slims23", value)

    @property
    def lsims31(self) -> typing.Optional[float]:
        """Get or set the Factor to determine the minimum stress limit after stress maximum (shear, ca-plane).
        """ # nopep8
        return self._cards[9].get_value("lsims31")

    @lsims31.setter
    def lsims31(self, value: float) -> None:
        self._cards[9].set_value("lsims31", value)

    @property
    def tau2(self) -> typing.Optional[float]:
        """Get or set the τ_2, stress limit of the first slightly nonlinear part of the shear stress as a function of shear strain curve.  The values τ_2 and γ_2 are used to define a curve of shear stress as a function of shear strain.  These values are input if FS, defined in Card 3, is set to a value of -1 (bc-plane).
        """ # nopep8
        return self._cards[9].get_value("tau2")

    @tau2.setter
    def tau2(self, value: float) -> None:
        self._cards[9].set_value("tau2", value)

    @property
    def gamma2(self) -> typing.Optional[float]:
        """Get or set the γ_2, strain limit of the first slightly nonlinear part of the shear stress as a function of engineering shear strain curve (bc-plane).
        """ # nopep8
        return self._cards[9].get_value("gamma2")

    @gamma2.setter
    def gamma2(self, value: float) -> None:
        self._cards[9].set_value("gamma2", value)

    @property
    def tau3(self) -> typing.Optional[float]:
        """Get or set the τ_3, stress limit of the first slightly nonlinear part of the shear stress as a function of shear strain curve.  The values τ_3 and γ_3 are used to define a curve of shear stress as a function of shear strain.  These values are input if FS, defined in Card 3, is set to a value of -1 (ca-plane).
        """ # nopep8
        return self._cards[9].get_value("tau3")

    @tau3.setter
    def tau3(self, value: float) -> None:
        self._cards[9].set_value("tau3", value)

    @property
    def gamma3(self) -> typing.Optional[float]:
        """Get or set the γ_3, strain limit of the first slightly nonlinear part of the shear stress as a function of engineering shear strain curve (bc-plane).
        """ # nopep8
        return self._cards[9].get_value("gamma3")

    @gamma3.setter
    def gamma3(self, value: float) -> None:
        self._cards[9].set_value("gamma3", value)

    @property
    def lcxc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining longitudinal compressive strength XC vs. strain rate (XC is ignored with that option). If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[10].get_value("lcxc")

    @lcxc.setter
    def lcxc(self, value: int) -> None:
        self._cards[10].set_value("lcxc", value)

    @property
    def lcxt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining longitudinal tensile strength XT vs. strain rate (XT is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[10].get_value("lcxt")

    @lcxt.setter
    def lcxt(self, value: int) -> None:
        self._cards[10].set_value("lcxt", value)

    @property
    def lcyc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining transverse compressive strength YC vs. strain rate (YC is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[10].get_value("lcyc")

    @lcyc.setter
    def lcyc(self, value: int) -> None:
        self._cards[10].set_value("lcyc", value)

    @property
    def lcyt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining transverse tensile strength YT vs. strain rate (YT is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[10].get_value("lcyt")

    @lcyt.setter
    def lcyt(self, value: int) -> None:
        self._cards[10].set_value("lcyt", value)

    @property
    def lcsc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining shear strength SC vs. strain rate (SC is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[10].get_value("lcsc")

    @lcsc.setter
    def lcsc(self, value: int) -> None:
        self._cards[10].set_value("lcsc", value)

    @property
    def lctau(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining TAU1 vs. strain rate (TAU1 is ignored with that option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[10].get_value("lctau")

    @lctau.setter
    def lctau(self, value: int) -> None:
        self._cards[10].set_value("lctau", value)

    @property
    def lcgam(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining GAMMA1 vs. strain rate (GAMMA1 is ignored with that option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[10].get_value("lcgam")

    @lcgam.setter
    def lcgam(self, value: int) -> None:
        self._cards[10].set_value("lcgam", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Strain rate averaging option.
        EQ.0.0: Strain rate is evaluated using a running average.
        LT.0.0: Strain rate is evaluated using average of last 11 time steps.
        GT.0.0: Strain rate is averaged over the last DT time units.
        """ # nopep8
        return self._cards[10].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[10].set_value("dt", value)

    @property
    def lce11c(self) -> int:
        """Get or set the Load curve ID defining E11C vs. strain rate (E11C is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[11].get_value("lce11c")

    @lce11c.setter
    def lce11c(self, value: int) -> None:
        self._cards[11].set_value("lce11c", value)

    @property
    def lce11t(self) -> int:
        """Get or set the Load curve ID defining E11T vs. strain rate (E11T is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[11].get_value("lce11t")

    @lce11t.setter
    def lce11t(self, value: int) -> None:
        self._cards[11].set_value("lce11t", value)

    @property
    def lce22c(self) -> int:
        """Get or set the Load curve ID defining E22C vs. strain rate (E22C is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[11].get_value("lce22c")

    @lce22c.setter
    def lce22c(self, value: int) -> None:
        self._cards[11].set_value("lce22c", value)

    @property
    def lce22t(self) -> int:
        """Get or set the Load curve ID defining E22T vs. strain rate (E22T is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[11].get_value("lce22t")

    @lce22t.setter
    def lce22t(self, value: int) -> None:
        self._cards[11].set_value("lce22t", value)

    @property
    def lcgms(self) -> int:
        """Get or set the Load curve ID defining GMS vs. strain rate (GMS is ignored with that option) If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[11].get_value("lcgms")

    @lcgms.setter
    def lcgms(self, value: int) -> None:
        self._cards[11].set_value("lcgms", value)

    @property
    def lcefs(self) -> int:
        """Get or set the Load curve ID defining ERODS as a function of strain rate (ERODS is ignored with this option). The full strain tensor is used to compute the equivalent strain (new option). If the first strain rate value in the curve is negative, it is assumed that all strain rate values are given as natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[11].get_value("lcefs")

    @lcefs.setter
    def lcefs(self, value: int) -> None:
        self._cards[11].set_value("lcefs", value)

    @property
    def lczc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining transverse tensile strength ZT as a function of strain rate (ZT is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[12].get_value("lczc")

    @lczc.setter
    def lczc(self, value: int) -> None:
        self._cards[12].set_value("lczc", value)

    @property
    def lczt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining shear strength SC23 as a function of strain rate (SC23 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[12].get_value("lczt")

    @lczt.setter
    def lczt(self, value: int) -> None:
        self._cards[12].set_value("lczt", value)

    @property
    def lcsc23(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining shear strength SC31 as a function of strain rate (SC31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[12].get_value("lcsc23")

    @lcsc23.setter
    def lcsc23(self, value: int) -> None:
        self._cards[12].set_value("lcsc23", value)

    @property
    def lcsc31(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining shear strength SC31 as a function of strain rate (SC31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[12].get_value("lcsc31")

    @lcsc31.setter
    def lcsc31(self, value: int) -> None:
        self._cards[12].set_value("lcsc31", value)

    @property
    def lctau2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining TAU2 as a function of strain rate (TAU2 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
        """ # nopep8
        return self._cards[12].get_value("lctau2")

    @lctau2.setter
    def lctau2(self, value: int) -> None:
        self._cards[12].set_value("lctau2", value)

    @property
    def lcgam2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining GAMMA2 as a function of strain rate (GAMMA2 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
        """ # nopep8
        return self._cards[12].get_value("lcgam2")

    @lcgam2.setter
    def lcgam2(self, value: int) -> None:
        self._cards[12].set_value("lcgam2", value)

    @property
    def lctau3(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining TAU3 as a function of strain rate (TAU3 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
        """ # nopep8
        return self._cards[12].get_value("lctau3")

    @lctau3.setter
    def lctau3(self, value: int) -> None:
        self._cards[12].set_value("lctau3", value)

    @property
    def lcgam3(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining GAMMA3 as a function of strain rate (GAMMA3 is ignored with this option). This value is only used for FS = -1. If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
        """ # nopep8
        return self._cards[12].get_value("lcgam3")

    @lcgam3.setter
    def lcgam3(self, value: int) -> None:
        self._cards[12].set_value("lcgam3", value)

    @property
    def lce33c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining E33C as a function of strain rate (E33C is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[13].get_value("lce33c")

    @lce33c.setter
    def lce33c(self, value: int) -> None:
        self._cards[13].set_value("lce33c", value)

    @property
    def lce33t(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining E33T as a function of strain rate (E33T is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
        """ # nopep8
        return self._cards[13].get_value("lce33t")

    @lce33t.setter
    def lce33t(self, value: int) -> None:
        self._cards[13].set_value("lce33t", value)

    @property
    def lcgms23(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining GMS23 as a function of strain rate (GMS23 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate.
        """ # nopep8
        return self._cards[13].get_value("lcgms23")

    @lcgms23.setter
    def lcgms23(self, value: int) -> None:
        self._cards[13].set_value("lcgms23", value)

    @property
    def lcgms31(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining GMS31 as a function of strain rate (GMS31 is ignored with this option). If the first strain rate value in the curve is negative, all the strain rate values are assumed to be given as the natural logarithm of the strain rate
        """ # nopep8
        return self._cards[13].get_value("lcgms31")

    @lcgms31.setter
    def lcgms31(self, value: int) -> None:
        self._cards[13].set_value("lcgms31", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[14].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[14].cards[0].set_value("title", value)


class MatLaminatedCompositeFabricSolid(Mat058Solid):
    subkeyword = "LAMINATED_COMPOSITE_FABRIC_SOLID"
