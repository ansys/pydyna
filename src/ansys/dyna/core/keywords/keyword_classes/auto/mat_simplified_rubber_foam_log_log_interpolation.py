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
from ansys.dyna.core.lib.duplicate_card import DuplicateCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatSimplifiedRubberFoamLogLogInterpolation(KeywordBase):
    """DYNA MAT_SIMPLIFIED_RUBBER/FOAM_LOG_LOG_INTERPOLATION keyword"""

    keyword = "MAT"
    subkeyword = "SIMPLIFIED_RUBBER/FOAM_LOG_LOG_INTERPOLATION"
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
                        "km",
                        float,
                        20,
                        10,
                        kwargs.get("km")
                    ),
                    Field(
                        "mu",
                        float,
                        30,
                        10,
                        kwargs.get("mu", 0.10)
                    ),
                    Field(
                        "g",
                        float,
                        40,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "sigf",
                        float,
                        50,
                        10,
                        kwargs.get("sigf")
                    ),
                    Field(
                        "ref",
                        float,
                        60,
                        10,
                        kwargs.get("ref", 0.0)
                    ),
                    Field(
                        "prten",
                        float,
                        70,
                        10,
                        kwargs.get("prten")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sgl",
                        float,
                        0,
                        10,
                        kwargs.get("sgl")
                    ),
                    Field(
                        "sw",
                        float,
                        10,
                        10,
                        kwargs.get("sw")
                    ),
                    Field(
                        "st",
                        float,
                        20,
                        10,
                        kwargs.get("st")
                    ),
                    Field(
                        "lc/tbid",
                        int,
                        30,
                        10,
                        kwargs.get("lc/tbid")
                    ),
                    Field(
                        "tension",
                        float,
                        40,
                        10,
                        kwargs.get("tension", -1.0)
                    ),
                    Field(
                        "rtype",
                        float,
                        50,
                        10,
                        kwargs.get("rtype", 0.0)
                    ),
                    Field(
                        "avgopt",
                        float,
                        60,
                        10,
                        kwargs.get("avgopt")
                    ),
                    Field(
                        "pra",
                        float,
                        70,
                        10,
                        kwargs.get("pra")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcunld",
                        int,
                        0,
                        10,
                        kwargs.get("lcunld")
                    ),
                    Field(
                        "hu",
                        float,
                        10,
                        10,
                        kwargs.get("hu", 1.0)
                    ),
                    Field(
                        "shape",
                        float,
                        20,
                        10,
                        kwargs.get("shape")
                    ),
                    Field(
                        "stol",
                        float,
                        30,
                        10,
                        kwargs.get("stol")
                    ),
                    Field(
                        "visco",
                        float,
                        40,
                        10,
                        kwargs.get("visco", 0.0)
                    ),
                    Field(
                        "hisout",
                        float,
                        50,
                        10,
                        kwargs.get("hisout", 0.0)
                    ),
                ],
            ),
            DuplicateCard(
                [
                    Field("gi", float, 0, 10),
                    Field("betai", float, 10, 10),
                    Field("vflag", int, 20, 10),
                ],
                None,
                data = kwargs.get("constants")),
            OptionCardSet(
                option_spec = MatSimplifiedRubberFoamLogLogInterpolation.option_specs[0],
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
    def km(self) -> typing.Optional[float]:
        """Get or set the Linear bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("km")

    @km.setter
    def km(self, value: float) -> None:
        self._cards[0].set_value("km", value)

    @property
    def mu(self) -> float:
        """Get or set the Damping coefficient (0.05 < recommended value < 0.50; default is 0.10).
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[0].set_value("mu", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based of a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def sigf(self) -> typing.Optional[float]:
        """Get or set the Limit stress for frequency independent, frictional, damping.
        """ # nopep8
        return self._cards[0].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        self._cards[0].set_value("sigf", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor.  The reference geometry is defined by the keyword:*INITIAL_FOAM_REFERENCE_ GEOMETRY (see there for more details).
        EQ.0.0:  off,
        EQ.1.0:  on.
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[0].set_value("ref", value)

    @property
    def prten(self) -> typing.Optional[float]:
        """Get or set the The tensile Poisson's ratio for shells (optional).  If PRTEN is zero, PR/BETA will serve as the Poisson's ratio for both tension and compression in shells.  If PRTEN is nonzero, PR/BETA will serve only as the compressive Poisson's ratio for shells.
        """ # nopep8
        return self._cards[0].get_value("prten")

    @prten.setter
    def prten(self, value: float) -> None:
        self._cards[0].set_value("prten", value)

    @property
    def sgl(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length.
        """ # nopep8
        return self._cards[1].get_value("sgl")

    @sgl.setter
    def sgl(self, value: float) -> None:
        self._cards[1].set_value("sgl", value)

    @property
    def sw(self) -> typing.Optional[float]:
        """Get or set the Specimen width.
        """ # nopep8
        return self._cards[1].get_value("sw")

    @sw.setter
    def sw(self, value: float) -> None:
        self._cards[1].set_value("sw", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness.
        """ # nopep8
        return self._cards[1].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        self._cards[1].set_value("st", value)

    @property
    def lc_tbid(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID, see *DEFINE_TABLE, defining the force versus actual change in the gauge length. If the table definition is used a family of curves are defined for discrete strain rates. The load curves should cover the complete range of expected loading, i.e., the smallest stretch ratio to the largest.
        """ # nopep8
        return self._cards[1].get_value("lc/tbid")

    @lc_tbid.setter
    def lc_tbid(self, value: int) -> None:
        self._cards[1].set_value("lc/tbid", value)

    @property
    def tension(self) -> float:
        """Get or set the Parameter that controls how the rate effects are treated. Applicable to the table definition.
        EQ.-1.-: rate effects are considered for loading either in tension or compression, but not for unloading,
        EQ.0.0: rate effects are considered for compressive loading only,
        EQ.1.0: rate effects are treated identically in tension and compression.
        """ # nopep8
        return self._cards[1].get_value("tension")

    @tension.setter
    def tension(self, value: float) -> None:
        if value not in [-1.0, 0.0, 1.0]:
            raise Exception("""tension must be one of {-1.0,0.0,1.0}""")
        self._cards[1].set_value("tension", value)

    @property
    def rtype(self) -> float:
        """Get or set the Strain rate type if a table is defined:
        EQ.0.0: true strain rate,
        EQ.1.0: engineering strain rate.
        """ # nopep8
        return self._cards[1].get_value("rtype")

    @rtype.setter
    def rtype(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""rtype must be one of {0.0,1.0}""")
        self._cards[1].set_value("rtype", value)

    @property
    def avgopt(self) -> typing.Optional[float]:
        """Get or set the Averaging option determine strain rate to reduce numerical noise.
        LT.0.0:	|AVGOPT| is a time window/interval over which the strain rates are averaged.
        EQ.0.0:	simple average of twelve time steps,
        EQ.1.0:	running average of last 12 averages.
        """ # nopep8
        return self._cards[1].get_value("avgopt")

    @avgopt.setter
    def avgopt(self, value: float) -> None:
        self._cards[1].set_value("avgopt", value)

    @property
    def pra(self) -> typing.Optional[float]:
        """Get or set the Poisson ratio or viscosity coefficient, If the value is specified between 0 and 0.5 exclusive, i.e.,the number defined here is taken as Poisson's ratio.  If zero, an incompressible rubber like behavior is assumed and a default value of 0.495 is used internally.   If a Poisson's ratio of 0.0 is desired, input a small value for PR such as 0.001.  When fully integrated solid elements are used and when a nonzero Poisson's ratio is specified, a foam material is assumed and selective-reduced integration is not used due to the compressibility.  This is true even if PR approaches 0.500.  If any other value excluding zero is define, then BETA is taken as the absolute value of the given number and a nearly incompressible rubber like behavior is assumed.  An incrementally updated mean viscous stress develops according to the equation:The BETA parameter does not apply to highly compressible foam materials.Material failure parameter that controls the volume enclosed by the failure surface.
        LE.0.0: ignore failure criterion;
        GT.0.0: use actual K value for failure criterions..
        """ # nopep8
        return self._cards[1].get_value("pra")

    @pra.setter
    def pra(self, value: float) -> None:
        self._cards[1].set_value("pra", value)

    @property
    def lcunld(self) -> typing.Optional[int]:
        """Get or set the Load curve, see *DEFINE_CURVE, defining the force versus actual
        length during unloading. The unload curve should cover exactly
        the same range as LC or the load curves of TBID and its end points
        should have identical values, i.e., the combination of LC and
        LCUNLD or the first curve of TBID and LCUNLD describes a
        complete cycle of loading and unloading. See also material *MAT_	083.
        """ # nopep8
        return self._cards[2].get_value("lcunld")

    @lcunld.setter
    def lcunld(self, value: int) -> None:
        self._cards[2].set_value("lcunld", value)

    @property
    def hu(self) -> float:
        """Get or set the Hysteretic unloading factor between 0 and 1 (default = 1., i.e. no
        energy dissipation), see also material *MAT_083 and Figure M57-1. This option is ignored if LCUNLD is used.
        """ # nopep8
        return self._cards[2].get_value("hu")

    @hu.setter
    def hu(self, value: float) -> None:
        self._cards[2].set_value("hu", value)

    @property
    def shape(self) -> typing.Optional[float]:
        """Get or set the Shape factor for unloading. Active for nonzero values of the hysteretic unloading factor HU. Values less than one reduces the energy
        dissipation and greater than one increases dissipation, see also material *MAT_083 and Figure M57-1.
        """ # nopep8
        return self._cards[2].get_value("shape")

    @shape.setter
    def shape(self, value: float) -> None:
        self._cards[2].set_value("shape", value)

    @property
    def stol(self) -> typing.Optional[float]:
        """Get or set the Tolerance in stability check.
        """ # nopep8
        return self._cards[2].get_value("stol")

    @stol.setter
    def stol(self, value: float) -> None:
        self._cards[2].set_value("stol", value)

    @property
    def visco(self) -> float:
        """Get or set the Flag to invoke visco-elastic formulation.  The visco-elastic formulation does not apply to shell elements and will be ignored for shells.
        EQ.0.0:	purely elastic;
        EQ.1.0:	visco-elastic formulation (solids only).
        """ # nopep8
        return self._cards[2].get_value("visco")

    @visco.setter
    def visco(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""visco must be one of {0.0,1.0}""")
        self._cards[2].set_value("visco", value)

    @property
    def hisout(self) -> float:
        """Get or set the History output flag.
        EQ.0.0:	default;
        EQ.1.0:	principal strains are written to history variables 25, 26, 27.
        """ # nopep8
        return self._cards[2].get_value("hisout")

    @hisout.setter
    def hisout(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""hisout must be one of {0.0,1.0}""")
        self._cards[2].set_value("hisout", value)

    @property
    def constants(self):
        '''Gets the table of constants'''
        return self._cards[3].table

    @constants.setter
    def constants(self, df):
        '''sets constants from the dataframe df'''
        self._cards[3].table = df

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

