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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseExtentBinary(KeywordBase):
    """DYNA DATABASE_EXTENT_BINARY keyword"""

    keyword = "DATABASE"
    subkeyword = "EXTENT_BINARY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "neiph",
                        int,
                        0,
                        10,
                        kwargs.get("neiph", 0)
                    ),
                    Field(
                        "neips",
                        int,
                        10,
                        10,
                        kwargs.get("neips", 0)
                    ),
                    Field(
                        "maxint",
                        int,
                        20,
                        10,
                        kwargs.get("maxint", 3)
                    ),
                    Field(
                        "strflg",
                        int,
                        30,
                        10,
                        kwargs.get("strflg", 0)
                    ),
                    Field(
                        "sigflg",
                        int,
                        40,
                        10,
                        kwargs.get("sigflg", 1)
                    ),
                    Field(
                        "epsflg",
                        int,
                        50,
                        10,
                        kwargs.get("epsflg", 1)
                    ),
                    Field(
                        "rltflg",
                        int,
                        60,
                        10,
                        kwargs.get("rltflg", 1)
                    ),
                    Field(
                        "engflg",
                        int,
                        70,
                        10,
                        kwargs.get("engflg", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cmpflg",
                        int,
                        0,
                        10,
                        kwargs.get("cmpflg", 0)
                    ),
                    Field(
                        "ieverp",
                        int,
                        10,
                        10,
                        kwargs.get("ieverp", 0)
                    ),
                    Field(
                        "beamip",
                        int,
                        20,
                        10,
                        kwargs.get("beamip", 0)
                    ),
                    Field(
                        "dcomp",
                        int,
                        30,
                        10,
                        kwargs.get("dcomp", 1)
                    ),
                    Field(
                        "shge",
                        int,
                        40,
                        10,
                        kwargs.get("shge", 1)
                    ),
                    Field(
                        "stssz",
                        int,
                        50,
                        10,
                        kwargs.get("stssz", 1)
                    ),
                    Field(
                        "n3thdt",
                        int,
                        60,
                        10,
                        kwargs.get("n3thdt", 2)
                    ),
                    Field(
                        "ialemat",
                        int,
                        70,
                        10,
                        kwargs.get("ialemat", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nintsld",
                        int,
                        0,
                        10,
                        kwargs.get("nintsld", 0)
                    ),
                    Field(
                        "pkp_sen",
                        int,
                        10,
                        10,
                        kwargs.get("pkp_sen", 0)
                    ),
                    Field(
                        "sclp",
                        float,
                        20,
                        10,
                        kwargs.get("sclp", 1.0)
                    ),
                    Field(
                        "hydro",
                        int,
                        30,
                        10,
                        kwargs.get("hydro", 0)
                    ),
                    Field(
                        "msscl",
                        int,
                        40,
                        10,
                        kwargs.get("msscl", 0)
                    ),
                    Field(
                        "therm",
                        int,
                        50,
                        10,
                        kwargs.get("therm", 0)
                    ),
                    Field(
                        "intout",
                        str,
                        60,
                        10,
                        kwargs.get("intout", " ")
                    ),
                    Field(
                        "nodout",
                        str,
                        70,
                        10,
                        kwargs.get("nodout", " ")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dtdt",
                        int,
                        0,
                        10,
                        kwargs.get("dtdt", 0)
                    ),
                    Field(
                        "resplt",
                        int,
                        10,
                        10,
                        kwargs.get("resplt", 0)
                    ),
                    Field(
                        "neipb",
                        int,
                        20,
                        10,
                        kwargs.get("neipb")
                    ),
                    Field(
                        "quadsld",
                        int,
                        30,
                        10,
                        kwargs.get("quadsld", 0)
                    ),
                    Field(
                        "cubsld",
                        int,
                        40,
                        10,
                        kwargs.get("cubsld", 0)
                    ),
                    Field(
                        "deleres",
                        int,
                        50,
                        10,
                        kwargs.get("deleres", 0)
                    ),
                ],
            ),
        ]

    @property
    def neiph(self) -> int:
        """Get or set the Number of additional integration point history variables written to the binary database for solid elements. The integration point data is written in the same order that it is stored in memory-each material modal has its own history variables that are stored. For user defined materials it is important to store the history data that is needed for plotting before the data which is not of interest.
        """ # nopep8
        return self._cards[0].get_value("neiph")

    @neiph.setter
    def neiph(self, value: int) -> None:
        self._cards[0].set_value("neiph", value)

    @property
    def neips(self) -> int:
        """Get or set the Number of additional integration point history variables written to the binary database for both shell and thick shell elements for each integration point, see NEIPH above.
        """ # nopep8
        return self._cards[0].get_value("neips")

    @neips.setter
    def neips(self, value: int) -> None:
        self._cards[0].set_value("neips", value)

    @property
    def maxint(self) -> int:
        """Get or set the Number of shell integration points written to the LS-DYNA database, see also *INTEGRATION_SHELL. If the default value of 3 is used then results are output for the outermost (top) and innermost (bottom) integration points together with results for the neutral axis. If MAXINT is set to 3 and the element has 1 integration point then all three results will be the same. If a value other than 3 is used then the results for the first MAXINT integration points in the element will be output. NOTE: If the element has an even number of integration points and MAXINT is not set to 3 then you will not get mid-surface results. (See Remarks in user's manual).If MAXINT is set to a negative
        number, MAXINT integration points are output for each in plane
        integration point location and no averaging is used. This can greatly
        increase the size of the binary databases d3plot, d3thdt, and d3part
        """ # nopep8
        return self._cards[0].get_value("maxint")

    @maxint.setter
    def maxint(self, value: int) -> None:
        self._cards[0].set_value("maxint", value)

    @property
    def strflg(self) -> int:
        """Get or set the Flag for output of strain tensors.  STRFLG is interpreted digit-wise STRFLG = [NML], STRFLG = 100*N + 10*M + L
        L.EQ.1: Write strain tensor data to d3plot, elout, and dynain.  For shell and thick shell elements two tensors are written, one at the innermost and one at the outermost integration point.  For solid elements a single strain tensor is written
        M.EQ.1:	Write plastic strain data to d3plot.
        N.EQ.1:	Write thermal strain data to d3plot.
        """ # nopep8
        return self._cards[0].get_value("strflg")

    @strflg.setter
    def strflg(self, value: int) -> None:
        self._cards[0].set_value("strflg", value)

    @property
    def sigflg(self) -> int:
        """Get or set the Flag for including stress tensor in the shell LS-DYNA database:
        EQ.1: include (default),
        EQ.2: exclude.
        """ # nopep8
        return self._cards[0].get_value("sigflg")

    @sigflg.setter
    def sigflg(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""sigflg must be one of {1,2}""")
        self._cards[0].set_value("sigflg", value)

    @property
    def epsflg(self) -> int:
        """Get or set the Flag for including the effective plastic strains in the shell LS-DYNA database:
        EQ.1: include (default),
        EQ.2: exclude.
        """ # nopep8
        return self._cards[0].get_value("epsflg")

    @epsflg.setter
    def epsflg(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""epsflg must be one of {1,2}""")
        self._cards[0].set_value("epsflg", value)

    @property
    def rltflg(self) -> int:
        """Get or set the Flag for including stress resultants in the shell LS-DYNA database:
        EQ.1: include (default),
        EQ.2: exclude.
        """ # nopep8
        return self._cards[0].get_value("rltflg")

    @rltflg.setter
    def rltflg(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""rltflg must be one of {1,2}""")
        self._cards[0].set_value("rltflg", value)

    @property
    def engflg(self) -> int:
        """Get or set the Flag for including internal energy and thickness in the LS-DYNA database:
        EQ.1: include (default),
        EQ.2: exclude.
        """ # nopep8
        return self._cards[0].get_value("engflg")

    @engflg.setter
    def engflg(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""engflg must be one of {1,2}""")
        self._cards[0].set_value("engflg", value)

    @property
    def cmpflg(self) -> int:
        """Get or set the Orthotropic and anisotropic material stress output in local coordinate system for shells and thick shells.
        EQ.0: global,
        EQ.1: local.
        """ # nopep8
        return self._cards[1].get_value("cmpflg")

    @cmpflg.setter
    def cmpflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cmpflg must be one of {0,1}""")
        self._cards[1].set_value("cmpflg", value)

    @property
    def ieverp(self) -> int:
        """Get or set the Every plot state for D3PLOT database is written to a separate file. This option will limit the database to 100 states:
        EQ.0: more than one state can be on each plotfile,
        EQ.1: one state only on each plotfile.
        """ # nopep8
        return self._cards[1].get_value("ieverp")

    @ieverp.setter
    def ieverp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ieverp must be one of {0,1}""")
        self._cards[1].set_value("ieverp", value)

    @property
    def beamip(self) -> int:
        """Get or set the Number of beam integration points for output. This option does not apply to beams that use a resultant formulation.
        """ # nopep8
        return self._cards[1].get_value("beamip")

    @beamip.setter
    def beamip(self, value: int) -> None:
        self._cards[1].set_value("beamip", value)

    @property
    def dcomp(self) -> int:
        """Get or set the Data compression to eliminate rigid body data:
        EQ.1: off (default), no data compression,
        EQ.2: on.
        EQ.3: off, no rigid body data compression, but nodal velocities and accelerations are eliminated from the database.
        EQ.4: on, rigid body data compression active and nodal velocities and accelerations are eliminaated from the database.
        EQ.5: on, rigid body data compression active and rigid nodal data are eliminated from the database. Only 6 dof rigid body motion is written.
        EQ.6: on, rigid body data compression active, rigid nodal data, and nodal velocities and accelerations are eliminated from the database.  Only 6 dof rigid body motion is written.
        """ # nopep8
        return self._cards[1].get_value("dcomp")

    @dcomp.setter
    def dcomp(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6]:
            raise Exception("""dcomp must be one of {1,2,3,4,5,6}""")
        self._cards[1].set_value("dcomp", value)

    @property
    def shge(self) -> int:
        """Get or set the Output shell hourglass energy:
        EQ.1: off (default), no hourglass energy written,
        EQ.2: on.
        """ # nopep8
        return self._cards[1].get_value("shge")

    @shge.setter
    def shge(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""shge must be one of {1,2}""")
        self._cards[1].set_value("shge", value)

    @property
    def stssz(self) -> int:
        """Get or set the Output shell element time step, mass or added mass:
        EQ.1: off (default),
        EQ.2: out time step size,
        EQ.3: output mass, added mass, or time step size.
        (See Remark 3 in user's manual).
        """ # nopep8
        return self._cards[1].get_value("stssz")

    @stssz.setter
    def stssz(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""stssz must be one of {1,2,3}""")
        self._cards[1].set_value("stssz", value)

    @property
    def n3thdt(self) -> int:
        """Get or set the Material energy write option for D3THDT database
        EQ.1: off, energy is NOT written to D3THDT database,
        EQ.2: on (default), energy is written to D3THDT database.
        """ # nopep8
        return self._cards[1].get_value("n3thdt")

    @n3thdt.setter
    def n3thdt(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""n3thdt must be one of {2,1}""")
        self._cards[1].set_value("n3thdt", value)

    @property
    def ialemat(self) -> int:
        """Get or set the Output solid part id list containing ale materials
        EQ.1: on (default)
        EQ.0: off
        """ # nopep8
        return self._cards[1].get_value("ialemat")

    @ialemat.setter
    def ialemat(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""ialemat must be one of {1,0}""")
        self._cards[1].set_value("ialemat", value)

    @property
    def nintsld(self) -> int:
        """Get or set the Number of solid element integration points written to the LS-DYNA database. The default value is 1. For solids with multiple integration points NINTSLD may be set to 8. Currently, no other values for INITSLD are allowed. For solids with multiple integration points, an average value is output if NINTSLD is set to 1.
        """ # nopep8
        return self._cards[2].get_value("nintsld")

    @nintsld.setter
    def nintsld(self, value: int) -> None:
        if value not in [0, 1, 8]:
            raise Exception("""nintsld must be one of {0,1,8}""")
        self._cards[2].set_value("nintsld", value)

    @property
    def pkp_sen(self) -> int:
        """Get or set the Flag to output the peak pressure and surface energy computed by each contact interface into the interface force database.   To obtain the surface energy, FRCENG, must be sent to 1 on the control contact card.  When PKP_SEN=1, it is possible to identify the energies generated on the upper and lower shell surfaces, which is important in metal forming applications.  This data is mapped after each H-adaptive remeshing.
        EQ.0: No data is written
        EQ.1: Output the peak pressures and surface energy by contact interface.
        """ # nopep8
        return self._cards[2].get_value("pkp_sen")

    @pkp_sen.setter
    def pkp_sen(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""pkp_sen must be one of {0,1}""")
        self._cards[2].set_value("pkp_sen", value)

    @property
    def sclp(self) -> float:
        """Get or set the A scaling parameter used in the computation of the peak pressure.  This parameter is generally set to unity (the default), but it must be greater than 0..
        """ # nopep8
        return self._cards[2].get_value("sclp")

    @sclp.setter
    def sclp(self, value: float) -> None:
        self._cards[2].set_value("sclp", value)

    @property
    def hydro(self) -> int:
        """Get or set the Either 3 or 5 additional history variables useful to shock physics are
        output as the last history variables. For HYDRO = 1, the internal energy
        per reference volume, the reference volume, and the value of the bulk
        viscosity are added to the database, and for HYDRO = 2, the relative
        volume and current density are also added
        """ # nopep8
        return self._cards[2].get_value("hydro")

    @hydro.setter
    def hydro(self, value: int) -> None:
        self._cards[2].set_value("hydro", value)

    @property
    def msscl(self) -> int:
        """Get or set the Output nodal information related to mass scaling into the D3PLOT database.  This option can be activated if and only if DT2MS < 0.0, see control card *CONTROL_TIMESTEP.  This option is available starting with the second release of Version 971.
        EQ.0: No data is written
        EQ.1: Output incremental nodal mass
        EQ.2: Output percentage increase in nodal mass.
        """ # nopep8
        return self._cards[2].get_value("msscl")

    @msscl.setter
    def msscl(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""msscl must be one of {0,1,2}""")
        self._cards[2].set_value("msscl", value)

    @property
    def therm(self) -> int:
        """Get or set the Output of thermal data to d3plot. The use of this option (THERM>0) may make the database incompatible with other 3rd party software.
        EQ.0: (default) output temperature
        EQ.1: output temperature
        EQ.2: output temperature and flux
        EQ.3: output temperature, flux, and shell bottom and top surface temperature
        """ # nopep8
        return self._cards[2].get_value("therm")

    @therm.setter
    def therm(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""therm must be one of {0,1,2,3}""")
        self._cards[2].set_value("therm", value)

    @property
    def intout(self) -> str:
        """Get or set the Output stress/strain at all integration points for detailed element output in the file ELOUTDET.  DT and BINARY of *DATABASE_ELOUT apply to ELOUTDET.  See remarks 4-10 below.
        EQ.STRESS: when stress output is required
        EQ.STRAIN when strain output is required
        EQ.ALL when both stress and strain output are required
        """ # nopep8
        return self._cards[2].get_value("intout")

    @intout.setter
    def intout(self, value: str) -> None:
        if value not in [" ", "STRESS", "STRAIN", "ALL"]:
            raise Exception("""intout must be one of {" ","STRESS","STRAIN","ALL"}""")
        self._cards[2].set_value("intout", value)

    @property
    def nodout(self) -> str:
        """Get or set the Output extrapolated stress/strain at connectivity nodes points for detailed element output in the file ELOUTDET.  DT and BINARY of *DATABASE_ELOUT apply to ELOUTDET.
        EQ.STRESS when stress output is required
        EQ.STRAIN when strain output is required
        EQ.ALL when both stress and strain output are required
        EQ.STRESS_GL when nodal averaged stress output is required
        EQ.STRAIN_GL when nodal averaged strain output is required
        EQ.ALL_GL for nodal averaged stress and strain output
        """ # nopep8
        return self._cards[2].get_value("nodout")

    @nodout.setter
    def nodout(self, value: str) -> None:
        if value not in [" ", "STRESS", "STRAIN", "ALL", "STRESS_GL", "STRAIN_GL", "ALL_GL"]:
            raise Exception("""nodout must be one of {" ","STRESS","STRAIN","ALL","STRESS_GL","STRAIN_GL","ALL_GL"}""")
        self._cards[2].set_value("nodout", value)

    @property
    def dtdt(self) -> int:
        """Get or set the Output of node point dtemperature/dtime data to d3plot
        EQ.0: (default) no output
        EQ.1: output dT/dt
        """ # nopep8
        return self._cards[3].get_value("dtdt")

    @dtdt.setter
    def dtdt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dtdt must be one of {0,1}""")
        self._cards[3].set_value("dtdt", value)

    @property
    def resplt(self) -> int:
        """Get or set the Output of translational and rotational residual forces to d3plot and d3iter
        EQ.0: No output
        EQ.1: Output residual
        """ # nopep8
        return self._cards[3].get_value("resplt")

    @resplt.setter
    def resplt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""resplt must be one of {0,1}""")
        self._cards[3].set_value("resplt", value)

    @property
    def neipb(self) -> typing.Optional[int]:
        """Get or set the Number of additional integration point history variables written to the binary database for beam elements.
        """ # nopep8
        return self._cards[3].get_value("neipb")

    @neipb.setter
    def neipb(self, value: int) -> None:
        self._cards[3].set_value("neipb", value)

    @property
    def quadsld(self) -> int:
        """Get or set the Output option for quadratic higher order solid elements EQ.1: output full connectivity,
        EQ.2: full connectivity and data at all integration points.
        """ # nopep8
        return self._cards[3].get_value("quadsld")

    @quadsld.setter
    def quadsld(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""quadsld must be one of {0,1,2}""")
        self._cards[3].set_value("quadsld", value)

    @property
    def cubsld(self) -> int:
        """Get or set the Output option for cubic higher order solid elements EQ.1: output full connectivity,
        EQ.2: full connectivity and data at all integration points.
        """ # nopep8
        return self._cards[3].get_value("cubsld")

    @cubsld.setter
    def cubsld(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""cubsld must be one of {0,1,2}""")
        self._cards[3].set_value("cubsld", value)

    @property
    def deleres(self) -> int:
        """Get or set the Output flag for results of deleted elements:
        EQ.0:	no results output(all zero)
        EQ.1 : last available results, e.g., stressesand history variables, are written to d3plotand d3part.
        """ # nopep8
        return self._cards[3].get_value("deleres")

    @deleres.setter
    def deleres(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""deleres must be one of {0,1}""")
        self._cards[3].set_value("deleres", value)

