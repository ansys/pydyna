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

class ControlSph(KeywordBase):
    """DYNA CONTROL_SPH keyword"""

    keyword = "CONTROL"
    subkeyword = "SPH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ncbs",
                        int,
                        0,
                        10,
                        kwargs.get("ncbs", 1)
                    ),
                    Field(
                        "boxid",
                        int,
                        10,
                        10,
                        kwargs.get("boxid")
                    ),
                    Field(
                        "dt",
                        float,
                        20,
                        10,
                        kwargs.get("dt", 1.E+20)
                    ),
                    Field(
                        "idim",
                        int,
                        30,
                        10,
                        kwargs.get("idim")
                    ),
                    Field(
                        "nmneigh",
                        int,
                        40,
                        10,
                        kwargs.get("nmneigh", 150)
                    ),
                    Field(
                        "form",
                        int,
                        50,
                        10,
                        kwargs.get("form", 0)
                    ),
                    Field(
                        "start",
                        float,
                        60,
                        10,
                        kwargs.get("start", 0.0)
                    ),
                    Field(
                        "maxv",
                        float,
                        70,
                        10,
                        kwargs.get("maxv", 1.0E+15)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cont",
                        int,
                        0,
                        10,
                        kwargs.get("cont", 0)
                    ),
                    Field(
                        "deriv",
                        int,
                        10,
                        10,
                        kwargs.get("deriv", 0)
                    ),
                    Field(
                        "ini",
                        int,
                        20,
                        10,
                        kwargs.get("ini", 0)
                    ),
                    Field(
                        "ishow",
                        int,
                        30,
                        10,
                        kwargs.get("ishow", 0)
                    ),
                    Field(
                        "ierod",
                        int,
                        40,
                        10,
                        kwargs.get("ierod", 0)
                    ),
                    Field(
                        "icont",
                        int,
                        50,
                        10,
                        kwargs.get("icont", 0)
                    ),
                    Field(
                        "iavis",
                        int,
                        60,
                        10,
                        kwargs.get("iavis", 0)
                    ),
                    Field(
                        "isymp",
                        int,
                        70,
                        10,
                        kwargs.get("isymp", 100)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ithk",
                        int,
                        0,
                        10,
                        kwargs.get("ithk", 0)
                    ),
                    Field(
                        "istab",
                        int,
                        10,
                        10,
                        kwargs.get("istab", 0)
                    ),
                    Field(
                        "ql",
                        float,
                        20,
                        10,
                        kwargs.get("ql", 0.01)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "sphsort",
                        int,
                        40,
                        10,
                        kwargs.get("sphsort", 0)
                    ),
                    Field(
                        "ishift",
                        int,
                        50,
                        10,
                        kwargs.get("ishift", 0)
                    ),
                ],
            ),
        ]

    @property
    def ncbs(self) -> int:
        """Get or set the Number of time steps between particle sorting.
        """ # nopep8
        return self._cards[0].get_value("ncbs")

    @ncbs.setter
    def ncbs(self, value: int) -> None:
        self._cards[0].set_value("ncbs", value)

    @property
    def boxid(self) -> typing.Optional[int]:
        """Get or set the SPH approximations are computed inside a specified box(see DEFINE_BOX). When a particle has gone outside the BOX, it is deactivated. This will save computational time by eliminating particles that no longer interact with the structure.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time. Determines when the SPH calculations are stopped.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def idim(self) -> typing.Optional[int]:
        """Get or set the Space dimension for SPH particles:
        EQ. 3: for 3D problems
        EQ. 2: for 2D plane strain problems
        EQ.-2: for 2D axisymmetric problems
        """ # nopep8
        return self._cards[0].get_value("idim")

    @idim.setter
    def idim(self, value: int) -> None:
        self._cards[0].set_value("idim", value)

    @property
    def nmneigh(self) -> int:
        """Get or set the Defines the initial number of neighbors per particle. This variable is just for memory allocation of arrays during the initialization phase. During the calculation, some particles can request more neighbors and LS-DYNA will automatically adapt the size of that variable. Default value should apply for most applications.
        """ # nopep8
        return self._cards[0].get_value("nmneigh")

    @nmneigh.setter
    def nmneigh(self, value: int) -> None:
        self._cards[0].set_value("nmneigh", value)

    @property
    def form(self) -> int:
        """Get or set the Particle approximation theory (Remark 2):
        EQ.0:	default formulation
        EQ.1 : renormalization approximation
        EQ.2 : symmetric formulation
        EQ.3 : symmetric renormalized approximation
        EQ.4 : tensor formulation
        EQ.5 : fluid particle approximation
        EQ.6 : fluid particle with renormalization approximation
        EQ.7 : total Lagrangian formulation
        EQ.8 : total Lagrangian formulation with renormalization
        EQ.9 : adaptive SPH formulation(ASPH) with anisotropic smoothing tensor(Remark 2g)
        EQ.10 : renormalization approximation for adaptive SPH formulation(ASPH) with anisotropic smoothing tensor
        EQ.12 : moving least - squares based formulation(MPP only, see Remark 2e)
        EQ.13 : implicit incompressible formulation. (MPP only).
        EQ.15 : enhanced fluid formulation
        EQ.16 : enhanced fluid formulation with renormalization
        """ # nopep8
        return self._cards[0].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 15, 16]:
            raise Exception("""form must be one of {0,1,2,3,4,5,6,7,8,9,10,12,13,15,16}""")
        self._cards[0].set_value("form", value)

    @property
    def start(self) -> float:
        """Get or set the Start time for particle approximation. Particle approximations will be computed when time of the analysis has reached the value defined in START.
        """ # nopep8
        return self._cards[0].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        self._cards[0].set_value("start", value)

    @property
    def maxv(self) -> float:
        """Get or set the Maximum value for velocity for the SPH particles. Particles with a velocity greater than MAXV are deactivated.
        """ # nopep8
        return self._cards[0].get_value("maxv")

    @maxv.setter
    def maxv(self, value: float) -> None:
        self._cards[0].set_value("maxv", value)

    @property
    def cont(self) -> int:
        """Get or set the Defines the computation of the particle approximation between two different SPH parts:
        EQ. 0: Particle approximation is defined (default),
        EQ. 1: Particle approximation is not computed. Two different SPH materials will not interact with each others and penetration is allowed.
        """ # nopep8
        return self._cards[1].get_value("cont")

    @cont.setter
    def cont(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cont must be one of {0,1}""")
        self._cards[1].set_value("cont", value)

    @property
    def deriv(self) -> int:
        """Get or set the Time integration type for the smoothing length. Please refer to 7.99 (CONTROL) on v970 LS-DYNA manual for EQ.0 and EQ.1.
        """ # nopep8
        return self._cards[1].get_value("deriv")

    @deriv.setter
    def deriv(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""deriv must be one of {0,1}""")
        self._cards[1].set_value("deriv", value)

    @property
    def ini(self) -> int:
        """Get or set the Computation of the smoothing length during the initialization:
        EQ.0: Bucket sort based algorithm (default, very fast).
        EQ.1: Global computation on all the particles of the model.
        EQ.2: Based on the mass of the SPH particle..
        """ # nopep8
        return self._cards[1].get_value("ini")

    @ini.setter
    def ini(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ini must be one of {0,1,2}""")
        self._cards[1].set_value("ini", value)

    @property
    def ishow(self) -> int:
        """Get or set the Display options for SPH particles:
        EQ.0: Show all SPH particles in LS-PrePost.
        EQ.1: Exclude deactivated SPH particles in LS-PrePost
        """ # nopep8
        return self._cards[1].get_value("ishow")

    @ishow.setter
    def ishow(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ishow must be one of {0,1}""")
        self._cards[1].set_value("ishow", value)

    @property
    def ierod(self) -> int:
        """Get or set the Deactivation control for SPH particles:
        EQ.0: Particles remain active.
        EQ.1: SPH particles are deactivated and stress states are set to 0 when erosion criteria are satisfied.
        EQ.2:  SPH particles are totally deactivated and stress states are set to 0 when erosion criteria are satisfied.  See Remark 3.
        EQ.3:  SPH particles are totally deactivated and stress states are set to 0 when erosion criteria are satisfied If an EOS is defined, the volumetric response is unaffected.  See Remark 3
        """ # nopep8
        return self._cards[1].get_value("ierod")

    @ierod.setter
    def ierod(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""ierod must be one of {0,1,2,3}""")
        self._cards[1].set_value("ierod", value)

    @property
    def icont(self) -> int:
        """Get or set the Controls contact behavior for deactivated SPH particles:
        EQ.0: Any contact defined for SPH remains active for deactivated particles.
        EQ.1: Contact is inactive for deactivated particles
        """ # nopep8
        return self._cards[1].get_value("icont")

    @icont.setter
    def icont(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icont must be one of {0,1}""")
        self._cards[1].set_value("icont", value)

    @property
    def iavis(self) -> int:
        """Get or set the Defines artificial viscosity formulation for SPH elements (Remark 3):
        EQ.0: Monaghan type artificial viscosity formulation is used.
        EQ.1: Standard type artificial viscosity formulation from solid element is used (this option is not supported in SPH 2D and 2D axisymmetric elements).
        """ # nopep8
        return self._cards[1].get_value("iavis")

    @iavis.setter
    def iavis(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iavis must be one of {0,1}""")
        self._cards[1].set_value("iavis", value)

    @property
    def isymp(self) -> int:
        """Get or set the Defines the percentage of original SPH particles used for memoryallocation of SPH symmetric planes ghost nodes generation process (default is 100%). Recommended for large SPH particles models (value range 10~20) to control the memory allocation for SPH ghost particles with *BOUNDARY_SPH_SYMMETRY_PLANE keyword.
        """ # nopep8
        return self._cards[1].get_value("isymp")

    @isymp.setter
    def isymp(self, value: int) -> None:
        self._cards[1].set_value("isymp", value)

    @property
    def ithk(self) -> int:
        """Get or set the Contact thickness option:
        EQ.0:	the contact thickness is set to zero(default).
        EQ.1 : the contact thickness is automatically calculated based on the volume of each SPH particle.
        This contact thickness calculation is ignored if a non - zero contact thickness for slave surface(SST) is provided by the contact card.
        """ # nopep8
        return self._cards[2].get_value("ithk")

    @ithk.setter
    def ithk(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ithk must be one of {0,1}""")
        self._cards[2].set_value("ithk", value)

    @property
    def istab(self) -> int:
        """Get or set the Stabilization type, only used when IFORM = 12:
        EQ.0:	incremental stabilization(default).Adequate for most materials.
        EQ.1 : total stabilization.Only recommended for hyperelastic materials.
        """ # nopep8
        return self._cards[2].get_value("istab")

    @istab.setter
    def istab(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""istab must be one of {0,1}""")
        self._cards[2].set_value("istab", value)

    @property
    def ql(self) -> float:
        """Get or set the Quasi-Linear coefficient, only used when IFORM = 12. See Remark 5.
        """ # nopep8
        return self._cards[2].get_value("ql")

    @ql.setter
    def ql(self, value: float) -> None:
        self._cards[2].set_value("ql", value)

    @property
    def sphsort(self) -> int:
        """Get or set the For the implicit solver, sort and move SPH nodes from *NODE list to the end of the list.
        EQ.0:	no sorting(default)
        EQ.1 : perform sorting.
        """ # nopep8
        return self._cards[2].get_value("sphsort")

    @sphsort.setter
    def sphsort(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sphsort must be one of {0,1}""")
        self._cards[2].set_value("sphsort", value)

    @property
    def ishift(self) -> int:
        """Get or set the Flag for applying the shifting algorithm to SPH particles (available from R13.0). With the shifting algorithm, particles are advanced and then shifted slightly across streamlines. This process reduces particle clustering in the maximum compression and stretching directions.
        EQ.0:	Do not apply shifting algorithm applied(default).
        EQ.1:	Apply shifting algorithm applied.
        """ # nopep8
        return self._cards[2].get_value("ishift")

    @ishift.setter
    def ishift(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ishift must be one of {0,1}""")
        self._cards[2].set_value("ishift", value)

