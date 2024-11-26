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

class ControlAle(KeywordBase):
    """DYNA CONTROL_ALE keyword"""

    keyword = "CONTROL"
    subkeyword = "ALE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dct",
                        int,
                        0,
                        10,
                        kwargs.get("dct", 1)
                    ),
                    Field(
                        "nadv",
                        int,
                        10,
                        10,
                        kwargs.get("nadv", 1)
                    ),
                    Field(
                        "meth",
                        int,
                        20,
                        10,
                        kwargs.get("meth", 2)
                    ),
                    Field(
                        "afac",
                        float,
                        30,
                        10,
                        kwargs.get("afac", 0.0)
                    ),
                    Field(
                        "bfac",
                        float,
                        40,
                        10,
                        kwargs.get("bfac", 0.0)
                    ),
                    Field(
                        "cfac",
                        float,
                        50,
                        10,
                        kwargs.get("cfac", 0.0)
                    ),
                    Field(
                        "dfac",
                        float,
                        60,
                        10,
                        kwargs.get("dfac", 0.0)
                    ),
                    Field(
                        "efac",
                        float,
                        70,
                        10,
                        kwargs.get("efac", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "start",
                        float,
                        0,
                        10,
                        kwargs.get("start", 0.0)
                    ),
                    Field(
                        "end",
                        float,
                        10,
                        10,
                        kwargs.get("end", 1.0E+20)
                    ),
                    Field(
                        "aafac",
                        float,
                        20,
                        10,
                        kwargs.get("aafac", 1.0)
                    ),
                    Field(
                        "vfact",
                        float,
                        30,
                        10,
                        kwargs.get("vfact", 1.0E-06)
                    ),
                    Field(
                        "prit",
                        int,
                        40,
                        10,
                        kwargs.get("prit", 0)
                    ),
                    Field(
                        "ebc",
                        int,
                        50,
                        10,
                        kwargs.get("ebc", 0)
                    ),
                    Field(
                        "pref",
                        float,
                        60,
                        10,
                        kwargs.get("pref", 0.0)
                    ),
                    Field(
                        "nsidebc",
                        int,
                        70,
                        10,
                        kwargs.get("nsidebc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ncpl",
                        int,
                        0,
                        10,
                        kwargs.get("ncpl", 1)
                    ),
                    Field(
                        "nbkt",
                        int,
                        10,
                        10,
                        kwargs.get("nbkt", 50)
                    ),
                    Field(
                        "imascl",
                        int,
                        20,
                        10,
                        kwargs.get("imascl", 0)
                    ),
                    Field(
                        "checkr",
                        float,
                        30,
                        10,
                        kwargs.get("checkr", 0.0)
                    ),
                    Field(
                        "beamin",
                        float,
                        40,
                        10,
                        kwargs.get("beamin", 0.0)
                    ),
                    Field(
                        "mmgpref",
                        int,
                        50,
                        10,
                        kwargs.get("mmgpref", 0)
                    ),
                    Field(
                        "pdifmx",
                        float,
                        60,
                        10,
                        kwargs.get("pdifmx", 0.0)
                    ),
                    Field(
                        "dtmufac",
                        float,
                        70,
                        10,
                        kwargs.get("dtmufac", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "optimpp",
                        int,
                        0,
                        10,
                        kwargs.get("optimpp", 0)
                    ),
                    Field(
                        "ialedr",
                        int,
                        10,
                        10,
                        kwargs.get("ialedr", 0)
                    ),
                    Field(
                        "bndflx",
                        int,
                        20,
                        10,
                        kwargs.get("bndflx", 0)
                    ),
                    Field(
                        "minmas",
                        float,
                        30,
                        10,
                        kwargs.get("minmas", 1.0E-5)
                    ),
                ],
            ),
        ]

    @property
    def dct(self) -> int:
        """Get or set the Flag to invoke alternate advection logic for ALE (see Remark 2):
        NE. - 1:	Use default advection logic.
        EQ. - 1 : Use alternate(improved) advection logic; generally recommended, especially for simulation of explosives.
        Note that for S - ALE DCT is ignored and the alternative advection option is always used.
        """ # nopep8
        return self._cards[0].get_value("dct")

    @dct.setter
    def dct(self, value: int) -> None:
        self._cards[0].set_value("dct", value)

    @property
    def nadv(self) -> int:
        """Get or set the Number of cycles between advections.
        """ # nopep8
        return self._cards[0].get_value("nadv")

    @nadv.setter
    def nadv(self, value: int) -> None:
        self._cards[0].set_value("nadv", value)

    @property
    def meth(self) -> int:
        """Get or set the Advection method:
        EQ.1: donor cell + HIS (first order accurate),
        EQ.2: Van Leer + half index shift (second order).
        EQ.-2 Modified Van Leer
        EQ.3: donor cell + HIS, first order accurate, conserving total energy over each advection step instead of conserving internal energy
        EQ.6:	Finite Volume Method with a Flux Corrected Transport. Only supported by ideal gases: the finite volume method is only applied to ALE elements fully filled with materials using *EOS_IDEAL_GAS or *EOS_001 for ideal gases. The advection in mixed ALE elements is handled by a donor cell method.
        """ # nopep8
        return self._cards[0].get_value("meth")

    @meth.setter
    def meth(self, value: int) -> None:
        if value not in [2, 1, -2, 3, 6]:
            raise Exception("""meth must be one of {2,1,-2,3,6}""")
        self._cards[0].set_value("meth", value)

    @property
    def afac(self) -> float:
        """Get or set the ALE smoothing weight factor - Simple average:
        EQ.-1: turn smoothing off.
        """ # nopep8
        return self._cards[0].get_value("afac")

    @afac.setter
    def afac(self, value: float) -> None:
        self._cards[0].set_value("afac", value)

    @property
    def bfac(self) -> float:
        """Get or set the ALE smoothing weight factor for Volume weighting.
        """ # nopep8
        return self._cards[0].get_value("bfac")

    @bfac.setter
    def bfac(self, value: float) -> None:
        self._cards[0].set_value("bfac", value)

    @property
    def cfac(self) -> float:
        """Get or set the ALE smoothing weight factor for Isoparametric.
        """ # nopep8
        return self._cards[0].get_value("cfac")

    @cfac.setter
    def cfac(self, value: float) -> None:
        self._cards[0].set_value("cfac", value)

    @property
    def dfac(self) -> float:
        """Get or set the ALE smoothing weight factor for Equipotential.
        """ # nopep8
        return self._cards[0].get_value("dfac")

    @dfac.setter
    def dfac(self, value: float) -> None:
        self._cards[0].set_value("dfac", value)

    @property
    def efac(self) -> float:
        """Get or set the ALE smoothing weight factor for Equilibrium.
        """ # nopep8
        return self._cards[0].get_value("efac")

    @efac.setter
    def efac(self, value: float) -> None:
        self._cards[0].set_value("efac", value)

    @property
    def start(self) -> float:
        """Get or set the Start time for ALE smoothing (default = 0.0).
        """ # nopep8
        return self._cards[1].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        self._cards[1].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the END End time for ALE smoothing (default = 1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        self._cards[1].set_value("end", value)

    @property
    def aafac(self) -> float:
        """Get or set the ALE advection factor (donor cell options, default=1.0).This field is obsolete
        """ # nopep8
        return self._cards[1].get_value("aafac")

    @aafac.setter
    def aafac(self, value: float) -> None:
        self._cards[1].set_value("aafac", value)

    @property
    def vfact(self) -> float:
        """Get or set the Void factor. This is the definition of a void. A void is obtained by multiplying the time zero density of an element by a factor called the void factor (default = 1.0E-06).
        """ # nopep8
        return self._cards[1].get_value("vfact")

    @vfact.setter
    def vfact(self, value: float) -> None:
        self._cards[1].set_value("vfact", value)

    @property
    def prit(self) -> int:
        """Get or set the A floag to turn on or off the pressure equilibrium iteration option for multimaterial elements.
        EQ. 0. Off (default)
        EQ. 1. On
        """ # nopep8
        return self._cards[1].get_value("prit")

    @prit.setter
    def prit(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""prit must be one of {0,1}""")
        self._cards[1].set_value("prit", value)

    @property
    def ebc(self) -> int:
        """Get or set the Automatic Euler boundary condition:
        EQ.0: off (default),
        EQ.1: on with stick condition,
        EQ.2: on with slip condition.
        """ # nopep8
        return self._cards[1].get_value("ebc")

    @ebc.setter
    def ebc(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ebc must be one of {0,1,2}""")
        self._cards[1].set_value("ebc", value)

    @property
    def pref(self) -> float:
        """Get or set the A pseudo reference pressure equivalent to an environmental pressure that is being applied to the free surfaces of the ALE domain or meh
        """ # nopep8
        return self._cards[1].get_value("pref")

    @pref.setter
    def pref(self, value: float) -> None:
        self._cards[1].set_value("pref", value)

    @property
    def nsidebc(self) -> typing.Optional[int]:
        """Get or set the A node set ID (NSID) which is to be excluded from the EBC constraint.
        """ # nopep8
        return self._cards[1].get_value("nsidebc")

    @nsidebc.setter
    def nsidebc(self, value: int) -> None:
        self._cards[1].set_value("nsidebc", value)

    @property
    def ncpl(self) -> int:
        """Get or set the Number of Lagrangian cycles between coupling calculations.  This is typically done every cycle; therefore, its default is 1.  This is on optional card 3.
        """ # nopep8
        return self._cards[2].get_value("ncpl")

    @ncpl.setter
    def ncpl(self, value: int) -> None:
        self._cards[2].set_value("ncpl", value)

    @property
    def nbkt(self) -> int:
        """Get or set the Number of Lagrangian cycles between global bucket-sort searches to locate the position of the Lagrangian structure (mesh) relative to the ALE fluid (mesh).  Default is 50.  This is on optional card 3.
        """ # nopep8
        return self._cards[2].get_value("nbkt")

    @nbkt.setter
    def nbkt(self, value: int) -> None:
        self._cards[2].set_value("nbkt", value)

    @property
    def imascl(self) -> int:
        """Get or set the A flag for turning ON/OFF mass scaling for ALE parts.  The global mass scaling control (parameter DT2MS under *CONTROL_ TIMESTEP card) must be ON.  If the run dt is lower than the mass scaling dt, then IMASCL has the following effects:
        EQ.0: (Default) No mass scaling for ALE parts.  Print out maximum 20 warnings.
        EQ.1: No mass scaling for ALE parts.  Stop the run.
        EQ.2: Do mass scaling for ALE parts (the result may not be correct due to this scaling)
        """ # nopep8
        return self._cards[2].get_value("imascl")

    @imascl.setter
    def imascl(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""imascl must be one of {0,1,2}""")
        self._cards[2].set_value("imascl", value)

    @property
    def checkr(self) -> float:
        """Get or set the A parameter for reducing or eliminating an ALE pressure locking pattern.  It may range from 0.01 to 0.1.
        """ # nopep8
        return self._cards[2].get_value("checkr")

    @checkr.setter
    def checkr(self, value: float) -> None:
        self._cards[2].set_value("checkr", value)

    @property
    def beamin(self) -> float:
        """Get or set the Flag to align the dynamics of plain strain and axisymmetric
        beams in 2D FSI ALE models to their shell counterparts in 3D FSI ALE models:
        EQ.0.0: Off (default)
        EQ.1.0: On
        """ # nopep8
        return self._cards[2].get_value("beamin")

    @beamin.setter
    def beamin(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""beamin must be one of {0.0,1.0}""")
        self._cards[2].set_value("beamin", value)

    @property
    def mmgpref(self) -> int:
        """Get or set the A flag to select the method for assigning a reference pressure to multiple ALE multi-material groups (see Remark 3).
        EQ.0: OFF(default) PREF applies to every AMMG in the model.
        LT.0 : Then | MMGPREF | is an ID of either(a) a curve defined via DEFINE_CURVE or (b)a table defined via DEFINE_TABLE.Since by convention Tables and Load curves may not share common ID's, there will be no confusion about which ID is to be read
        """ # nopep8
        return self._cards[2].get_value("mmgpref")

    @mmgpref.setter
    def mmgpref(self, value: int) -> None:
        self._cards[2].set_value("mmgpref", value)

    @property
    def pdifmx(self) -> float:
        """Get or set the Maximum of pressure difference between neighboring ALE
        elements under which the stresses are zeroed out:
        EQ.0: Off (default)
        GT.0: On
        """ # nopep8
        return self._cards[2].get_value("pdifmx")

    @pdifmx.setter
    def pdifmx(self, value: float) -> None:
        self._cards[2].set_value("pdifmx", value)

    @property
    def dtmufac(self) -> float:
        """Get or set the Scale a time step called DTMU that depends on the dynamic viscosity
        DTMU is emitted by the element to the solver as an element time
        step, thereby making DTMU an upper bound on the global time step.
        EQ.0: Off (default)
        GT.0: On.
        """ # nopep8
        return self._cards[2].get_value("dtmufac")

    @dtmufac.setter
    def dtmufac(self, value: float) -> None:
        self._cards[2].set_value("dtmufac", value)

    @property
    def optimpp(self) -> int:
        """Get or set the Optimize the MPP communications in the penalty coupling
        (*CONSTRAINED_LAGRANGE_IN_SOLID, CTYPE = 4) and
        group ALE parts together for the element processing.
        EQ.0: Off (default)
        EQ.1: On.
        """ # nopep8
        return self._cards[3].get_value("optimpp")

    @optimpp.setter
    def optimpp(self, value: int) -> None:
        self._cards[3].set_value("optimpp", value)

    @property
    def ialedr(self) -> int:
        """Get or set the Include ALE computations in the dynamic relaxation analysis (*CONTROL_DYNAMIC_RELAXATION).
        EQ.0:	Off (default)
        EQ.1:	On.
        """ # nopep8
        return self._cards[3].get_value("ialedr")

    @ialedr.setter
    def ialedr(self, value: int) -> None:
        self._cards[3].set_value("ialedr", value)

    @property
    def bndflx(self) -> int:
        """Get or set the Multi-Material ALE group set ID selecting only the materials in elements at mesh boundaries with influxes that can flow in. By default, when the flow is inwards at boundary faces of ALE elements, every materials in these elements flow in. This option can select only a few of these ALE groups.
        EQ.0:	Off (default)
        GT.0:	*SET_MULTI-MATERIAL_GROUP_LIST ID
        EQ.-1: No influx.
        """ # nopep8
        return self._cards[3].get_value("bndflx")

    @bndflx.setter
    def bndflx(self, value: int) -> None:
        self._cards[3].set_value("bndflx", value)

    @property
    def minmas(self) -> float:
        """Get or set the Factor of the minimum mass allowed in an element: MINMAS*initial_density*element_volume.
        """ # nopep8
        return self._cards[3].get_value("minmas")

    @minmas.setter
    def minmas(self, value: float) -> None:
        self._cards[3].set_value("minmas", value)

