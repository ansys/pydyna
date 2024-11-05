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

class ControlTimeStep(KeywordBase):
    """DYNA CONTROL_TIME_STEP keyword"""

    keyword = "CONTROL"
    subkeyword = "TIMESTEP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dtinit",
                        float,
                        0,
                        10,
                        kwargs.get("dtinit", 0)
                    ),
                    Field(
                        "tssfac",
                        float,
                        10,
                        10,
                        kwargs.get("tssfac", 0.0)
                    ),
                    Field(
                        "isdo",
                        int,
                        20,
                        10,
                        kwargs.get("isdo", 0)
                    ),
                    Field(
                        "tslimt",
                        float,
                        30,
                        10,
                        kwargs.get("tslimt", 0.0)
                    ),
                    Field(
                        "dt2ms",
                        float,
                        40,
                        10,
                        kwargs.get("dt2ms", 0.0)
                    ),
                    Field(
                        "lctm",
                        int,
                        50,
                        10,
                        kwargs.get("lctm", 0)
                    ),
                    Field(
                        "erode",
                        int,
                        60,
                        10,
                        kwargs.get("erode", 0)
                    ),
                    Field(
                        "ms1st",
                        int,
                        70,
                        10,
                        kwargs.get("ms1st", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dt2msf",
                        float,
                        0,
                        10,
                        kwargs.get("dt2msf")
                    ),
                    Field(
                        "dt2mslc",
                        int,
                        10,
                        10,
                        kwargs.get("dt2mslc")
                    ),
                    Field(
                        "imscl",
                        int,
                        20,
                        10,
                        kwargs.get("imscl")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "rmscl",
                        float,
                        50,
                        10,
                        kwargs.get("rmscl", 0.0)
                    ),
                    Field(
                        "emscl",
                        float,
                        60,
                        10,
                        kwargs.get("emscl", 0.0)
                    ),
                    Field(
                        "ihdo",
                        int,
                        70,
                        10,
                        kwargs.get("ihdo", 0)
                    ),
                ],
            ),
        ]

    @property
    def dtinit(self) -> float:
        """Get or set the Initial time step size:
        EQ.0.0: LS-DYNA determines initial step size (default).
        """ # nopep8
        return self._cards[0].get_value("dtinit")

    @dtinit.setter
    def dtinit(self, value: float) -> None:
        self._cards[0].set_value("dtinit", value)

    @property
    def tssfac(self) -> float:
        """Get or set the Scale factor for computed time step.
        LT.0:	|TSSFAC| is the load curve or function defining the scale factor as a function of time.
        """ # nopep8
        return self._cards[0].get_value("tssfac")

    @tssfac.setter
    def tssfac(self, value: float) -> None:
        self._cards[0].set_value("tssfac", value)

    @property
    def isdo(self) -> int:
        """Get or set the Basis of time size calculation.
        EQ.0: characteristic length=area/(longest side),
        EQ.1: characteristic length=area/(longest diagonal),
        EQ.2: based on bar wave speed and MAX [shortest side, area/longest side].
        """ # nopep8
        return self._cards[0].get_value("isdo")

    @isdo.setter
    def isdo(self, value: int) -> None:
        self._cards[0].set_value("isdo", value)

    @property
    def tslimt(self) -> float:
        """Get or set the Shell element minimum time step assignment. Applies only to *MAT_PLASTIC_KINEMATIC, *MAT_PONER_LAW_PLASTICITY, *MAT_STRAIN_RATE_DEPENDENT_PLASTICITY, *MAT_PIECE-WISE_LINEAR_PLASTICITY.
        """ # nopep8
        return self._cards[0].get_value("tslimt")

    @tslimt.setter
    def tslimt(self, value: float) -> None:
        self._cards[0].set_value("tslimt", value)

    @property
    def dt2ms(self) -> float:
        """Get or set the Time step size for mass scaled solutions (default set to 0.0).
        """ # nopep8
        return self._cards[0].get_value("dt2ms")

    @dt2ms.setter
    def dt2ms(self, value: float) -> None:
        self._cards[0].set_value("dt2ms", value)

    @property
    def lctm(self) -> int:
        """Get or set the Load curve ID that limits the maximum time step size (optional).
        """ # nopep8
        return self._cards[0].get_value("lctm")

    @lctm.setter
    def lctm(self, value: int) -> None:
        self._cards[0].set_value("lctm", value)

    @property
    def erode(self) -> int:
        """Get or set the Erosion flag for elements with small time step.  See Remark 5.
        EQ.0:	calculation will terminate if the solution time step drops to  (see *CONTROL_‌TERMINATION).
        EQ.1:	solid elements or thick shell elements that cause the time step to drop to  will erode; similarly, SPH particles that cause the time step to drop will be deactivated.
        EQ.10:	shell elements with time step below  will erode.
        EQ.11:	same as ERODE = 1 but shell elements will also erode
        EQ.100:	beam elements with time step below  will erode.
        EQ.101:	same as ERODE = 1 but beam elements will also erode
        EQ.110:	beam and shell elements will erode.
        EQ.111:	same as ERODE = 1 but beam and shell elements will also erode
        """ # nopep8
        return self._cards[0].get_value("erode")

    @erode.setter
    def erode(self, value: int) -> None:
        if value not in [0, 1, 10, 11, 100, 101, 110, 111]:
            raise Exception("""erode must be one of {0,1,10,11,100,101,110,111}""")
        self._cards[0].set_value("erode", value)

    @property
    def ms1st(self) -> int:
        """Get or set the Limit mass scaling to the first step and fix the mass vector according to the time steps. The time step will not be fixed but may drop during the calculation from the specified minimum:
        EQ.0: no,
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("ms1st")

    @ms1st.setter
    def ms1st(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ms1st must be one of {0,1}""")
        self._cards[0].set_value("ms1st", value)

    @property
    def dt2msf(self) -> typing.Optional[float]:
        """Get or set the Reduction factor for initial time step size to determine the minimum time step size permitted.
        """ # nopep8
        return self._cards[1].get_value("dt2msf")

    @dt2msf.setter
    def dt2msf(self, value: float) -> None:
        self._cards[1].set_value("dt2msf", value)

    @property
    def dt2mslc(self) -> typing.Optional[int]:
        """Get or set the Load curve specifying DT2MS as a function of time during the explicit solutions phase. The load curve can only be used for increasing the magnitude of DT2MS. Consequently, the magnitude of DT2MS is taken as the maximum of the current value and the value from the load curve.
        """ # nopep8
        return self._cards[1].get_value("dt2mslc")

    @dt2mslc.setter
    def dt2mslc(self, value: int) -> None:
        self._cards[1].set_value("dt2mslc", value)

    @property
    def imscl(self) -> typing.Optional[int]:
        """Get or set the Flag for selective mass scaling if and only if mass scaling active.  Selective mass scaling does not scale the rigid body mass and is therefore more accurate.  Since it is memory and CPU intensive, it should be applied only to small finely meshed parts.  This option is available starting with the third revision of version 971.
        EQ.0: no selective mass scaling.
        EQ.1: all parts undergo selective mass scaling.
        LT.0: recommended.  |IMSCL| is the part set ID of the parts that undergo selective mass scaling; all other parts are mass scaled the usual way
        """ # nopep8
        return self._cards[1].get_value("imscl")

    @imscl.setter
    def imscl(self, value: int) -> None:
        self._cards[1].set_value("imscl", value)

    @property
    def rmscl(self) -> float:
        """Get or set the Flag for using rotational option in selective mass scaling
        EQ.0.: Only translational inertia are selectively mass scaled
        NE.0.: Both translational and rotational inertia are selectively mass scaled.
        """ # nopep8
        return self._cards[1].get_value("rmscl")

    @rmscl.setter
    def rmscl(self, value: float) -> None:
        self._cards[1].set_value("rmscl", value)

    @property
    def emscl(self) -> float:
        """Get or set the Fraction of added mass from mass scaling that contributes to gravity loads, in addition to the physical mass. See also *LOAD_BODY, this number should be between 0 and 1.
        """ # nopep8
        return self._cards[1].get_value("emscl")

    @emscl.setter
    def emscl(self, value: float) -> None:
        self._cards[1].set_value("emscl", value)

    @property
    def ihdo(self) -> int:
        """Get or set the Method for calculating solid element time steps:
        EQ.0:	default method
        EQ.1:	modified method to improve time step continuity.
        """ # nopep8
        return self._cards[1].get_value("ihdo")

    @ihdo.setter
    def ihdo(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ihdo must be one of {0,1}""")
        self._cards[1].set_value("ihdo", value)

