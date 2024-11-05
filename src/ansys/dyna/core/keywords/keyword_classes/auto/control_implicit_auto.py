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

class ControlImplicitAuto(KeywordBase):
    """DYNA CONTROL_IMPLICIT_AUTO keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_AUTO"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "iauto",
                        int,
                        0,
                        10,
                        kwargs.get("iauto", 0)
                    ),
                    Field(
                        "iteopt",
                        int,
                        10,
                        10,
                        kwargs.get("iteopt", 11)
                    ),
                    Field(
                        "itewin",
                        int,
                        20,
                        10,
                        kwargs.get("itewin", 5)
                    ),
                    Field(
                        "dtmin",
                        float,
                        30,
                        10,
                        kwargs.get("dtmin")
                    ),
                    Field(
                        "dtmax",
                        float,
                        40,
                        10,
                        kwargs.get("dtmax")
                    ),
                    Field(
                        "dtexp",
                        float,
                        50,
                        10,
                        kwargs.get("dtexp")
                    ),
                    Field(
                        "kfail",
                        int,
                        60,
                        10,
                        kwargs.get("kfail")
                    ),
                    Field(
                        "kcycle",
                        int,
                        70,
                        10,
                        kwargs.get("kcycle")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hcmin",
                        float,
                        0,
                        10,
                        kwargs.get("hcmin")
                    ),
                    Field(
                        "hcmax",
                        float,
                        10,
                        10,
                        kwargs.get("hcmax")
                    ),
                    Field(
                        "hmmin",
                        float,
                        20,
                        10,
                        kwargs.get("hmmin")
                    ),
                    Field(
                        "hmmax",
                        float,
                        30,
                        10,
                        kwargs.get("hmmax")
                    ),
                    Field(
                        "hntmax",
                        float,
                        40,
                        10,
                        kwargs.get("hntmax")
                    ),
                    Field(
                        "hnrmax",
                        float,
                        50,
                        10,
                        kwargs.get("hnrmax")
                    ),
                    Field(
                        "hrtmax",
                        float,
                        60,
                        10,
                        kwargs.get("hrtmax")
                    ),
                    Field(
                        "hrrmax",
                        float,
                        70,
                        10,
                        kwargs.get("hrrmax")
                    ),
                ],
                lambda: self.iauto == 3,
            ),
        ]

    @property
    def iauto(self) -> int:
        """Get or set the Automatic time step control flag
        EQ.0:	constant time step size
        EQ.1 : automatically adjust time step size
        EQ.2 : automatically adjust time step size and synchronize with thermal mechanical time step.
        EQ.3 : same as 1, but accounting for mid step residual values with respect to parameters on card 2 and according to the Remark for IAUTO.
        LT.0 : Curve ID = (-IAUTO) gives time step size as a function of time.If specified, DTMIN and DTMAX will still be applied
        """ # nopep8
        return self._cards[0].get_value("iauto")

    @iauto.setter
    def iauto(self, value: int) -> None:
        self._cards[0].set_value("iauto", value)

    @property
    def iteopt(self) -> int:
        """Get or set the Optimum equilibrium iteration count per time step
        """ # nopep8
        return self._cards[0].get_value("iteopt")

    @iteopt.setter
    def iteopt(self, value: int) -> None:
        self._cards[0].set_value("iteopt", value)

    @property
    def itewin(self) -> int:
        """Get or set the Allowable iteration window. If iteration count is within ITEWIN iterations of ITEOPT, step size will not be adjusted.
        """ # nopep8
        return self._cards[0].get_value("itewin")

    @itewin.setter
    def itewin(self, value: int) -> None:
        self._cards[0].set_value("itewin", value)

    @property
    def dtmin(self) -> typing.Optional[float]:
        """Get or set the Minimum allowable time step size.  Simulation stops with error termination if time step falls below DTMIN.
        LT.0:	enable automatic key point generation.Minimum allowable time step is |DTMIN|.
        """ # nopep8
        return self._cards[0].get_value("dtmin")

    @dtmin.setter
    def dtmin(self, value: float) -> None:
        self._cards[0].set_value("dtmin", value)

    @property
    def dtmax(self) -> typing.Optional[float]:
        """Get or set the Maximum allowable time step size (default = DT*10).
        """ # nopep8
        return self._cards[0].get_value("dtmax")

    @dtmax.setter
    def dtmax(self, value: float) -> None:
        self._cards[0].set_value("dtmax", value)

    @property
    def dtexp(self) -> typing.Optional[float]:
        """Get or set the Time interval to run in explicit mode before returning to implicit mode.
        Applies only when automatic implicit-explicit switching is active (IMFLAG= 4 or 5 on *CONTROL_IMPLICIT_GENERAL).  Also, see KCYCLE.
        EQ.0:	defaults to the current implicit time step size.
        LT.0 : curve ID = (-DTEXP) gives the time interval as a function of time.
        """ # nopep8
        return self._cards[0].get_value("dtexp")

    @dtexp.setter
    def dtexp(self, value: float) -> None:
        self._cards[0].set_value("dtexp", value)

    @property
    def kfail(self) -> typing.Optional[int]:
        """Get or set the Number of failed attempts to converge implicitly for the current time
        step before automatically switching to explicit time integration.
        Applies only when automatic implicit-explicit switching is active. The
        default is one attempt. If IAUTO = 0, any input value is reset to unity
        """ # nopep8
        return self._cards[0].get_value("kfail")

    @kfail.setter
    def kfail(self, value: int) -> None:
        self._cards[0].set_value("kfail", value)

    @property
    def kcycle(self) -> typing.Optional[int]:
        """Get or set the Number of explicit cycles to run in explicit mode before returning to
        the implicit mode. The actual time interval that is used will be the
        maximum between DTEXP and KCYCLE*(latest estimate of the explicit time step size).
        """ # nopep8
        return self._cards[0].get_value("kcycle")

    @kcycle.setter
    def kcycle(self, value: int) -> None:
        self._cards[0].set_value("kcycle", value)

    @property
    def hcmin(self) -> typing.Optional[float]:
        """Get or set the Mid-point relative Euclidian residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
        Only active if RCTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
        """ # nopep8
        return self._cards[1].get_value("hcmin")

    @hcmin.setter
    def hcmin(self, value: float) -> None:
        self._cards[1].set_value("hcmin", value)

    @property
    def hcmax(self) -> typing.Optional[float]:
        """Get or set the Mid-point relative Euclidian residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
        Only active if RCTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
        """ # nopep8
        return self._cards[1].get_value("hcmax")

    @hcmax.setter
    def hcmax(self, value: float) -> None:
        self._cards[1].set_value("hcmax", value)

    @property
    def hmmin(self) -> typing.Optional[float]:
        """Get or set the Mid-point relative maximum residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
        Only active if RMTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
        """ # nopep8
        return self._cards[1].get_value("hmmin")

    @hmmin.setter
    def hmmin(self, value: float) -> None:
        self._cards[1].set_value("hmmin", value)

    @property
    def hmmax(self) -> typing.Optional[float]:
        """Get or set the Mid-point relative maximum residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
        Only active if RMTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
        """ # nopep8
        return self._cards[1].get_value("hmmax")

    @hmmax.setter
    def hmmax(self, value: float) -> None:
        self._cards[1].set_value("hmmax", value)

    @property
    def hntmax(self) -> typing.Optional[float]:
        """Get or set the Mid-point absolute Nodal Translational norm tolerance, see Remark for IAUTO = 3.
        Only active if NTTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
        """ # nopep8
        return self._cards[1].get_value("hntmax")

    @hntmax.setter
    def hntmax(self, value: float) -> None:
        self._cards[1].set_value("hntmax", value)

    @property
    def hnrmax(self) -> typing.Optional[float]:
        """Get or set the Mid-point absolute Nodal Rotational norm tolerance, see Remark for IAUTO = 3.
        Only active if NRTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
        """ # nopep8
        return self._cards[1].get_value("hnrmax")

    @hnrmax.setter
    def hnrmax(self, value: float) -> None:
        self._cards[1].set_value("hnrmax", value)

    @property
    def hrtmax(self) -> typing.Optional[float]:
        """Get or set the Mid-point absolute Rigid body Translational norm tolerance, see Remark for IAUTO = 3.
        Only active if RTTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
        """ # nopep8
        return self._cards[1].get_value("hrtmax")

    @hrtmax.setter
    def hrtmax(self, value: float) -> None:
        self._cards[1].set_value("hrtmax", value)

    @property
    def hrrmax(self) -> typing.Optional[float]:
        """Get or set the Mid-point absolute Rigid body Rotational norm tolerance, see Remark for IAUTO=3.
        Only active if RRTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
        """ # nopep8
        return self._cards[1].get_value("hrrmax")

    @hrrmax.setter
    def hrrmax(self, value: float) -> None:
        self._cards[1].set_value("hrrmax", value)

