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

class ControlImplicitGeneralSpr(KeywordBase):
    """DYNA CONTROL_IMPLICIT_GENERAL_SPR keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_GENERAL_SPR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "imflag",
                        int,
                        0,
                        10,
                        kwargs.get("imflag", 0)
                    ),
                    Field(
                        "dt0",
                        float,
                        10,
                        10,
                        kwargs.get("dt0")
                    ),
                    Field(
                        "imform",
                        int,
                        20,
                        10,
                        kwargs.get("imform", 2)
                    ),
                    Field(
                        "nsbs",
                        int,
                        30,
                        10,
                        kwargs.get("nsbs", 1)
                    ),
                    Field(
                        "igs",
                        int,
                        40,
                        10,
                        kwargs.get("igs", 2)
                    ),
                    Field(
                        "cnstn",
                        int,
                        50,
                        10,
                        kwargs.get("cnstn", 0)
                    ),
                    Field(
                        "form",
                        int,
                        60,
                        10,
                        kwargs.get("form", 0)
                    ),
                    Field(
                        "zero_v",
                        int,
                        70,
                        10,
                        kwargs.get("zero_v", 0)
                    ),
                ],
            ),
        ]

    @property
    def imflag(self) -> int:
        """Get or set the Implicit/Explicit switching flag
        EQ.0: explicit analysis (default)
        EQ.1: implicit analysis
        EQ.2: explicit followed by one implicit step (springback analysis)
        EQ.4: implicit with automatic implicit-explicit switching
        EQ.5: implicit with automatic switching and mandatory implicit finish
        EQ.6: explicit with intermittent eigenvalue extraction
        EQ.-n: curve ID=n gives IMFLAG as a function of time.
        """ # nopep8
        return self._cards[0].get_value("imflag")

    @imflag.setter
    def imflag(self, value: int) -> None:
        self._cards[0].set_value("imflag", value)

    @property
    def dt0(self) -> typing.Optional[float]:
        """Get or set the Initial time step size for implicit analysis.  See Remarks 2 and 5.
        LT.0:	eliminate negative principal stresses in geometric(initial stress) stiffness.Initial time step is |DT0|.
        """ # nopep8
        return self._cards[0].get_value("dt0")

    @dt0.setter
    def dt0(self, value: float) -> None:
        self._cards[0].set_value("dt0", value)

    @property
    def imform(self) -> int:
        """Get or set the Element formulation switching flag
        EQ.1: switch to fully integrated formulation for implicit springback
        EQ.2: retain original element formulation (default).
        """ # nopep8
        return self._cards[0].get_value("imform")

    @imform.setter
    def imform(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""imform must be one of {2,1}""")
        self._cards[0].set_value("imform", value)

    @property
    def nsbs(self) -> int:
        """Get or set the Number of steps in nonlinear springback (default = 1).
        """ # nopep8
        return self._cards[0].get_value("nsbs")

    @nsbs.setter
    def nsbs(self, value: int) -> None:
        self._cards[0].set_value("nsbs", value)

    @property
    def igs(self) -> int:
        """Get or set the Geometric (initial stress) stiffness flag
        EQ.2: ignore(default)
        EQ.1: include
        LT.0:	include on part set |IGS|
        """ # nopep8
        return self._cards[0].get_value("igs")

    @igs.setter
    def igs(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""igs must be one of {2,1}""")
        self._cards[0].set_value("igs", value)

    @property
    def cnstn(self) -> int:
        """Get or set the Indicator for consistent tangent stiffness:
        EQ.0: do not use (default)
        EQ.1: use.
        """ # nopep8
        return self._cards[0].get_value("cnstn")

    @cnstn.setter
    def cnstn(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cnstn must be one of {0,1}""")
        self._cards[0].set_value("cnstn", value)

    @property
    def form(self) -> int:
        """Get or set the Element formulation when using IMFORM flag.
        EQ.0: type 16
        EQ.1: type 6.
        """ # nopep8
        return self._cards[0].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""form must be one of {0,1}""")
        self._cards[0].set_value("form", value)

    @property
    def zero_v(self) -> int:
        """Get or set the Zero out the velocity before switching from explicit to implicit.
        EQ.0: The velocities are not zeroed out.
        EQ.1: The velocities are set to zero.
        """ # nopep8
        return self._cards[0].get_value("zero_v")

    @zero_v.setter
    def zero_v(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""zero_v must be one of {0,1}""")
        self._cards[0].set_value("zero_v", value)

