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

"""Module providing the ControlImplicitGeneral class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLIMPLICITGENERAL_CARD0 = (
    FieldSchema("imflag", int, 0, 10, 0),
    FieldSchema("dt0", float, 10, 10, None),
    FieldSchema("imform", int, 20, 10, 2),
    FieldSchema("nsbs", int, 30, 10, 1),
    FieldSchema("igs", int, 40, 10, 2),
    FieldSchema("cnstn", int, 50, 10, 0),
    FieldSchema("form", int, 60, 10, 0),
    FieldSchema("zero_v", int, 70, 10, 0),
)

class ControlImplicitGeneral(KeywordBase):
    """DYNA CONTROL_IMPLICIT_GENERAL keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_GENERAL"

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitGeneral class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITGENERAL_CARD0,
                **kwargs,
            ),        ]
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
        """Set the imflag property."""
        self._cards[0].set_value("imflag", value)

    @property
    def dt0(self) -> typing.Optional[float]:
        """Get or set the Initial time step size for implicit analysis.  See Remarks 2 and 5.
        LT.0:	eliminate negative principal stresses in geometric(initial stress) stiffness.Initial time step is |DT0|.
        """ # nopep8
        return self._cards[0].get_value("dt0")

    @dt0.setter
    def dt0(self, value: float) -> None:
        """Set the dt0 property."""
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
        """Set the imform property."""
        if value not in [2, 1, None]:
            raise Exception("""imform must be `None` or one of {2,1}.""")
        self._cards[0].set_value("imform", value)

    @property
    def nsbs(self) -> int:
        """Get or set the Number of steps in nonlinear springback (default = 1).
        """ # nopep8
        return self._cards[0].get_value("nsbs")

    @nsbs.setter
    def nsbs(self, value: int) -> None:
        """Set the nsbs property."""
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
        """Set the igs property."""
        if value not in [2, 1, None]:
            raise Exception("""igs must be `None` or one of {2,1}.""")
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
        """Set the cnstn property."""
        if value not in [0, 1, None]:
            raise Exception("""cnstn must be `None` or one of {0,1}.""")
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
        """Set the form property."""
        if value not in [0, 1, None]:
            raise Exception("""form must be `None` or one of {0,1}.""")
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
        """Set the zero_v property."""
        if value not in [0, 1, None]:
            raise Exception("""zero_v must be `None` or one of {0,1}.""")
        self._cards[0].set_value("zero_v", value)

