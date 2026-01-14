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

"""Module providing the ControlImplicitEigenvalues class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLIMPLICITEIGENVALUES_CARD0 = (
    FieldSchema("neig", int, 0, 10, 0),
    FieldSchema("center", float, 10, 10, 0.0),
    FieldSchema("lflag", int, 20, 10, 0),
    FieldSchema("lftend", float, 30, 10, -1e+29),
    FieldSchema("rflag", int, 40, 10, 0),
    FieldSchema("rhtend", float, 50, 10, 1e+29),
    FieldSchema("eigmth", int, 60, 10, 2),
    FieldSchema("shfscl", float, 70, 10, 0.0),
)

_CONTROLIMPLICITEIGENVALUES_CARD1 = (
    FieldSchema("isolid", int, 0, 10, 0),
    FieldSchema("ibeam", int, 10, 10, 0),
    FieldSchema("ishell", int, 20, 10, 0),
    FieldSchema("itshell", int, 30, 10, 0),
    FieldSchema("mstres", int, 40, 10, 0),
    FieldSchema("evdump", int, 50, 10, None),
    FieldSchema("mstrscl", float, 60, 10, 0.001),
)

class ControlImplicitEigenvalues(KeywordBase):
    """DYNA CONTROL_IMPLICIT_EIGENVALUES keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_EIGENVALUES"

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitEigenvalues class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD1,
                **kwargs,
            ),        ]
    @property
    def neig(self) -> int:
        """Get or set the Number of eigenvalues to extract. This must be specified. The other parameters below are optional.
        LT.0: curve ID = (-NEIG) used for intermittent eigenvalue analysis
        """ # nopep8
        return self._cards[0].get_value("neig")

    @neig.setter
    def neig(self, value: int) -> None:
        """Set the neig property."""
        self._cards[0].set_value("neig", value)

    @property
    def center(self) -> float:
        """Get or set the Center frequency. This option finds the nearest NEIG eigenvalues located about this value.
        """ # nopep8
        return self._cards[0].get_value("center")

    @center.setter
    def center(self, value: float) -> None:
        """Set the center property."""
        self._cards[0].set_value("center", value)

    @property
    def lflag(self) -> int:
        """Get or set the Left end point finite flag.
        EQ.0: left end point is -infinity
        EQ.1: left end point is LFTEND.
        """ # nopep8
        return self._cards[0].get_value("lflag")

    @lflag.setter
    def lflag(self, value: int) -> None:
        """Set the lflag property."""
        if value not in [0, 1, None]:
            raise Exception("""lflag must be `None` or one of {0,1}.""")
        self._cards[0].set_value("lflag", value)

    @property
    def lftend(self) -> float:
        """Get or set the Left end point of interval. Only used when LFLAG = 1.
        """ # nopep8
        return self._cards[0].get_value("lftend")

    @lftend.setter
    def lftend(self, value: float) -> None:
        """Set the lftend property."""
        self._cards[0].set_value("lftend", value)

    @property
    def rflag(self) -> int:
        """Get or set the Right end point finite flag:
        EQ.0: right end point is +infinity
        EQ.1: right end point is RHTEND.
        """ # nopep8
        return self._cards[0].get_value("rflag")

    @rflag.setter
    def rflag(self, value: int) -> None:
        """Set the rflag property."""
        if value not in [0, 1, None]:
            raise Exception("""rflag must be `None` or one of {0,1}.""")
        self._cards[0].set_value("rflag", value)

    @property
    def rhtend(self) -> float:
        """Get or set the Right end point of interval. Only used when RFLAG = 1.
        """ # nopep8
        return self._cards[0].get_value("rhtend")

    @rhtend.setter
    def rhtend(self, value: float) -> None:
        """Set the rhtend property."""
        self._cards[0].set_value("rhtend", value)

    @property
    def eigmth(self) -> int:
        """Get or set the Eigenvalue extraction method:
        EQ.2: Block Shift and Invert Lanczos (default).
        EQ.3: Lanczos with [M] = [I](for debug only).
        EQ.5: Same as 3 but include Dynamic Terms.
        EQ.6:	Same as 2 but include Dynamic Terms
        EQ.101:	MCMS.  See Remark 4.
        EQ.102:	LOBPCG.See Remark 5.
        EQ.111 : Sectoral Symmetry.See Remark 10
        """ # nopep8
        return self._cards[0].get_value("eigmth")

    @eigmth.setter
    def eigmth(self, value: int) -> None:
        """Set the eigmth property."""
        if value not in [2, 3, 5, 6, 101, 102, 111, None]:
            raise Exception("""eigmth must be `None` or one of {2,3,5,6,101,102,111}.""")
        self._cards[0].set_value("eigmth", value)

    @property
    def shfscl(self) -> float:
        """Get or set the Shift scale.
        """ # nopep8
        return self._cards[0].get_value("shfscl")

    @shfscl.setter
    def shfscl(self, value: float) -> None:
        """Set the shfscl property."""
        self._cards[0].set_value("shfscl", value)

    @property
    def isolid(self) -> int:
        """Get or set the If nonzero, reset all solid element formulations to ISOLID for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations..
        """ # nopep8
        return self._cards[1].get_value("isolid")

    @isolid.setter
    def isolid(self, value: int) -> None:
        """Set the isolid property."""
        self._cards[1].set_value("isolid", value)

    @property
    def ibeam(self) -> int:
        """Get or set the If nonzero, reset all beam element formulations to IBEAM for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
        """ # nopep8
        return self._cards[1].get_value("ibeam")

    @ibeam.setter
    def ibeam(self, value: int) -> None:
        """Set the ibeam property."""
        self._cards[1].set_value("ibeam", value)

    @property
    def ishell(self) -> int:
        """Get or set the If nonzero, reset all shell element formulations to ISHELL for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
        """ # nopep8
        return self._cards[1].get_value("ishell")

    @ishell.setter
    def ishell(self, value: int) -> None:
        """Set the ishell property."""
        self._cards[1].set_value("ishell", value)

    @property
    def itshell(self) -> int:
        """Get or set the If nonzero, reset all thick shell element formulations to ITSHELL for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
        """ # nopep8
        return self._cards[1].get_value("itshell")

    @itshell.setter
    def itshell(self, value: int) -> None:
        """Set the itshell property."""
        self._cards[1].set_value("itshell", value)

    @property
    def mstres(self) -> int:
        """Get or set the Flag for computing the stresses for the eigenmodes:
        EQ.0: Do not compute the stresses.
        EQ.1: Compute the stresses.
        """ # nopep8
        return self._cards[1].get_value("mstres")

    @mstres.setter
    def mstres(self, value: int) -> None:
        """Set the mstres property."""
        if value not in [0, 1, None]:
            raise Exception("""mstres must be `None` or one of {0,1}.""")
        self._cards[1].set_value("mstres", value)

    @property
    def evdump(self) -> typing.Optional[int]:
        """Get or set the Flag for writing eigenvalues and eigenvectors to file Eigen_Vectors(SMP only):
        EQ.0: Do not write eigenvalues and eigenvectors.
        GT.0: Write eigenvalues and eigenvectors using an ASCII format.
        LT.0: Write eigenvalues and eigenvectors using a binary format.
        """ # nopep8
        return self._cards[1].get_value("evdump")

    @evdump.setter
    def evdump(self, value: int) -> None:
        """Set the evdump property."""
        self._cards[1].set_value("evdump", value)

    @property
    def mstrscl(self) -> float:
        """Get or set the Scaling for computing the velocity based on the mode shape for the stress computation.
        """ # nopep8
        return self._cards[1].get_value("mstrscl")

    @mstrscl.setter
    def mstrscl(self, value: float) -> None:
        """Set the mstrscl property."""
        self._cards[1].set_value("mstrscl", value)

