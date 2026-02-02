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

"""Module providing the ControlImplicitEigenvalue class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_CONTROLIMPLICITEIGENVALUE_CARD0 = (
    FieldSchema("neig", int, 0, 10, 0),
    FieldSchema("center", float, 10, 10, 0.0),
    FieldSchema("lflag", int, 20, 10, 0),
    FieldSchema("lftend", float, 30, 10, -1e+29),
    FieldSchema("rflag", int, 40, 10, 0),
    FieldSchema("rhtend", float, 50, 10, 1e+29),
    FieldSchema("eigmth", int, 60, 10, 2),
    FieldSchema("shfscl", float, 70, 10, 0.0),
)

_CONTROLIMPLICITEIGENVALUE_CARD1 = (
    FieldSchema("isolid", int, 0, 10, 0),
    FieldSchema("ibeam", int, 10, 10, 0),
    FieldSchema("ishell", int, 20, 10, 0),
    FieldSchema("itshell", int, 30, 10, 0),
    FieldSchema("mstres", int, 40, 10, 0),
    FieldSchema("evdump", int, 50, 10, None),
    FieldSchema("mstrscl", float, 60, 10, 0.001),
)

_CONTROLIMPLICITEIGENVALUE_CARD2 = (
    FieldSchema("iparm1", int, 0, 10, 100),
    FieldSchema("iparm2", int, 10, 10, None),
    FieldSchema("iparm3", int, 20, 10, None),
    FieldSchema("iparm4", int, 30, 10, 1500),
    FieldSchema("rparm1", float, 40, 10, 4.0),
)

_CONTROLIMPLICITEIGENVALUE_CARD3 = (
    FieldSchema("iparm1", int, 0, 10, 100),
    FieldSchema("iparm2", int, 10, 10, 100),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("rparm1", int, 40, 10, 0),
    FieldSchema("rparm2", int, 50, 10, 0),
)

_CONTROLIMPLICITEIGENVALUE_CARD4 = (
    FieldSchema("iparm1", int, 0, 10, None),
    FieldSchema("iparm2", int, 10, 10, 0),
    FieldSchema("iparm3", int, 20, 10, None),
    FieldSchema("iparm4", int, 30, 10, None),
    FieldSchema("rparm5", int, 40, 10, None),
    FieldSchema("rparm6", int, 50, 10, 0),
)

class ControlImplicitEigenvalue(KeywordBase):
    """DYNA CONTROL_IMPLICIT_EIGENVALUE keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_EIGENVALUE"
    _link_fields = {
        "rparm6": LinkType.DEFINE_VECTOR,
        "iparm3": LinkType.SET_NODE,
        "iparm1": LinkType.SET_NODE,
        "iparm2": LinkType.SET_NODE,
        "iparm3": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitEigenvalue class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUE_CARD1,
                active_func=lambda: (self.eigmth in [101, 102, 111]) or (self.isolid != 0 or self.ibeam != 0 or self.ishell != 0 or self.itshell != 0 or self.mstres != 0 or self.evdump != None or self.mstrscl != 0.001),
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUE_CARD2,
                active_func=lambda: self.eigmth == 101,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUE_CARD3,
                active_func=lambda: self.eigmth == 102,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUE_CARD4,
                active_func=lambda: self.eigmth == 111,
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

    @property
    def iparm1(self) -> int:
        """Get or set the Minimum block size for the Cholesky factorization
        """ # nopep8
        return self._cards[2].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        """Set the iparm1 property."""
        self._cards[2].set_value("iparm1", value)

    @property
    def iparm2(self) -> typing.Optional[int]:
        """Get or set the Maximum block size for the Cholesky factorization.  Default is the model size
        """ # nopep8
        return self._cards[2].get_value("iparm2")

    @iparm2.setter
    def iparm2(self, value: int) -> None:
        """Set the iparm2 property."""
        self._cards[2].set_value("iparm2", value)

    @property
    def iparm3(self) -> typing.Optional[int]:
        """Get or set the Node set ID specifying special nodes in the model where increased accuracy is desired
        """ # nopep8
        return self._cards[2].get_value("iparm3")

    @iparm3.setter
    def iparm3(self, value: int) -> None:
        """Set the iparm3 property."""
        self._cards[2].set_value("iparm3", value)

    @property
    def iparm4(self) -> int:
        """Get or set the MCMS minimum group/substructure size
        """ # nopep8
        return self._cards[2].get_value("iparm4")

    @iparm4.setter
    def iparm4(self, value: int) -> None:
        """Set the iparm4 property."""
        self._cards[2].set_value("iparm4", value)

    @property
    def rparm1(self) -> float:
        """Get or set the Eigenvalue expansion factor
        """ # nopep8
        return self._cards[2].get_value("rparm1")

    @rparm1.setter
    def rparm1(self, value: float) -> None:
        """Set the rparm1 property."""
        self._cards[2].set_value("rparm1", value)

    @property
    def iparm1(self) -> int:
        """Get or set the Maximum number of iterations
        """ # nopep8
        return self._cards[3].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        """Set the iparm1 property."""
        self._cards[3].set_value("iparm1", value)

    @property
    def iparm2(self) -> int:
        """Get or set the Block size
        """ # nopep8
        return self._cards[3].get_value("iparm2")

    @iparm2.setter
    def iparm2(self, value: int) -> None:
        """Set the iparm2 property."""
        self._cards[3].set_value("iparm2", value)

    @property
    def rparm1(self) -> int:
        """Get or set the Convergence tolerance
        """ # nopep8
        return self._cards[3].get_value("rparm1")

    @rparm1.setter
    def rparm1(self, value: int) -> None:
        """Set the rparm1 property."""
        self._cards[3].set_value("rparm1", value)

    @property
    def rparm2(self) -> int:
        """Get or set the BLR preconditioner tolerance
        """ # nopep8
        return self._cards[3].get_value("rparm2")

    @rparm2.setter
    def rparm2(self, value: int) -> None:
        """Set the rparm2 property."""
        self._cards[3].set_value("rparm2", value)

    @property
    def iparm1(self) -> typing.Optional[int]:
        """Get or set the Node set ID for nodes on the left surface of the sector
        """ # nopep8
        return self._cards[4].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        """Set the iparm1 property."""
        self._cards[4].set_value("iparm1", value)

    @property
    def iparm2(self) -> int:
        """Get or set the Node set ID for nodes on the axis of rotation
        EQ.0:No nodes on the axis of rotation (default)
        """ # nopep8
        return self._cards[4].get_value("iparm2")

    @iparm2.setter
    def iparm2(self, value: int) -> None:
        """Set the iparm2 property."""
        self._cards[4].set_value("iparm2", value)

    @property
    def iparm3(self) -> typing.Optional[int]:
        """Get or set the Node set ID for nodes on the left surface of the sector
        """ # nopep8
        return self._cards[4].get_value("iparm3")

    @iparm3.setter
    def iparm3(self, value: int) -> None:
        """Set the iparm3 property."""
        self._cards[4].set_value("iparm3", value)

    @property
    def iparm4(self) -> typing.Optional[int]:
        """Get or set the Number of sectors
        """ # nopep8
        return self._cards[4].get_value("iparm4")

    @iparm4.setter
    def iparm4(self, value: int) -> None:
        """Set the iparm4 property."""
        self._cards[4].set_value("iparm4", value)

    @property
    def rparm5(self) -> typing.Optional[int]:
        """Get or set the Harmonic index
        """ # nopep8
        return self._cards[4].get_value("rparm5")

    @rparm5.setter
    def rparm5(self, value: int) -> None:
        """Set the rparm5 property."""
        self._cards[4].set_value("rparm5", value)

    @property
    def rparm6(self) -> int:
        """Get or set the Vector ID for the axis of rotation
        EQ.0 Axis of rotation if the global z-axis(default)
        """ # nopep8
        return self._cards[4].get_value("rparm6")

    @rparm6.setter
    def rparm6(self, value: int) -> None:
        """Set the rparm6 property."""
        self._cards[4].set_value("rparm6", value)

    @property
    def rparm6_link(self) -> DefineVector:
        """Get the DefineVector object for rparm6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.rparm6:
                return kwd
        return None

    @rparm6_link.setter
    def rparm6_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for rparm6."""
        self.rparm6 = value.vid

    @property
    def iparm3_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for iparm3."""
        return self._get_set_link("NODE", self.iparm3)

    @iparm3_link.setter
    def iparm3_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm3."""
        self.iparm3 = value.sid

    @property
    def iparm1_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for iparm1."""
        return self._get_set_link("NODE", self.iparm1)

    @iparm1_link.setter
    def iparm1_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm1."""
        self.iparm1 = value.sid

    @property
    def iparm2_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for iparm2."""
        return self._get_set_link("NODE", self.iparm2)

    @iparm2_link.setter
    def iparm2_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm2."""
        self.iparm2 = value.sid

    @property
    def iparm3_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for iparm3."""
        return self._get_set_link("NODE", self.iparm3)

    @iparm3_link.setter
    def iparm3_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm3."""
        self.iparm3 = value.sid

