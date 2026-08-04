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
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_CONTROLIMPLICITEIGENVALUES_CARD0 = (
    FieldSchema("neig", int, 0, 10, None),
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
    FieldSchema("evdump", int, 50, 10, 0),
    FieldSchema("mstrscl", float, 60, 10, 0.001),
)

_CONTROLIMPLICITEIGENVALUES_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("iparm3", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("rparm4", float, 70, 10, None),
)

_CONTROLIMPLICITEIGENVALUES_CARD3 = (
    FieldSchema("iparm1", int, 0, 10, 100),
    FieldSchema("iparm2", int, 10, 10, None),
    FieldSchema("iparm3", int, 20, 10, None),
    FieldSchema("iparm4", int, 30, 10, 1500),
    FieldSchema("rparm1", float, 40, 10, 4.0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("rparm4", float, 70, 10, 0.0),
)

_CONTROLIMPLICITEIGENVALUES_CARD4 = (
    FieldSchema("iparm1", int, 0, 10, 100),
    FieldSchema("iparm2", int, 10, 10, 100),
    FieldSchema("iparm3", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("rparm1", float, 40, 10, 1e-11),
    FieldSchema("rparm2", float, 50, 10, 0.0001),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("rparm4", float, 70, 10, 0.0),
)

_CONTROLIMPLICITEIGENVALUES_CARD5 = (
    FieldSchema("iparm1", int, 0, 10, 40),
    FieldSchema("iparm2", int, 10, 10, 40),
    FieldSchema("iparm3", int, 20, 10, 0),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("rparm1", float, 40, 10, 200.0),
    FieldSchema("rparm2", float, 50, 10, 0.8),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("rparm4", float, 70, 10, 0.0),
)

_CONTROLIMPLICITEIGENVALUES_CARD6 = (
    FieldSchema("iparm1", int, 0, 10, None),
    FieldSchema("iparm2", int, 10, 10, 0),
    FieldSchema("iparm3", int, 20, 10, None),
    FieldSchema("iparm4", int, 30, 10, None),
    FieldSchema("iparm5", int, 40, 10, None),
    FieldSchema("iparm6", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_CONTROLIMPLICITEIGENVALUES_CARD7 = (
    FieldSchema("rotscl", float, 0, 10, 0.001),
    FieldSchema("eigmscl", int, 10, 10, 0),
)

class ControlImplicitEigenvalues(KeywordBase):
    """DYNA CONTROL_IMPLICIT_EIGENVALUES keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_EIGENVALUES"
    _link_fields = {
        "iparm6": LinkType.DEFINE_VECTOR,
        "iparm3": LinkType.SET_NODE,
        "iparm3": LinkType.SET_NODE,
        "iparm3": LinkType.SET_NODE,
        "iparm1": LinkType.SET_NODE,
        "iparm2": LinkType.SET_NODE,
        "iparm3": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitEigenvalues class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITEIGENVALUES_CARD7,
                **kwargs,
            ),
        ]
    @property
    def neig(self) -> typing.Optional[int]:
        """Get or set the Number of eigenvalues to extract. This must be specified. The other parameters below are optional. See Remark 1.
        LT.0: curve ID = (-NEIG) used for intermittent eigenvalue analysis
        """ # nopep8
        return self._cards[0].get_value("neig")

    @neig.setter
    def neig(self, value: int) -> None:
        """Set the neig property."""
        self._cards[0].set_value("neig", value)

    @property
    def center(self) -> float:
        """Get or set the Center frequency. This option finds the nearest NEIG eigenvalues located about this value. See Remarks 2, 3, and 10.
        """ # nopep8
        return self._cards[0].get_value("center")

    @center.setter
    def center(self, value: float) -> None:
        """Set the center property."""
        self._cards[0].set_value("center", value)

    @property
    def lflag(self) -> int:
        """Get or set the Left end point finite flag. (see Remarks 2 and 3):
        EQ.0: Left end point is -infinity
        EQ.1: Left end point is LFTEND.
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
        """Get or set the Left end point of interval. Only used when LFLAG = 1. See Remarks 2, 3, and 10.
        """ # nopep8
        return self._cards[0].get_value("lftend")

    @lftend.setter
    def lftend(self, value: float) -> None:
        """Set the lftend property."""
        self._cards[0].set_value("lftend", value)

    @property
    def rflag(self) -> int:
        """Get or set the Right end point finite flag (see Remarks 2 and 3):
        EQ.0: Right end point is +infinity
        EQ.1: Right end point is RHTEND.
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
        """Get or set the Right end point of interval. Only used when RFLAG = 1. See Remarks 2, 3, and 10.
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
        EQ.6: Same as 2 but include Dynamic Terms
        EQ.101: MCMS. See Remark 4.
        EQ.102: LOBPCG. SeeLOBPCGSee Remark 5.
        EQ.103: Fast Lanczos.See Remark 6.
        EQ.111: Sectoral Symmetry.See Remark 10
        """ # nopep8
        return self._cards[0].get_value("eigmth")

    @eigmth.setter
    def eigmth(self, value: int) -> None:
        """Set the eigmth property."""
        if value not in [2, 3, 5, 6, 101, 102, 103, 111, None]:
            raise Exception("""eigmth must be `None` or one of {2,3,5,6,101,102,103,111}.""")
        self._cards[0].set_value("eigmth", value)

    @property
    def shfscl(self) -> float:
        """Get or set the Shift scale. Generally, not used.  See Remarks 3, 9, and 10.
        """ # nopep8
        return self._cards[0].get_value("shfscl")

    @shfscl.setter
    def shfscl(self, value: float) -> None:
        """Set the shfscl property."""
        self._cards[0].set_value("shfscl", value)

    @property
    def isolid(self) -> int:
        """Get or set the If nonzero, reset all solid element formulations to ISOLID for the implicit computations. Can be used for all implicit computations not just eigenvalue computations.
        """ # nopep8
        return self._cards[1].get_value("isolid")

    @isolid.setter
    def isolid(self, value: int) -> None:
        """Set the isolid property."""
        self._cards[1].set_value("isolid", value)

    @property
    def ibeam(self) -> int:
        """Get or set the If nonzero, reset all beam element formulations to IBEAM for the implicit computations. Can be used for all implicit computations not just eigenvalue computations.
        """ # nopep8
        return self._cards[1].get_value("ibeam")

    @ibeam.setter
    def ibeam(self, value: int) -> None:
        """Set the ibeam property."""
        self._cards[1].set_value("ibeam", value)

    @property
    def ishell(self) -> int:
        """Get or set the If nonzero, reset all shell element formulations to ISHELL for the implicit computations. Can be used for all implicit computations not just eigenvalue computations.
        """ # nopep8
        return self._cards[1].get_value("ishell")

    @ishell.setter
    def ishell(self, value: int) -> None:
        """Set the ishell property."""
        self._cards[1].set_value("ishell", value)

    @property
    def itshell(self) -> int:
        """Get or set the If nonzero, reset all thick shell element formulations to ITSHELL for the implicit computations. Can be used for all implicit computations not just eigenvalue computations.
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
    def evdump(self) -> int:
        """Get or set the Flag for writing eigenvalues and eigenvectors to file Eigen_Vectors in SMP or files Eigen_Vectors.xxxx in MPP where xxxx is the process ID
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
    def iparm3(self) -> typing.Optional[int]:
        """Get or set the Node set to reduce output of entries from each eigenvector. See Remark 6.
        """ # nopep8
        return self._cards[2].get_value("iparm3")

    @iparm3.setter
    def iparm3(self, value: int) -> None:
        """Set the iparm3 property."""
        self._cards[2].set_value("iparm3", value)

    @property
    def rparm4(self) -> typing.Optional[float]:
        """Get or set the Control output of eigenvectors to the d3eigv database:
        LT.0.0: No output
        EQ.0.0: Write all eigenmodes to d3eigv.
        GT.0.0: Write only the first RPARM4 eigenmodes to d3eigv.
        """ # nopep8
        return self._cards[2].get_value("rparm4")

    @rparm4.setter
    def rparm4(self, value: float) -> None:
        """Set the rparm4 property."""
        self._cards[2].set_value("rparm4", value)

    @property
    def iparm1(self) -> int:
        """Get or set the Minimum block size for the Cholesky factorization
        """ # nopep8
        return self._cards[3].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        """Set the iparm1 property."""
        self._cards[3].set_value("iparm1", value)

    @property
    def iparm2(self) -> typing.Optional[int]:
        """Get or set the Maximum block size for the Cholesky factorization. The default is the model size.
        """ # nopep8
        return self._cards[3].get_value("iparm2")

    @iparm2.setter
    def iparm2(self, value: int) -> None:
        """Set the iparm2 property."""
        self._cards[3].set_value("iparm2", value)

    @property
    def iparm3(self) -> typing.Optional[int]:
        """Get or set the Node set ID specifying particular nodes in the model where increased accuracy is desired. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("iparm3")

    @iparm3.setter
    def iparm3(self, value: int) -> None:
        """Set the iparm3 property."""
        self._cards[3].set_value("iparm3", value)

    @property
    def iparm4(self) -> int:
        """Get or set the MCMS minimum group/substructure size. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("iparm4")

    @iparm4.setter
    def iparm4(self, value: int) -> None:
        """Set the iparm4 property."""
        self._cards[3].set_value("iparm4", value)

    @property
    def rparm1(self) -> float:
        """Get or set the Eigenvalue expansion factor ?. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("rparm1")

    @rparm1.setter
    def rparm1(self, value: float) -> None:
        """Set the rparm1 property."""
        self._cards[3].set_value("rparm1", value)

    @property
    def rparm4(self) -> float:
        """Get or set the Control output of eigenvectors to the d3eigv database:
        LT.0.0: No output
        EQ.0.0: Write all eigenmodes to d3eigv.
        GT.0.0: Write only the first RPARM4 eigenmodes to d3eigv.
        """ # nopep8
        return self._cards[3].get_value("rparm4")

    @rparm4.setter
    def rparm4(self, value: float) -> None:
        """Set the rparm4 property."""
        self._cards[3].set_value("rparm4", value)

    @property
    def iparm1(self) -> int:
        """Get or set the Maximum number of iterations.
        """ # nopep8
        return self._cards[4].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        """Set the iparm1 property."""
        self._cards[4].set_value("iparm1", value)

    @property
    def iparm2(self) -> int:
        """Get or set the Block size.
        """ # nopep8
        return self._cards[4].get_value("iparm2")

    @iparm2.setter
    def iparm2(self, value: int) -> None:
        """Set the iparm2 property."""
        self._cards[4].set_value("iparm2", value)

    @property
    def iparm3(self) -> typing.Optional[int]:
        """Get or set the Node set to reduce output of entries from each eigenvector. See Remark 6.
        """ # nopep8
        return self._cards[4].get_value("iparm3")

    @iparm3.setter
    def iparm3(self, value: int) -> None:
        """Set the iparm3 property."""
        self._cards[4].set_value("iparm3", value)

    @property
    def rparm1(self) -> float:
        """Get or set the Convergence tolerance
        """ # nopep8
        return self._cards[4].get_value("rparm1")

    @rparm1.setter
    def rparm1(self, value: float) -> None:
        """Set the rparm1 property."""
        self._cards[4].set_value("rparm1", value)

    @property
    def rparm2(self) -> float:
        """Get or set the Flag for using a Block Low-Rank (BLR) factorization for the preconditioner:
        GT.0.0: Use BLR preconditioner with compression threshold RPARM2.
        LT.0.0: Use exact factorization instead of a BLR preconditioner.
        """ # nopep8
        return self._cards[4].get_value("rparm2")

    @rparm2.setter
    def rparm2(self, value: float) -> None:
        """Set the rparm2 property."""
        self._cards[4].set_value("rparm2", value)

    @property
    def rparm4(self) -> float:
        """Get or set the Control output of eigenvectors to the d3eigv database:
        LT.0.0: No output
        EQ.0.0: Write all eigenmodes to d3eigv.
        GT.0.0: Write only the first RPARM4 eigenmodes to d3eigv.
        """ # nopep8
        return self._cards[4].get_value("rparm4")

    @rparm4.setter
    def rparm4(self, value: float) -> None:
        """Set the rparm4 property."""
        self._cards[4].set_value("rparm4", value)

    @property
    def iparm1(self) -> int:
        """Get or set the Maximum number of Block Lanczos iterations
        """ # nopep8
        return self._cards[5].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        """Set the iparm1 property."""
        self._cards[5].set_value("iparm1", value)

    @property
    def iparm2(self) -> int:
        """Get or set the Block size for the Block Lanczos recurrence
        """ # nopep8
        return self._cards[5].get_value("iparm2")

    @iparm2.setter
    def iparm2(self, value: int) -> None:
        """Set the iparm2 property."""
        self._cards[5].set_value("iparm2", value)

    @property
    def iparm3(self) -> int:
        """Get or set the Node set to reduce output of entries from each eigenvector. See Remark 6
        """ # nopep8
        return self._cards[5].get_value("iparm3")

    @iparm3.setter
    def iparm3(self, value: int) -> None:
        """Set the iparm3 property."""
        self._cards[5].set_value("iparm3", value)

    @property
    def rparm1(self) -> float:
        """Get or set the First shift. Supply the approximate value of the 100th eigenvalue
        """ # nopep8
        return self._cards[5].get_value("rparm1")

    @rparm1.setter
    def rparm1(self, value: float) -> None:
        """Set the rparm1 property."""
        self._cards[5].set_value("rparm1", value)

    @property
    def rparm2(self) -> float:
        """Get or set the Shift factor. This field reduces the aggressiveness of the shift logic
        """ # nopep8
        return self._cards[5].get_value("rparm2")

    @rparm2.setter
    def rparm2(self, value: float) -> None:
        """Set the rparm2 property."""
        self._cards[5].set_value("rparm2", value)

    @property
    def rparm4(self) -> float:
        """Get or set the Control output of eigenvectors to the d3eigv database:
        LT.0.0: No output
        EQ.0.0: Write all eigenmodes to d3eigv.
        GT.0.0: Write only the first RPARM4 eigenmodes to d3eigv.
        """ # nopep8
        return self._cards[5].get_value("rparm4")

    @rparm4.setter
    def rparm4(self, value: float) -> None:
        """Set the rparm4 property."""
        self._cards[5].set_value("rparm4", value)

    @property
    def iparm1(self) -> typing.Optional[int]:
        """Get or set the Node set ID for nodes on the left surface of the sector
        """ # nopep8
        return self._cards[6].get_value("iparm1")

    @iparm1.setter
    def iparm1(self, value: int) -> None:
        """Set the iparm1 property."""
        self._cards[6].set_value("iparm1", value)

    @property
    def iparm2(self) -> int:
        """Get or set the Node set ID for nodes on the axis of rotation.
        EQ.0: No nodes on the axis of rotation(default)
        """ # nopep8
        return self._cards[6].get_value("iparm2")

    @iparm2.setter
    def iparm2(self, value: int) -> None:
        """Set the iparm2 property."""
        self._cards[6].set_value("iparm2", value)

    @property
    def iparm3(self) -> typing.Optional[int]:
        """Get or set the Node set ID for nodes on the left surface of the sector
        """ # nopep8
        return self._cards[6].get_value("iparm3")

    @iparm3.setter
    def iparm3(self, value: int) -> None:
        """Set the iparm3 property."""
        self._cards[6].set_value("iparm3", value)

    @property
    def iparm4(self) -> typing.Optional[int]:
        """Get or set the Number of sectors
        """ # nopep8
        return self._cards[6].get_value("iparm4")

    @iparm4.setter
    def iparm4(self, value: int) -> None:
        """Set the iparm4 property."""
        self._cards[6].set_value("iparm4", value)

    @property
    def iparm5(self) -> typing.Optional[int]:
        """Get or set the Harmonic index
        """ # nopep8
        return self._cards[6].get_value("iparm5")

    @iparm5.setter
    def iparm5(self, value: int) -> None:
        """Set the iparm5 property."""
        self._cards[6].set_value("iparm5", value)

    @property
    def iparm6(self) -> int:
        """Get or set the Vector ID for the axis of rotation.
        EQ.0: Axis of rotation is the global z - axis(default)
        """ # nopep8
        return self._cards[6].get_value("iparm6")

    @iparm6.setter
    def iparm6(self, value: int) -> None:
        """Set the iparm6 property."""
        self._cards[6].set_value("iparm6", value)

    @property
    def rotscl(self) -> float:
        """Get or set the Scale factor for the inertia of rotational degrees of freedom.
        EQ.0.0: Default value of 0.001
        EQ.1.0: Inertia not scaled.
        """ # nopep8
        return self._cards[7].get_value("rotscl")

    @rotscl.setter
    def rotscl(self, value: float) -> None:
        """Set the rotscl property."""
        self._cards[7].set_value("rotscl", value)

    @property
    def eigmscl(self) -> int:
        """Get or set the For intermittent eigenvalue extractions, control whether to use the original (unscaled) or the scaled mass (when mass scaling is active through parameter DT2MS on *CONTROL_TIMESTEP). Prior to version R15 (unless noted otherwise in the release notes), the scaled mass was incorrectly used. We added this option for backwards compatibility.
        EQ.0: Use original mass(default).
        EQ.1: Use scaled mass.
        """ # nopep8
        return self._cards[7].get_value("eigmscl")

    @eigmscl.setter
    def eigmscl(self, value: int) -> None:
        """Set the eigmscl property."""
        if value not in [0, 1, None]:
            raise Exception("""eigmscl must be `None` or one of {0,1}.""")
        self._cards[7].set_value("eigmscl", value)

    @property
    def iparm6_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for iparm6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.iparm6:
                return kwd
        return None

    @iparm6_link.setter
    def iparm6_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for iparm6."""
        self.iparm6 = value.vid

    @property
    def iparm3_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for iparm3."""
        return self._get_set_link("NODE", self.iparm3)

    @iparm3_link.setter
    def iparm3_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm3."""
        self.iparm3 = value.sid

    @property
    def iparm3_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for iparm3."""
        return self._get_set_link("NODE", self.iparm3)

    @iparm3_link.setter
    def iparm3_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm3."""
        self.iparm3 = value.sid

    @property
    def iparm3_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for iparm3."""
        return self._get_set_link("NODE", self.iparm3)

    @iparm3_link.setter
    def iparm3_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm3."""
        self.iparm3 = value.sid

    @property
    def iparm1_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for iparm1."""
        return self._get_set_link("NODE", self.iparm1)

    @iparm1_link.setter
    def iparm1_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm1."""
        self.iparm1 = value.sid

    @property
    def iparm2_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for iparm2."""
        return self._get_set_link("NODE", self.iparm2)

    @iparm2_link.setter
    def iparm2_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm2."""
        self.iparm2 = value.sid

    @property
    def iparm3_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for iparm3."""
        return self._get_set_link("NODE", self.iparm3)

    @iparm3_link.setter
    def iparm3_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for iparm3."""
        self.iparm3 = value.sid

