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

"""Module providing the ConstrainedInterpolation class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_CONSTRAINEDINTERPOLATION_CARD0 = (
    FieldSchema("icid", int, 0, 10, None),
    FieldSchema("dnid", int, 10, 10, 0),
    FieldSchema("ddof", int, 20, 10, 123456),
    FieldSchema("cidd", int, 30, 10, None),
    FieldSchema("ityp", int, 40, 10, 0),
    FieldSchema("idnsw", int, 50, 10, 0),
    FieldSchema("fgm", int, 60, 10, 0),
)

_CONSTRAINEDINTERPOLATION_CARD1 = (
    FieldSchema("inid", int, 0, 10, 0),
    FieldSchema("idof", int, 10, 10, 123456),
    FieldSchema("twghtx", float, 20, 10, 1.0),
    FieldSchema("twghty", float, 30, 10, 1.0),
    FieldSchema("twghtz", float, 40, 10, 1.0),
    FieldSchema("rwghtx", float, 50, 10, 1.0),
    FieldSchema("rwghty", float, 60, 10, 1.0),
    FieldSchema("rwghtz", float, 70, 10, 1.0),
)

class ConstrainedInterpolation(KeywordBase):
    """DYNA CONSTRAINED_INTERPOLATION keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "INTERPOLATION"
    _link_fields = {
        "dnid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedInterpolation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDINTERPOLATION_CARD1,
                **kwargs,
            ),        ]
    @property
    def icid(self) -> typing.Optional[int]:
        """Get or set the Interpolation constraint ID.
        """ # nopep8
        return self._cards[0].get_value("icid")

    @icid.setter
    def icid(self, value: int) -> None:
        """Set the icid property."""
        self._cards[0].set_value("icid", value)

    @property
    def dnid(self) -> int:
        """Get or set the Dependent node ID. This node should not be a member of a rigid body, or elsewhere constrained in the input.
        """ # nopep8
        return self._cards[0].get_value("dnid")

    @dnid.setter
    def dnid(self, value: int) -> None:
        """Set the dnid property."""
        self._cards[0].set_value("dnid", value)

    @property
    def ddof(self) -> int:
        """Get or set the Dependent degrees-of-freedom. The list of dependent DOF consists of a number with up to six digits, with each digit representing a degree of freedom, e.g., the value 1356 indicates that degrees of freedom 1, 3, 5, and 6 are controlled by the RBE3 constraint. Default=123456.
        Degree of freedom IDs:
        EQ.1: x,
        EQ.2: y,
        EQ.3: z,
        EQ.4: rotation about x-axis,
        EQ.5: rotation about y-axis,
        EQ.6: rotation about z-axis.
        """ # nopep8
        return self._cards[0].get_value("ddof")

    @ddof.setter
    def ddof(self, value: int) -> None:
        """Set the ddof property."""
        self._cards[0].set_value("ddof", value)

    @property
    def cidd(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID of LOCAL option is active. If blank the global coordinate system is assumed.
        """ # nopep8
        return self._cards[0].get_value("cidd")

    @cidd.setter
    def cidd(self, value: int) -> None:
        """Set the cidd property."""
        self._cards[0].set_value("cidd", value)

    @property
    def ityp(self) -> int:
        """Get or set the Specifies the meaning of INID.
        EQ.0: INID is a node ID
        EQ.1: INID is a node set ID.
        """ # nopep8
        return self._cards[0].get_value("ityp")

    @ityp.setter
    def ityp(self, value: int) -> None:
        """Set the ityp property."""
        if value not in [0, 1, None]:
            raise Exception("""ityp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ityp", value)

    @property
    def idnsw(self) -> int:
        """Get or set the Switch for controlling the explicit solution  when an independent (or dependent) node is deleted.
        EQ.0:	default to option 1.
        EQ.1:	terminate the explicit analysis when an independent node or the dependent node is deleted.
        EQ.2:	continue the explicit analysis with the constraints unchanged. .
        """ # nopep8
        return self._cards[0].get_value("idnsw")

    @idnsw.setter
    def idnsw(self, value: int) -> None:
        """Set the idnsw property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""idnsw must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("idnsw", value)

    @property
    def fgm(self) -> int:
        """Get or set the Flag for special treatment of this constraint for implicit problems only:
        EQ.0:	use standard constraint processing for implicit.
        EQ.1 : use special processing for this constraint for implicit only; see Remarks.
        """ # nopep8
        return self._cards[0].get_value("fgm")

    @fgm.setter
    def fgm(self, value: int) -> None:
        """Set the fgm property."""
        if value not in [0, 1, None]:
            raise Exception("""fgm must be `None` or one of {0,1}.""")
        self._cards[0].set_value("fgm", value)

    @property
    def inid(self) -> int:
        """Get or set the Independent node ID or node set ID.
        """ # nopep8
        return self._cards[1].get_value("inid")

    @inid.setter
    def inid(self, value: int) -> None:
        """Set the inid property."""
        self._cards[1].set_value("inid", value)

    @property
    def idof(self) -> int:
        """Get or set the Independent degrees-of-freedom using the same form as DDOF above.
        """ # nopep8
        return self._cards[1].get_value("idof")

    @idof.setter
    def idof(self, value: int) -> None:
        """Set the idof property."""
        self._cards[1].set_value("idof", value)

    @property
    def twghtx(self) -> float:
        """Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the x-translational component. It is normally sufficient to define only TWGHTX even if its degree-of-freedom is inactive since the other factors are set equal to this input value as the default. There is no requirement on the values that are chosen as the weighting factors, i.e., that they sum to unity. The default value for the weighting factor is unity.
        """ # nopep8
        return self._cards[1].get_value("twghtx")

    @twghtx.setter
    def twghtx(self, value: float) -> None:
        """Set the twghtx property."""
        self._cards[1].set_value("twghtx", value)

    @property
    def twghty(self) -> float:
        """Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the y-translational component.
        """ # nopep8
        return self._cards[1].get_value("twghty")

    @twghty.setter
    def twghty(self, value: float) -> None:
        """Set the twghty property."""
        self._cards[1].set_value("twghty", value)

    @property
    def twghtz(self) -> float:
        """Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the z-translational component.
        """ # nopep8
        return self._cards[1].get_value("twghtz")

    @twghtz.setter
    def twghtz(self, value: float) -> None:
        """Set the twghtz property."""
        self._cards[1].set_value("twghtz", value)

    @property
    def rwghtx(self) -> float:
        """Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the x-rotational component.
        """ # nopep8
        return self._cards[1].get_value("rwghtx")

    @rwghtx.setter
    def rwghtx(self, value: float) -> None:
        """Set the rwghtx property."""
        self._cards[1].set_value("rwghtx", value)

    @property
    def rwghty(self) -> float:
        """Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the y-rotational component.
        """ # nopep8
        return self._cards[1].get_value("rwghty")

    @rwghty.setter
    def rwghty(self, value: float) -> None:
        """Set the rwghty property."""
        self._cards[1].set_value("rwghty", value)

    @property
    def rwghtz(self) -> float:
        """Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the z-rotational component.
        """ # nopep8
        return self._cards[1].get_value("rwghtz")

    @rwghtz.setter
    def rwghtz(self, value: float) -> None:
        """Set the rwghtz property."""
        self._cards[1].set_value("rwghtz", value)

    @property
    def dnid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given dnid."""
        return self._get_link_by_attr("NODE", "nid", self.dnid, "parts")

