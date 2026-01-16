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

"""Module providing the ConstrainedJointGears class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONSTRAINEDJOINTGEARS_CARD0 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n2", int, 10, 10, None),
    FieldSchema("n3", int, 20, 10, None),
    FieldSchema("n4", int, 30, 10, None),
    FieldSchema("n5", int, 40, 10, None),
    FieldSchema("n6", int, 50, 10, None),
    FieldSchema("rps", float, 60, 10, 1.0),
    FieldSchema("damp", float, 70, 10, 0.0),
)

_CONSTRAINEDJOINTGEARS_CARD1 = (
    FieldSchema("parm", float, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, 0),
    FieldSchema("type", int, 20, 10, 0),
    FieldSchema("r1", float, 30, 10, None),
    FieldSchema("h_angle", float, 40, 10, 0.0),
)

class ConstrainedJointGears(KeywordBase):
    """DYNA CONSTRAINED_JOINT_GEARS keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_GEARS"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedJointGears class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTGEARS_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTGEARS_CARD1,
                **kwargs,
            ),        ]
    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node 1, in rigid body A.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node 2, in rigid body B.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node 3, in rigid body A.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node 4, in rigid body B.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Node 5, in rigid body A.
        """ # nopep8
        return self._cards[0].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[0].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Node 6, in rigid body B.
        """ # nopep8
        return self._cards[0].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[0].set_value("n6", value)

    @property
    def rps(self) -> float:
        """Get or set the Relative penalty stiffness (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("rps")

    @rps.setter
    def rps(self, value: float) -> None:
        """Set the rps property."""
        self._cards[0].set_value("rps", value)

    @property
    def damp(self) -> float:
        """Get or set the Not to be defined.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[0].set_value("damp", value)

    @property
    def parm(self) -> typing.Optional[float]:
        """Get or set the Define the ratio R2/R1.
        """ # nopep8
        return self._cards[1].get_value("parm")

    @parm.setter
    def parm(self, value: float) -> None:
        """Set the parm property."""
        self._cards[1].set_value("parm", value)

    @property
    def lcid(self) -> int:
        """Get or set the Not to be defined.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def type(self) -> int:
        """Get or set the Not to be defined.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[1].set_value("type", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Radius, R_1, for the gear and pulley joint type.  If undefined, nodal points 5 and 6 are assumed to be on the outer radius. The values of R1 and R2 affect the outputted reaction forces. The forces are calculated from the moments by dividing them by the radii
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[1].set_value("r1", value)

    @property
    def h_angle(self) -> float:
        """Get or set the Helix angle in degrees. This is only necessary for the gear joint if the gears do not mesh tangentially, e.g., worm gears.
        """ # nopep8
        return self._cards[1].get_value("h_angle")

    @h_angle.setter
    def h_angle(self, value: float) -> None:
        """Set the h_angle property."""
        self._cards[1].set_value("h_angle", value)

    @property
    def n1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

    @property
    def n5_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n5."""
        return self._get_link_by_attr("NODE", "nid", self.n5, "parts")

    @property
    def n6_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n6."""
        return self._get_link_by_attr("NODE", "nid", self.n6, "parts")

    @property
    def lcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

