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

"""Module providing the BoundarySpcNodeBirthDeath class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_BOUNDARYSPCNODEBIRTHDEATH_CARD0 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, 0),
    FieldSchema("dofx", int, 20, 10, 0),
    FieldSchema("dofy", int, 30, 10, 0),
    FieldSchema("dofz", int, 40, 10, 0),
    FieldSchema("dofrx", int, 50, 10, 0),
    FieldSchema("dofry", int, 60, 10, 0),
    FieldSchema("dofrz", int, 70, 10, 0),
)

_BOUNDARYSPCNODEBIRTHDEATH_CARD1 = (
    FieldSchema("birth", float, 0, 10, 0.0),
    FieldSchema("death", float, 10, 10, 1e+20),
)

_BOUNDARYSPCNODEBIRTHDEATH_OPTION0_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

class BoundarySpcNodeBirthDeath(KeywordBase):
    """DYNA BOUNDARY_SPC_NODE_BIRTH_DEATH keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPC_NODE_BIRTH_DEATH"
    option_specs = [
        OptionSpec("ID", -2, 1),
    ]
    _link_fields = {
        "nid": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundarySpcNodeBirthDeath class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPCNODEBIRTHDEATH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPCNODEBIRTHDEATH_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = BoundarySpcNodeBirthDeath.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _BOUNDARYSPCNODEBIRTHDEATH_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def dofx(self) -> int:
        """Get or set the EQ.0: no translational constraint in local x-direction,
        EQ.1: translational constraint in local x-direction.
        """ # nopep8
        return self._cards[0].get_value("dofx")

    @dofx.setter
    def dofx(self, value: int) -> None:
        """Set the dofx property."""
        if value not in [0, 1, None]:
            raise Exception("""dofx must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofx", value)

    @property
    def dofy(self) -> int:
        """Get or set the EQ.0: no translational constraint in local y-direction,
        EQ.1: translational constraint in local y-direction.
        """ # nopep8
        return self._cards[0].get_value("dofy")

    @dofy.setter
    def dofy(self, value: int) -> None:
        """Set the dofy property."""
        if value not in [0, 1, None]:
            raise Exception("""dofy must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofy", value)

    @property
    def dofz(self) -> int:
        """Get or set the EQ.0: no translational constraint in local z-direction,
        EQ.1: translational constraint in local z-direction.
        """ # nopep8
        return self._cards[0].get_value("dofz")

    @dofz.setter
    def dofz(self, value: int) -> None:
        """Set the dofz property."""
        if value not in [0, 1, None]:
            raise Exception("""dofz must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofz", value)

    @property
    def dofrx(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local x-axis,
        EQ.1: rotational constraint about local x-axis.
        """ # nopep8
        return self._cards[0].get_value("dofrx")

    @dofrx.setter
    def dofrx(self, value: int) -> None:
        """Set the dofrx property."""
        if value not in [0, 1, None]:
            raise Exception("""dofrx must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofrx", value)

    @property
    def dofry(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local y-axis,
        EQ.1: rotational constraint about local y-axis.
        """ # nopep8
        return self._cards[0].get_value("dofry")

    @dofry.setter
    def dofry(self, value: int) -> None:
        """Set the dofry property."""
        if value not in [0, 1, None]:
            raise Exception("""dofry must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofry", value)

    @property
    def dofrz(self) -> int:
        """Get or set the EQ.0: no rotational constraint about the local z-axiis
        EQ.1: rotational constraint about local z-axis.
        """ # nopep8
        return self._cards[0].get_value("dofrz")

    @dofrz.setter
    def dofrz(self, value: int) -> None:
        """Set the dofrz property."""
        if value not in [0, 1, None]:
            raise Exception("""dofrz must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dofrz", value)

    @property
    def birth(self) -> float:
        """Get or set the Activation time for constraint
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Deactivation time for constraint.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[2].cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[2].cards[0].set_value("id", value)

        if value:
            self.activate_option("ID")

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[2].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[2].cards[0].set_value("heading", value)

        if value:
            self.activate_option("HEADING")

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def cid_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

