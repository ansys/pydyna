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

"""Module providing the ControlFormingBlankmesh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONTROLFORMINGBLANKMESH_CARD0 = (
    FieldSchema("idmesh", int, 0, 10, None),
    FieldSchema("eleng", float, 10, 10, 0.0),
    FieldSchema("xleng", float, 20, 10, 0.0),
    FieldSchema("yleng", float, 30, 10, 0.0),
    FieldSchema("angelx", float, 40, 10, 0.0),
    FieldSchema("nplane", int, 50, 10, 1),
    FieldSchema("cid", int, 60, 10, 0),
)

_CONTROLFORMINGBLANKMESH_CARD1 = (
    FieldSchema("pidbk", int, 0, 10, None),
    FieldSchema("nid", int, 10, 10, None),
    FieldSchema("eid", int, 20, 10, None),
    FieldSchema("xcent", float, 30, 10, 0.0),
    FieldSchema("ycent", float, 40, 10, 0.0),
    FieldSchema("zcent", float, 50, 10, 0.0),
    FieldSchema("xshift", float, 60, 10, 0.0),
    FieldSchema("yshift", float, 70, 10, 0.0),
)

class ControlFormingBlankmesh(KeywordBase):
    """DYNA CONTROL_FORMING_BLANKMESH keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_BLANKMESH"
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "pidbk": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlFormingBlankmesh class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGBLANKMESH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGBLANKMESH_CARD1,
                **kwargs,
            ),        ]
    @property
    def idmesh(self) -> typing.Optional[int]:
        """Get or set the ID of the blankmesh (not the blank PID); must be unique
        """ # nopep8
        return self._cards[0].get_value("idmesh")

    @idmesh.setter
    def idmesh(self, value: int) -> None:
        """Set the idmesh property."""
        self._cards[0].set_value("idmesh", value)

    @property
    def eleng(self) -> float:
        """Get or set the Element edge length.
        """ # nopep8
        return self._cards[0].get_value("eleng")

    @eleng.setter
    def eleng(self, value: float) -> None:
        """Set the eleng property."""
        self._cards[0].set_value("eleng", value)

    @property
    def xleng(self) -> float:
        """Get or set the Length of the rectangular blank along X-axis in the coordinate system (CID) defined
        """ # nopep8
        return self._cards[0].get_value("xleng")

    @xleng.setter
    def xleng(self, value: float) -> None:
        """Set the xleng property."""
        self._cards[0].set_value("xleng", value)

    @property
    def yleng(self) -> float:
        """Get or set the Length of the rectangular blank along Y-axis in the coordinate system (CID) defined
        """ # nopep8
        return self._cards[0].get_value("yleng")

    @yleng.setter
    def yleng(self, value: float) -> None:
        """Set the yleng property."""
        self._cards[0].set_value("yleng", value)

    @property
    def angelx(self) -> float:
        """Get or set the X-coordinate of a reference point for the net to be generated
        """ # nopep8
        return self._cards[0].get_value("angelx")

    @angelx.setter
    def angelx(self, value: float) -> None:
        """Set the angelx property."""
        self._cards[0].set_value("angelx", value)

    @property
    def nplane(self) -> int:
        """Get or set the Plane in which a flat blank to be generated, in reference to the coordinate system defined (CID):
        EQ.1: XY-plane,
        EQ.2: XZ-plane,
        EQ.3: YZ-plane
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        """Set the nplane property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""nplane must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("nplane", value)

    @property
    def cid(self) -> int:
        """Get or set the ID of the local coordinate system
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def pidbk(self) -> typing.Optional[int]:
        """Get or set the Part ID of the blank, as defined by *PART
        """ # nopep8
        return self._cards[1].get_value("pidbk")

    @pidbk.setter
    def pidbk(self, value: int) -> None:
        """Set the pidbk property."""
        self._cards[1].set_value("pidbk", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Starting node ID of the blank to be generated
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Starting element ID of the blank to be generated
        """ # nopep8
        return self._cards[1].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[1].set_value("eid", value)

    @property
    def xcent(self) -> float:
        """Get or set the X-coordinate of the center of the blank
        """ # nopep8
        return self._cards[1].get_value("xcent")

    @xcent.setter
    def xcent(self, value: float) -> None:
        """Set the xcent property."""
        self._cards[1].set_value("xcent", value)

    @property
    def ycent(self) -> float:
        """Get or set the Y-coordinate of the center of the blank
        """ # nopep8
        return self._cards[1].get_value("ycent")

    @ycent.setter
    def ycent(self, value: float) -> None:
        """Set the ycent property."""
        self._cards[1].set_value("ycent", value)

    @property
    def zcent(self) -> float:
        """Get or set the Z-coordinate of the center of the blank
        """ # nopep8
        return self._cards[1].get_value("zcent")

    @zcent.setter
    def zcent(self, value: float) -> None:
        """Set the zcent property."""
        self._cards[1].set_value("zcent", value)

    @property
    def xshift(self) -> float:
        """Get or set the Blank shift distance in X-axis in coordinate system defined (CID).
        """ # nopep8
        return self._cards[1].get_value("xshift")

    @xshift.setter
    def xshift(self, value: float) -> None:
        """Set the xshift property."""
        self._cards[1].set_value("xshift", value)

    @property
    def yshift(self) -> float:
        """Get or set the Blank shift distance in Y-axis in coordinate system defined (CID).
        """ # nopep8
        return self._cards[1].get_value("yshift")

    @yshift.setter
    def yshift(self, value: float) -> None:
        """Set the yshift property."""
        self._cards[1].set_value("yshift", value)

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

    @property
    def pidbk_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pidbk."""
        return self._get_link_by_attr("PART", "pid", self.pidbk, "parts")

