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

"""Module providing the LoadBodyGeneralizedSetNode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADBODYGENERALIZEDSETNODE_CARD0 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("n2", int, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("drlcid", int, 30, 10, 0),
    FieldSchema("xc", float, 40, 10, 0.0),
    FieldSchema("yc", float, 50, 10, 0.0),
    FieldSchema("zc", float, 60, 10, 0.0),
)

_LOADBODYGENERALIZEDSETNODE_CARD1 = (
    FieldSchema("ax", float, 0, 10, 0.0),
    FieldSchema("ay", float, 10, 10, 0.0),
    FieldSchema("az", float, 20, 10, 0.0),
    FieldSchema("omx", float, 30, 10, 0.0),
    FieldSchema("omy", float, 40, 10, 0.0),
    FieldSchema("omz", float, 50, 10, 0.0),
    FieldSchema("cid", int, 60, 10, None),
    FieldSchema("angtyp", str, 70, 10, "CENT"),
)

class LoadBodyGeneralizedSetNode(KeywordBase):
    """DYNA LOAD_BODY_GENERALIZED_SET_NODE keyword"""

    keyword = "LOAD"
    subkeyword = "BODY_GENERALIZED_SET_NODE"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "drlcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadBodyGeneralizedSetNode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADBODYGENERALIZEDSETNODE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADBODYGENERALIZEDSETNODE_CARD1,
                **kwargs,
            ),        ]
    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID for body force load.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Ending node ID for body force load.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def drlcid(self) -> int:
        """Get or set the Load curve ID for dynamic relaxation phase. Only if dynamic relaxation is defined.
        """ # nopep8
        return self._cards[0].get_value("drlcid")

    @drlcid.setter
    def drlcid(self, value: int) -> None:
        """Set the drlcid property."""
        self._cards[0].set_value("drlcid", value)

    @property
    def xc(self) -> float:
        """Get or set the X-center of rotation. Define only for angular velocity.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Y-center of rotation. Define only for angular velocity.
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Z-center of rotation. Define only for angular velocity.
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[0].set_value("zc", value)

    @property
    def ax(self) -> float:
        """Get or set the Scale factor for acceleration in x-direction.
        """ # nopep8
        return self._cards[1].get_value("ax")

    @ax.setter
    def ax(self, value: float) -> None:
        """Set the ax property."""
        self._cards[1].set_value("ax", value)

    @property
    def ay(self) -> float:
        """Get or set the Scale factor for acceleration in y-direction.
        """ # nopep8
        return self._cards[1].get_value("ay")

    @ay.setter
    def ay(self, value: float) -> None:
        """Set the ay property."""
        self._cards[1].set_value("ay", value)

    @property
    def az(self) -> float:
        """Get or set the Scale factor for acceleration in z-direction.
        """ # nopep8
        return self._cards[1].get_value("az")

    @az.setter
    def az(self, value: float) -> None:
        """Set the az property."""
        self._cards[1].set_value("az", value)

    @property
    def omx(self) -> float:
        """Get or set the Scale factor for x-angular velocity.
        """ # nopep8
        return self._cards[1].get_value("omx")

    @omx.setter
    def omx(self, value: float) -> None:
        """Set the omx property."""
        self._cards[1].set_value("omx", value)

    @property
    def omy(self) -> float:
        """Get or set the Scale factor for y-angular velocity.
        """ # nopep8
        return self._cards[1].get_value("omy")

    @omy.setter
    def omy(self, value: float) -> None:
        """Set the omy property."""
        self._cards[1].set_value("omy", value)

    @property
    def omz(self) -> float:
        """Get or set the Scale factor for z-angular velocity.
        """ # nopep8
        return self._cards[1].get_value("omz")

    @omz.setter
    def omz(self, value: float) -> None:
        """Set the omz property."""
        self._cards[1].set_value("omz", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID to define acceleration in the local coordinate system.  The coordinate (XC, YC, ZC) is defined with respect to the local coordinate system if CID is nonzero.  The accelerations, LCID and their scale factors are with respect to CID.EQ.0: global.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[1].set_value("cid", value)

    @property
    def angtyp(self) -> str:
        """Get or set the Type of body loads due to angular motion
        EQ.CENT: body load from centrifugal acceleration,
        EQ.CORI: body load from Coriolis-type acceleration,
        EQ.ROTA: body load from rotational acceleration
        """ # nopep8
        return self._cards[1].get_value("angtyp")

    @angtyp.setter
    def angtyp(self, value: str) -> None:
        """Set the angtyp property."""
        if value not in ["CENT", "CORI", "ROTA", None]:
            raise Exception("""angtyp must be `None` or one of {"CENT","CORI","ROTA"}.""")
        self._cards[1].set_value("angtyp", value)

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

    @property
    def drlcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for drlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.drlcid:
                return kwd
        return None

    @drlcid_link.setter
    def drlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for drlcid."""
        self.drlcid = value.lcid

