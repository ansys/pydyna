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

"""Module providing the EmEpPurkinjeNetwork class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_EMEPPURKINJENETWORK_CARD0 = (
    FieldSchema("purkid", int, 0, 10, None),
    FieldSchema("buildnet", int, 10, 10, None),
    FieldSchema("ssid", int, 20, 10, None),
    FieldSchema("mid", int, 30, 10, None),
    FieldSchema("pointstx", float, 40, 10, None),
    FieldSchema("pointsty", float, 50, 10, None),
    FieldSchema("pointstz", float, 60, 10, None),
    FieldSchema("edgetlen", float, 70, 10, None),
)

_EMEPPURKINJENETWORK_CARD1 = (
    FieldSchema("numgen", int, 0, 10, None),
    FieldSchema("numbrinit", int, 10, 10, None),
    FieldSchema("numsplit", int, 20, 10, None),
    FieldSchema("inodestld", int, 30, 10, None),
    FieldSchema("iedgestld", int, 40, 10, None),
)

class EmEpPurkinjeNetwork(KeywordBase):
    """DYNA EM_EP_PURKINJE_NETWORK keyword"""

    keyword = "EM"
    subkeyword = "EP_PURKINJE_NETWORK"
    _link_fields = {
        "inodestld": LinkType.NODE,
        "mid": LinkType.MAT,
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpPurkinjeNetwork class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPPURKINJENETWORK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMEPPURKINJENETWORK_CARD1,
                **kwargs,
            ),        ]
    @property
    def purkid(self) -> typing.Optional[int]:
        """Get or set the Material ID: refers to MID in the *PART card
        """ # nopep8
        return self._cards[0].get_value("purkid")

    @purkid.setter
    def purkid(self, value: int) -> None:
        """Set the purkid property."""
        self._cards[0].set_value("purkid", value)

    @property
    def buildnet(self) -> typing.Optional[int]:
        """Get or set the If EQ.1, creates a new Purkinje network.
        EQ.0: does not
        """ # nopep8
        return self._cards[0].get_value("buildnet")

    @buildnet.setter
    def buildnet(self, value: int) -> None:
        """Set the buildnet property."""
        self._cards[0].set_value("buildnet", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set on which the Purkinje network is lying
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in the *MAT section
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def pointstx(self) -> typing.Optional[float]:
        """Get or set the X coordinate of the tree origin
        """ # nopep8
        return self._cards[0].get_value("pointstx")

    @pointstx.setter
    def pointstx(self, value: float) -> None:
        """Set the pointstx property."""
        self._cards[0].set_value("pointstx", value)

    @property
    def pointsty(self) -> typing.Optional[float]:
        """Get or set the Y coordinate of the tree origin
        """ # nopep8
        return self._cards[0].get_value("pointsty")

    @pointsty.setter
    def pointsty(self, value: float) -> None:
        """Set the pointsty property."""
        self._cards[0].set_value("pointsty", value)

    @property
    def pointstz(self) -> typing.Optional[float]:
        """Get or set the Z coordinate of the tree origin
        """ # nopep8
        return self._cards[0].get_value("pointstz")

    @pointstz.setter
    def pointstz(self, value: float) -> None:
        """Set the pointstz property."""
        self._cards[0].set_value("pointstz", value)

    @property
    def edgetlen(self) -> typing.Optional[float]:
        """Get or set the Edge length
        """ # nopep8
        return self._cards[0].get_value("edgetlen")

    @edgetlen.setter
    def edgetlen(self, value: float) -> None:
        """Set the edgetlen property."""
        self._cards[0].set_value("edgetlen", value)

    @property
    def numgen(self) -> typing.Optional[int]:
        """Get or set the Number of generations of branches
        """ # nopep8
        return self._cards[1].get_value("numgen")

    @numgen.setter
    def numgen(self, value: int) -> None:
        """Set the numgen property."""
        self._cards[1].set_value("numgen", value)

    @property
    def numbrinit(self) -> typing.Optional[int]:
        """Get or set the Number of branches attaches to the tree origin
        """ # nopep8
        return self._cards[1].get_value("numbrinit")

    @numbrinit.setter
    def numbrinit(self, value: int) -> None:
        """Set the numbrinit property."""
        self._cards[1].set_value("numbrinit", value)

    @property
    def numsplit(self) -> typing.Optional[int]:
        """Get or set the Number of children branches at each node of the tree
        """ # nopep8
        return self._cards[1].get_value("numsplit")

    @numsplit.setter
    def numsplit(self, value: int) -> None:
        """Set the numsplit property."""
        self._cards[1].set_value("numsplit", value)

    @property
    def inodestld(self) -> typing.Optional[int]:
        """Get or set the Initial node id
        """ # nopep8
        return self._cards[1].get_value("inodestld")

    @inodestld.setter
    def inodestld(self, value: int) -> None:
        """Set the inodestld property."""
        self._cards[1].set_value("inodestld", value)

    @property
    def iedgestld(self) -> typing.Optional[int]:
        """Get or set the Initial edge id
        """ # nopep8
        return self._cards[1].get_value("iedgestld")

    @iedgestld.setter
    def iedgestld(self, value: int) -> None:
        """Set the iedgestld property."""
        self._cards[1].set_value("iedgestld", value)

    @property
    def inodestld_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given inodestld."""
        return self._get_link_by_attr("NODE", "nid", self.inodestld, "parts")

    @property
    def mid_link(self) -> KeywordBase:
        """Get the MAT_* keyword for mid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid:
                return kwd
        return None

    @mid_link.setter
    def mid_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid."""
        self.mid = value.mid

    @property
    def ssid_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for ssid."""
        return self._get_set_link("SEGMENT", self.ssid)

    @ssid_link.setter
    def ssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid."""
        self.ssid = value.sid

