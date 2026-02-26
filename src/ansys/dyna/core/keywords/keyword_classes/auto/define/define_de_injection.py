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

"""Module providing the DefineDeInjection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DEFINEDEINJECTION_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("sid", int, 10, 10, None),
    FieldSchema("xc", float, 20, 10, 0.0),
    FieldSchema("yc", float, 30, 10, 0.0),
    FieldSchema("zc", float, 40, 10, 0.0),
    FieldSchema("xl", float, 50, 10, 0.0),
    FieldSchema("yl", float, 60, 10, 0.0),
    FieldSchema("cid", int, 70, 10, 0),
)

_DEFINEDEINJECTION_CARD1 = (
    FieldSchema("rmass", float, 0, 10, None),
    FieldSchema("rmin", float, 10, 10, None),
    FieldSchema("rmax", float, 20, 10, None),
    FieldSchema("vx", float, 30, 10, 0.0),
    FieldSchema("vy", float, 40, 10, 0.0),
    FieldSchema("vz", float, 50, 10, 0.0),
    FieldSchema("tbeg", float, 60, 10, 0.0),
    FieldSchema("tend", float, 70, 10, 1e+20),
)

_DEFINEDEINJECTION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeInjection(KeywordBase):
    """DYNA DEFINE_DE_INJECTION keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_INJECTION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "sid": LinkType.SET_NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineDeInjection class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEINJECTION_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeInjection.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDEINJECTION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of new generated DES nodes
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set ID of new generated DES nodes
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def xc(self) -> float:
        """Get or set the X coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Y coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Z coordinate of the center of injection plane.
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[0].set_value("zc", value)

    @property
    def xl(self) -> float:
        """Get or set the Length of the rectangular injection plane along X-axis in the coordinate	system(CID) defined.
        """ # nopep8
        return self._cards[0].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        """Set the xl property."""
        self._cards[0].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the Length of the rectangular injection plane along Y-axis in the coordinate	system(CID) defined.
        """ # nopep8
        return self._cards[0].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        """Set the yl property."""
        self._cards[0].set_value("yl", value)

    @property
    def cid(self) -> int:
        """Get or set the Optional local coordinate system ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def rmass(self) -> typing.Optional[float]:
        """Get or set the Mass flow rate
        """ # nopep8
        return self._cards[1].get_value("rmass")

    @rmass.setter
    def rmass(self, value: float) -> None:
        """Set the rmass property."""
        self._cards[1].set_value("rmass", value)

    @property
    def rmin(self) -> typing.Optional[float]:
        """Get or set the Minimum DES radius (ignored if IMULTI > 1)
        """ # nopep8
        return self._cards[1].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        """Set the rmin property."""
        self._cards[1].set_value("rmin", value)

    @property
    def rmax(self) -> typing.Optional[float]:
        """Get or set the Maximum DES radius.(ignored if IMULTI > 1)
        """ # nopep8
        return self._cards[1].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        """Set the rmax property."""
        self._cards[1].set_value("rmax", value)

    @property
    def vx(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[1].set_value("vz", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Birth time.
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        """Set the tbeg property."""
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the Death time.
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[1].set_value("tend", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
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
    def sid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for sid."""
        return self._get_set_link("NODE", self.sid)

    @sid_link.setter
    def sid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for sid."""
        self.sid = value.sid

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

