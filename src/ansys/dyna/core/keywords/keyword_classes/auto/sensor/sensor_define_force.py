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

"""Module providing the SensorDefineForce class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_SENSORDEFINEFORCE_CARD0 = (
    FieldSchema("sensid", int, 0, 10, None),
    FieldSchema("ftype", str, 10, 10, "AIRBAG"),
    FieldSchema("typeid", int, 20, 10, None),
    FieldSchema("vid", str, 30, 10, None),
    FieldSchema("crd", int, 40, 10, None),
    FieldSchema("cpmpid", int, 50, 10, None),
)

_SENSORDEFINEFORCE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SensorDefineForce(KeywordBase):
    """DYNA SENSOR_DEFINE_FORCE keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_FORCE"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "vid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "crd": LinkType.DEFINE_COORDINATE_SYSTEM,
        "cpmpid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the SensorDefineForce class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SENSORDEFINEFORCE_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = SensorDefineForce._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SENSORDEFINEFORCE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sensid(self) -> typing.Optional[int]:
        """Get or set the Sensor ID.
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        """Set the sensid property."""
        self._cards[0].set_value("sensid", value)

    @property
    def ftype(self) -> str:
        """Get or set the Force type.
        """ # nopep8
        return self._cards[0].get_value("ftype")

    @ftype.setter
    def ftype(self, value: str) -> None:
        """Set the ftype property."""
        if value not in ["AIRBAG", "CONTACT", "CONTACT2D", "CPM", "JOINT", "JOINTSTIF", "PRESC-MOT", "RWALL", "SPC", "SPOTWELD", "X-SECTION", None]:
            raise Exception("""ftype must be `None` or one of {"AIRBAG","CONTACT","CONTACT2D","CPM","JOINT","JOINTSTIF","PRESC-MOT","RWALL","SPC","SPOTWELD","X-SECTION"}.""")
        self._cards[0].set_value("ftype", value)

    @property
    def typeid(self) -> typing.Optional[int]:
        """Get or set the ID defined in the associated keyword command
        """ # nopep8
        return self._cards[0].get_value("typeid")

    @typeid.setter
    def typeid(self, value: int) -> None:
        """Set the typeid property."""
        self._cards[0].set_value("typeid", value)

    @property
    def vid(self) -> typing.Optional[str]:
        """Get or set the Vector along which the forces is measured.
        EQ.X: x - direction in coordinate system CRD
        EQ.Y: y - direction in coordinate system CRD
        EQ.Z: z - direction in coordinate system CRD
        EQ.XL: x - direction in the local coordinate system for JOINTSTIF only
        EQ.YL: y - direction in the local coordinate system for JOINTSTIF only
        EQ.ZL: z - direction in the local coordinate system for JOINTSTIF only
        EQ.M: Force magnitude
        EQ.XMOMENT: x - direction moment for JOINT, JOINTSTIF, PRESC - MOT, SPC or X - SECTION
        EQ.YMOMENT: y - direction moment for JOINT, JOINTSTIF, PRESC - MOT, SPC or X - SECTION
        EQ.ZMOMENT: z - direction moment for JOINT, JOINTSTIF, PRESC - MOT, SPC or X - SECTION
        EQ.XLMOMENT: x - direction moment in the local coordinate system for JOINTSTIF only
        EQ.YLMOMENT: y - direction moment in the local coordinate system for JOINTSTIF only
        EQ.ZLMOMENT: z - direction moment in the local coordinate system for JOINTSTIF only
        EQ.MMOMENT: Moment magnitude for JOINT, JOINTSTIF, PRESC - MOT or SPC
        VID {INT}: Vector ID n in coordinate system CRD
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: str) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def crd(self) -> typing.Optional[int]:
        """Get or set the Coordinate system, defined by *DEFINE_COORDINATE_NODES, to which VECT is attached.
        """ # nopep8
        return self._cards[0].get_value("crd")

    @crd.setter
    def crd(self, value: int) -> None:
        """Set the crd property."""
        self._cards[0].set_value("crd", value)

    @property
    def cpmpid(self) -> typing.Optional[int]:
        """Get or set the Part ID of a CPM airbag (see *AIRBAG_PARTICLE). This field is optional and is ignored unless FTYPE = CPM.  If it is unset, the sensor returns airbag pressure. If a part ID is provided, the sensor returns the part pressure.
        """ # nopep8
        return self._cards[0].get_value("cpmpid")

    @cpmpid.setter
    def cpmpid(self, value: int) -> None:
        """Set the cpmpid property."""
        self._cards[0].set_value("cpmpid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def vid_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for vid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.vid:
                return kwd
        return None

    @vid_link.setter
    def vid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for vid."""
        self.vid = value.cid

    @property
    def crd_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for crd."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.crd:
                return kwd
        return None

    @crd_link.setter
    def crd_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for crd."""
        self.crd = value.cid

    @property
    def cpmpid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given cpmpid."""
        return self._get_link_by_attr("PART", "pid", self.cpmpid, "parts")

