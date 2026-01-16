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

"""Module providing the LoadMask class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_LOADMASK_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("vid1", int, 20, 10, 1),
    FieldSchema("off", float, 30, 10, 0.0),
    FieldSchema("boxid", int, 40, 10, 0),
    FieldSchema("lcidm", int, 50, 10, 0),
    FieldSchema("vid2", int, 60, 10, None),
    FieldSchema("inout", int, 70, 10, 0),
)

_LOADMASK_CARD1 = (
    FieldSchema("icycle", int, 0, 10, 200),
)

class LoadMask(KeywordBase):
    """DYNA LOAD_MASK keyword"""

    keyword = "LOAD"
    subkeyword = "MASK"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "lcidm": LinkType.DEFINE_CURVE,
        "boxid": LinkType.DEFINE_BOX,
        "vid1": LinkType.DEFINE_VECTOR,
        "vid2": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadMask class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADMASK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADMASK_CARD1,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID. This part must consist of 3D shell elements. To use this option with solid element the surface of the solid elements must be covered with null shells, see *MAT_NULL.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the pressure time history, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def vid1(self) -> int:
        """Get or set the Vector ID normal to the suface on which the applied pressure acts. Positive pressure acts in a direction that is in the opposite direction. This vector may be used if the surface on which the pressure acts is relatively flat. If zero, the pressure load depends on the orientation of the shell elements.
        """ # nopep8
        return self._cards[0].get_value("vid1")

    @vid1.setter
    def vid1(self, value: int) -> None:
        """Set the vid1 property."""
        self._cards[0].set_value("vid1", value)

    @property
    def off(self) -> float:
        """Get or set the Pressure loads will be discontinued if | VID1*n | < OFF, where n is the normal vector to the shell element.
        """ # nopep8
        return self._cards[0].get_value("off")

    @off.setter
    def off(self, value: float) -> None:
        """Set the off property."""
        self._cards[0].set_value("off", value)

    @property
    def boxid(self) -> int:
        """Get or set the Only elements inside the box with part ID, SSID , are considered. If no ID is given all elements of part ID, SSID, are included. When the active list of elements are updated, elements outside the box will no longer have pressure applied, i.e., the current configuration is always used.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def lcidm(self) -> int:
        """Get or set the Curve ID defining the mask. This curve gives (x,y) pairs of points in a local coordinate system defined by the vector ID, VID2. See also *DEFINE_CURVE. Curve should be flagged as DATTYP = 1.
        """ # nopep8
        return self._cards[0].get_value("lcidm")

    @lcidm.setter
    def lcidm(self, value: int) -> None:
        """Set the lcidm property."""
        self._cards[0].set_value("lcidm", value)

    @property
    def vid2(self) -> typing.Optional[int]:
        """Get or set the Vector ID used to project the masking curve onto the surface of part ID, PID. The origin of this vector determines the origin of the local system that the coordinates of the PID are transformed into prior to determining the pressure distribution in the local system. This curve must be defined if LCIDM is nonzero.
        """ # nopep8
        return self._cards[0].get_value("vid2")

    @vid2.setter
    def vid2(self, value: int) -> None:
        """Set the vid2 property."""
        self._cards[0].set_value("vid2", value)

    @property
    def inout(self) -> int:
        """Get or set the EQ.0: Elements whose center falls inside the projected curve are considered (default),
        EQ.1: Elements whose center falls outside the projected curve are considered.
        """ # nopep8
        return self._cards[0].get_value("inout")

    @inout.setter
    def inout(self, value: int) -> None:
        """Set the inout property."""
        if value not in [0, 1, None]:
            raise Exception("""inout must be `None` or one of {0,1}.""")
        self._cards[0].set_value("inout", value)

    @property
    def icycle(self) -> int:
        """Get or set the Number of time steps between updating the list of active elements (default=200). The list update can be quite expensive and should be done at a reasonable interval. The default is not be appropiate for all problems.
        """ # nopep8
        return self._cards[1].get_value("icycle")

    @icycle.setter
    def icycle(self, value: int) -> None:
        """Set the icycle property."""
        self._cards[1].set_value("icycle", value)

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
    def lcidm_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidm."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidm:
                return kwd
        return None

    @lcidm_link.setter
    def lcidm_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidm."""
        self.lcidm = value.lcid

    @property
    def boxid_link(self) -> DefineBox:
        """Get the DefineBox object for boxid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.boxid:
                return kwd
        return None

    @boxid_link.setter
    def boxid_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for boxid."""
        self.boxid = value.boxid

    @property
    def vid1_link(self) -> DefineVector:
        """Get the DefineVector object for vid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid1:
                return kwd
        return None

    @vid1_link.setter
    def vid1_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid1."""
        self.vid1 = value.vid

    @property
    def vid2_link(self) -> DefineVector:
        """Get the DefineVector object for vid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid2:
                return kwd
        return None

    @vid2_link.setter
    def vid2_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid2."""
        self.vid2 = value.vid

