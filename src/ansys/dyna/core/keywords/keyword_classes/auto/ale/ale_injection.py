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

"""Module providing the AleInjection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_ALEINJECTION_CARD0 = (
    FieldSchema("mmgset", int, 0, 10, None),
    FieldSchema("segset", int, 10, 10, None),
    FieldSchema("global_", int, 20, 10, 0, "global"),
    FieldSchema("lce", int, 30, 10, 0),
    FieldSchema("lcrvl", int, 40, 10, 0),
)

_ALEINJECTION_CARD1 = (
    FieldSchema("lcvt", int, 0, 10, 0),
    FieldSchema("vect", int, 10, 10, 0),
    FieldSchema("lcvr", int, 20, 10, 0),
    FieldSchema("vecr", int, 30, 10, 0),
    FieldSchema("boxv", int, 40, 10, 0),
    FieldSchema("xg", float, 50, 10, 0.0),
    FieldSchema("yg", float, 60, 10, 0.0),
    FieldSchema("zg", float, 70, 10, 0.0),
)

_ALEINJECTION_CARD2 = (
    FieldSchema("surfct", int, 0, 10, 0),
    FieldSchema("ndiv", int, 10, 10, 3),
    FieldSchema("xl", float, 20, 10, 0.0),
    FieldSchema("yl", float, 30, 10, 0.0),
    FieldSchema("zd", float, 40, 10, 0.0),
    FieldSchema("zu", float, 50, 10, 0.0),
    FieldSchema("xc", float, 60, 10, 0.0),
    FieldSchema("yc", float, 70, 10, 0.0),
)

class AleInjection(KeywordBase):
    """DYNA ALE_INJECTION keyword"""

    keyword = "ALE"
    subkeyword = "INJECTION"
    _link_fields = {
        "boxv": LinkType.DEFINE_BOX,
        "vect": LinkType.DEFINE_VECTOR,
        "vecr": LinkType.DEFINE_VECTOR,
        "segset": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the AleInjection class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEINJECTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEINJECTION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEINJECTION_CARD2,
                **kwargs,
            ),        ]
    @property
    def mmgset(self) -> typing.Optional[int]:
        """Get or set the Multi-Material Set ID (see *SET_MULTI-MATERIAL_GROUP_LIST).
        """ # nopep8
        return self._cards[0].get_value("mmgset")

    @mmgset.setter
    def mmgset(self, value: int) -> None:
        """Set the mmgset property."""
        self._cards[0].set_value("mmgset", value)

    @property
    def segset(self) -> typing.Optional[int]:
        """Get or set the Segment set ID (see *SET_SEGMENT). A local coordinate system
        is created for each segment. See Remark 2.
        """ # nopep8
        return self._cards[0].get_value("segset")

    @segset.setter
    def segset(self, value: int) -> None:
        """Set the segset property."""
        self._cards[0].set_value("segset", value)

    @property
    def global_(self) -> int:
        """Get or set the Three digit flag to control how to select the elements, how to
        prescribe the velocities and how to define the geometrical
        parameters of Cards 2 and 3 (including BOXV):
        EQ._ _ 0: Geometrical parameters are local to the segments of SEGSET
        EQ._ _ 1: Geometrical parameters are natural to SEGSET
        segments (see Remark 3 and Figure 4-1)
        EQ._ 0 _: Velocities are applied in local coordinate systems
        attached to each segment of SEGSET
        EQ._ 1 _: Velocities are applied in the global coordinate system
        EQ.0 _ _: Select the elements and nodes in the local volume
        around each segment of SEGSET
        EQ.1 _ _: Select the elements in the global volume formed by
        all the segments of SEGSET
        EQ.2 _ _: Select the elements and nodes in the global volume
        formed by all the segments of SEGSET. Velocities are
        applied in the global coordinate system.
        """ # nopep8
        return self._cards[0].get_value("global_")

    @global_.setter
    def global_(self, value: int) -> None:
        """Set the global_ property."""
        self._cards[0].set_value("global_", value)

    @property
    def lce(self) -> int:
        """Get or set the Curve ID for the internal energy (see Remark 6):
        GT.0: Load curve ID; see *DEFINE_CURVE. See Remark 2.
        LT.0: -LCE is the function ID for the internal energy which
        depends on 26 arguments: time, number of cycles, and
        nodal coordinates of the 8 nodes for the ALE element.
        See *DEFINE_FUNCTION. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("lce")

    @lce.setter
    def lce(self, value: int) -> None:
        """Set the lce property."""
        self._cards[0].set_value("lce", value)

    @property
    def lcrvl(self) -> int:
        """Get or set the Curve ID for the relative volume (see Remark 6):
        GT.0: Load curve ID; see *DEFINE_CURVE. See Remark 2.
        LT.0: -LCRVL is the function ID for the relative volume which
        depends on 26 arguments: time, number of cycles, and
        nodal coordinates of the 8 nodes for the ALE element.
        See *DEFINE_FUNCTION. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("lcrvl")

    @lcrvl.setter
    def lcrvl(self, value: int) -> None:
        """Set the lcrvl property."""
        self._cards[0].set_value("lcrvl", value)

    @property
    def lcvt(self) -> int:
        """Get or set the Curve ID for the translational velocity:
        GT.0: Load curve ID; see *DEFINE_CURVE.
        LT.0: -LCVT is the function ID for the translational velocity
        which depends on 5 arguments: time, number of cycles,
        and nodal coordinates. See *DEFINE_FUNCTION. See Remark 5..
        """ # nopep8
        return self._cards[1].get_value("lcvt")

    @lcvt.setter
    def lcvt(self, value: int) -> None:
        """Set the lcvt property."""
        self._cards[1].set_value("lcvt", value)

    @property
    def vect(self) -> int:
        """Get or set the Vector to orient the translation. See *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[1].get_value("vect")

    @vect.setter
    def vect(self, value: int) -> None:
        """Set the vect property."""
        self._cards[1].set_value("vect", value)

    @property
    def lcvr(self) -> int:
        """Get or set the Curve ID for the rotational velocity:
        GT.0: Load curve ID; see *DEFINE_CURVE.
        LT.0: -LCVR is the function ID for the rotational velocity which
        depends on 5 arguments: time, number of cycles, and
        nodal coordinates. See *DEFINE_FUNCTION. See Remark	5.
        """ # nopep8
        return self._cards[1].get_value("lcvr")

    @lcvr.setter
    def lcvr(self, value: int) -> None:
        """Set the lcvr property."""
        self._cards[1].set_value("lcvr", value)

    @property
    def vecr(self) -> int:
        """Get or set the Vector to orient the rotational axis (see *DEFINE_VECTOR).
        """ # nopep8
        return self._cards[1].get_value("vecr")

    @vecr.setter
    def vecr(self, value: int) -> None:
        """Set the vecr property."""
        self._cards[1].set_value("vecr", value)

    @property
    def boxv(self) -> int:
        """Get or set the Box (see *DEFINE_BOX) defining the region where the velocities are applied (see Remark 7).
        """ # nopep8
        return self._cards[1].get_value("boxv")

    @boxv.setter
    def boxv(self, value: int) -> None:
        """Set the boxv property."""
        self._cards[1].set_value("boxv", value)

    @property
    def xg(self) -> float:
        """Get or set the Position of the rotation center (see Remark 8).
        """ # nopep8
        return self._cards[1].get_value("xg")

    @xg.setter
    def xg(self, value: float) -> None:
        """Set the xg property."""
        self._cards[1].set_value("xg", value)

    @property
    def yg(self) -> float:
        """Get or set the Position of the rotation center (see Remark 8).
        """ # nopep8
        return self._cards[1].get_value("yg")

    @yg.setter
    def yg(self, value: float) -> None:
        """Set the yg property."""
        self._cards[1].set_value("yg", value)

    @property
    def zg(self) -> float:
        """Get or set the Position of the rotation center (see Remark 8).
        """ # nopep8
        return self._cards[1].get_value("zg")

    @zg.setter
    def zg(self, value: float) -> None:
        """Set the zg property."""
        self._cards[1].set_value("zg", value)

    @property
    def surfct(self) -> int:
        """Get or set the Flag to define the surface, inside which the nodes and elements are selected:
        LT.0: -SURFCT is the Function ID (see *DEFINE_FUNCTION)
        for the rotational velocity with 17 arguments: time, number
        of cycles, ALE element center coordinates, segment nodal coordinates.
        EQ.0: Ellipsoid;
        EQ.1: Ellipse-based cylinder;
        EQ.2: Truncated ellipse-based cone;
        EQ.3: Drop geometry meaning a cone for -ZD < z < 0 and
        half an ellipsoid for 0< z < ZU (see Remark 11 and Figure 4-6);
        EQ.4: Box with side lengths -XL < x < XL, -YL < y < YL,
        and -ZD < z < ZU (see Figure 4-7)
        EQ.5: Segment based cylinder (see Remark 12 and Figure 4-8).
        """ # nopep8
        return self._cards[2].get_value("surfct")

    @surfct.setter
    def surfct(self, value: int) -> None:
        """Set the surfct property."""
        self._cards[2].set_value("surfct", value)

    @property
    def ndiv(self) -> int:
        """Get or set the Number of divisions of an element cut by the surface SURFCT to
        compute the volume fractions (see Remark 13 and Figure 4-2).
        """ # nopep8
        return self._cards[2].get_value("ndiv")

    @ndiv.setter
    def ndiv(self, value: int) -> None:
        """Set the ndiv property."""
        self._cards[2].set_value("ndiv", value)

    @property
    def xl(self) -> float:
        """Get or set the Length of the geometry SURFCT in the local x-direction.
        """ # nopep8
        return self._cards[2].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        """Set the xl property."""
        self._cards[2].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the Length of the geometry SURFCT in the local y-direction.
        """ # nopep8
        return self._cards[2].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        """Set the yl property."""
        self._cards[2].set_value("yl", value)

    @property
    def zd(self) -> float:
        """Get or set the Length for the geometry SURFCT in the local 𝑧-direction for z < 0,
        except for SURFCT = 2 where z > 0. ZD can be input as a negative or positive value.
        """ # nopep8
        return self._cards[2].get_value("zd")

    @zd.setter
    def zd(self, value: float) -> None:
        """Set the zd property."""
        self._cards[2].set_value("zd", value)

    @property
    def zu(self) -> float:
        """Get or set the Length for the geometry SURFCT in the local 𝑧-direction for z > 0.
        """ # nopep8
        return self._cards[2].get_value("zu")

    @zu.setter
    def zu(self, value: float) -> None:
        """Set the zu property."""
        self._cards[2].set_value("zu", value)

    @property
    def xc(self) -> float:
        """Get or set the x-coordinate in the segment of the local coordinate center (see Remark 14).
        """ # nopep8
        return self._cards[2].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[2].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the y-coordinate in the segment of the local coordinate center (see Remark 14).
        """ # nopep8
        return self._cards[2].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[2].set_value("yc", value)

    @property
    def boxv_link(self) -> DefineBox:
        """Get the DefineBox object for boxv."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.boxv:
                return kwd
        return None

    @boxv_link.setter
    def boxv_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for boxv."""
        self.boxv = value.boxid

    @property
    def vect_link(self) -> DefineVector:
        """Get the DefineVector object for vect."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vect:
                return kwd
        return None

    @vect_link.setter
    def vect_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vect."""
        self.vect = value.vid

    @property
    def vecr_link(self) -> DefineVector:
        """Get the DefineVector object for vecr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vecr:
                return kwd
        return None

    @vecr_link.setter
    def vecr_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vecr."""
        self.vecr = value.vid

    @property
    def segset_link(self) -> KeywordBase:
        """Get the SET_SEGMENT_* keyword for segset."""
        return self._get_set_link("SEGMENT", self.segset)

    @segset_link.setter
    def segset_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for segset."""
        self.segset = value.sid

