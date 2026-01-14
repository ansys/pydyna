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

"""Module providing the AlePrescribedMotion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ALEPRESCRIBEDMOTION_CARD0 = (
    FieldSchema("mmsid", int, 0, 10, None),
    FieldSchema("inside", int, 10, 10, 0),
    FieldSchema("sidr", int, 20, 10, 0),
)

_ALEPRESCRIBEDMOTION_CARD1 = (
    FieldSchema("lcvtx", int, 0, 10, None),
    FieldSchema("lcvty", int, 10, 10, None),
    FieldSchema("lcvtz", int, 20, 10, None),
)

_ALEPRESCRIBEDMOTION_CARD2 = (
    FieldSchema("lcvrx", int, 0, 10, None),
    FieldSchema("lcvry", int, 10, 10, None),
    FieldSchema("lcvrz", int, 20, 10, None),
)

_ALEPRESCRIBEDMOTION_CARD3 = (
    FieldSchema("xg", float, 0, 10, None),
    FieldSchema("yg", float, 10, 10, None),
    FieldSchema("zg", float, 20, 10, None),
)

class AlePrescribedMotion(KeywordBase):
    """DYNA ALE_PRESCRIBED_MOTION keyword"""

    keyword = "ALE"
    subkeyword = "PRESCRIBED_MOTION"
    _link_fields = {
        "lcvtx": LinkType.DEFINE_CURVE,
        "lcvty": LinkType.DEFINE_CURVE,
        "lcvtz": LinkType.DEFINE_CURVE,
        "lcvrx": LinkType.DEFINE_CURVE,
        "lcvry": LinkType.DEFINE_CURVE,
        "lcvrz": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the AlePrescribedMotion class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEPRESCRIBEDMOTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEPRESCRIBEDMOTION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEPRESCRIBEDMOTION_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEPRESCRIBEDMOTION_CARD3,
                **kwargs,
            ),        ]
    @property
    def mmsid(self) -> typing.Optional[int]:
        """Get or set the Multi-Material Set ID (see *SET_‌MULTI-MATERIAL_‌GROUP_‌LIST).
        """ # nopep8
        return self._cards[0].get_value("mmsid")

    @mmsid.setter
    def mmsid(self, value: int) -> None:
        """Set the mmsid property."""
        self._cards[0].set_value("mmsid", value)

    @property
    def inside(self) -> int:
        """Get or set the Flag to define which nodes the motion is prescribed for (see Remark 2):
        EQ.0:	Nodes connected to at least one ALE element that is at the minimum partially filled by a group of MMSID
        EQ.1:	Nodes connected to at least one ALE element that is fully filled by a group of MMSID
        EQ.2:	Nodes only connected to ALE elements that are fully filled by a group of MMSID.
        """ # nopep8
        return self._cards[0].get_value("inside")

    @inside.setter
    def inside(self, value: int) -> None:
        """Set the inside property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""inside must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("inside", value)

    @property
    def sidr(self) -> int:
        """Get or set the Flag controlling the use of this keyword during dynamic relaxation.
        EQ.0:	the keyword is applied in normal analysis phase only,
        EQ.1:	the keyword is applied in dynamic relaxation phase but not the normal analysis phase,
        EQ.2:	the keyword is applied in both dynamic relaxation phase and normal analysis phase.
        """ # nopep8
        return self._cards[0].get_value("sidr")

    @sidr.setter
    def sidr(self, value: int) -> None:
        """Set the sidr property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sidr must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("sidr", value)

    @property
    def lcvtx(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("lcvtx")

    @lcvtx.setter
    def lcvtx(self, value: int) -> None:
        """Set the lcvtx property."""
        self._cards[1].set_value("lcvtx", value)

    @property
    def lcvty(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3..
        """ # nopep8
        return self._cards[1].get_value("lcvty")

    @lcvty.setter
    def lcvty(self, value: int) -> None:
        """Set the lcvty property."""
        self._cards[1].set_value("lcvty", value)

    @property
    def lcvtz(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3..
        """ # nopep8
        return self._cards[1].get_value("lcvtz")

    @lcvtz.setter
    def lcvtz(self, value: int) -> None:
        """Set the lcvtz property."""
        self._cards[1].set_value("lcvtz", value)

    @property
    def lcvrx(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("lcvrx")

    @lcvrx.setter
    def lcvrx(self, value: int) -> None:
        """Set the lcvrx property."""
        self._cards[2].set_value("lcvrx", value)

    @property
    def lcvry(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("lcvry")

    @lcvry.setter
    def lcvry(self, value: int) -> None:
        """Set the lcvry property."""
        self._cards[2].set_value("lcvry", value)

    @property
    def lcvrz(self) -> typing.Optional[int]:
        """Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("lcvrz")

    @lcvrz.setter
    def lcvrz(self, value: int) -> None:
        """Set the lcvrz property."""
        self._cards[2].set_value("lcvrz", value)

    @property
    def xg(self) -> typing.Optional[float]:
        """Get or set the Position of the rotation center. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("xg")

    @xg.setter
    def xg(self, value: float) -> None:
        """Set the xg property."""
        self._cards[3].set_value("xg", value)

    @property
    def yg(self) -> typing.Optional[float]:
        """Get or set the Position of the rotation center. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("yg")

    @yg.setter
    def yg(self, value: float) -> None:
        """Set the yg property."""
        self._cards[3].set_value("yg", value)

    @property
    def zg(self) -> typing.Optional[float]:
        """Get or set the Position of the rotation center. See Remark 4.
        """ # nopep8
        return self._cards[3].get_value("zg")

    @zg.setter
    def zg(self, value: float) -> None:
        """Set the zg property."""
        self._cards[3].set_value("zg", value)

    @property
    def lcvtx_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvtx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvtx:
                return kwd
        return None

    @lcvtx_link.setter
    def lcvtx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvtx."""
        self.lcvtx = value.lcid

    @property
    def lcvty_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvty."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvty:
                return kwd
        return None

    @lcvty_link.setter
    def lcvty_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvty."""
        self.lcvty = value.lcid

    @property
    def lcvtz_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvtz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvtz:
                return kwd
        return None

    @lcvtz_link.setter
    def lcvtz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvtz."""
        self.lcvtz = value.lcid

    @property
    def lcvrx_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvrx."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvrx:
                return kwd
        return None

    @lcvrx_link.setter
    def lcvrx_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvrx."""
        self.lcvrx = value.lcid

    @property
    def lcvry_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvry."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvry:
                return kwd
        return None

    @lcvry_link.setter
    def lcvry_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvry."""
        self.lcvry = value.lcid

    @property
    def lcvrz_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcvrz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvrz:
                return kwd
        return None

    @lcvrz_link.setter
    def lcvrz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvrz."""
        self.lcvrz = value.lcid

