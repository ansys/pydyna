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

"""Module providing the LoadErodingPartSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADERODINGPARTSET_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("sf", float, 20, 10, 1.0),
    FieldSchema("at", float, 30, 10, 0.0),
    FieldSchema("psid", int, 40, 10, None),
    FieldSchema("boxid", int, 50, 10, 0),
    FieldSchema("mem", int, 60, 10, 50),
    FieldSchema("alpha", float, 70, 10, 80.0),
)

_LOADERODINGPARTSET_CARD1 = (
    FieldSchema("iflag", int, 0, 10, 0),
    FieldSchema("x", float, 10, 10, 0.0),
    FieldSchema("y", float, 20, 10, 0.0),
    FieldSchema("z", float, 30, 10, 0.0),
    FieldSchema("beta", float, 40, 10, 90.0),
)

class LoadErodingPartSet(KeywordBase):
    """DYNA LOAD_ERODING_PART_SET keyword"""

    keyword = "LOAD"
    subkeyword = "ERODING_PART_SET"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadErodingPartSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADERODINGPARTSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADERODINGPARTSET_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID number.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining pressure as a function of time, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Arrival time.
        """ # nopep8
        return self._cards[0].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        """Set the at property."""
        self._cards[0].set_value("at", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID, see *SET_PART..
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def boxid(self) -> int:
        """Get or set the Box ID, see *DEFINE_BOX.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def mem(self) -> int:
        """Get or set the Extra memory, in percent, to be allocated above the initial memory for storing the new load segments exposed by the erosion.
        """ # nopep8
        return self._cards[0].get_value("mem")

    @mem.setter
    def mem(self, value: int) -> None:
        """Set the mem property."""
        self._cards[0].set_value("mem", value)

    @property
    def alpha(self) -> float:
        """Get or set the The maximum angle (in degrees) permitted between the normal of a segment at its centroid and the average normal at its nodes. This angle is used to eliminate interior segments.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def iflag(self) -> int:
        """Get or set the Flag for choosing a subset of the exposed surface that is oriented towards a blast or other loading source. The vector from the center of the element to the source location must be within an angle of BETA of the surface normal. If IFLAG>0, then the subset is chosen, otherwise if  IFLAG=0, the entire surface is loaded.
        """ # nopep8
        return self._cards[1].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        """Set the iflag property."""
        self._cards[1].set_value("iflag", value)

    @property
    def x(self) -> float:
        """Get or set the Optional source location.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the Optional source location.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the Optional source location.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def beta(self) -> float:
        """Get or set the Maximum permitted angle (in degrees) between the surface normal and the vector to the source. The exposed segment is not loaded if the calculated angle is greater than BETA.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

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

