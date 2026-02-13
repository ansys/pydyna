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

"""Module providing the IcfdDefineNoninertial class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDDEFINENONINERTIAL_CARD0 = (
    FieldSchema("w1", float, 0, 10, None),
    FieldSchema("w2", float, 10, 10, None),
    FieldSchema("w3", float, 20, 10, None),
    FieldSchema("r", float, 30, 10, None),
    FieldSchema("ptid", int, 40, 10, None),
    FieldSchema("l", float, 50, 10, None),
    FieldSchema("lcid", int, 60, 10, None),
    FieldSchema("relv", int, 70, 10, 0),
)

class IcfdDefineNoninertial(KeywordBase):
    """DYNA ICFD_DEFINE_NONINERTIAL keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_NONINERTIAL"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdDefineNoninertial class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINENONINERTIAL_CARD0,
                **kwargs,
            ),        ]
    @property
    def w1(self) -> typing.Optional[float]:
        """Get or set the Rotational Velocity along the X,Y,Z axes
        """ # nopep8
        return self._cards[0].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        """Set the w1 property."""
        self._cards[0].set_value("w1", value)

    @property
    def w2(self) -> typing.Optional[float]:
        """Get or set the Rotational Velocity along the X,Y,Z axes
        """ # nopep8
        return self._cards[0].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        """Set the w2 property."""
        self._cards[0].set_value("w2", value)

    @property
    def w3(self) -> typing.Optional[float]:
        """Get or set the Rotational Velocity along the X,Y,Z axes
        """ # nopep8
        return self._cards[0].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        """Set the w3 property."""
        self._cards[0].set_value("w3", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the rotating reference frame
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def ptid(self) -> typing.Optional[int]:
        """Get or set the Starting point ID for the reference frame (See *ICFD_DEFINE_POINT)
        """ # nopep8
        return self._cards[0].get_value("ptid")

    @ptid.setter
    def ptid(self, value: int) -> None:
        """Set the ptid property."""
        self._cards[0].set_value("ptid", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Length of the rotating reference frame.
        """ # nopep8
        return self._cards[0].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        """Set the l property."""
        self._cards[0].set_value("l", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve for scaling factor of w.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def relv(self) -> int:
        """Get or set the Velocities computed and displayed:
        EQ.0: Relative velocity, only the non-rotating components of the velocity are used and displayed.
        EQ.1: Absolute velocity . All the components of the velocity are used. Useful in cases where several or at least one noninertial reference frame is combined with an inertial "classic" reference frame.
        """ # nopep8
        return self._cards[0].get_value("relv")

    @relv.setter
    def relv(self, value: int) -> None:
        """Set the relv property."""
        if value not in [0, 1, None]:
            raise Exception("""relv must be `None` or one of {0,1}.""")
        self._cards[0].set_value("relv", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
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

