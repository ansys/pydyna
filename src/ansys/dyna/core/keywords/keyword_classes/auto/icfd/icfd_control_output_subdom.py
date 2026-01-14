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

"""Module providing the IcfdControlOutputSubdom class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLOUTPUTSUBDOM_CARD0 = (
    FieldSchema("sname", str, 0, 20, "BOX"),
)

_ICFDCONTROLOUTPUTSUBDOM_CARD1 = (
    FieldSchema("pminx", float, 0, 10, None),
    FieldSchema("pminy", float, 10, 10, None),
    FieldSchema("pminz", float, 20, 10, None),
    FieldSchema("pmaxx", float, 30, 10, None),
    FieldSchema("pmaxy", float, 40, 10, None),
    FieldSchema("pmaxz", float, 50, 10, None),
)

_ICFDCONTROLOUTPUTSUBDOM_CARD2 = (
    FieldSchema("radius", float, 0, 10, None),
    FieldSchema("centerx", float, 10, 10, None),
    FieldSchema("centery", float, 20, 10, None),
    FieldSchema("centerz", float, 30, 10, None),
)

_ICFDCONTROLOUTPUTSUBDOM_CARD3 = (
    FieldSchema("radius", float, 0, 10, None),
    FieldSchema("pminx", float, 10, 10, None),
    FieldSchema("pminy", float, 20, 10, None),
    FieldSchema("pminz", float, 30, 10, None),
    FieldSchema("pmaxx", float, 40, 10, None),
    FieldSchema("pmaxy", float, 50, 10, None),
    FieldSchema("pmaxz", float, 60, 10, None),
)

class IcfdControlOutputSubdom(KeywordBase):
    """DYNA ICFD_CONTROL_OUTPUT_SUBDOM keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_OUTPUT_SUBDOM"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlOutputSubdom class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLOUTPUTSUBDOM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLOUTPUTSUBDOM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLOUTPUTSUBDOM_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLOUTPUTSUBDOM_CARD3,
                **kwargs,
            ),        ]
    @property
    def sname(self) -> str:
        """Get or set the Shape name.
        """ # nopep8
        return self._cards[0].get_value("sname")

    @sname.setter
    def sname(self, value: str) -> None:
        """Set the sname property."""
        if value not in ["BOX", "CYLINDER", "SPHERE", None]:
            raise Exception("""sname must be `None` or one of {"BOX","CYLINDER","SPHERE"}.""")
        self._cards[0].set_value("sname", value)

    @property
    def pminx(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[1].get_value("pminx")

    @pminx.setter
    def pminx(self, value: float) -> None:
        """Set the pminx property."""
        self._cards[1].set_value("pminx", value)

    @property
    def pminy(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[1].get_value("pminy")

    @pminy.setter
    def pminy(self, value: float) -> None:
        """Set the pminy property."""
        self._cards[1].set_value("pminy", value)

    @property
    def pminz(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[1].get_value("pminz")

    @pminz.setter
    def pminz(self, value: float) -> None:
        """Set the pminz property."""
        self._cards[1].set_value("pminz", value)

    @property
    def pmaxx(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[1].get_value("pmaxx")

    @pmaxx.setter
    def pmaxx(self, value: float) -> None:
        """Set the pmaxx property."""
        self._cards[1].set_value("pmaxx", value)

    @property
    def pmaxy(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[1].get_value("pmaxy")

    @pmaxy.setter
    def pmaxy(self, value: float) -> None:
        """Set the pmaxy property."""
        self._cards[1].set_value("pmaxy", value)

    @property
    def pmaxz(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[1].get_value("pmaxz")

    @pmaxz.setter
    def pmaxz(self, value: float) -> None:
        """Set the pmaxz property."""
        self._cards[1].set_value("pmaxz", value)

    @property
    def radius(self) -> typing.Optional[float]:
        """Get or set the Radius of the sphere.
        """ # nopep8
        return self._cards[2].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        """Set the radius property."""
        self._cards[2].set_value("radius", value)

    @property
    def centerx(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the sphere center in cases where Sname is Sphere
        """ # nopep8
        return self._cards[2].get_value("centerx")

    @centerx.setter
    def centerx(self, value: float) -> None:
        """Set the centerx property."""
        self._cards[2].set_value("centerx", value)

    @property
    def centery(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the sphere center in cases where Sname is Sphere
        """ # nopep8
        return self._cards[2].get_value("centery")

    @centery.setter
    def centery(self, value: float) -> None:
        """Set the centery property."""
        self._cards[2].set_value("centery", value)

    @property
    def centerz(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the sphere center in cases where Sname is Sphere
        """ # nopep8
        return self._cards[2].get_value("centerz")

    @centerz.setter
    def centerz(self, value: float) -> None:
        """Set the centerz property."""
        self._cards[2].set_value("centerz", value)

    @property
    def radius(self) -> typing.Optional[float]:
        """Get or set the the cross section disk if SNAME is cylinder.
        """ # nopep8
        return self._cards[3].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        """Set the radius property."""
        self._cards[3].set_value("radius", value)

    @property
    def pminx(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[3].get_value("pminx")

    @pminx.setter
    def pminx(self, value: float) -> None:
        """Set the pminx property."""
        self._cards[3].set_value("pminx", value)

    @property
    def pminy(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[3].get_value("pminy")

    @pminy.setter
    def pminy(self, value: float) -> None:
        """Set the pminy property."""
        self._cards[3].set_value("pminy", value)

    @property
    def pminz(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[3].get_value("pminz")

    @pminz.setter
    def pminz(self, value: float) -> None:
        """Set the pminz property."""
        self._cards[3].set_value("pminz", value)

    @property
    def pmaxx(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[3].get_value("pmaxx")

    @pmaxx.setter
    def pmaxx(self, value: float) -> None:
        """Set the pmaxx property."""
        self._cards[3].set_value("pmaxx", value)

    @property
    def pmaxy(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[3].get_value("pmaxy")

    @pmaxy.setter
    def pmaxy(self, value: float) -> None:
        """Set the pmaxy property."""
        self._cards[3].set_value("pmaxy", value)

    @property
    def pmaxz(self) -> typing.Optional[float]:
        """Get or set the X, Y, Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[3].get_value("pmaxz")

    @pmaxz.setter
    def pmaxz(self, value: float) -> None:
        """Set the pmaxz property."""
        self._cards[3].set_value("pmaxz", value)

