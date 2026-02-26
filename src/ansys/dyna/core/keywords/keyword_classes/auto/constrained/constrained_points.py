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

"""Module providing the ConstrainedPoints class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONSTRAINEDPOINTS_CARD0 = (
    FieldSchema("cid", int, 0, 10, None),
)

_CONSTRAINEDPOINTS_CARD1 = (
    FieldSchema("eid1", int, 0, 8, None),
    FieldSchema("x1", float, 8, 16, 0.0),
    FieldSchema("y1", float, 24, 16, 0.0),
    FieldSchema("z1", float, 40, 16, 0.0),
)

_CONSTRAINEDPOINTS_CARD2 = (
    FieldSchema("eid2", int, 0, 8, None),
    FieldSchema("x2", float, 8, 16, 0.0),
    FieldSchema("y2", float, 24, 16, 0.0),
    FieldSchema("z2", float, 40, 16, 0.0),
)

_CONSTRAINEDPOINTS_CARD3 = (
    FieldSchema("psf", float, 0, 10, 1.0),
    FieldSchema("faila", float, 10, 10, 0.0),
    FieldSchema("fails", float, 20, 10, 0.0),
    FieldSchema("failm", float, 30, 10, 0.0),
)

class ConstrainedPoints(KeywordBase):
    """DYNA CONSTRAINED_POINTS keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "POINTS"
    _link_fields = {
        "eid1": LinkType.ELEMENT_SHELL,
        "eid2": LinkType.ELEMENT_SHELL,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedPoints class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDPOINTS_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDPOINTS_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDPOINTS_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDPOINTS_CARD3,
                **kwargs,
            ),        ]
    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Constrained points ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def eid1(self) -> typing.Optional[int]:
        """Get or set the First Shell element ID
        """ # nopep8
        return self._cards[1].get_value("eid1")

    @eid1.setter
    def eid1(self, value: int) -> None:
        """Set the eid1 property."""
        self._cards[1].set_value("eid1", value)

    @property
    def x1(self) -> float:
        """Get or set the X Coordinates of the constrained point 1
        """ # nopep8
        return self._cards[1].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[1].set_value("x1", value)

    @property
    def y1(self) -> float:
        """Get or set the Y Coordinates of the constrained point 1
        """ # nopep8
        return self._cards[1].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[1].set_value("y1", value)

    @property
    def z1(self) -> float:
        """Get or set the Z Coordinates of the constrained point 1
        """ # nopep8
        return self._cards[1].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[1].set_value("z1", value)

    @property
    def eid2(self) -> typing.Optional[int]:
        """Get or set the Second Shell element ID
        """ # nopep8
        return self._cards[2].get_value("eid2")

    @eid2.setter
    def eid2(self, value: int) -> None:
        """Set the eid2 property."""
        self._cards[2].set_value("eid2", value)

    @property
    def x2(self) -> float:
        """Get or set the X Coordinates of the constrained point 2
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> float:
        """Get or set the Y Coordinates of the constrained point 2
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[2].set_value("y2", value)

    @property
    def z2(self) -> float:
        """Get or set the Z Coordinates of the constrained point 2
        """ # nopep8
        return self._cards[2].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        """Set the z2 property."""
        self._cards[2].set_value("z2", value)

    @property
    def psf(self) -> float:
        """Get or set the Penalty scale factor (Default=1.0).
        """ # nopep8
        return self._cards[3].get_value("psf")

    @psf.setter
    def psf(self, value: float) -> None:
        """Set the psf property."""
        self._cards[3].set_value("psf", value)

    @property
    def faila(self) -> float:
        """Get or set the Axial force resultant failure value (Skip if zero.).
        """ # nopep8
        return self._cards[3].get_value("faila")

    @faila.setter
    def faila(self, value: float) -> None:
        """Set the faila property."""
        self._cards[3].set_value("faila", value)

    @property
    def fails(self) -> float:
        """Get or set the Shear force resultant failure value (Skip if zero.).
        """ # nopep8
        return self._cards[3].get_value("fails")

    @fails.setter
    def fails(self, value: float) -> None:
        """Set the fails property."""
        self._cards[3].set_value("fails", value)

    @property
    def failm(self) -> float:
        """Get or set the Moment resultant failure value (Skip if zero.).
        """ # nopep8
        return self._cards[3].get_value("failm")

    @failm.setter
    def failm(self, value: float) -> None:
        """Set the failm property."""
        self._cards[3].set_value("failm", value)

    @property
    def eid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid1."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid1, "parts")

    @property
    def eid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid2."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid2, "parts")

