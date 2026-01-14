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

"""Module providing the CeseBoundaryCyclicPartSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CESEBOUNDARYCYCLICPARTSET_CARD0 = (
    FieldSchema("surfsid1", int, 0, 10, None),
    FieldSchema("surfsid2", int, 10, 10, None),
    FieldSchema("cyctyp", int, 20, 10, 0),
)

_CESEBOUNDARYCYCLICPARTSET_CARD1 = (
    FieldSchema("axisx1", float, 0, 10, 0.0),
    FieldSchema("axisy1", float, 10, 10, 0.0),
    FieldSchema("axisz1", float, 20, 10, 0.0),
    FieldSchema("dirx", float, 30, 10, None),
    FieldSchema("diry", float, 40, 10, None),
    FieldSchema("dirz", float, 50, 10, None),
    FieldSchema("rotang", float, 60, 10, None),
)

_CESEBOUNDARYCYCLICPARTSET_CARD2 = (
    FieldSchema("transx", float, 0, 10, None),
    FieldSchema("transy", float, 10, 10, None),
    FieldSchema("transz", float, 20, 10, None),
)

class CeseBoundaryCyclicPartSet(KeywordBase):
    """DYNA CESE_BOUNDARY_CYCLIC_PART_SET keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_CYCLIC_PART_SET"

    def __init__(self, **kwargs):
        """Initialize the CeseBoundaryCyclicPartSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYCYCLICPARTSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYCYCLICPARTSET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYCYCLICPARTSET_CARD2,
                **kwargs,
            ),        ]
    @property
    def surfsid1(self) -> typing.Optional[int]:
        """Get or set the Identifiers of two sets of surface part IDs, each created with a *LSO_ID_SET card, where each surface part ID in each set is referenced in *MESH_SURFACE_ELEMENT cards.
        """ # nopep8
        return self._cards[0].get_value("surfsid1")

    @surfsid1.setter
    def surfsid1(self, value: int) -> None:
        """Set the surfsid1 property."""
        self._cards[0].set_value("surfsid1", value)

    @property
    def surfsid2(self) -> typing.Optional[int]:
        """Get or set the Identifiers of two sets of surface part IDs, each created with a *LSO_ID_SET card, where each surface part ID in each set is referenced in *MESH_SURFACE_ELEMENT cards.
        """ # nopep8
        return self._cards[0].get_value("surfsid2")

    @surfsid2.setter
    def surfsid2(self, value: int) -> None:
        """Set the surfsid2 property."""
        self._cards[0].set_value("surfsid2", value)

    @property
    def cyctyp(self) -> int:
        """Get or set the Relationship between the two cyclic boundary condition surfaces:EQ.0: none assumed (default)
        EQ.1: The first surface is rotated about an axis to match the second surface.
        EQ.2: The faces of the first surface are translated in a given direction to obtain the corresponding faces on the second surface.
        """ # nopep8
        return self._cards[0].get_value("cyctyp")

    @cyctyp.setter
    def cyctyp(self, value: int) -> None:
        """Set the cyctyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""cyctyp must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("cyctyp", value)

    @property
    def axisx1(self) -> float:
        """Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("axisx1")

    @axisx1.setter
    def axisx1(self, value: float) -> None:
        """Set the axisx1 property."""
        self._cards[1].set_value("axisx1", value)

    @property
    def axisy1(self) -> float:
        """Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("axisy1")

    @axisy1.setter
    def axisy1(self, value: float) -> None:
        """Set the axisy1 property."""
        self._cards[1].set_value("axisy1", value)

    @property
    def axisz1(self) -> float:
        """Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("axisz1")

    @axisz1.setter
    def axisz1(self, value: float) -> None:
        """Set the axisz1 property."""
        self._cards[1].set_value("axisz1", value)

    @property
    def dirx(self) -> typing.Optional[float]:
        """Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("dirx")

    @dirx.setter
    def dirx(self, value: float) -> None:
        """Set the dirx property."""
        self._cards[1].set_value("dirx", value)

    @property
    def diry(self) -> typing.Optional[float]:
        """Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("diry")

    @diry.setter
    def diry(self, value: float) -> None:
        """Set the diry property."""
        self._cards[1].set_value("diry", value)

    @property
    def dirz(self) -> typing.Optional[float]:
        """Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("dirz")

    @dirz.setter
    def dirz(self, value: float) -> None:
        """Set the dirz property."""
        self._cards[1].set_value("dirz", value)

    @property
    def rotang(self) -> typing.Optional[float]:
        """Get or set the The angle of rotation (in degrees) that transforms the centroid of each face on the first surface to the centroid of the corresponding face on the second surface (for CYCTYP.EQ.1).
        """ # nopep8
        return self._cards[1].get_value("rotang")

    @rotang.setter
    def rotang(self, value: float) -> None:
        """Set the rotang property."""
        self._cards[1].set_value("rotang", value)

    @property
    def transx(self) -> typing.Optional[float]:
        """Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
        """ # nopep8
        return self._cards[2].get_value("transx")

    @transx.setter
    def transx(self, value: float) -> None:
        """Set the transx property."""
        self._cards[2].set_value("transx", value)

    @property
    def transy(self) -> typing.Optional[float]:
        """Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
        """ # nopep8
        return self._cards[2].get_value("transy")

    @transy.setter
    def transy(self, value: float) -> None:
        """Set the transy property."""
        self._cards[2].set_value("transy", value)

    @property
    def transz(self) -> typing.Optional[float]:
        """Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
        """ # nopep8
        return self._cards[2].get_value("transz")

    @transz.setter
    def transz(self, value: float) -> None:
        """Set the transz property."""
        self._cards[2].set_value("transz", value)

