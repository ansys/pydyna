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

"""Module providing the EfvBoundaryTransmit class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVBOUNDARYTRANSMIT_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("imn", int, 10, 10, None),
    FieldSchema("imx", int, 20, 10, None),
    FieldSchema("jmn", int, 30, 10, None),
    FieldSchema("jmx", int, 40, 10, None),
    FieldSchema("kmn", int, 50, 10, None),
    FieldSchema("kmx", int, 60, 10, None),
)

_EFVBOUNDARYTRANSMIT_CARD1 = (
    FieldSchema("vel", float, 0, 10, None),
    FieldSchema("prs", float, 10, 10, None),
    FieldSchema("imp", float, 20, 10, None),
)

class EfvBoundaryTransmit(KeywordBase):
    """DYNA EFV_BOUNDARY_TRANSMIT keyword"""

    keyword = "EFV"
    subkeyword = "BOUNDARY_TRANSMIT"

    def __init__(self, **kwargs):
        """Initialize the EfvBoundaryTransmit class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVBOUNDARYTRANSMIT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVBOUNDARYTRANSMIT_CARD1,
                **kwargs,
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID of part or segment sets needed for providing the boundary:
        LT.0: | ID | is a segment set id(see *SET_SEGMENT).This segment set provides the boundary faces.
        GT.0: ID is a part set id(see *SET_PART) defined in *EFV_STRUCTURED_MESH.This part set identifies the Finite Volume Euler mesh from which the boundary is extracted with IMN, IMX, JMN, JMX, KMN, and KMX.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def imn(self) -> typing.Optional[int]:
        """Get or set the Minimum control point along the x-axis to select the outer faces of the mesh. This field is ignored if ID < 0.
        """ # nopep8
        return self._cards[0].get_value("imn")

    @imn.setter
    def imn(self, value: int) -> None:
        """Set the imn property."""
        self._cards[0].set_value("imn", value)

    @property
    def imx(self) -> typing.Optional[int]:
        """Get or set the Maximum control point along the x-axis to select the outer faces of the mesh. This field is ignored if ID < 0.
        """ # nopep8
        return self._cards[0].get_value("imx")

    @imx.setter
    def imx(self, value: int) -> None:
        """Set the imx property."""
        self._cards[0].set_value("imx", value)

    @property
    def jmn(self) -> typing.Optional[int]:
        """Get or set the Minimum control point along the y-axis to select the outer faces of the mesh. This field is ignored if ID < 0.
        """ # nopep8
        return self._cards[0].get_value("jmn")

    @jmn.setter
    def jmn(self, value: int) -> None:
        """Set the jmn property."""
        self._cards[0].set_value("jmn", value)

    @property
    def jmx(self) -> typing.Optional[int]:
        """Get or set the Maximum control point along the y-axis to select the outer faces of the mesh. This field is ignored if ID < 0.
        """ # nopep8
        return self._cards[0].get_value("jmx")

    @jmx.setter
    def jmx(self, value: int) -> None:
        """Set the jmx property."""
        self._cards[0].set_value("jmx", value)

    @property
    def kmn(self) -> typing.Optional[int]:
        """Get or set the Maximum control point along the z-axis to select the outer faces of the mesh. This field is ignored if ID < 0.
        """ # nopep8
        return self._cards[0].get_value("kmn")

    @kmn.setter
    def kmn(self, value: int) -> None:
        """Set the kmn property."""
        self._cards[0].set_value("kmn", value)

    @property
    def kmx(self) -> typing.Optional[int]:
        """Get or set the Maximum control point along the z-axis to select the outer faces of the mesh. This field is ignored if ID < 0.
        """ # nopep8
        return self._cards[0].get_value("kmx")

    @kmx.setter
    def kmx(self, value: int) -> None:
        """Set the kmx property."""
        self._cards[0].set_value("kmx", value)

    @property
    def vel(self) -> typing.Optional[float]:
        """Get or set the Normal velocity offset. (see Remark 1)
        """ # nopep8
        return self._cards[1].get_value("vel")

    @vel.setter
    def vel(self, value: float) -> None:
        """Set the vel property."""
        self._cards[1].set_value("vel", value)

    @property
    def prs(self) -> typing.Optional[float]:
        """Get or set the Pressure offset. (see Remark 1)
        """ # nopep8
        return self._cards[1].get_value("prs")

    @prs.setter
    def prs(self, value: float) -> None:
        """Set the prs property."""
        self._cards[1].set_value("prs", value)

    @property
    def imp(self) -> typing.Optional[float]:
        """Get or set the Impedance. (see Remark 1)
        """ # nopep8
        return self._cards[1].get_value("imp")

    @imp.setter
    def imp(self, value: float) -> None:
        """Set the imp property."""
        self._cards[1].set_value("imp", value)

