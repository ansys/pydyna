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

"""Module providing the EfvBoundaryMeshFace class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EFVBOUNDARYMESHFACE_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("typ", int, 10, 10, None),
    FieldSchema("fimn", int, 20, 10, None),
    FieldSchema("fimx", int, 30, 10, None),
    FieldSchema("fjmn", int, 40, 10, None),
    FieldSchema("fjmx", int, 50, 10, None),
    FieldSchema("fkmn", int, 60, 10, None),
    FieldSchema("fkmx", int, 70, 10, None),
)

_EFVBOUNDARYMESHFACE_CARD1 = (
    FieldSchema("vx", float, 0, 10, None),
    FieldSchema("vy", float, 10, 10, None),
    FieldSchema("vz", float, 20, 10, None),
    FieldSchema("prs", float, 30, 10, None),
    FieldSchema("rho", float, 40, 10, None),
    FieldSchema("ei", float, 50, 10, None),
    FieldSchema("impid", int, 60, 10, None),
)

_EFVBOUNDARYMESHFACE_CARD2 = (
    FieldSchema("vx", float, 0, 10, None),
    FieldSchema("vy", float, 10, 10, None),
    FieldSchema("vz", float, 20, 10, None),
)

class EfvBoundaryMeshFace(KeywordBase):
    """DYNA EFV_BOUNDARY_MESH_FACE keyword"""

    keyword = "EFV"
    subkeyword = "BOUNDARY_MESH_FACE"
    _link_fields = {
        "impid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EfvBoundaryMeshFace class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVBOUNDARYMESHFACE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVBOUNDARYMESHFACE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVBOUNDARYMESHFACE_CARD2,
                **kwargs,
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part set ID (see *SET_PART) defined in *EFV_STRUCTURED_MESH. This part set identifies the finite volume Euler mesh for which the boundary faces are defined by FIMN, FIMX, FJMN, FJMX, FKMN, FKMX.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def typ(self) -> typing.Optional[int]:
        """Get or set the Flag selecting the boundary type:
        EQ.1:	Flow - in boundary(see * EFV_BOUNDARY_FLOW_IN)
        EQ.2 : Flow - out boundary(see * EFV_BOUNDARY_FLOW_OUT)
        EQ.3 : Silent boundary(see * EFV_BOUNDARY_TRANSMIT)
        """ # nopep8
        return self._cards[0].get_value("typ")

    @typ.setter
    def typ(self, value: int) -> None:
        """Set the typ property."""
        self._cards[0].set_value("typ", value)

    @property
    def fimn(self) -> typing.Optional[int]:
        """Get or set the Flag selecting the lowermost boundary mesh faces orthogonal to x-axis if this field is > 0.
        """ # nopep8
        return self._cards[0].get_value("fimn")

    @fimn.setter
    def fimn(self, value: int) -> None:
        """Set the fimn property."""
        self._cards[0].set_value("fimn", value)

    @property
    def fimx(self) -> typing.Optional[int]:
        """Get or set the Flag selecting the uppermost boundary mesh faces orthogonal to x-axis if this field is > 0.
        """ # nopep8
        return self._cards[0].get_value("fimx")

    @fimx.setter
    def fimx(self, value: int) -> None:
        """Set the fimx property."""
        self._cards[0].set_value("fimx", value)

    @property
    def fjmn(self) -> typing.Optional[int]:
        """Get or set the Flag selecting the lowermost boundary mesh faces orthogonal to y-axis if this field is > 0.
        """ # nopep8
        return self._cards[0].get_value("fjmn")

    @fjmn.setter
    def fjmn(self, value: int) -> None:
        """Set the fjmn property."""
        self._cards[0].set_value("fjmn", value)

    @property
    def fjmx(self) -> typing.Optional[int]:
        """Get or set the Flag selecting the uppermost boundary mesh faces orthogonal to y-axis if this field is > 0.
        """ # nopep8
        return self._cards[0].get_value("fjmx")

    @fjmx.setter
    def fjmx(self, value: int) -> None:
        """Set the fjmx property."""
        self._cards[0].set_value("fjmx", value)

    @property
    def fkmn(self) -> typing.Optional[int]:
        """Get or set the Flag selecting the lowermost boundary mesh faces orthogonal to z-axis if this field is > 0.
        """ # nopep8
        return self._cards[0].get_value("fkmn")

    @fkmn.setter
    def fkmn(self, value: int) -> None:
        """Set the fkmn property."""
        self._cards[0].set_value("fkmn", value)

    @property
    def fkmx(self) -> typing.Optional[int]:
        """Get or set the Flag selecting the uppermost boundary mesh faces orthogonal to z-axis if this field is > 0.
        """ # nopep8
        return self._cards[0].get_value("fkmx")

    @fkmx.setter
    def fkmx(self, value: int) -> None:
        """Set the fkmx property."""
        self._cards[0].set_value("fkmx", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the Inflow x-velocity for TYP=1
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the Inflow Y-velocity for TYP=1
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the Inflow Z-velocity for TYP=1
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[1].set_value("vz", value)

    @property
    def prs(self) -> typing.Optional[float]:
        """Get or set the Inflow pressure for TYP=1
        """ # nopep8
        return self._cards[1].get_value("prs")

    @prs.setter
    def prs(self, value: float) -> None:
        """Set the prs property."""
        self._cards[1].set_value("prs", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Inflow density for TYP=1
        """ # nopep8
        return self._cards[1].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[1].set_value("rho", value)

    @property
    def ei(self) -> typing.Optional[float]:
        """Get or set the Inflow internal energy for TYP=1
        """ # nopep8
        return self._cards[1].get_value("ei")

    @ei.setter
    def ei(self, value: float) -> None:
        """Set the ei property."""
        self._cards[1].set_value("ei", value)

    @property
    def impid(self) -> typing.Optional[int]:
        """Get or set the Part ID for the inflow material for TYP=1
        """ # nopep8
        return self._cards[1].get_value("impid")

    @impid.setter
    def impid(self, value: int) -> None:
        """Set the impid property."""
        self._cards[1].set_value("impid", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the Inflow x-velocity for TYP=1
        """ # nopep8
        return self._cards[2].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[2].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the Inflow Y-velocity for TYP=1
        """ # nopep8
        return self._cards[2].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[2].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the Inflow Z-velocity for TYP=1
        """ # nopep8
        return self._cards[2].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[2].set_value("vz", value)

    @property
    def impid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given impid."""
        return self._get_link_by_attr("PART", "pid", self.impid, "parts")

