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

"""Module providing the EfvCylindricalMesh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVCYLINDRICALMESH_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("nbid", int, 10, 10, None),
    FieldSchema("ebid", int, 20, 10, None),
    FieldSchema("etyp", int, 30, 10, None),
)

_EFVCYLINDRICALMESH_CARD1 = (
    FieldSchema("nid0", int, 0, 10, None),
    FieldSchema("angl", float, 10, 10, None),
)

_EFVCYLINDRICALMESH_CARD2 = (
    FieldSchema("nr", int, 0, 10, None),
    FieldSchema("dr", float, 10, 10, None),
    FieldSchema("rout0", float, 20, 10, None),
    FieldSchema("rin0", float, 30, 10, None),
    FieldSchema("rout1", float, 40, 10, None),
    FieldSchema("rin1", float, 50, 10, None),
)

_EFVCYLINDRICALMESH_CARD3 = (
    FieldSchema("nc", int, 0, 10, None),
    FieldSchema("dc", float, 10, 10, None),
)

_EFVCYLINDRICALMESH_CARD4 = (
    FieldSchema("nl", int, 0, 10, None),
    FieldSchema("dl", float, 10, 10, None),
    FieldSchema("l", float, 20, 10, None),
)

class EfvCylindricalMesh(KeywordBase):
    """DYNA EFV_CYLINDRICAL_MESH keyword"""

    keyword = "EFV"
    subkeyword = "CYLINDRICAL_MESH"

    def __init__(self, **kwargs):
        """Initialize the EfvCylindricalMesh class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVCYLINDRICALMESH_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVCYLINDRICALMESH_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVCYLINDRICALMESH_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVCYLINDRICALMESH_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVCYLINDRICALMESH_CARD4,
                **kwargs,
            ),
        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID.  A unique number must be specified. The part set will be automatically created for the shell (ELFORM = 5 or 6 on *SECTION_EFV) parts involved in the Finite Volume Euler computations. The sections of these parts should be a *SECTION_EFV that selects the Finite Volume Euler solver. This ID can be used to refer to the mesh in other keywords like *EFV_FILLING.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def nbid(self) -> typing.Optional[int]:
        """Get or set the Nodes are generated and assigned with node IDs starting from NBID
        """ # nopep8
        return self._cards[0].get_value("nbid")

    @nbid.setter
    def nbid(self, value: int) -> None:
        """Set the nbid property."""
        self._cards[0].set_value("nbid", value)

    @property
    def ebid(self) -> typing.Optional[int]:
        """Get or set the Elements are generated and assigned with element IDs starting from EBID
        """ # nopep8
        return self._cards[0].get_value("ebid")

    @ebid.setter
    def ebid(self, value: int) -> None:
        """Set the ebid property."""
        self._cards[0].set_value("ebid", value)

    @property
    def etyp(self) -> typing.Optional[int]:
        """Get or set the Element type:
        EQ.1: shells (*SECTION_EFV elform=5 or 6)
        """ # nopep8
        return self._cards[0].get_value("etyp")

    @etyp.setter
    def etyp(self, value: int) -> None:
        """Set the etyp property."""
        self._cards[0].set_value("etyp", value)

    @property
    def nid0(self) -> typing.Optional[int]:
        """Get or set the Node that sets the mesh's origin
        """ # nopep8
        return self._cards[1].get_value("nid0")

    @nid0.setter
    def nid0(self, value: int) -> None:
        """Set the nid0 property."""
        self._cards[1].set_value("nid0", value)

    @property
    def angl(self) -> typing.Optional[float]:
        """Get or set the Cylindrical angle:
        EQ.0: If DIM = 2 in *SECTION_EFV, ANGL = 10
        EQ.90: Quarter cylinder
        EQ.180: Half cylinder
        EQ.360: Full cylinder
        """ # nopep8
        return self._cards[1].get_value("angl")

    @angl.setter
    def angl(self, value: float) -> None:
        """Set the angl property."""
        self._cards[1].set_value("angl", value)

    @property
    def nr(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the radial direction
        """ # nopep8
        return self._cards[2].get_value("nr")

    @nr.setter
    def nr(self, value: int) -> None:
        """Set the nr property."""
        self._cards[2].set_value("nr", value)

    @property
    def dr(self) -> typing.Optional[float]:
        """Get or set the Initial radial increment. If NR=0 and ETYP=1, DR is the shell thickness
        """ # nopep8
        return self._cards[2].get_value("dr")

    @dr.setter
    def dr(self, value: float) -> None:
        """Set the dr property."""
        self._cards[2].set_value("dr", value)

    @property
    def rout0(self) -> typing.Optional[float]:
        """Get or set the Cylinder radius (external radius for a hollow cylinder) for the base at NID0
        """ # nopep8
        return self._cards[2].get_value("rout0")

    @rout0.setter
    def rout0(self, value: float) -> None:
        """Set the rout0 property."""
        self._cards[2].set_value("rout0", value)

    @property
    def rin0(self) -> typing.Optional[float]:
        """Get or set the Internal radius for a hollow cylinder for the base at NID0
        """ # nopep8
        return self._cards[2].get_value("rin0")

    @rin0.setter
    def rin0(self, value: float) -> None:
        """Set the rin0 property."""
        self._cards[2].set_value("rin0", value)

    @property
    def rout1(self) -> typing.Optional[float]:
        """Get or set the Cylinder radius (external radius for a hollow cylinder) for the other base
        """ # nopep8
        return self._cards[2].get_value("rout1")

    @rout1.setter
    def rout1(self, value: float) -> None:
        """Set the rout1 property."""
        self._cards[2].set_value("rout1", value)

    @property
    def rin1(self) -> typing.Optional[float]:
        """Get or set the Internal radius for a hollow cylinder for the other base
        """ # nopep8
        return self._cards[2].get_value("rin1")

    @rin1.setter
    def rin1(self, value: float) -> None:
        """Set the rin1 property."""
        self._cards[2].set_value("rin1", value)

    @property
    def nc(self) -> typing.Optional[int]:
        """Get or set the Number of elements along the circumference (ignored if the mesh is not hollow)
        """ # nopep8
        return self._cards[3].get_value("nc")

    @nc.setter
    def nc(self, value: int) -> None:
        """Set the nc property."""
        self._cards[3].set_value("nc", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Initial circumferential increment (ignored if the mesh is not hollow).
        """ # nopep8
        return self._cards[3].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[3].set_value("dc", value)

    @property
    def nl(self) -> typing.Optional[int]:
        """Get or set the Number of elements along the cylinder length
        """ # nopep8
        return self._cards[4].get_value("nl")

    @nl.setter
    def nl(self, value: int) -> None:
        """Set the nl property."""
        self._cards[4].set_value("nl", value)

    @property
    def dl(self) -> typing.Optional[float]:
        """Get or set the Initial axial increment.
        """ # nopep8
        return self._cards[4].get_value("dl")

    @dl.setter
    def dl(self, value: float) -> None:
        """Set the dl property."""
        self._cards[4].set_value("dl", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Cylinder length
        """ # nopep8
        return self._cards[4].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        """Set the l property."""
        self._cards[4].set_value("l", value)

