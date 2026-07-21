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

"""Module providing the EfvBoxMesh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVBOXMESH_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("nbid", int, 10, 10, None),
    FieldSchema("ebid", int, 20, 10, None),
    FieldSchema("etyp", int, 30, 10, None),
)

_EFVBOXMESH_CARD1 = (
    FieldSchema("nid0", int, 0, 10, None),
)

_EFVBOXMESH_CARD2 = (
    FieldSchema("nx", int, 0, 10, None),
    FieldSchema("xl", float, 10, 10, None),
    FieldSchema("dx", float, 20, 10, None),
    FieldSchema("ni", int, 30, 10, None),
    FieldSchema("ic", int, 40, 10, None),
)

_EFVBOXMESH_CARD3 = (
    FieldSchema("ny", int, 0, 10, None),
    FieldSchema("yl", float, 10, 10, None),
    FieldSchema("dy", float, 20, 10, None),
    FieldSchema("nj", int, 30, 10, None),
    FieldSchema("jc", int, 40, 10, None),
)

_EFVBOXMESH_CARD4 = (
    FieldSchema("nz", int, 0, 10, None),
    FieldSchema("zl", float, 10, 10, None),
    FieldSchema("dz", float, 20, 10, None),
    FieldSchema("nk", int, 30, 10, None),
    FieldSchema("kc", int, 40, 10, None),
)

class EfvBoxMesh(KeywordBase):
    """DYNA EFV_BOX_MESH keyword"""

    keyword = "EFV"
    subkeyword = "BOX_MESH"

    def __init__(self, **kwargs):
        """Initialize the EfvBoxMesh class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVBOXMESH_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVBOXMESH_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVBOXMESH_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVBOXMESH_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVBOXMESH_CARD4,
                **kwargs,
            ),
        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID.  A unique number must be specified. The part set should list the shell (such as ELFORM = 5 with IDIM = 2 on *SECTION_EFV) and solid (such as ELFORM = 6 with IDIM = 3 on *SECTION_EFV) parts involved in the finite volume Euler computations. The sections of these parts must be specified with *SECTION_EFV, which sets the finite volume Euler solver. This ID can be used to refer to the mesh in other keywords like *EFV_FILLING for instance.
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
        EQ.0: solids
        EQ.1: shells
        """ # nopep8
        return self._cards[0].get_value("etyp")

    @etyp.setter
    def etyp(self, value: int) -> None:
        """Set the etyp property."""
        self._cards[0].set_value("etyp", value)

    @property
    def nid0(self) -> typing.Optional[int]:
        """Get or set the NID0 sets the mesh's origin node
        """ # nopep8
        return self._cards[1].get_value("nid0")

    @nid0.setter
    def nid0(self, value: int) -> None:
        """Set the nid0 property."""
        self._cards[1].set_value("nid0", value)

    @property
    def nx(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the x-direction. If NX=0 and ETYP=1, a plane of shells orthogonal to the x-axis is created.
        """ # nopep8
        return self._cards[2].get_value("nx")

    @nx.setter
    def nx(self, value: int) -> None:
        """Set the nx property."""
        self._cards[2].set_value("nx", value)

    @property
    def xl(self) -> typing.Optional[float]:
        """Get or set the Mesh length in x-direction. If NX=0 and ETYP=1, a plane of shells orthogonal to the x-axis is created at the x-coordinate XL
        """ # nopep8
        return self._cards[2].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        """Set the xl property."""
        self._cards[2].set_value("xl", value)

    @property
    def dx(self) -> typing.Optional[float]:
        """Get or set the Fixed size in x-direction for the uniform mesh. If NX=0 and ETYP=1, a plane of shells orthogonal to the x-axis has a thickness DX
        """ # nopep8
        return self._cards[2].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        """Set the dx property."""
        self._cards[2].set_value("dx", value)

    @property
    def ni(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the uniform mesh (NINX)
        """ # nopep8
        return self._cards[2].get_value("ni")

    @ni.setter
    def ni(self, value: int) -> None:
        """Set the ni property."""
        self._cards[2].set_value("ni", value)

    @property
    def ic(self) -> typing.Optional[int]:
        """Get or set the Position of the uniform mesh between 0.0 and  XL:
        EQ.1: centered
        EQ.2: starting at 0.0
        EQ.3: ending at XL
        """ # nopep8
        return self._cards[2].get_value("ic")

    @ic.setter
    def ic(self, value: int) -> None:
        """Set the ic property."""
        self._cards[2].set_value("ic", value)

    @property
    def ny(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the y-direction. If NY=0 and ETYP=1, a plane of shells orthogonal to the y-axis is created.
        """ # nopep8
        return self._cards[3].get_value("ny")

    @ny.setter
    def ny(self, value: int) -> None:
        """Set the ny property."""
        self._cards[3].set_value("ny", value)

    @property
    def yl(self) -> typing.Optional[float]:
        """Get or set the Mesh length in y-direction. If NY=0 and ETYP=1, a plane of shells orthogonal to the y-axis is created at the y-coordinate YL
        """ # nopep8
        return self._cards[3].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        """Set the yl property."""
        self._cards[3].set_value("yl", value)

    @property
    def dy(self) -> typing.Optional[float]:
        """Get or set the Fixed size in y-direction for the uniform mesh. If NY=0 and ETYP=1, a plane of shells orthogonal to the y-axis has a thickness DY
        """ # nopep8
        return self._cards[3].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        """Set the dy property."""
        self._cards[3].set_value("dy", value)

    @property
    def nj(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the uniform mesh (NJNY)
        """ # nopep8
        return self._cards[3].get_value("nj")

    @nj.setter
    def nj(self, value: int) -> None:
        """Set the nj property."""
        self._cards[3].set_value("nj", value)

    @property
    def jc(self) -> typing.Optional[int]:
        """Get or set the Position of the uniform mesh between 0.0 and  YL:
        EQ.1: centered
        EQ.2: starting at 0.0
        EQ.3: ending at YL
        """ # nopep8
        return self._cards[3].get_value("jc")

    @jc.setter
    def jc(self, value: int) -> None:
        """Set the jc property."""
        self._cards[3].set_value("jc", value)

    @property
    def nz(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the z-direction. If NZ=0 and ETYP=1, a plane of shells orthogonal to the z-axis is created
        """ # nopep8
        return self._cards[4].get_value("nz")

    @nz.setter
    def nz(self, value: int) -> None:
        """Set the nz property."""
        self._cards[4].set_value("nz", value)

    @property
    def zl(self) -> typing.Optional[float]:
        """Get or set the Mesh length in z-direction. If NZ=0 and ETYP=1, a plane of shells orthogonal to the z-axis is created at the z-coordinate ZL
        """ # nopep8
        return self._cards[4].get_value("zl")

    @zl.setter
    def zl(self, value: float) -> None:
        """Set the zl property."""
        self._cards[4].set_value("zl", value)

    @property
    def dz(self) -> typing.Optional[float]:
        """Get or set the Fixed size in z-direction for the uniform mesh. If NZ=0 and ETYP=1, a plane of shells orthogonal to the z-axis has a thickness DZ
        """ # nopep8
        return self._cards[4].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        """Set the dz property."""
        self._cards[4].set_value("dz", value)

    @property
    def nk(self) -> typing.Optional[int]:
        """Get or set the Number of elements in the uniform mesh (NKNY)
        """ # nopep8
        return self._cards[4].get_value("nk")

    @nk.setter
    def nk(self, value: int) -> None:
        """Set the nk property."""
        self._cards[4].set_value("nk", value)

    @property
    def kc(self) -> typing.Optional[int]:
        """Get or set the Position of the uniform mesh between 0.0 and  ZL:
        EQ.1: centered
        EQ.2: starting at 0.0
        EQ.3: ending at ZL
        """ # nopep8
        return self._cards[4].get_value("kc")

    @kc.setter
    def kc(self, value: int) -> None:
        """Set the kc property."""
        self._cards[4].set_value("kc", value)

