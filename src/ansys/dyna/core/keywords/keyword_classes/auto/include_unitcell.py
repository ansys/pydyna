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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IncludeUnitcell(KeywordBase):
    """DYNA INCLUDE_UNITCELL keyword"""

    keyword = "INCLUDE"
    subkeyword = "UNITCELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        256,
                        kwargs.get("filename")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "inpt",
                        int,
                        0,
                        10,
                        kwargs.get("inpt", 0)
                    ),
                    Field(
                        "oupt",
                        int,
                        10,
                        10,
                        kwargs.get("oupt", 0)
                    ),
                    Field(
                        "nedof",
                        int,
                        20,
                        10,
                        kwargs.get("nedof", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dx",
                        float,
                        0,
                        10,
                        kwargs.get("dx", 1.0)
                    ),
                    Field(
                        "dy",
                        float,
                        10,
                        10,
                        kwargs.get("dy", 1.0)
                    ),
                    Field(
                        "dz",
                        float,
                        20,
                        10,
                        kwargs.get("dz", 1.0)
                    ),
                    Field(
                        "nex",
                        int,
                        30,
                        10,
                        kwargs.get("nex", 1)
                    ),
                    Field(
                        "ney",
                        int,
                        40,
                        10,
                        kwargs.get("ney", 1)
                    ),
                    Field(
                        "nez",
                        int,
                        50,
                        10,
                        kwargs.get("nez", 1)
                    ),
                    Field(
                        "nnpe",
                        int,
                        60,
                        10,
                        kwargs.get("nnpe", 8)
                    ),
                    Field(
                        "tol",
                        float,
                        70,
                        10,
                        kwargs.get("tol")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "noff",
                        int,
                        0,
                        10,
                        kwargs.get("noff")
                    ),
                    Field(
                        "eoff",
                        int,
                        10,
                        10,
                        kwargs.get("eoff")
                    ),
                    Field(
                        "pnm",
                        int,
                        20,
                        10,
                        kwargs.get("pnm")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cnx",
                        int,
                        0,
                        10,
                        kwargs.get("cnx")
                    ),
                    Field(
                        "cny",
                        int,
                        10,
                        10,
                        kwargs.get("cny")
                    ),
                    Field(
                        "cnz",
                        int,
                        20,
                        10,
                        kwargs.get("cnz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ecnx",
                        int,
                        0,
                        10,
                        kwargs.get("ecnx")
                    ),
                    Field(
                        "ecny",
                        int,
                        10,
                        10,
                        kwargs.get("ecny")
                    ),
                    Field(
                        "ecnz",
                        int,
                        20,
                        10,
                        kwargs.get("ecnz")
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the keyword file containing X, Y, Z coordinates as defined using keyword *DEFINE_CURVE_TRIM_3D..
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def inpt(self) -> int:
        """Get or set the Type of input:
        EQ.0:	Read * NODE information from the include file and add periodic boundary conditions to the include file.
        EQ.1 : Create a unit cell mesh with periodic boundary conditions,and output to the include file
        """ # nopep8
        return self._cards[1].get_value("inpt")

    @inpt.setter
    def inpt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""inpt must be one of {0,1}""")
        self._cards[1].set_value("inpt", value)

    @property
    def oupt(self) -> int:
        """Get or set the Type of output:
        EQ.1:	Create a new main keyword file where the keyword * INCLUDE_?UNITCELL is replaced by * INCLUDE with the include file name
        """ # nopep8
        return self._cards[1].get_value("oupt")

    @oupt.setter
    def oupt(self, value: int) -> None:
        self._cards[1].set_value("oupt", value)

    @property
    def nedof(self) -> int:
        """Get or set the Number of extra nodal degrees of freedom (DOFs) for user-defined element. In the current implementation, the limit of NEDOF is 15
        """ # nopep8
        return self._cards[1].get_value("nedof")

    @nedof.setter
    def nedof(self, value: int) -> None:
        self._cards[1].set_value("nedof", value)

    @property
    def dx(self) -> float:
        """Get or set the Length of the unit cell in the  x-direction
        """ # nopep8
        return self._cards[2].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        self._cards[2].set_value("dx", value)

    @property
    def dy(self) -> float:
        """Get or set the Length of the unit cell in the y -direction
        """ # nopep8
        return self._cards[2].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        self._cards[2].set_value("dy", value)

    @property
    def dz(self) -> float:
        """Get or set the Length of the unit cell in the  z-direction
        """ # nopep8
        return self._cards[2].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        self._cards[2].set_value("dz", value)

    @property
    def nex(self) -> int:
        """Get or set the Number of elements along the  x-direction
        """ # nopep8
        return self._cards[2].get_value("nex")

    @nex.setter
    def nex(self, value: int) -> None:
        self._cards[2].set_value("nex", value)

    @property
    def ney(self) -> int:
        """Get or set the Number of elements along the  y-direction
        """ # nopep8
        return self._cards[2].get_value("ney")

    @ney.setter
    def ney(self, value: int) -> None:
        self._cards[2].set_value("ney", value)

    @property
    def nez(self) -> int:
        """Get or set the Number of elements along the  z-direction
        """ # nopep8
        return self._cards[2].get_value("nez")

    @nez.setter
    def nez(self, value: int) -> None:
        self._cards[2].set_value("nez", value)

    @property
    def nnpe(self) -> int:
        """Get or set the Number of nodes per element. The current implementation supports only 4-node tetrahedron or 8-node hexahedron elements
        """ # nopep8
        return self._cards[2].get_value("nnpe")

    @nnpe.setter
    def nnpe(self, value: int) -> None:
        self._cards[2].set_value("nnpe", value)

    @property
    def tol(self) -> typing.Optional[float]:
        """Get or set the Tolerance for searching for each pair of nodes in the periodic positions to create the periodic boundary conditions. This tolerance may be needed because numerical errors in the mesh can cause the coordinates of the pairs of nodes to not be exactly in the periodic positions. The default tolerance is computed based on the size of unit cell
        """ # nopep8
        return self._cards[2].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        self._cards[2].set_value("tol", value)

    @property
    def noff(self) -> typing.Optional[int]:
        """Get or set the Offset of node IDs
        """ # nopep8
        return self._cards[3].get_value("noff")

    @noff.setter
    def noff(self, value: int) -> None:
        self._cards[3].set_value("noff", value)

    @property
    def eoff(self) -> typing.Optional[int]:
        """Get or set the Offset of element IDs
        """ # nopep8
        return self._cards[3].get_value("eoff")

    @eoff.setter
    def eoff(self, value: int) -> None:
        self._cards[3].set_value("eoff", value)

    @property
    def pnm(self) -> typing.Optional[int]:
        """Get or set the Part ID
        """ # nopep8
        return self._cards[3].get_value("pnm")

    @pnm.setter
    def pnm(self, value: int) -> None:
        self._cards[3].set_value("pnm", value)

    @property
    def cnx(self) -> typing.Optional[int]:
        """Get or set the Node ID of the 1st control point for the constraint in the  x direction
        """ # nopep8
        return self._cards[4].get_value("cnx")

    @cnx.setter
    def cnx(self, value: int) -> None:
        self._cards[4].set_value("cnx", value)

    @property
    def cny(self) -> typing.Optional[int]:
        """Get or set the Node ID of the 2nd control point for the constraint in the  y direction
        """ # nopep8
        return self._cards[4].get_value("cny")

    @cny.setter
    def cny(self, value: int) -> None:
        self._cards[4].set_value("cny", value)

    @property
    def cnz(self) -> typing.Optional[int]:
        """Get or set the Node ID of the 3rd control point for the constraint in the  z direction
        """ # nopep8
        return self._cards[4].get_value("cnz")

    @cnz.setter
    def cnz(self, value: int) -> None:
        self._cards[4].set_value("cnz", value)

    @property
    def ecnx(self) -> typing.Optional[int]:
        """Get or set the Node ID of extra control point for the constraint in x direction of 3 extra nodal DOFs
        """ # nopep8
        return self._cards[5].get_value("ecnx")

    @ecnx.setter
    def ecnx(self, value: int) -> None:
        self._cards[5].set_value("ecnx", value)

    @property
    def ecny(self) -> typing.Optional[int]:
        """Get or set the Node ID of extra control point for the constraint in y  direction of 3 extra nodal DOFs
        """ # nopep8
        return self._cards[5].get_value("ecny")

    @ecny.setter
    def ecny(self, value: int) -> None:
        self._cards[5].set_value("ecny", value)

    @property
    def ecnz(self) -> typing.Optional[int]:
        """Get or set the Node ID of extra control point for the constraint in  z direction of 3 extra nodal DOFs
        """ # nopep8
        return self._cards[5].get_value("ecnz")

    @ecnz.setter
    def ecnz(self, value: int) -> None:
        self._cards[5].set_value("ecnz", value)

