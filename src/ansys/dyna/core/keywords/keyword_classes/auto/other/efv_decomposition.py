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

"""Module providing the EfvDecomposition class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVDECOMPOSITION_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("nx", int, 10, 10, None),
    FieldSchema("ny", int, 20, 10, None),
    FieldSchema("nz", int, 30, 10, None),
)

_EFVDECOMPOSITION_CARD1 = (
    FieldSchema("proc", int, 0, 10, None),
    FieldSchema("imn", int, 10, 10, None),
    FieldSchema("imx", int, 20, 10, None),
    FieldSchema("jmn", int, 30, 10, None),
    FieldSchema("jmx", int, 40, 10, None),
    FieldSchema("kmn", int, 50, 10, None),
    FieldSchema("kmx", int, 60, 10, None),
    FieldSchema("box", int, 70, 10, None),
)

class EfvDecomposition(KeywordBase):
    """DYNA EFV_DECOMPOSITION keyword"""

    keyword = "EFV"
    subkeyword = "DECOMPOSITION"

    def __init__(self, **kwargs):
        """Initialize the EfvDecomposition class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVDECOMPOSITION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVDECOMPOSITION_CARD1,
                **kwargs,
            ),
        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID defined in *EFV_STRUCTURED_MESH.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def nx(self) -> typing.Optional[int]:
        """Get or set the Number of submeshes or boxes in each direction. If they are not input, a default number for each of them is computed according to the number of processes involved in the calculation.
        """ # nopep8
        return self._cards[0].get_value("nx")

    @nx.setter
    def nx(self, value: int) -> None:
        """Set the nx property."""
        self._cards[0].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[int]:
        """Get or set the Number of submeshes or boxes in each direction. If they are not input, a default number for each of them is computed according to the number of processes involved in the calculation.
        """ # nopep8
        return self._cards[0].get_value("ny")

    @ny.setter
    def ny(self, value: int) -> None:
        """Set the ny property."""
        self._cards[0].set_value("ny", value)

    @property
    def nz(self) -> typing.Optional[int]:
        """Get or set the Number of submeshes or boxes in each direction. If they are not input, a default number for each of them is computed according to the number of processes involved in the calculation.
        """ # nopep8
        return self._cards[0].get_value("nz")

    @nz.setter
    def nz(self, value: int) -> None:
        """Set the nz property."""
        self._cards[0].set_value("nz", value)

    @property
    def proc(self) -> typing.Optional[int]:
        """Get or set the Process ID owning the N-th submesh (or box) if PROC is input in the N-th optional line (or if BOX=N in any other submesh line). The process ID should be between 0 and the number of processes minus 1.
        """ # nopep8
        return self._cards[1].get_value("proc")

    @proc.setter
    def proc(self, value: int) -> None:
        """Set the proc property."""
        self._cards[1].set_value("proc", value)

    @property
    def imn(self) -> typing.Optional[int]:
        """Get or set the Minimum control point along the x-axis to select the elements of the mesh PSID
        """ # nopep8
        return self._cards[1].get_value("imn")

    @imn.setter
    def imn(self, value: int) -> None:
        """Set the imn property."""
        self._cards[1].set_value("imn", value)

    @property
    def imx(self) -> typing.Optional[int]:
        """Get or set the Maximum control point along the x-axis to select the elements of the mesh PSID
        """ # nopep8
        return self._cards[1].get_value("imx")

    @imx.setter
    def imx(self, value: int) -> None:
        """Set the imx property."""
        self._cards[1].set_value("imx", value)

    @property
    def jmn(self) -> typing.Optional[int]:
        """Get or set the Minimum control point along the y - axis to select the elements of the mesh PSID
        """ # nopep8
        return self._cards[1].get_value("jmn")

    @jmn.setter
    def jmn(self, value: int) -> None:
        """Set the jmn property."""
        self._cards[1].set_value("jmn", value)

    @property
    def jmx(self) -> typing.Optional[int]:
        """Get or set the Maximum control point along the y-axis to select the elements of the mesh PSID
        """ # nopep8
        return self._cards[1].get_value("jmx")

    @jmx.setter
    def jmx(self, value: int) -> None:
        """Set the jmx property."""
        self._cards[1].set_value("jmx", value)

    @property
    def kmn(self) -> typing.Optional[int]:
        """Get or set the Minimum control point along the z-axis to select the elements of the mesh PSID
        """ # nopep8
        return self._cards[1].get_value("kmn")

    @kmn.setter
    def kmn(self, value: int) -> None:
        """Set the kmn property."""
        self._cards[1].set_value("kmn", value)

    @property
    def kmx(self) -> typing.Optional[int]:
        """Get or set the Maximum control point along the z-axis to select the elements of the mesh PSID
        """ # nopep8
        return self._cards[1].get_value("kmx")

    @kmx.setter
    def kmx(self, value: int) -> None:
        """Set the kmx property."""
        self._cards[1].set_value("kmx", value)

    @property
    def box(self) -> typing.Optional[int]:
        """Get or set the Optional submesh ID. The rank of a submesh card between 1 and the number of processes defines the submesh id. If the number of submesh cards is less than the number of processes, it's possible to set BOX in these cards to define the submesh ID.
        """ # nopep8
        return self._cards[1].get_value("box")

    @box.setter
    def box(self, value: int) -> None:
        """Set the box property."""
        self._cards[1].set_value("box", value)

