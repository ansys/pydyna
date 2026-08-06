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

"""Module providing the MeshBl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MESHBL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("nelth", int, 10, 10, None),
    FieldSchema("blth", float, 20, 10, 0.0),
    FieldSchema("blfe", float, 30, 10, 0.0),
    FieldSchema("blst", int, 40, 10, 0),
    FieldSchema("bldr", int, 50, 10, 0),
    FieldSchema("iside", int, 60, 10, 0),
)

class MeshBl(KeywordBase):
    """DYNA MESH_BL keyword"""

    keyword = "MESH"
    subkeyword = "BL"

    def __init__(self, **kwargs):
        """Initialize the MeshBl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MESHBL_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part identification.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nelth(self) -> typing.Optional[int]:
        """Get or set the Number of elements normal to the surface.
        """ # nopep8
        return self._cards[0].get_value("nelth")

    @nelth.setter
    def nelth(self, value: int) -> None:
        """Set the nelth property."""
        self._cards[0].set_value("nelth", value)

    @property
    def blth(self) -> float:
        """Get or set the Boundary layer mesh thickness if BLST = 1 or 2. Growth scale factor if BLST = 3. Ignored if BLST = 0. See Remark 5.
        """ # nopep8
        return self._cards[0].get_value("blth")

    @blth.setter
    def blth(self, value: float) -> None:
        """Set the blth property."""
        self._cards[0].set_value("blth", value)

    @property
    def blfe(self) -> float:
        """Get or set the Distance between the wall and the first volume mesh node if BLST = 3. Scaling coefficient if BLST = 1 or 2. Ignored if BLST =0.  See Remark 5
        """ # nopep8
        return self._cards[0].get_value("blfe")

    @blfe.setter
    def blfe(self, value: float) -> None:
        """Set the blfe property."""
        self._cards[0].set_value("blfe", value)

    @property
    def blst(self) -> int:
        """Get or set the Boundary layer mesh generation strategy:
        EQ.0: 2 ** (NELTH + 1) subdivisions based on surface mesh size(default).See Remark 1.
        EQ.1: Power law using BLTHand NELTH with BLFE as a scale factor.See Remark 2 and Figure 0 - 1.
        EQ.2: Geometric series based on BLTH and BLFE.See Remark 3 and Figure 0 - 2.
        EQ.3: Repartition following a growth scale factor(BLTH).See Remark 4 and Figure 0 - 3.
        """ # nopep8
        return self._cards[0].get_value("blst")

    @blst.setter
    def blst(self, value: int) -> None:
        """Set the blst property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""blst must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("blst", value)

    @property
    def bldr(self) -> int:
        """Get or set the Boundary layer mesh generation during the dynamic relaxation phase when present (see *ICFD_CONTROL_GENERAL):
        EQ.0: On.No distinction for boundary layer mesh generation between the dynamic relaxationand transient phases.
        EQ.1: Off.Boundary layer mesh is not generated during the dynamic relaxation phase.
        """ # nopep8
        return self._cards[0].get_value("bldr")

    @bldr.setter
    def bldr(self, value: int) -> None:
        """Set the bldr property."""
        if value not in [0, 1, None]:
            raise Exception("""bldr must be `None` or one of {0,1}.""")
        self._cards[0].set_value("bldr", value)

    @property
    def iside(self) -> int:
        """Get or set the For embedded shells only flag to define the boundary layer on one side of the embedded surface:
        EQ.0:	Generates the boundary layer mesh on both sides of * MESH_EMBEDSHELL.
        EQ.1 : Generates the boundary layer mesh on one side only � on the faces that are originally present in the embedded surface.
        EQ.2 : Generates the boundary layer mesh on one side only � on the additional faces that are generated by the embedded surface.
        """ # nopep8
        return self._cards[0].get_value("iside")

    @iside.setter
    def iside(self, value: int) -> None:
        """Set the iside property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iside must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("iside", value)

