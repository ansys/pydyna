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
from ansys.dyna.core.lib.keyword_base import LinkType

_MESHBL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("nelth", int, 10, 10, None),
    FieldSchema("blth", float, 20, 10, 0.0),
    FieldSchema("blfe", float, 30, 10, 0.0),
    FieldSchema("blst", int, 40, 10, 0),
)

class MeshBl(KeywordBase):
    """DYNA MESH_BL keyword"""

    keyword = "MESH"
    subkeyword = "BL"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the MeshBl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MESHBL_CARD0,
                **kwargs,
            ),        ]
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
        """Get or set the Boundary layer mesh thickness.
        """ # nopep8
        return self._cards[0].get_value("blth")

    @blth.setter
    def blth(self, value: float) -> None:
        """Set the blth property."""
        self._cards[0].set_value("blth", value)

    @property
    def blfe(self) -> float:
        """Get or set the Option to impose the distance between the wall and the first volume mesh node.
        """ # nopep8
        return self._cards[0].get_value("blfe")

    @blfe.setter
    def blfe(self, value: float) -> None:
        """Set the blfe property."""
        self._cards[0].set_value("blfe", value)

    @property
    def blst(self) -> int:
        """Get or set the Boundary layer mesh generation strategy:
        A default boundary layer mesh thickness based on the surface mesh size will be chosen.BLTH and BLFE are not needed.
        EQ.1:Constant element size using BLFE and NELTH.
        EQ.2: Repartition following a quadratic polynomial and using BLFE, NELTH and BLTH.
        EQ.3: Repartition following a growth scale factor.
        """ # nopep8
        return self._cards[0].get_value("blst")

    @blst.setter
    def blst(self, value: int) -> None:
        """Set the blst property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""blst must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("blst", value)

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

