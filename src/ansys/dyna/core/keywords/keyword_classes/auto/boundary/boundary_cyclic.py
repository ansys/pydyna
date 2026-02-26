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

"""Module providing the BoundaryCyclic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_BOUNDARYCYCLIC_CARD0 = (
    FieldSchema("xc", float, 0, 10, None),
    FieldSchema("yc", float, 10, 10, None),
    FieldSchema("zc", float, 20, 10, None),
    FieldSchema("nsid1", int, 30, 10, None),
    FieldSchema("nsid2", int, 40, 10, None),
    FieldSchema("iglobal", int, 50, 10, 0),
    FieldSchema("isort", int, 60, 10, 0),
)

class BoundaryCyclic(KeywordBase):
    """DYNA BOUNDARY_CYCLIC keyword"""

    keyword = "BOUNDARY"
    subkeyword = "CYCLIC"
    _link_fields = {
        "nsid1": LinkType.SET_NODE,
        "nsid2": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryCyclic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYCYCLIC_CARD0,
                **kwargs,
            ),        ]
    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the x-component axis vector of axis of rotation.
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the y-component axis vector of axis of rotation.
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> typing.Optional[float]:
        """Get or set the z-component axis vector of axis of rotation.
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[0].set_value("zc", value)

    @property
    def nsid1(self) -> typing.Optional[int]:
        """Get or set the Node set ID for first boundary plane.
        """ # nopep8
        return self._cards[0].get_value("nsid1")

    @nsid1.setter
    def nsid1(self, value: int) -> None:
        """Set the nsid1 property."""
        self._cards[0].set_value("nsid1", value)

    @property
    def nsid2(self) -> typing.Optional[int]:
        """Get or set the Node set ID for second boundary plane. Each boundary node in this boundary plane is constrained to its corresponding node in the first node set. Node sets NSID1 and NSID2 must contain the same number of nodal points. Care has to be taken that the nodes in both node sets have a location which, if given in cylindrical coordinates, differ all by the same angle.
        """ # nopep8
        return self._cards[0].get_value("nsid2")

    @nsid2.setter
    def nsid2(self, value: int) -> None:
        """Set the nsid2 property."""
        self._cards[0].set_value("nsid2", value)

    @property
    def iglobal(self) -> int:
        """Get or set the Flag for repeating symmetry:
        EQ. 0: Cyclic symmetry (default).
        EQ. 1: Repeating symmetry in planes normal to global X.
        EQ. 2: Repeating symmetry in planes normal to global Y.
        EQ. 3: Repeating symmetry in planes normal to global Z.
        """ # nopep8
        return self._cards[0].get_value("iglobal")

    @iglobal.setter
    def iglobal(self, value: int) -> None:
        """Set the iglobal property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""iglobal must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("iglobal", value)

    @property
    def isort(self) -> int:
        """Get or set the Flag for automatic sorting of boundary nodes:
        EQ. 0: No automatic sorting (default).
        EQ. 1: Automatic sorting of nodes.
        """ # nopep8
        return self._cards[0].get_value("isort")

    @isort.setter
    def isort(self, value: int) -> None:
        """Set the isort property."""
        if value not in [0, 1, None]:
            raise Exception("""isort must be `None` or one of {0,1}.""")
        self._cards[0].set_value("isort", value)

    @property
    def nsid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid1."""
        return self._get_set_link("NODE", self.nsid1)

    @nsid1_link.setter
    def nsid1_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid1."""
        self.nsid1 = value.sid

    @property
    def nsid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid2."""
        return self._get_set_link("NODE", self.nsid2)

    @nsid2_link.setter
    def nsid2_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid2."""
        self.nsid2 = value.sid

