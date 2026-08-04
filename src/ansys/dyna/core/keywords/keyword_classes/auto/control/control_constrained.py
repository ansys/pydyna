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

"""Module providing the ControlConstrained class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLCONSTRAINED_CARD0 = (
    FieldSchema("sprchk", int, 0, 10, 0),
    FieldSchema("sprsmd", int, 10, 10, 0),
    FieldSchema("sprsrch", int, 20, 10, 0),
    FieldSchema("skpcnrbe", int, 30, 10, 0),
)

class ControlConstrained(KeywordBase):
    """DYNA CONTROL_CONSTRAINED keyword"""

    keyword = "CONTROL"
    subkeyword = "CONSTRAINED"

    def __init__(self, **kwargs):
        """Initialize the ControlConstrained class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLCONSTRAINED_CARD0,
                **kwargs,
            ),
        ]
    @property
    def sprchk(self) -> int:
        """Get or set the SPR2/SPR3 initialization check:
        EQ.0: automatically increase search radius to find enough nodes(default)
        EQ.1: same as 0 but also write a warning
        EQ.2: error termination if not enough nodes found immediately
        """ # nopep8
        return self._cards[0].get_value("sprchk")

    @sprchk.setter
    def sprchk(self, value: int) -> None:
        """Set the sprchk property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sprchk must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("sprchk", value)

    @property
    def sprsmd(self) -> int:
        """Get or set the Shear moment distribution behavior for SPR3:nEQ.0: Distributed as force pairs
        EQ.1: Distributed as nodal moments(old behavior)
        """ # nopep8
        return self._cards[0].get_value("sprsmd")

    @sprsmd.setter
    def sprsmd(self, value: int) -> None:
        """Set the sprsmd property."""
        if value not in [0, 1, None]:
            raise Exception("""sprsmd must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sprsmd", value)

    @property
    def sprsrch(self) -> int:
        """Get or set the Search method for SPR2 and SPR3:
        EQ.0:	Include nodes inside the search radius.
        EQ.1 : Include not only nodes inside the search radius but also all nodes from elements inside the search radius.This search method generally leads to more nodes involved in the connection.In addition, this option invokes a different search algorithm from SPRSRCH = 0 when solid element parts are involved : nodes on the surface facing the SPR connector are detected, leading to automatically finding enough nodes.Because the default algorithm depends on a user - specified search cylinder, the algorithm sometimes did not detect enough nodes for coarse meshes.Also, sometimes the default algorithm finds nodes beneath the surface of the solid element part.Thus, this alternative algorithm for elements leads to more robust search results in general.
        """ # nopep8
        return self._cards[0].get_value("sprsrch")

    @sprsrch.setter
    def sprsrch(self, value: int) -> None:
        """Set the sprsrch property."""
        if value not in [0, 1, None]:
            raise Exception("""sprsrch must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sprsrch", value)

    @property
    def skpcnrbe(self) -> int:
        """Get or set the Skip stress calculation of a part when it is part of a nodal rigid body, *CONSTRAINED_NODAL_RIGID_BODY.  The value can be any combination of 1 to 3:
        EQ.1:	skip shell elements
        EQ.2 : skip solid elements
        EQ.3 : skip beam elements
        """ # nopep8
        return self._cards[0].get_value("skpcnrbe")

    @skpcnrbe.setter
    def skpcnrbe(self, value: int) -> None:
        """Set the skpcnrbe property."""
        self._cards[0].set_value("skpcnrbe", value)

