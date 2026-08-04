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

"""Module providing the IcfdControlAdvection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLADVECTION_CARD0 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("slls", int, 30, 10, 0),
    FieldSchema("slns", int, 40, 10, 0),
    FieldSchema("slt", int, 50, 10, 0),
    FieldSchema("slst", int, 60, 10, 0),
    FieldSchema("slrt", int, 70, 10, 0),
)

class IcfdControlAdvection(KeywordBase):
    """DYNA ICFD_CONTROL_ADVECTION keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_ADVECTION"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlAdvection class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLADVECTION_CARD0,
                **kwargs,
            ),
        ]
    @property
    def slls(self) -> int:
        """Get or set the Advection scheme for the Level Set solver:
        EQ.0: Default advection scheme
        EQ.1: Changes the default advection scheme in the Level Set solver to use a linear semi - Lagrangian approach
        EQ.2: Changes the default advection scheme in the Level Set solver to use a higher-order semi-Lagrangian approach.
        """ # nopep8
        return self._cards[0].get_value("slls")

    @slls.setter
    def slls(self, value: int) -> None:
        """Set the slls property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""slls must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("slls", value)

    @property
    def slns(self) -> int:
        """Get or set the Advection scheme for the Navier-Stokes solver:
        EQ.0: Default advection scheme
        EQ.1: Changes the default advection scheme in the Navier - Stokes solver to use a semi - Lagrangian approach(Min and Gibou[2006]).
        """ # nopep8
        return self._cards[0].get_value("slns")

    @slns.setter
    def slns(self, value: int) -> None:
        """Set the slns property."""
        if value not in [0, 1, None]:
            raise Exception("""slns must be `None` or one of {0,1}.""")
        self._cards[0].set_value("slns", value)

    @property
    def slt(self) -> int:
        """Get or set the Advection scheme for the Thermal solver:
        EQ.0: Default advection scheme
        EQ.1: Changes the default advection scheme in the Thermal solver to use a semi - Lagrangian approach.
        """ # nopep8
        return self._cards[0].get_value("slt")

    @slt.setter
    def slt(self, value: int) -> None:
        """Set the slt property."""
        if value not in [0, 1, None]:
            raise Exception("""slt must be `None` or one of {0,1}.""")
        self._cards[0].set_value("slt", value)

    @property
    def slst(self) -> int:
        """Get or set the Advection scheme for the Species Transport solver:
        EQ.0: Default advection scheme
        EQ.1: Changes the default advection scheme in the Species Transport solver to use a semi - Lagrangian approach.
        """ # nopep8
        return self._cards[0].get_value("slst")

    @slst.setter
    def slst(self, value: int) -> None:
        """Set the slst property."""
        if value not in [0, 1, None]:
            raise Exception("""slst must be `None` or one of {0,1}.""")
        self._cards[0].set_value("slst", value)

    @property
    def slrt(self) -> int:
        """Get or set the Advection scheme for the Residence Time solver:
        EQ.0: Default advection scheme
        EQ.1: Changes the default advection scheme in the Residence Time solver to use a semi - Lagrangian approach.
        """ # nopep8
        return self._cards[0].get_value("slrt")

    @slrt.setter
    def slrt(self, value: int) -> None:
        """Set the slrt property."""
        if value not in [0, 1, None]:
            raise Exception("""slrt must be `None` or one of {0,1}.""")
        self._cards[0].set_value("slrt", value)

