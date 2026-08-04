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

"""Module providing the EfvControlTransport class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVCONTROLTRANSPORT_CARD0 = (
    FieldSchema("vtsf", float, 0, 10, 1.0),
    FieldSchema("ifmult", int, 10, 10, 2),
    FieldSchema("nettyp", int, 20, 10, 2),
    FieldSchema("ntalg", int, 30, 10, 0),
)

class EfvControlTransport(KeywordBase):
    """DYNA EFV_CONTROL_TRANSPORT keyword"""

    keyword = "EFV"
    subkeyword = "CONTROL_TRANSPORT"

    def __init__(self, **kwargs):
        """Initialize the EfvControlTransport class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVCONTROLTRANSPORT_CARD0,
                **kwargs,
            ),
        ]
    @property
    def vtsf(self) -> float:
        """Get or set the Timestep scale factor for material velocities. The time step is computed with dx/(c+u/VTSF) where dx is an element length, c is the sound speed and u is the material velocity
        """ # nopep8
        return self._cards[0].get_value("vtsf")

    @vtsf.setter
    def vtsf(self, value: float) -> None:
        """Set the vtsf property."""
        self._cards[0].set_value("vtsf", value)

    @property
    def ifmult(self) -> int:
        """Get or set the Flag selecting the transport method used for the Euler-FCT calculations:
        EQ.1: activates an operator split logic that solves each spatial dimension in turn.
        EQ.2: all 3 spatial dimensions are solved simultaneously
        """ # nopep8
        return self._cards[0].get_value("ifmult")

    @ifmult.setter
    def ifmult(self, value: int) -> None:
        """Set the ifmult property."""
        if value not in [2, 1, None]:
            raise Exception("""ifmult must be `None` or one of {2,1}.""")
        self._cards[0].set_value("ifmult", value)

    @property
    def nettyp(self) -> int:
        """Get or set the Transport method to update the energies:
        EQ.1: Only the internal energy is transported.
        EQ.2: The total energy is transported
        """ # nopep8
        return self._cards[0].get_value("nettyp")

    @nettyp.setter
    def nettyp(self, value: int) -> None:
        """Set the nettyp property."""
        if value not in [2, 1, None]:
            raise Exception("""nettyp must be `None` or one of {2,1}.""")
        self._cards[0].set_value("nettyp", value)

    @property
    def ntalg(self) -> int:
        """Get or set the Transport algorithm for the multi-material Eulerian method:
        EQ.0: SLIC.
        EQ.1: update SLIC
        """ # nopep8
        return self._cards[0].get_value("ntalg")

    @ntalg.setter
    def ntalg(self, value: int) -> None:
        """Set the ntalg property."""
        if value not in [0, 1, None]:
            raise Exception("""ntalg must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ntalg", value)

