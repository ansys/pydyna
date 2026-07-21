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

"""Module providing the ControlCpg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLCPG_CARD0 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("ncpc", int, 10, 10, 5),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("verb", int, 30, 10, 0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("nslip", int, 50, 10, 0),
    FieldSchema("ibchk", int, 60, 10, 1),
    FieldSchema("icorr", int, 70, 10, 1),
)

_CONTROLCPG_CARD1 = (
    FieldSchema("iturb", int, 0, 10, 0),
)

class ControlCpg(KeywordBase):
    """DYNA CONTROL_CPG keyword"""

    keyword = "CONTROL"
    subkeyword = "CPG"

    def __init__(self, **kwargs):
        """Initialize the ControlCpg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLCPG_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLCPG_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ncpc(self) -> int:
        """Get or set the Number of cycles between point cloud checks. A negative value points to a load curve ID giving the number of a cycles as a function of time
        """ # nopep8
        return self._cards[0].get_value("ncpc")

    @ncpc.setter
    def ncpc(self, value: int) -> None:
        """Set the ncpc property."""
        self._cards[0].set_value("ncpc", value)

    @property
    def verb(self) -> int:
        """Get or set the CPG verbosity control:
        EQ.0: No CPG screen/stdout output.
        EQ.1: CPG output displayed on screen/stdout.
        EQ.2: Same as 1 except the information is also output in the mes0000 file.
        """ # nopep8
        return self._cards[0].get_value("verb")

    @verb.setter
    def verb(self, value: int) -> None:
        """Set the verb property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""verb must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("verb", value)

    @property
    def nslip(self) -> int:
        """Get or set the Slip condition flag:
        EQ.0: Free slip condition applied at walls.
        EQ.1: Non - slip condition applied at wall.
        """ # nopep8
        return self._cards[0].get_value("nslip")

    @nslip.setter
    def nslip(self, value: int) -> None:
        """Set the nslip property."""
        self._cards[0].set_value("nslip", value)

    @property
    def ibchk(self) -> int:
        """Get or set the Mesh integrity check:
        EQ.1:	Verify that the external mesh is closed and the normal vectors are consistently oriented (default).
        EQ.2:	Turn off integrity check.
        """ # nopep8
        return self._cards[0].get_value("ibchk")

    @ibchk.setter
    def ibchk(self, value: int) -> None:
        """Set the ibchk property."""
        if value not in [1, 2, None]:
            raise Exception("""ibchk must be `None` or one of {1,2}.""")
        self._cards[0].set_value("ibchk", value)

    @property
    def icorr(self) -> int:
        """Get or set the Correction of density and total energy:
        Correction of density and total energy:
        EQ.1:	Thermodynamic balance correction on(default).See Remark 2.
        EQ.2 : Turn off correction.
        """ # nopep8
        return self._cards[0].get_value("icorr")

    @icorr.setter
    def icorr(self, value: int) -> None:
        """Set the icorr property."""
        if value not in [1, 2, None]:
            raise Exception("""icorr must be `None` or one of {1,2}.""")
        self._cards[0].set_value("icorr", value)

    @property
    def iturb(self) -> int:
        """Get or set the Turbulence model:
        EQ.0:	Off.
        EQ.1 : Standard k - ? model.
        """ # nopep8
        return self._cards[1].get_value("iturb")

    @iturb.setter
    def iturb(self, value: int) -> None:
        """Set the iturb property."""
        if value not in [0, 1, None]:
            raise Exception("""iturb must be `None` or one of {0,1}.""")
        self._cards[1].set_value("iturb", value)

