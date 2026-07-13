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

"""Module providing the IcfdInitialSptransp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDINITIALSPTRANSP_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
)

_ICFDINITIALSPTRANSP_CARD1 = (
    FieldSchema("dfid1", int, 0, 10, None),
    FieldSchema("dfid2", int, 10, 10, None),
    FieldSchema("dfid3", int, 20, 10, None),
    FieldSchema("dfid4", int, 30, 10, None),
    FieldSchema("dfid5", int, 40, 10, None),
    FieldSchema("dfid6", int, 50, 10, None),
    FieldSchema("dfid7", int, 60, 10, None),
    FieldSchema("dfid8", int, 70, 10, None),
)

class IcfdInitialSptransp(KeywordBase):
    """DYNA ICFD_INITIAL_SPTRANSP keyword"""

    keyword = "ICFD"
    subkeyword = "INITIAL_SPTRANSP"

    def __init__(self, **kwargs):
        """Initialize the IcfdInitialSptransp class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDINITIALSPTRANSP_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDINITIALSPTRANSP_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for the volume elements or surface elements where the values are initialized.
        EQ.0: Assign the initial condition to all nodes at once.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def dfid1(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID defining the initialization for the concentration of the ith species at t=0. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[1].get_value("dfid1")

    @dfid1.setter
    def dfid1(self, value: int) -> None:
        """Set the dfid1 property."""
        self._cards[1].set_value("dfid1", value)

    @property
    def dfid2(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID defining the initialization for the concentration of the ith species at t=0. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[1].get_value("dfid2")

    @dfid2.setter
    def dfid2(self, value: int) -> None:
        """Set the dfid2 property."""
        self._cards[1].set_value("dfid2", value)

    @property
    def dfid3(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID defining the initialization for the concentration of the ith species at t=0. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[1].get_value("dfid3")

    @dfid3.setter
    def dfid3(self, value: int) -> None:
        """Set the dfid3 property."""
        self._cards[1].set_value("dfid3", value)

    @property
    def dfid4(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID defining the initialization for the concentration of the ith species at t=0. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[1].get_value("dfid4")

    @dfid4.setter
    def dfid4(self, value: int) -> None:
        """Set the dfid4 property."""
        self._cards[1].set_value("dfid4", value)

    @property
    def dfid5(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID defining the initialization for the concentration of the ith species at t=0. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[1].get_value("dfid5")

    @dfid5.setter
    def dfid5(self, value: int) -> None:
        """Set the dfid5 property."""
        self._cards[1].set_value("dfid5", value)

    @property
    def dfid6(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID defining the initialization for the concentration of the ith species at t=0. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[1].get_value("dfid6")

    @dfid6.setter
    def dfid6(self, value: int) -> None:
        """Set the dfid6 property."""
        self._cards[1].set_value("dfid6", value)

    @property
    def dfid7(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID defining the initialization for the concentration of the ith species at t=0. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[1].get_value("dfid7")

    @dfid7.setter
    def dfid7(self, value: int) -> None:
        """Set the dfid7 property."""
        self._cards[1].set_value("dfid7", value)

    @property
    def dfid8(self) -> typing.Optional[int]:
        """Get or set the *DEFINE_FUNCTION ID defining the initialization for the concentration of the ith species at t=0. The following parameters are allowed: f(x, y, z).
        """ # nopep8
        return self._cards[1].get_value("dfid8")

    @dfid8.setter
    def dfid8(self, value: int) -> None:
        """Set the dfid8 property."""
        self._cards[1].set_value("dfid8", value)

