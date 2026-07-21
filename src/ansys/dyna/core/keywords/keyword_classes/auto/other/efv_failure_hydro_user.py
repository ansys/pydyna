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

"""Module providing the EfvFailureHydroUser class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILUREHYDROUSER_CARD0 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("fc1", float, 10, 10, None),
    FieldSchema("fc2", float, 20, 10, None),
    FieldSchema("fc3", float, 30, 10, None),
    FieldSchema("fc4", float, 40, 10, None),
    FieldSchema("fc5", float, 50, 10, None),
    FieldSchema("fc6", float, 60, 10, None),
    FieldSchema("fc7", float, 70, 10, None),
)

_EFVFAILUREHYDROUSER_CARD1 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("fc1", float, 10, 10, None),
    FieldSchema("fc2", float, 20, 10, None),
)

class EfvFailureHydroUser(KeywordBase):
    """DYNA EFV_FAILURE_HYDRO_USER keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_HYDRO_USER"

    def __init__(self, **kwargs):
        """Initialize the EfvFailureHydroUser class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILUREHYDROUSER_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVFAILUREHYDROUSER_CARD1,
                **kwargs,
            ),
        ]
    @property
    def failid(self) -> typing.Optional[int]:
        """Get or set the Failure model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("failid")

    @failid.setter
    def failid(self, value: int) -> None:
        """Set the failid property."""
        self._cards[0].set_value("failid", value)

    @property
    def fc1(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[0].get_value("fc1")

    @fc1.setter
    def fc1(self, value: float) -> None:
        """Set the fc1 property."""
        self._cards[0].set_value("fc1", value)

    @property
    def fc2(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[0].get_value("fc2")

    @fc2.setter
    def fc2(self, value: float) -> None:
        """Set the fc2 property."""
        self._cards[0].set_value("fc2", value)

    @property
    def fc3(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[0].get_value("fc3")

    @fc3.setter
    def fc3(self, value: float) -> None:
        """Set the fc3 property."""
        self._cards[0].set_value("fc3", value)

    @property
    def fc4(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[0].get_value("fc4")

    @fc4.setter
    def fc4(self, value: float) -> None:
        """Set the fc4 property."""
        self._cards[0].set_value("fc4", value)

    @property
    def fc5(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[0].get_value("fc5")

    @fc5.setter
    def fc5(self, value: float) -> None:
        """Set the fc5 property."""
        self._cards[0].set_value("fc5", value)

    @property
    def fc6(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[0].get_value("fc6")

    @fc6.setter
    def fc6(self, value: float) -> None:
        """Set the fc6 property."""
        self._cards[0].set_value("fc6", value)

    @property
    def fc7(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[0].get_value("fc7")

    @fc7.setter
    def fc7(self, value: float) -> None:
        """Set the fc7 property."""
        self._cards[0].set_value("fc7", value)

    @property
    def failid(self) -> typing.Optional[int]:
        """Get or set the Failure model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[1].get_value("failid")

    @failid.setter
    def failid(self, value: int) -> None:
        """Set the failid property."""
        self._cards[1].set_value("failid", value)

    @property
    def fc1(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[1].get_value("fc1")

    @fc1.setter
    def fc1(self, value: float) -> None:
        """Set the fc1 property."""
        self._cards[1].set_value("fc1", value)

    @property
    def fc2(self) -> typing.Optional[float]:
        """Get or set the Failure parameter i
        """ # nopep8
        return self._cards[1].get_value("fc2")

    @fc2.setter
    def fc2(self, value: float) -> None:
        """Set the fc2 property."""
        self._cards[1].set_value("fc2", value)

