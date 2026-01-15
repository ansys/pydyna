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

"""Module providing the ControlFormingTipping class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLFORMINGTIPPING_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("itype", int, 10, 10, 1),
    FieldSchema("ifstrn", int, 20, 10, None),
    FieldSchema("ifstrs", int, 30, 10, None),
    FieldSchema("nmove", int, 40, 10, None),
)

_CONTROLFORMINGTIPPING_CARD1 = (
    FieldSchema("rot_tran", int, 0, 10, 1, "rot/tran"),
    FieldSchema("v11", float, 10, 10, None),
    FieldSchema("v12", float, 20, 10, None),
    FieldSchema("v13", float, 30, 10, None),
    FieldSchema("x01", float, 40, 10, None),
    FieldSchema("y01", float, 50, 10, None),
    FieldSchema("z01", float, 60, 10, None),
    FieldSchema("dista1", float, 70, 10, None),
)

_CONTROLFORMINGTIPPING_CARD2 = (
    FieldSchema("rot_tran", int, 0, 10, 1, "rot/tran"),
    FieldSchema("dx", float, 10, 10, None),
    FieldSchema("dy", float, 20, 10, None),
    FieldSchema("dz", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

class ControlFormingTipping(KeywordBase):
    """DYNA CONTROL_FORMING_TIPPING keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_TIPPING"

    def __init__(self, **kwargs):
        """Initialize the ControlFormingTipping class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGTIPPING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGTIPPING_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGTIPPING_CARD2,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID or part set ID that requires tipping and/or translation
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def itype(self) -> int:
        """Get or set the Type of PID (see remark 1):
        EQ.0:  part set ID (PSID).
        EQ.1:  part ID (PID)

        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        if value not in [1, 2, None]:
            raise Exception("""itype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("itype", value)

    @property
    def ifstrn(self) -> typing.Optional[int]:
        """Get or set the Strain tensors.4
        EQ.1: included in tipping/translation.
        """ # nopep8
        return self._cards[0].get_value("ifstrn")

    @ifstrn.setter
    def ifstrn(self, value: int) -> None:
        """Set the ifstrn property."""
        self._cards[0].set_value("ifstrn", value)

    @property
    def ifstrs(self) -> typing.Optional[int]:
        """Get or set the Stress tensors.
        EQ.1: included in tipping/translation.
        """ # nopep8
        return self._cards[0].get_value("ifstrs")

    @ifstrs.setter
    def ifstrs(self, value: int) -> None:
        """Set the ifstrs property."""
        self._cards[0].set_value("ifstrs", value)

    @property
    def nmove(self) -> typing.Optional[int]:
        """Get or set the Total number of tipping and translation included under this keyword
        """ # nopep8
        return self._cards[0].get_value("nmove")

    @nmove.setter
    def nmove(self, value: int) -> None:
        """Set the nmove property."""
        self._cards[0].set_value("nmove", value)

    @property
    def rot_tran(self) -> int:
        """Get or set the Transformation type.
        EQ.1: rotation.
        EQ.2: translation.
        """ # nopep8
        return self._cards[1].get_value("rot_tran")

    @rot_tran.setter
    def rot_tran(self, value: int) -> None:
        """Set the rot_tran property."""
        if value not in [1, 2, None]:
            raise Exception("""rot_tran must be `None` or one of {1,2}.""")
        self._cards[1].set_value("rot_tran", value)

    @property
    def v11(self) -> typing.Optional[float]:
        """Get or set the Direction cosines of an axis about which tipping is performed
        """ # nopep8
        return self._cards[1].get_value("v11")

    @v11.setter
    def v11(self, value: float) -> None:
        """Set the v11 property."""
        self._cards[1].set_value("v11", value)

    @property
    def v12(self) -> typing.Optional[float]:
        """Get or set the Direction cosines of an axis about which tipping is performed
        """ # nopep8
        return self._cards[1].get_value("v12")

    @v12.setter
    def v12(self, value: float) -> None:
        """Set the v12 property."""
        self._cards[1].set_value("v12", value)

    @property
    def v13(self) -> typing.Optional[float]:
        """Get or set the Direction cosines of an axis about which tipping is performed
        """ # nopep8
        return self._cards[1].get_value("v13")

    @v13.setter
    def v13(self, value: float) -> None:
        """Set the v13 property."""
        self._cards[1].set_value("v13", value)

    @property
    def x01(self) -> typing.Optional[float]:
        """Get or set the X coordinates of a point through which the tipping axis passes
        """ # nopep8
        return self._cards[1].get_value("x01")

    @x01.setter
    def x01(self, value: float) -> None:
        """Set the x01 property."""
        self._cards[1].set_value("x01", value)

    @property
    def y01(self) -> typing.Optional[float]:
        """Get or set the Y coordinates of a point through which the tipping axis passes
        """ # nopep8
        return self._cards[1].get_value("y01")

    @y01.setter
    def y01(self, value: float) -> None:
        """Set the y01 property."""
        self._cards[1].set_value("y01", value)

    @property
    def z01(self) -> typing.Optional[float]:
        """Get or set the Z coordinates of a point through which the tipping axis passes
        """ # nopep8
        return self._cards[1].get_value("z01")

    @z01.setter
    def z01(self, value: float) -> None:
        """Set the z01 property."""
        self._cards[1].set_value("z01", value)

    @property
    def dista1(self) -> typing.Optional[float]:
        """Get or set the Tipping angle in degree
        """ # nopep8
        return self._cards[1].get_value("dista1")

    @dista1.setter
    def dista1(self, value: float) -> None:
        """Set the dista1 property."""
        self._cards[1].set_value("dista1", value)

    @property
    def rot_tran(self) -> int:
        """Get or set the Transformation type.
        EQ.1: rotation.
        EQ.2: translation
        """ # nopep8
        return self._cards[2].get_value("rot_tran")

    @rot_tran.setter
    def rot_tran(self, value: int) -> None:
        """Set the rot_tran property."""
        if value not in [1, 2, None]:
            raise Exception("""rot_tran must be `None` or one of {1,2}.""")
        self._cards[2].set_value("rot_tran", value)

    @property
    def dx(self) -> typing.Optional[float]:
        """Get or set the Translation distance along X-axis
        """ # nopep8
        return self._cards[2].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        """Set the dx property."""
        self._cards[2].set_value("dx", value)

    @property
    def dy(self) -> typing.Optional[float]:
        """Get or set the Translation distance along Y-axis
        """ # nopep8
        return self._cards[2].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        """Set the dy property."""
        self._cards[2].set_value("dy", value)

    @property
    def dz(self) -> typing.Optional[float]:
        """Get or set the Translation distance along Z-axis.
        """ # nopep8
        return self._cards[2].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        """Set the dz property."""
        self._cards[2].set_value("dz", value)

