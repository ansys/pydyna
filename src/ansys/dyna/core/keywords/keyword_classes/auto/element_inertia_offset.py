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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ElementInertiaOffset(KeywordBase):
    """DYNA ELEMENT_INERTIA_OFFSET keyword"""

    keyword = "ELEMENT"
    subkeyword = "INERTIA_OFFSET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        8,
                        kwargs.get("eid")
                    ),
                    Field(
                        "nid",
                        int,
                        8,
                        8,
                        kwargs.get("nid")
                    ),
                    Field(
                        "csid",
                        int,
                        16,
                        8,
                        kwargs.get("csid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ixx",
                        float,
                        0,
                        10,
                        kwargs.get("ixx", 0.0)
                    ),
                    Field(
                        "ixy",
                        float,
                        10,
                        10,
                        kwargs.get("ixy", 0.0)
                    ),
                    Field(
                        "ixz",
                        float,
                        20,
                        10,
                        kwargs.get("ixz", 0.0)
                    ),
                    Field(
                        "iyy",
                        float,
                        30,
                        10,
                        kwargs.get("iyy", 0.0)
                    ),
                    Field(
                        "iyz",
                        float,
                        40,
                        10,
                        kwargs.get("iyz", 0.0)
                    ),
                    Field(
                        "izz",
                        float,
                        50,
                        10,
                        kwargs.get("izz", 0.0)
                    ),
                    Field(
                        "mass",
                        float,
                        60,
                        10,
                        kwargs.get("mass", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x-off",
                        float,
                        0,
                        10,
                        kwargs.get("x-off", 0.0)
                    ),
                    Field(
                        "y_off",
                        float,
                        10,
                        10,
                        kwargs.get("y_off", 0.0)
                    ),
                    Field(
                        "z_off",
                        float,
                        20,
                        10,
                        kwargs.get("z_off", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number must be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID. Node to which the mass is assigned.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def csid(self) -> typing.Optional[int]:
        """Get or set the Coordinate set ID.
        EQ.0: global inertia tensor,
        EQ.1: principal moments of inertias with orientation vectors defined by coordinate set, CSID.  See *DEFINE_COORDINATE_SYSTEM and *DEFINE_COORDINATE_VECTOR.
        """ # nopep8
        return self._cards[0].get_value("csid")

    @csid.setter
    def csid(self, value: int) -> None:
        self._cards[0].set_value("csid", value)

    @property
    def ixx(self) -> float:
        """Get or set the XX component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("ixx")

    @ixx.setter
    def ixx(self, value: float) -> None:
        self._cards[1].set_value("ixx", value)

    @property
    def ixy(self) -> float:
        """Get or set the XY component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("ixy")

    @ixy.setter
    def ixy(self, value: float) -> None:
        self._cards[1].set_value("ixy", value)

    @property
    def ixz(self) -> float:
        """Get or set the XZ component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("ixz")

    @ixz.setter
    def ixz(self, value: float) -> None:
        self._cards[1].set_value("ixz", value)

    @property
    def iyy(self) -> float:
        """Get or set the YY component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("iyy")

    @iyy.setter
    def iyy(self, value: float) -> None:
        self._cards[1].set_value("iyy", value)

    @property
    def iyz(self) -> float:
        """Get or set the YZ component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("iyz")

    @iyz.setter
    def iyz(self, value: float) -> None:
        self._cards[1].set_value("iyz", value)

    @property
    def izz(self) -> float:
        """Get or set the ZZ component of inertia tensor.
        """ # nopep8
        return self._cards[1].get_value("izz")

    @izz.setter
    def izz(self, value: float) -> None:
        self._cards[1].set_value("izz", value)

    @property
    def mass(self) -> float:
        """Get or set the Lumped mass.
        """ # nopep8
        return self._cards[1].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        self._cards[1].set_value("mass", value)

    @property
    def x_off(self) -> float:
        """Get or set the x-offset from nodal point.
        """ # nopep8
        return self._cards[2].get_value("x-off")

    @x_off.setter
    def x_off(self, value: float) -> None:
        self._cards[2].set_value("x-off", value)

    @property
    def y_off(self) -> float:
        """Get or set the y-offset from nodal point.
        """ # nopep8
        return self._cards[2].get_value("y_off")

    @y_off.setter
    def y_off(self, value: float) -> None:
        self._cards[2].set_value("y_off", value)

    @property
    def z_off(self) -> float:
        """Get or set the Z-offset from nodal point.
        """ # nopep8
        return self._cards[2].get_value("z_off")

    @z_off.setter
    def z_off(self, value: float) -> None:
        self._cards[2].set_value("z_off", value)

