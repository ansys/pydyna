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

"""Module providing the ChangeVelocityNodeOnly class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ChangeVelocityNodeOnly(KeywordBase):
    """DYNA CHANGE_VELOCITY_NODE_ONLY keyword"""

    keyword = "CHANGE"
    subkeyword = "VELOCITY_NODE_ONLY"

    def __init__(self, **kwargs):
        """Initialize the ChangeVelocityNodeOnly class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "vx",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vy",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vz",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vxr",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vyr",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vzr",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def vx(self) -> float:
        """Get or set the Translational velocity in x-direction.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Translational velocity in y-direction.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Translational velocity in z-direction.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def vxr(self) -> float:
        """Get or set the Rotational velocity about the x-axis.
        """ # nopep8
        return self._cards[0].get_value("vxr")

    @vxr.setter
    def vxr(self, value: float) -> None:
        """Set the vxr property."""
        self._cards[0].set_value("vxr", value)

    @property
    def vyr(self) -> float:
        """Get or set the Rotational velocity about the y-axis.
        """ # nopep8
        return self._cards[0].get_value("vyr")

    @vyr.setter
    def vyr(self, value: float) -> None:
        """Set the vyr property."""
        self._cards[0].set_value("vyr", value)

    @property
    def vzr(self) -> float:
        """Get or set the Rotational velocity about the z-axis.
        """ # nopep8
        return self._cards[0].get_value("vzr")

    @vzr.setter
    def vzr(self, value: float) -> None:
        """Set the vzr property."""
        self._cards[0].set_value("vzr", value)

