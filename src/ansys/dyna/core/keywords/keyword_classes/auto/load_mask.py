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

class LoadMask(KeywordBase):
    """DYNA LOAD_MASK keyword"""

    keyword = "LOAD"
    subkeyword = "MASK"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "vid1",
                        int,
                        20,
                        10,
                        kwargs.get("vid1", 1)
                    ),
                    Field(
                        "off",
                        float,
                        30,
                        10,
                        kwargs.get("off", 0)
                    ),
                    Field(
                        "boxid",
                        int,
                        40,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                    Field(
                        "lcidm",
                        int,
                        50,
                        10,
                        kwargs.get("lcidm", 0)
                    ),
                    Field(
                        "vid2",
                        int,
                        60,
                        10,
                        kwargs.get("vid2")
                    ),
                    Field(
                        "inout",
                        int,
                        70,
                        10,
                        kwargs.get("inout", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "icycle",
                        int,
                        0,
                        10,
                        kwargs.get("icycle", 200)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID. This part must consist of 3D shell elements. To use this option with solid element the surface of the solid elements must be covered with null shells, see *MAT_NULL.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the pressure time history, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def vid1(self) -> int:
        """Get or set the Vector ID normal to the suface on which the applied pressure acts. Positive pressure acts in a direction that is in the opposite direction. This vector may be used if the surface on which the pressure acts is relatively flat. If zero, the pressure load depends on the orientation of the shell elements.
        """ # nopep8
        return self._cards[0].get_value("vid1")

    @vid1.setter
    def vid1(self, value: int) -> None:
        self._cards[0].set_value("vid1", value)

    @property
    def off(self) -> float:
        """Get or set the Pressure loads will be discontinued if | VID1*n | < OFF, where n is the normal vector to the shell element.
        """ # nopep8
        return self._cards[0].get_value("off")

    @off.setter
    def off(self, value: float) -> None:
        self._cards[0].set_value("off", value)

    @property
    def boxid(self) -> int:
        """Get or set the Only elements inside the box with part ID, SSID , are considered. If no ID is given all elements of part ID, SSID, are included. When the active list of elements are updated, elements outside the box will no longer have pressure applied, i.e., the current configuration is always used.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def lcidm(self) -> int:
        """Get or set the Curve ID defining the mask. This curve gives (x,y) pairs of points in a local coordinate system defined by the vector ID, VID2. See also *DEFINE_CURVE. Curve should be flagged as DATTYP = 1.
        """ # nopep8
        return self._cards[0].get_value("lcidm")

    @lcidm.setter
    def lcidm(self, value: int) -> None:
        self._cards[0].set_value("lcidm", value)

    @property
    def vid2(self) -> typing.Optional[int]:
        """Get or set the Vector ID used to project the masking curve onto the surface of part ID, PID. The origin of this vector determines the origin of the local system that the coordinates of the PID are transformed into prior to determining the pressure distribution in the local system. This curve must be defined if LCIDM is nonzero.
        """ # nopep8
        return self._cards[0].get_value("vid2")

    @vid2.setter
    def vid2(self, value: int) -> None:
        self._cards[0].set_value("vid2", value)

    @property
    def inout(self) -> int:
        """Get or set the EQ.0: Elements whose center falls inside the projected curve are considered (default),
        EQ.1: Elements whose center falls outside the projected curve are considered.
        """ # nopep8
        return self._cards[0].get_value("inout")

    @inout.setter
    def inout(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""inout must be one of {0,1}""")
        self._cards[0].set_value("inout", value)

    @property
    def icycle(self) -> int:
        """Get or set the Number of time steps between updating the list of active elements (default=200). The list update can be quite expensive and should be done at a reasonable interval. The default is not be appropiate for all problems.
        """ # nopep8
        return self._cards[1].get_value("icycle")

    @icycle.setter
    def icycle(self, value: int) -> None:
        self._cards[1].set_value("icycle", value)

