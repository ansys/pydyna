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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SensorDefineForce(KeywordBase):
    """DYNA SENSOR_DEFINE_FORCE keyword"""

    keyword = "SENSOR"
    subkeyword = "DEFINE_FORCE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "sensid",
                        int,
                        0,
                        10,
                        kwargs.get("sensid")
                    ),
                    Field(
                        "ftype",
                        str,
                        10,
                        10,
                        kwargs.get("ftype", "AIRBAG")
                    ),
                    Field(
                        "typeid",
                        int,
                        20,
                        10,
                        kwargs.get("typeid")
                    ),
                    Field(
                        "vid",
                        str,
                        30,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "crd",
                        int,
                        40,
                        10,
                        kwargs.get("crd")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SensorDefineForce.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def sensid(self) -> typing.Optional[int]:
        """Get or set the Sensor ID.
        """ # nopep8
        return self._cards[0].get_value("sensid")

    @sensid.setter
    def sensid(self, value: int) -> None:
        self._cards[0].set_value("sensid", value)

    @property
    def ftype(self) -> str:
        """Get or set the Force type.
        """ # nopep8
        return self._cards[0].get_value("ftype")

    @ftype.setter
    def ftype(self, value: str) -> None:
        if value not in ["AIRBAG", "CONTACT", "CONTACT2D", "CPM", "JOINT", "JOINTSTIF", "PRESC-MOT", "RWALL", "SPC", "SPOTWELD", "XSECTION"]:
            raise Exception("""ftype must be one of {"AIRBAG","CONTACT","CONTACT2D","CPM","JOINT","JOINTSTIF","PRESC-MOT","RWALL","SPC","SPOTWELD","XSECTION"}""")
        self._cards[0].set_value("ftype", value)

    @property
    def typeid(self) -> typing.Optional[int]:
        """Get or set the ID defined in the associated KEYWORD command
        """ # nopep8
        return self._cards[0].get_value("typeid")

    @typeid.setter
    def typeid(self, value: int) -> None:
        self._cards[0].set_value("typeid", value)

    @property
    def vid(self) -> typing.Optional[str]:
        """Get or set the Vector along which the forces is measured.
        EQ.X:x-direction in coordinate system CRD.
        EQ.Y:y-direction in coordinate system CRD.
        EQ.Z:z-direction in coordinate system CRD.
        EQ.XL:	x-direction in the local coordinate system, in JOINTSTIF only.
        EQ.YL:	y - direction in the local coordinate system, in JOINTSTIF only.
        EQ.ZL : z - direction in the local coordinate system, in JOINTSTIF only.
        EQ.M: Force magnitude.
        EQ.XMOMENT:	x-direction moment for JOINT.
        EQ.YMOMENT:	y-direction moment for JOINT.
        EQ.ZMOMENT:	z-direction moment for JOINT.
        EQ.XLMOMENT:	x-direction moment for the local coordinate system, in JOINTSTIF only.
        EQ.YLMOMENT:	y - direction moment for the local coordinate system, in JOINTSTIF only.
        EQ.ZLMOMENT : z - direction moment for the local coordinate system, in JOINTSTIF only.
        EQ.MMOMENT: Moment magnitude for JOINT, JOINTSTIF, PRESC-MOT or SPC.
        EQ.n:		Vector ID n in coordinate system CRD.
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: str) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def crd(self) -> typing.Optional[int]:
        """Get or set the Coordinate system, defined by *DEFINE_COORDINATE_NODES, to which VECT is attached.
        """ # nopep8
        return self._cards[0].get_value("crd")

    @crd.setter
    def crd(self, value: int) -> None:
        self._cards[0].set_value("crd", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

