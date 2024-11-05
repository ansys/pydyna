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

class DefineTransform(KeywordBase):
    """DYNA DEFINE_TRANSFORM keyword"""

    keyword = "DEFINE"
    subkeyword = "TRANSFORM"
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
                        "tranid",
                        int,
                        0,
                        10,
                        kwargs.get("tranid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "option",
                        str,
                        0,
                        10,
                        kwargs.get("option", "MIRROR")
                    ),
                    Field(
                        "a1",
                        float,
                        10,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        20,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        30,
                        10,
                        kwargs.get("a3")
                    ),
                    Field(
                        "a4",
                        float,
                        40,
                        10,
                        kwargs.get("a4")
                    ),
                    Field(
                        "a5",
                        float,
                        50,
                        10,
                        kwargs.get("a5")
                    ),
                    Field(
                        "a6",
                        float,
                        60,
                        10,
                        kwargs.get("a6")
                    ),
                    Field(
                        "a7",
                        float,
                        70,
                        10,
                        kwargs.get("a7")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineTransform.option_specs[0],
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
    def tranid(self) -> typing.Optional[int]:
        """Get or set the Transform ID.
        """ # nopep8
        return self._cards[0].get_value("tranid")

    @tranid.setter
    def tranid(self, value: int) -> None:
        self._cards[0].set_value("tranid", value)

    @property
    def option(self) -> str:
        """Get or set the .
        Parameters.  0-1 below for the available options.
        MIRROR
        a1, a2, a3, a4, a5, a6, a7
        Reflect, about a mirror plane defined to contain the point (a1, a2, a3) having its normal pointing from point (a1, a2, a3) toward (a4, a5, a6).  Setting a7=1 reflects the coordinate system as well, i.e., the mirrored coordinate system uses the left-hand-rule to determine the local z-axis.
        SCALE
        a1, a2, a3
        Scale the global x, y, and z coordinates of a point by a1, a2, and a3, respectively.  If zero, a default of unity is set.
        ROTATE
        a1, a2, a3, a4, a5, a6, a7Rotate through an angle (deg), a7, about a line with direction cosines a1, a2, and a3 passing through the point with coordinates a4, a5, and a6.If a4 through a7 are zero, then a1 and a2 are the ID's of two POINTs and a3 defines the rotation angle. The axis of rotation is defined by a vector going from point with ID a1 to point with ID a2.
        ROTATE3NA
        a1, a2, a3, a4
        Rotate through an angle (deg), a4. The axis of rotation is defined by a vector going from node with ID a1 to node with ID a2 and passing through node with ID a3 (a3 could be the same as a1 or a2). The three nodes must be defined before they are referenced.
        TRANSL
        a1, a2, a3
        Translate the x, y, and z coordinates of a point by a1, a2, and a3, respectively.
        TRANSL2ND
        a1, a2, a3
        Translate by distance a3. The direction is defined by a vector going from node with ID a1 to node with ID a2. The two nodes must be defined before they are referenced.
        POINT
        a1,a2,a3,a4
        Define a point with ID, a1, with the initial coordinates a2, a3, and a4.
        POS6P
        a1, a2, a3, a4, a5, a6
        Positioning by 6 points. Affine transformation (rotation and translation, no scaling) given by three start points a1, a2, and a3 and three target points a4, a5, and a6. The six POINTs must be defined before they are referenced. Only 1 POS6P option is permitted within a *DEFINE_TRANSFORMATION definition.
        POS6N
        a1, a2, a3, a4, a5, a6
        Positioning by 6 nodes. Affine transformation (rotation and translation, no scaling) given by three start nodes a1, a2, and a3 and three target nodes a4, a5, and a6. The six nodes must be defined before they are referenced. Only 1 POS6N option is permitted within a *DEFINE_TRANSFORMATION definition..
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        if value not in ["MIRROR", "SCALE", "ROTATE", "ROTATE3NA", "TRANSL", "TRANSL2ND", "POINT", "POS6P", "POS6N"]:
            raise Exception("""option must be one of {"MIRROR","SCALE","ROTATE","ROTATE3NA","TRANSL","TRANSL2ND","POINT","POS6P","POS6N"}""")
        self._cards[1].set_value("option", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Specified entity.
        See Keyword Manual Section 10.25.
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Specified entity.
        See Keyword Manual Section 10.25.
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Specified entity.
        See Keyword Manual Section 10.25.
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[1].set_value("a3", value)

    @property
    def a4(self) -> typing.Optional[float]:
        """Get or set the Specified entity.
        See Keyword Manual Section 10.25.
        """ # nopep8
        return self._cards[1].get_value("a4")

    @a4.setter
    def a4(self, value: float) -> None:
        self._cards[1].set_value("a4", value)

    @property
    def a5(self) -> typing.Optional[float]:
        """Get or set the Specified entity.
        See Keyword Manual Section 10.25.
        """ # nopep8
        return self._cards[1].get_value("a5")

    @a5.setter
    def a5(self, value: float) -> None:
        self._cards[1].set_value("a5", value)

    @property
    def a6(self) -> typing.Optional[float]:
        """Get or set the Specified entity.
        See Keyword Manual Section 10.25.
        """ # nopep8
        return self._cards[1].get_value("a6")

    @a6.setter
    def a6(self, value: float) -> None:
        self._cards[1].set_value("a6", value)

    @property
    def a7(self) -> typing.Optional[float]:
        """Get or set the Specified entity.
        See Keyword Manual Section 10.25.
        """ # nopep8
        return self._cards[1].get_value("a7")

    @a7.setter
    def a7(self, value: float) -> None:
        self._cards[1].set_value("a7", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

