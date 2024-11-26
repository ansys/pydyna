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

class DefineCurveTrim3D(KeywordBase):
    """DYNA DEFINE_CURVE_TRIM_3D keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_TRIM_3D"
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
                        "tcid",
                        int,
                        0,
                        10,
                        kwargs.get("tcid")
                    ),
                    Field(
                        "tctype",
                        int,
                        10,
                        10,
                        kwargs.get("tctype", 1)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "tdir",
                        int,
                        30,
                        10,
                        kwargs.get("tdir")
                    ),
                    Field(
                        "tctol",
                        float,
                        40,
                        10,
                        kwargs.get("tctol", 0.25)
                    ),
                    Field(
                        "toln",
                        float,
                        50,
                        10,
                        kwargs.get("toln")
                    ),
                    Field(
                        "nseed1",
                        int,
                        60,
                        10,
                        kwargs.get("nseed1")
                    ),
                    Field(
                        "nseed2",
                        int,
                        70,
                        10,
                        kwargs.get("nseed2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cx",
                        float,
                        0,
                        20,
                        kwargs.get("cx", 0.0)
                    ),
                    Field(
                        "cy",
                        float,
                        20,
                        20,
                        kwargs.get("cy", 0.0)
                    ),
                    Field(
                        "cz",
                        float,
                        40,
                        20,
                        kwargs.get("cz", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        80,
                        kwargs.get("filename")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveTrim3D.option_specs[0],
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
    def tcid(self) -> typing.Optional[int]:
        """Get or set the ID number for trim curve. A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("tcid")

    @tcid.setter
    def tcid(self, value: int) -> None:
        self._cards[0].set_value("tcid", value)

    @property
    def tctype(self) -> int:
        """Get or set the Trim curve type:
        EQ.1: digitized curve provided,
        EQ.2: IGES trim curve.
        """ # nopep8
        return self._cards[0].get_value("tctype")

    @tctype.setter
    def tctype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""tctype must be one of {1,2}""")
        self._cards[0].set_value("tctype", value)

    @property
    def tdir(self) -> typing.Optional[int]:
        """Get or set the ID of vector (*DEFINE_VECTOR) giving direction of projection for trim curve.
        EQ. 0: default vector (0,0,1) is used. Curve is defined in the global xy plane, and projected onto mesh in global z-direction to define trim line.
        """ # nopep8
        return self._cards[0].get_value("tdir")

    @tdir.setter
    def tdir(self, value: int) -> None:
        self._cards[0].set_value("tdir", value)

    @property
    def tctol(self) -> float:
        """Get or set the Tolerance limiting size of small elements created during trimming (default = 0.25)
        """ # nopep8
        return self._cards[0].get_value("tctol")

    @tctol.setter
    def tctol(self, value: float) -> None:
        self._cards[0].set_value("tctol", value)

    @property
    def toln(self) -> typing.Optional[float]:
        """Get or set the The maximum gap between the trimming curve and the mesh. If the gap is bigger than this value, this section in the curve will not be used. Used only option 3D is chosen, If option 3D is not used, then
        IGB.EQ.0: trimming curv is defined in local coordinate system
        IGB.EQ.1: trimming curve is defined in global coordinate system
        """ # nopep8
        return self._cards[0].get_value("toln")

    @toln.setter
    def toln(self, value: float) -> None:
        self._cards[0].set_value("toln", value)

    @property
    def nseed1(self) -> typing.Optional[int]:
        """Get or set the A node ID on the blank in the area that remains after trimming, applicable to both options _3D or _NEW.
        LT.0: positive number is a node ID, which may not necessarily be from the blank
        """ # nopep8
        return self._cards[0].get_value("nseed1")

    @nseed1.setter
    def nseed1(self, value: int) -> None:
        self._cards[0].set_value("nseed1", value)

    @property
    def nseed2(self) -> typing.Optional[int]:
        """Get or set the A node ID on the blank in the area that remains after trimming, applicable to both options _3D or _NEW.
        LT.0: positive number is a node ID, which may not necessarily be from the blank
        """ # nopep8
        return self._cards[0].get_value("nseed2")

    @nseed2.setter
    def nseed2(self, value: int) -> None:
        self._cards[0].set_value("nseed2", value)

    @property
    def cx(self) -> float:
        """Get or set the x-coordinate of trim curve Defined if and only if TCTYPE=1.
        """ # nopep8
        return self._cards[1].get_value("cx")

    @cx.setter
    def cx(self, value: float) -> None:
        self._cards[1].set_value("cx", value)

    @property
    def cy(self) -> float:
        """Get or set the y-coordinate of trim curve Defined if and only if TCTYPE=1.
        """ # nopep8
        return self._cards[1].get_value("cy")

    @cy.setter
    def cy(self, value: float) -> None:
        self._cards[1].set_value("cy", value)

    @property
    def cz(self) -> float:
        """Get or set the z-coordinate of trim curve Defined if and only if TCTYPE=1.
        """ # nopep8
        return self._cards[1].get_value("cz")

    @cz.setter
    def cz(self, value: float) -> None:
        self._cards[1].set_value("cz", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of IGES database containing trim curve(s). Defined if and only if TCTYPE=2.
        """ # nopep8
        return self._cards[2].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[2].set_value("filename", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

