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

class DefineCurveDrawbead(KeywordBase):
    """DYNA DEFINE_CURVE_DRAWBEAD keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_DRAWBEAD"
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
                        "cid",
                        int,
                        0,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "tcype",
                        int,
                        10,
                        10,
                        kwargs.get("tcype", 1)
                    ),
                    Field(
                        "vid",
                        int,
                        20,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "pid",
                        int,
                        30,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "blkid",
                        int,
                        40,
                        10,
                        kwargs.get("blkid")
                    ),
                    Field(
                        "perct",
                        int,
                        50,
                        10,
                        kwargs.get("perct")
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
                option_spec = DefineCurveDrawbead.option_specs[0],
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
    def cid(self) -> typing.Optional[int]:
        """Get or set the Curve ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def tcype(self) -> int:
        """Get or set the Bea data type.
        EQ.1:x,y,z data
        EQ. 2. IGES data
        """ # nopep8
        return self._cards[0].get_value("tcype")

    @tcype.setter
    def tcype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""tcype must be one of {1,2}""")
        self._cards[0].set_value("tcype", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID, See DEFINE_VECTOR. This vector is used to project the bead to the rigid part (PID)
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[0].set_value("vid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID to attach the drawbead.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def blkid(self) -> typing.Optional[int]:
        """Get or set the The part id of the blank.
        """ # nopep8
        return self._cards[0].get_value("blkid")

    @blkid.setter
    def blkid(self, value: int) -> None:
        self._cards[0].set_value("blkid", value)

    @property
    def perct(self) -> typing.Optional[int]:
        """Get or set the Percentage of restraining force (the ratio of restraining force over Lock force). The value should be between 0 and 100
        """ # nopep8
        return self._cards[0].get_value("perct")

    @perct.setter
    def perct(self, value: int) -> None:
        self._cards[0].set_value("perct", value)

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

