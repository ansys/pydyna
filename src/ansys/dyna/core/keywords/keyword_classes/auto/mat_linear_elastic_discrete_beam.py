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

class MatLinearElasticDiscreteBeam(KeywordBase):
    """DYNA MAT_LINEAR_ELASTIC_DISCRETE_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "LINEAR_ELASTIC_DISCRETE_BEAM"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "tkr",
                        float,
                        20,
                        10,
                        kwargs.get("tkr")
                    ),
                    Field(
                        "tks",
                        float,
                        30,
                        10,
                        kwargs.get("tks")
                    ),
                    Field(
                        "tkt",
                        float,
                        40,
                        10,
                        kwargs.get("tkt")
                    ),
                    Field(
                        "rkr",
                        float,
                        50,
                        10,
                        kwargs.get("rkr")
                    ),
                    Field(
                        "rks",
                        float,
                        60,
                        10,
                        kwargs.get("rks")
                    ),
                    Field(
                        "rkt",
                        float,
                        70,
                        10,
                        kwargs.get("rkt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tdr",
                        float,
                        0,
                        10,
                        kwargs.get("tdr")
                    ),
                    Field(
                        "tds",
                        float,
                        10,
                        10,
                        kwargs.get("tds")
                    ),
                    Field(
                        "tdt",
                        float,
                        20,
                        10,
                        kwargs.get("tdt")
                    ),
                    Field(
                        "rdr",
                        float,
                        30,
                        10,
                        kwargs.get("rdr")
                    ),
                    Field(
                        "rds",
                        float,
                        40,
                        10,
                        kwargs.get("rds")
                    ),
                    Field(
                        "rdt",
                        float,
                        50,
                        10,
                        kwargs.get("rdt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "for",
                        float,
                        0,
                        10,
                        kwargs.get("for")
                    ),
                    Field(
                        "fos",
                        float,
                        10,
                        10,
                        kwargs.get("fos")
                    ),
                    Field(
                        "fot",
                        float,
                        20,
                        10,
                        kwargs.get("fot")
                    ),
                    Field(
                        "mor",
                        float,
                        30,
                        10,
                        kwargs.get("mor")
                    ),
                    Field(
                        "mos",
                        float,
                        40,
                        10,
                        kwargs.get("mos")
                    ),
                    Field(
                        "mot",
                        float,
                        50,
                        10,
                        kwargs.get("mot")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatLinearElasticDiscreteBeam.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in the *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def tkr(self) -> typing.Optional[float]:
        """Get or set the Translational stiffness about local r-axis.
        """ # nopep8
        return self._cards[0].get_value("tkr")

    @tkr.setter
    def tkr(self, value: float) -> None:
        self._cards[0].set_value("tkr", value)

    @property
    def tks(self) -> typing.Optional[float]:
        """Get or set the Translational stiffness about local s-axis.
        """ # nopep8
        return self._cards[0].get_value("tks")

    @tks.setter
    def tks(self, value: float) -> None:
        self._cards[0].set_value("tks", value)

    @property
    def tkt(self) -> typing.Optional[float]:
        """Get or set the Translational stiffness about local t-axis.
        """ # nopep8
        return self._cards[0].get_value("tkt")

    @tkt.setter
    def tkt(self, value: float) -> None:
        self._cards[0].set_value("tkt", value)

    @property
    def rkr(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about the local r-axis.
        """ # nopep8
        return self._cards[0].get_value("rkr")

    @rkr.setter
    def rkr(self, value: float) -> None:
        self._cards[0].set_value("rkr", value)

    @property
    def rks(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about the local s-axis.
        """ # nopep8
        return self._cards[0].get_value("rks")

    @rks.setter
    def rks(self, value: float) -> None:
        self._cards[0].set_value("rks", value)

    @property
    def rkt(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness about the local t-axis.
        """ # nopep8
        return self._cards[0].get_value("rkt")

    @rkt.setter
    def rkt(self, value: float) -> None:
        self._cards[0].set_value("rkt", value)

    @property
    def tdr(self) -> typing.Optional[float]:
        """Get or set the Translational viscous damper about local r-axis (optional).
        """ # nopep8
        return self._cards[1].get_value("tdr")

    @tdr.setter
    def tdr(self, value: float) -> None:
        self._cards[1].set_value("tdr", value)

    @property
    def tds(self) -> typing.Optional[float]:
        """Get or set the Translational viscous damper about local s-axis (optional).
        """ # nopep8
        return self._cards[1].get_value("tds")

    @tds.setter
    def tds(self, value: float) -> None:
        self._cards[1].set_value("tds", value)

    @property
    def tdt(self) -> typing.Optional[float]:
        """Get or set the Translational viscous damper about local t-axis (opitonal).
        """ # nopep8
        return self._cards[1].get_value("tdt")

    @tdt.setter
    def tdt(self, value: float) -> None:
        self._cards[1].set_value("tdt", value)

    @property
    def rdr(self) -> typing.Optional[float]:
        """Get or set the Rotational viscous damper about the local r-axis (optional).
        """ # nopep8
        return self._cards[1].get_value("rdr")

    @rdr.setter
    def rdr(self, value: float) -> None:
        self._cards[1].set_value("rdr", value)

    @property
    def rds(self) -> typing.Optional[float]:
        """Get or set the Rotational viscous damper about the local s-axis (optional).
        """ # nopep8
        return self._cards[1].get_value("rds")

    @rds.setter
    def rds(self, value: float) -> None:
        self._cards[1].set_value("rds", value)

    @property
    def rdt(self) -> typing.Optional[float]:
        """Get or set the Rotational viscous damper about the local t-axis (optional).
        """ # nopep8
        return self._cards[1].get_value("rdt")

    @rdt.setter
    def rdt(self, value: float) -> None:
        self._cards[1].set_value("rdt", value)

    @property
    def for_(self) -> typing.Optional[float]:
        """Get or set the Preload force in r-direction
        """ # nopep8
        return self._cards[2].get_value("for")

    @for_.setter
    def for_(self, value: float) -> None:
        self._cards[2].set_value("for", value)

    @property
    def fos(self) -> typing.Optional[float]:
        """Get or set the Preload force in s-direction
        """ # nopep8
        return self._cards[2].get_value("fos")

    @fos.setter
    def fos(self, value: float) -> None:
        self._cards[2].set_value("fos", value)

    @property
    def fot(self) -> typing.Optional[float]:
        """Get or set the Preload force in t-direction
        """ # nopep8
        return self._cards[2].get_value("fot")

    @fot.setter
    def fot(self, value: float) -> None:
        self._cards[2].set_value("fot", value)

    @property
    def mor(self) -> typing.Optional[float]:
        """Get or set the Preload moment about r-axis
        """ # nopep8
        return self._cards[2].get_value("mor")

    @mor.setter
    def mor(self, value: float) -> None:
        self._cards[2].set_value("mor", value)

    @property
    def mos(self) -> typing.Optional[float]:
        """Get or set the Preload moment about s-axis
        """ # nopep8
        return self._cards[2].get_value("mos")

    @mos.setter
    def mos(self, value: float) -> None:
        self._cards[2].set_value("mos", value)

    @property
    def mot(self) -> typing.Optional[float]:
        """Get or set the Preload moment about t-axis
        """ # nopep8
        return self._cards[2].get_value("mot")

    @mot.setter
    def mot(self, value: float) -> None:
        self._cards[2].set_value("mot", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

