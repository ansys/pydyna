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

class Mat070(KeywordBase):
    """DYNA MAT_070 keyword"""

    keyword = "MAT"
    subkeyword = "070"
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
                        "co",
                        float,
                        20,
                        10,
                        kwargs.get("co")
                    ),
                    Field(
                        "n",
                        float,
                        30,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "p0",
                        float,
                        40,
                        10,
                        kwargs.get("p0")
                    ),
                    Field(
                        "pa",
                        float,
                        50,
                        10,
                        kwargs.get("pa")
                    ),
                    Field(
                        "ap",
                        float,
                        60,
                        10,
                        kwargs.get("ap")
                    ),
                    Field(
                        "kh",
                        float,
                        70,
                        10,
                        kwargs.get("kh")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "fr",
                        float,
                        10,
                        10,
                        kwargs.get("fr")
                    ),
                    Field(
                        "sclf",
                        float,
                        20,
                        10,
                        kwargs.get("sclf", 1.0)
                    ),
                    Field(
                        "clear",
                        float,
                        30,
                        10,
                        kwargs.get("clear")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat070.option_specs[0],
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
        """Get or set the Mass density, see also volume in *SECTION_BEAM defintion.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def co(self) -> typing.Optional[float]:
        """Get or set the Length of gas column.
        """ # nopep8
        return self._cards[0].get_value("co")

    @co.setter
    def co(self, value: float) -> None:
        self._cards[0].set_value("co", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Adiabatic constant.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def p0(self) -> typing.Optional[float]:
        """Get or set the Initial gas pressure.
        """ # nopep8
        return self._cards[0].get_value("p0")

    @p0.setter
    def p0(self, value: float) -> None:
        self._cards[0].set_value("p0", value)

    @property
    def pa(self) -> typing.Optional[float]:
        """Get or set the Atmospheric pressure.
        """ # nopep8
        return self._cards[0].get_value("pa")

    @pa.setter
    def pa(self, value: float) -> None:
        self._cards[0].set_value("pa", value)

    @property
    def ap(self) -> typing.Optional[float]:
        """Get or set the Piston cross sectional area.
        """ # nopep8
        return self._cards[0].get_value("ap")

    @ap.setter
    def ap(self, value: float) -> None:
        self._cards[0].set_value("ap", value)

    @property
    def kh(self) -> typing.Optional[float]:
        """Get or set the Hydraulic constant, K.
        """ # nopep8
        return self._cards[0].get_value("kh")

    @kh.setter
    def kh(self, value: float) -> None:
        self._cards[0].set_value("kh", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, defining the orifice area, versus element deflection.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def fr(self) -> typing.Optional[float]:
        """Get or set the Return factor on orifice force. Represents a valve, that opens when the piston unloads to relieve hydraulic pressure. Set to one for no relief.
        """ # nopep8
        return self._cards[1].get_value("fr")

    @fr.setter
    def fr(self, value: float) -> None:
        self._cards[1].set_value("fr", value)

    @property
    def sclf(self) -> float:
        """Get or set the Scale factor on force (default = 1.0).
        """ # nopep8
        return self._cards[1].get_value("sclf")

    @sclf.setter
    def sclf(self, value: float) -> None:
        self._cards[1].set_value("sclf", value)

    @property
    def clear(self) -> typing.Optional[float]:
        """Get or set the Clearance (if nonzero, no tensile force develops for positive displacements and negative forces develop only after the clearance is closed).
        """ # nopep8
        return self._cards[1].get_value("clear")

    @clear.setter
    def clear(self, value: float) -> None:
        self._cards[1].set_value("clear", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

