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

"""Module providing the MatT07 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatT07(KeywordBase):
    """DYNA MAT_T07 keyword"""

    keyword = "MAT"
    subkeyword = "T07"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatT07 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "tmid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tgrlc",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tgmult",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "hdead",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tdead",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tlat",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "hlat",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lchc",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lctc",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tlstart",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tlend",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tistart",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tiend",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "hghost",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tghost",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatT07.option_specs[0],
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
    def tmid(self) -> typing.Optional[int]:
        """Get or set the Thermal material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        """Set the tmid property."""
        self._cards[0].set_value("tmid", value)

    @property
    def tro(self) -> typing.Optional[float]:
        """Get or set the Thermal density:	EQ.0.0: default to structural density.
        """ # nopep8
        return self._cards[0].get_value("tro")

    @tro.setter
    def tro(self, value: float) -> None:
        """Set the tro property."""
        self._cards[0].set_value("tro", value)

    @property
    def tgrlc(self) -> typing.Optional[float]:
        """Get or set the Thermal generation rate (see *DEFINE_‌CURVE):
        GT.0:	load curve ID defining thermal generation rate as a function of time
        EQ.0 : thermal generation rate is the constant multiplier, TGMULT.
        LT.0 : | TGRLC | is a load curve ID defining thermal generation rate as a function of temperature.
        Feature is similar to the volumetric heat generation rate in * LOAD_HEAT_GENERATION and has units W / m ^ 3 in the SI units system.
        """ # nopep8
        return self._cards[0].get_value("tgrlc")

    @tgrlc.setter
    def tgrlc(self, value: float) -> None:
        """Set the tgrlc property."""
        self._cards[0].set_value("tgrlc", value)

    @property
    def tgmult(self) -> typing.Optional[float]:
        """Get or set the Thermal generation rate multiplier: EQ.0.0: no heat generation.
        """ # nopep8
        return self._cards[0].get_value("tgmult")

    @tgmult.setter
    def tgmult(self, value: float) -> None:
        """Set the tgmult property."""
        self._cards[0].set_value("tgmult", value)

    @property
    def hdead(self) -> typing.Optional[float]:
        """Get or set the Specific heat for inactive material before birth time
        """ # nopep8
        return self._cards[0].get_value("hdead")

    @hdead.setter
    def hdead(self, value: float) -> None:
        """Set the hdead property."""
        self._cards[0].set_value("hdead", value)

    @property
    def tdead(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity for inactive material before birth time
        """ # nopep8
        return self._cards[0].get_value("tdead")

    @tdead.setter
    def tdead(self, value: float) -> None:
        """Set the tdead property."""
        self._cards[0].set_value("tdead", value)

    @property
    def tlat(self) -> typing.Optional[float]:
        """Get or set the Phase change temperature
        """ # nopep8
        return self._cards[0].get_value("tlat")

    @tlat.setter
    def tlat(self, value: float) -> None:
        """Set the tlat property."""
        self._cards[0].set_value("tlat", value)

    @property
    def hlat(self) -> typing.Optional[float]:
        """Get or set the Latent heat
        """ # nopep8
        return self._cards[0].get_value("hlat")

    @hlat.setter
    def hlat(self, value: float) -> None:
        """Set the hlat property."""
        self._cards[0].set_value("hlat", value)

    @property
    def lchc(self) -> typing.Optional[int]:
        """Get or set the Load curve for heat capacity as function of temperature.
        """ # nopep8
        return self._cards[1].get_value("lchc")

    @lchc.setter
    def lchc(self, value: int) -> None:
        """Set the lchc property."""
        self._cards[1].set_value("lchc", value)

    @property
    def lctc(self) -> typing.Optional[int]:
        """Get or set the Load curve for thermal conductivity as function of temperature.
        """ # nopep8
        return self._cards[1].get_value("lctc")

    @lctc.setter
    def lctc(self, value: int) -> None:
        """Set the lctc property."""
        self._cards[1].set_value("lctc", value)

    @property
    def tlstart(self) -> typing.Optional[float]:
        """Get or set the Birth temperature of material start.
        """ # nopep8
        return self._cards[1].get_value("tlstart")

    @tlstart.setter
    def tlstart(self, value: float) -> None:
        """Set the tlstart property."""
        self._cards[1].set_value("tlstart", value)

    @property
    def tlend(self) -> typing.Optional[float]:
        """Get or set the Birth temperature of material end.
        """ # nopep8
        return self._cards[1].get_value("tlend")

    @tlend.setter
    def tlend(self, value: float) -> None:
        """Set the tlend property."""
        self._cards[1].set_value("tlend", value)

    @property
    def tistart(self) -> typing.Optional[float]:
        """Get or set the Birth time start.
        """ # nopep8
        return self._cards[1].get_value("tistart")

    @tistart.setter
    def tistart(self, value: float) -> None:
        """Set the tistart property."""
        self._cards[1].set_value("tistart", value)

    @property
    def tiend(self) -> typing.Optional[float]:
        """Get or set the Birth time end.
        """ # nopep8
        return self._cards[1].get_value("tiend")

    @tiend.setter
    def tiend(self, value: float) -> None:
        """Set the tiend property."""
        self._cards[1].set_value("tiend", value)

    @property
    def hghost(self) -> typing.Optional[float]:
        """Get or set the Heat capacity for ghost (quiet) material.
        """ # nopep8
        return self._cards[1].get_value("hghost")

    @hghost.setter
    def hghost(self, value: float) -> None:
        """Set the hghost property."""
        self._cards[1].set_value("hghost", value)

    @property
    def tghost(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity for ghost (quiet) material.
        """ # nopep8
        return self._cards[1].get_value("tghost")

    @tghost.setter
    def tghost(self, value: float) -> None:
        """Set the tghost property."""
        self._cards[1].set_value("tghost", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

