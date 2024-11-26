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

class Mat193(KeywordBase):
    """DYNA MAT_193 keyword"""

    keyword = "MAT"
    subkeyword = "193"
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
                        "gmod",
                        float,
                        20,
                        10,
                        kwargs.get("gmod")
                    ),
                    Field(
                        "rnu",
                        float,
                        30,
                        10,
                        kwargs.get("rnu")
                    ),
                    Field(
                        "rkf",
                        float,
                        40,
                        10,
                        kwargs.get("rkf", 1.0)
                    ),
                    Field(
                        "phi",
                        float,
                        50,
                        10,
                        kwargs.get("phi")
                    ),
                    Field(
                        "cval",
                        float,
                        60,
                        10,
                        kwargs.get("cval")
                    ),
                    Field(
                        "psi",
                        float,
                        70,
                        10,
                        kwargs.get("psi")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "str_lim",
                        float,
                        0,
                        10,
                        kwargs.get("str_lim", 5.0E-03)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gmoddp",
                        float,
                        0,
                        10,
                        kwargs.get("gmoddp")
                    ),
                    Field(
                        "phidp",
                        float,
                        10,
                        10,
                        kwargs.get("phidp")
                    ),
                    Field(
                        "cvaldp",
                        float,
                        20,
                        10,
                        kwargs.get("cvaldp")
                    ),
                    Field(
                        "psidp",
                        float,
                        30,
                        10,
                        kwargs.get("psidp")
                    ),
                    Field(
                        "gmodgr",
                        float,
                        40,
                        10,
                        kwargs.get("gmodgr")
                    ),
                    Field(
                        "phigr",
                        float,
                        50,
                        10,
                        kwargs.get("phigr")
                    ),
                    Field(
                        "cvalgr",
                        float,
                        60,
                        10,
                        kwargs.get("cvalgr")
                    ),
                    Field(
                        "psigr",
                        float,
                        70,
                        10,
                        kwargs.get("psigr")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat193.option_specs[0],
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def gmod(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus.
        """ # nopep8
        return self._cards[0].get_value("gmod")

    @gmod.setter
    def gmod(self, value: float) -> None:
        self._cards[0].set_value("gmod", value)

    @property
    def rnu(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("rnu")

    @rnu.setter
    def rnu(self, value: float) -> None:
        self._cards[0].set_value("rnu", value)

    @property
    def rkf(self) -> float:
        """Get or set the Failure surface shape parameter.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("rkf")

    @rkf.setter
    def rkf(self, value: float) -> None:
        self._cards[0].set_value("rkf", value)

    @property
    def phi(self) -> typing.Optional[float]:
        """Get or set the Angle of friction (in radians).
        """ # nopep8
        return self._cards[0].get_value("phi")

    @phi.setter
    def phi(self, value: float) -> None:
        self._cards[0].set_value("phi", value)

    @property
    def cval(self) -> typing.Optional[float]:
        """Get or set the Cohesion value.
        """ # nopep8
        return self._cards[0].get_value("cval")

    @cval.setter
    def cval(self, value: float) -> None:
        self._cards[0].set_value("cval", value)

    @property
    def psi(self) -> typing.Optional[float]:
        """Get or set the Dilation angle (in radians).
        Default is set to 0.0.
        """ # nopep8
        return self._cards[0].get_value("psi")

    @psi.setter
    def psi(self, value: float) -> None:
        self._cards[0].set_value("psi", value)

    @property
    def str_lim(self) -> float:
        """Get or set the Minimum shear strength of material is given by STR_LIM*CVAL.
        Default is set to 5.0E-03
        """ # nopep8
        return self._cards[1].get_value("str_lim")

    @str_lim.setter
    def str_lim(self, value: float) -> None:
        self._cards[1].set_value("str_lim", value)

    @property
    def gmoddp(self) -> typing.Optional[float]:
        """Get or set the Depth at which shear modulus (GMOD) is correct.
        """ # nopep8
        return self._cards[2].get_value("gmoddp")

    @gmoddp.setter
    def gmoddp(self, value: float) -> None:
        self._cards[2].set_value("gmoddp", value)

    @property
    def phidp(self) -> typing.Optional[float]:
        """Get or set the Depth at which angle of friction (PHI) is correct.
        """ # nopep8
        return self._cards[2].get_value("phidp")

    @phidp.setter
    def phidp(self, value: float) -> None:
        self._cards[2].set_value("phidp", value)

    @property
    def cvaldp(self) -> typing.Optional[float]:
        """Get or set the Depth at which cohesion value (CVAL) is correct.
        """ # nopep8
        return self._cards[2].get_value("cvaldp")

    @cvaldp.setter
    def cvaldp(self, value: float) -> None:
        self._cards[2].set_value("cvaldp", value)

    @property
    def psidp(self) -> typing.Optional[float]:
        """Get or set the Depth at which dilation angle (PSI) is correct.
        """ # nopep8
        return self._cards[2].get_value("psidp")

    @psidp.setter
    def psidp(self, value: float) -> None:
        self._cards[2].set_value("psidp", value)

    @property
    def gmodgr(self) -> typing.Optional[float]:
        """Get or set the Gradient at which shear modulus (GMOD) increases with depth.
        """ # nopep8
        return self._cards[2].get_value("gmodgr")

    @gmodgr.setter
    def gmodgr(self, value: float) -> None:
        self._cards[2].set_value("gmodgr", value)

    @property
    def phigr(self) -> typing.Optional[float]:
        """Get or set the Gradient at which friction angle (PHI) increases with depth.
        """ # nopep8
        return self._cards[2].get_value("phigr")

    @phigr.setter
    def phigr(self, value: float) -> None:
        self._cards[2].set_value("phigr", value)

    @property
    def cvalgr(self) -> typing.Optional[float]:
        """Get or set the Gradient at which cohesion value (CVAL) increases with depth.
        """ # nopep8
        return self._cards[2].get_value("cvalgr")

    @cvalgr.setter
    def cvalgr(self, value: float) -> None:
        self._cards[2].set_value("cvalgr", value)

    @property
    def psigr(self) -> typing.Optional[float]:
        """Get or set the Gradient at which dilation angle (PSI) increases with depth.
        """ # nopep8
        return self._cards[2].get_value("psigr")

    @psigr.setter
    def psigr(self, value: float) -> None:
        self._cards[2].set_value("psigr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

