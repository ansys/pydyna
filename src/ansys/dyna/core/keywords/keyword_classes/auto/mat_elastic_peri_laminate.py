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

class MatElasticPeriLaminate(KeywordBase):
    """DYNA MAT_ELASTIC_PERI_LAMINATE keyword"""

    keyword = "MAT"
    subkeyword = "ELASTIC_PERI_LAMINATE"
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
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "em",
                        float,
                        30,
                        10,
                        kwargs.get("em")
                    ),
                    Field(
                        "vfm",
                        float,
                        40,
                        10,
                        kwargs.get("vfm")
                    ),
                    Field(
                        "gfm",
                        float,
                        50,
                        10,
                        kwargs.get("gfm")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fopt",
                        float,
                        0,
                        10,
                        kwargs.get("fopt")
                    ),
                    Field(
                        "fcf",
                        float,
                        10,
                        10,
                        kwargs.get("fcf")
                    ),
                    Field(
                        "fcm",
                        float,
                        20,
                        10,
                        kwargs.get("fcm")
                    ),
                    Field(
                        "fcfc",
                        float,
                        30,
                        10,
                        kwargs.get("fcfc")
                    ),
                    Field(
                        "fcmc",
                        float,
                        40,
                        10,
                        kwargs.get("fcmc")
                    ),
                    Field(
                        "fcd",
                        float,
                        50,
                        10,
                        kwargs.get("fcd")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        kwargs.get("v3")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatElasticPeriLaminate.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the the material density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the material Elastic modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def em(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("em")

    @em.setter
    def em(self, value: float) -> None:
        self._cards[0].set_value("em", value)

    @property
    def vfm(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("vfm")

    @vfm.setter
    def vfm(self, value: float) -> None:
        self._cards[0].set_value("vfm", value)

    @property
    def gfm(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[0].get_value("gfm")

    @gfm.setter
    def gfm(self, value: float) -> None:
        self._cards[0].set_value("gfm", value)

    @property
    def fopt(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("fopt")

    @fopt.setter
    def fopt(self, value: float) -> None:
        self._cards[1].set_value("fopt", value)

    @property
    def fcf(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("fcf")

    @fcf.setter
    def fcf(self, value: float) -> None:
        self._cards[1].set_value("fcf", value)

    @property
    def fcm(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("fcm")

    @fcm.setter
    def fcm(self, value: float) -> None:
        self._cards[1].set_value("fcm", value)

    @property
    def fcfc(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("fcfc")

    @fcfc.setter
    def fcfc(self, value: float) -> None:
        self._cards[1].set_value("fcfc", value)

    @property
    def fcmc(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("fcmc")

    @fcmc.setter
    def fcmc(self, value: float) -> None:
        self._cards[1].set_value("fcmc", value)

    @property
    def fcd(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[1].get_value("fcd")

    @fcd.setter
    def fcd(self, value: float) -> None:
        self._cards[1].set_value("fcd", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[2].set_value("v3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

