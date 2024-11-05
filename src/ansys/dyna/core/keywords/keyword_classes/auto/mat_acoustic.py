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

class MatAcoustic(KeywordBase):
    """DYNA MAT_ACOUSTIC keyword"""

    keyword = "MAT"
    subkeyword = "ACOUSTIC"
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
                        "c",
                        float,
                        20,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "beta",
                        float,
                        30,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "cf",
                        float,
                        40,
                        10,
                        kwargs.get("cf", 0.0)
                    ),
                    Field(
                        "atmos",
                        float,
                        50,
                        10,
                        kwargs.get("atmos")
                    ),
                    Field(
                        "grav",
                        float,
                        60,
                        10,
                        kwargs.get("grav")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "xn",
                        float,
                        30,
                        10,
                        kwargs.get("xn")
                    ),
                    Field(
                        "yn",
                        float,
                        40,
                        10,
                        kwargs.get("yn")
                    ),
                    Field(
                        "zn",
                        float,
                        50,
                        10,
                        kwargs.get("zn")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAcoustic.option_specs[0],
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
    def c(self) -> typing.Optional[float]:
        """Get or set the Sound speed.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[0].set_value("c", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Damping factor. Recommend values are between 0.1 and 1.0.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def cf(self) -> float:
        """Get or set the Cavitation flag:
        EQ.0.0: off (default),
        EQ.1.0: on.
        """ # nopep8
        return self._cards[0].get_value("cf")

    @cf.setter
    def cf(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""cf must be one of {0.0,1.0}""")
        self._cards[0].set_value("cf", value)

    @property
    def atmos(self) -> typing.Optional[float]:
        """Get or set the Atmospheric pressure (optional).
        """ # nopep8
        return self._cards[0].get_value("atmos")

    @atmos.setter
    def atmos(self, value: float) -> None:
        self._cards[0].set_value("atmos", value)

    @property
    def grav(self) -> typing.Optional[float]:
        """Get or set the Gravitational acceleration constant (optional).
        """ # nopep8
        return self._cards[0].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        self._cards[0].set_value("grav", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of free surface point.
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of free surface point.
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of free surface point.
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[1].set_value("zp", value)

    @property
    def xn(self) -> typing.Optional[float]:
        """Get or set the x-direction cosine of free surface normal vector.
        """ # nopep8
        return self._cards[1].get_value("xn")

    @xn.setter
    def xn(self, value: float) -> None:
        self._cards[1].set_value("xn", value)

    @property
    def yn(self) -> typing.Optional[float]:
        """Get or set the y-direction cosine of free surface normal vector.
        """ # nopep8
        return self._cards[1].get_value("yn")

    @yn.setter
    def yn(self, value: float) -> None:
        self._cards[1].set_value("yn", value)

    @property
    def zn(self) -> typing.Optional[float]:
        """Get or set the z-direction cosine of free surface normal vector.
        """ # nopep8
        return self._cards[1].get_value("zn")

    @zn.setter
    def zn(self, value: float) -> None:
        self._cards[1].set_value("zn", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

