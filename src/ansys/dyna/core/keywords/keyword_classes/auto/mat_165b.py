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

"""Module providing the Mat165B class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat165B(KeywordBase):
    """DYNA MAT_165B keyword"""

    keyword = "MAT"
    subkeyword = "165B"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat165B class."""
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "re",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "q",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c1",
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
                        "gamma1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gamma2",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c3",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gamma3",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat165B.option_specs[0],
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
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def re(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("re")

    @re.setter
    def re(self, value: float) -> None:
        """Set the re property."""
        self._cards[0].set_value("re", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Material parameter.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Material parameter.
        """ # nopep8
        return self._cards[0].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[0].set_value("q", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Material parameter.
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[0].set_value("c1", value)

    @property
    def gamma1(self) -> typing.Optional[float]:
        """Get or set the Material parameter.
        """ # nopep8
        return self._cards[1].get_value("gamma1")

    @gamma1.setter
    def gamma1(self, value: float) -> None:
        """Set the gamma1 property."""
        self._cards[1].set_value("gamma1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Material parameter.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def gamma2(self) -> typing.Optional[float]:
        """Get or set the Material parameter
        """ # nopep8
        return self._cards[1].get_value("gamma2")

    @gamma2.setter
    def gamma2(self, value: float) -> None:
        """Set the gamma2 property."""
        self._cards[1].set_value("gamma2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Material parameter.
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[1].set_value("c3", value)

    @property
    def gamma3(self) -> typing.Optional[float]:
        """Get or set the Material parameter.
        """ # nopep8
        return self._cards[1].get_value("gamma3")

    @gamma3.setter
    def gamma3(self, value: float) -> None:
        """Set the gamma3 property."""
        self._cards[1].set_value("gamma3", value)

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

