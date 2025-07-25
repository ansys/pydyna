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

"""Module providing the Mat154 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat154(KeywordBase):
    """DYNA MAT_154 keyword"""

    keyword = "MAT"
    subkeyword = "154"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat154 class."""
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
                        "alpha",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "gamma",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epsd",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "alpha2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sigp",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "derfi",
                        float,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "cfail",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pfail",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "num",
                        int,
                        50,
                        10,
                        1000,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat154.option_specs[0],
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
        """Get or set the Material identification, a unique number has to be chosen.
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
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Controls shape of yield surface.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the See remarks.
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def epsd(self) -> typing.Optional[float]:
        """Get or set the Densification strain.
        """ # nopep8
        return self._cards[1].get_value("epsd")

    @epsd.setter
    def epsd(self, value: float) -> None:
        """Set the epsd property."""
        self._cards[1].set_value("epsd", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the See remarks.
        """ # nopep8
        return self._cards[1].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[1].set_value("alpha2", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the See remarks.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

    @property
    def sigp(self) -> typing.Optional[float]:
        """Get or set the See remarks.
        """ # nopep8
        return self._cards[1].get_value("sigp")

    @sigp.setter
    def sigp(self, value: float) -> None:
        """Set the sigp property."""
        self._cards[1].set_value("sigp", value)

    @property
    def derfi(self) -> float:
        """Get or set the Type of derivation used in material subroutine
        EQ.0: Numerical derivation.
        EQ.1: Analytical derivation.
        """ # nopep8
        return self._cards[1].get_value("derfi")

    @derfi.setter
    def derfi(self, value: float) -> None:
        """Set the derfi property."""
        if value not in [0, 1, None]:
            raise Exception("""derfi must be `None` or one of {0,1}.""")
        self._cards[1].set_value("derfi", value)

    @property
    def cfail(self) -> typing.Optional[float]:
        """Get or set the Tensile volumetric strain at failure.  Default is no failure due to tensile volumetric strain..
        """ # nopep8
        return self._cards[1].get_value("cfail")

    @cfail.setter
    def cfail(self, value: float) -> None:
        """Set the cfail property."""
        self._cards[1].set_value("cfail", value)

    @property
    def pfail(self) -> typing.Optional[float]:
        """Get or set the Maximum principal stress at failure.  Must be sustained NUM ( > 0) timesteps to fail element.  Default is no failure due to maximum principal stress
        """ # nopep8
        return self._cards[1].get_value("pfail")

    @pfail.setter
    def pfail(self, value: float) -> None:
        """Set the pfail property."""
        self._cards[1].set_value("pfail", value)

    @property
    def num(self) -> int:
        """Get or set the Number of timesteps at or above PFAIL to trigger element failure
        """ # nopep8
        return self._cards[1].get_value("num")

    @num.setter
    def num(self, value: int) -> None:
        """Set the num property."""
        self._cards[1].set_value("num", value)

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

