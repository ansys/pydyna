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

"""Module providing the Mat177 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat177(KeywordBase):
    """DYNA MAT_177 keyword"""

    keyword = "MAT"
    subkeyword = "177"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat177 class."""
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
                        "k",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n",
                        float,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nu",
                        float,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lcid",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "fittype",
                        int,
                        60,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "lcsr",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c1",
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
                        "c3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c8",
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
                        "b1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "b8",
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
                        "r",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "m",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat177.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be chosen.
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus. This modulus is used for determining the contact interface stiffness.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def n(self) -> float:
        """Get or set the Material constant. Define if LCID=0 below; otherwise, N is fit from the load curve data.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def nu(self) -> float:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("nu")

    @nu.setter
    def nu(self, value: float) -> None:
        """Set the nu property."""
        self._cards[0].set_value("nu", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID that defines the force per unit area versus the stretch ratio. This curve can be given for either uniaxial or biaxial data depending on FITTYPE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def fittype(self) -> int:
        """Get or set the Type of fit:
        EQ.1:uniaxial data,
        EQ.2:biaxial data.
        """ # nopep8
        return self._cards[0].get_value("fittype")

    @fittype.setter
    def fittype(self, value: int) -> None:
        """Set the fittype property."""
        if value not in [1, 2, None]:
            raise Exception("""fittype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("fittype", value)

    @property
    def lcsr(self) -> int:
        """Get or set the Load curve ID that defines the uniaxial or biaxial stress ratio (see FITTYPE) versus the transverse stretch ratio.
        """ # nopep8
        return self._cards[0].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        """Set the lcsr property."""
        self._cards[0].set_value("lcsr", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[1].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[1].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        """Set the c4 property."""
        self._cards[1].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[1].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        """Set the c5 property."""
        self._cards[1].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[1].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        """Set the c6 property."""
        self._cards[1].set_value("c6", value)

    @property
    def c7(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[1].get_value("c7")

    @c7.setter
    def c7(self, value: float) -> None:
        """Set the c7 property."""
        self._cards[1].set_value("c7", value)

    @property
    def c8(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[1].get_value("c8")

    @c8.setter
    def c8(self, value: float) -> None:
        """Set the c8 property."""
        self._cards[1].set_value("c8", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[2].set_value("b1", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        """Set the b2 property."""
        self._cards[2].set_value("b2", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        """Set the b3 property."""
        self._cards[2].set_value("b3", value)

    @property
    def b4(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("b4")

    @b4.setter
    def b4(self, value: float) -> None:
        """Set the b4 property."""
        self._cards[2].set_value("b4", value)

    @property
    def b5(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("b5")

    @b5.setter
    def b5(self, value: float) -> None:
        """Set the b5 property."""
        self._cards[2].set_value("b5", value)

    @property
    def b6(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("b6")

    @b6.setter
    def b6(self, value: float) -> None:
        """Set the b6 property."""
        self._cards[2].set_value("b6", value)

    @property
    def b7(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("b7")

    @b7.setter
    def b7(self, value: float) -> None:
        """Set the b7 property."""
        self._cards[2].set_value("b7", value)

    @property
    def b8(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("b8")

    @b8.setter
    def b8(self, value: float) -> None:
        """Set the b8 property."""
        self._cards[2].set_value("b8", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Mullins effect model r coefficient.
        """ # nopep8
        return self._cards[3].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[3].set_value("r", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Mullins effect model m coefficient
        """ # nopep8
        return self._cards[3].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[3].set_value("m", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

