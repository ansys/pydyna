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

"""Module providing the MatSpringMuscle class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATSPRINGMUSCLE_CARD0 = (
    FieldSchema("mid", int, 0, 10, 0),
    FieldSchema("lo", float, 10, 10, None),
    FieldSchema("vmax", float, 20, 10, None),
    FieldSchema("sv", float, 30, 10, 1.0),
    FieldSchema("a", float, 40, 10, None),
    FieldSchema("fmax", float, 50, 10, None),
    FieldSchema("tl", float, 60, 10, 1.0),
    FieldSchema("tv", float, 70, 10, 1.0),
)

_MATSPRINGMUSCLE_CARD1 = (
    FieldSchema("fpe", float, 0, 10, None),
    FieldSchema("lmax", float, 10, 10, None),
    FieldSchema("ksh", float, 20, 10, None),
)

_MATSPRINGMUSCLE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatSpringMuscle(KeywordBase):
    """DYNA MAT_SPRING_MUSCLE keyword"""

    keyword = "MAT"
    subkeyword = "SPRING_MUSCLE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatSpringMuscle class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSPRINGMUSCLE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSPRINGMUSCLE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatSpringMuscle.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATSPRINGMUSCLE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> int:
        """Get or set the Material identification. A uniques number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def lo(self) -> typing.Optional[float]:
        """Get or set the Initial muscle length.
        """ # nopep8
        return self._cards[0].get_value("lo")

    @lo.setter
    def lo(self, value: float) -> None:
        """Set the lo property."""
        self._cards[0].set_value("lo", value)

    @property
    def vmax(self) -> typing.Optional[float]:
        """Get or set the Maximum CE shortening velocity.
        """ # nopep8
        return self._cards[0].get_value("vmax")

    @vmax.setter
    def vmax(self, value: float) -> None:
        """Set the vmax property."""
        self._cards[0].set_value("vmax", value)

    @property
    def sv(self) -> float:
        """Get or set the Scale factor for Vmax vs. active stat:
        LT.0.0: absolute value gives load curve ID,
        GE.0.0: constant value of 1.0 is used.
        """ # nopep8
        return self._cards[0].get_value("sv")

    @sv.setter
    def sv(self, value: float) -> None:
        """Set the sv property."""
        self._cards[0].set_value("sv", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Activation level vs. time function:
        LT.0.0: absolute value gives load curve ID,
        GT.0.0: constant value of A is used.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def fmax(self) -> typing.Optional[float]:
        """Get or set the Peak isometric force.
        """ # nopep8
        return self._cards[0].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        """Set the fmax property."""
        self._cards[0].set_value("fmax", value)

    @property
    def tl(self) -> float:
        """Get or set the Active tension vs. length function:
        LT.0.0: absolute value gives load curve ID,
        GT.0.0: constant value of 1.0 is used.
        """ # nopep8
        return self._cards[0].get_value("tl")

    @tl.setter
    def tl(self, value: float) -> None:
        """Set the tl property."""
        self._cards[0].set_value("tl", value)

    @property
    def tv(self) -> float:
        """Get or set the Active tension vs. velocity function:
        LT.0.0: absolute value gives load curve ID,
        GT.0.0: constant value of 1.0 is used.
        """ # nopep8
        return self._cards[0].get_value("tv")

    @tv.setter
    def tv(self, value: float) -> None:
        """Set the tv property."""
        self._cards[0].set_value("tv", value)

    @property
    def fpe(self) -> typing.Optional[float]:
        """Get or set the Force vs. length function for parallel elastic element:
        LT.0.0: absolute value gives load curve ID,
        EQ.0.0: exponential function is used,
        GT.0.0: constant value of 0.0 is used.
        """ # nopep8
        return self._cards[1].get_value("fpe")

    @fpe.setter
    def fpe(self, value: float) -> None:
        """Set the fpe property."""
        self._cards[1].set_value("fpe", value)

    @property
    def lmax(self) -> typing.Optional[float]:
        """Get or set the Relative length when Fpe reaches Fmax. Required if Fpe=0 above.
        """ # nopep8
        return self._cards[1].get_value("lmax")

    @lmax.setter
    def lmax(self, value: float) -> None:
        """Set the lmax property."""
        self._cards[1].set_value("lmax", value)

    @property
    def ksh(self) -> typing.Optional[float]:
        """Get or set the Constant governing the exponential rise of Fpe. Required if Fpe=0 above.
        """ # nopep8
        return self._cards[1].get_value("ksh")

    @ksh.setter
    def ksh(self, value: float) -> None:
        """Set the ksh property."""
        self._cards[1].set_value("ksh", value)

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

