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

"""Module providing the Mat171 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT171_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ym", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("lamda", float, 50, 10, None),
    FieldSchema("fbuck", float, 60, 10, None),
    FieldSchema("fbuck2", float, 70, 10, None),
)

_MAT171_CARD1 = (
    FieldSchema("ccbrf", float, 0, 10, None),
    FieldSchema("bcur", float, 10, 10, None),
)

_MAT171_CARD2 = (
    FieldSchema("ts1", float, 0, 10, 0.0),
    FieldSchema("ts2", float, 10, 10, 0.0),
    FieldSchema("ts3", float, 20, 10, 0.0),
    FieldSchema("ts4", float, 30, 10, 0.0),
    FieldSchema("cs1", float, 40, 10, 0.0),
    FieldSchema("cs2", float, 50, 10, 0.0),
    FieldSchema("cs3", float, 60, 10, 0.0),
    FieldSchema("cs4", float, 70, 10, 0.0),
)

class Mat171(KeywordBase):
    """DYNA MAT_171 keyword"""

    keyword = "MAT"
    subkeyword = "171"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat171 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT171_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT171_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT171_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat171.option_specs[0],
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
        """Get or set the Material identification.  A unique number must be specified.
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
    def ym(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus
        """ # nopep8
        return self._cards[0].get_value("ym")

    @ym.setter
    def ym(self, value: float) -> None:
        """Set the ym property."""
        self._cards[0].set_value("ym", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's Ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def lamda(self) -> typing.Optional[float]:
        """Get or set the Slenderness ratio (optional ¨C see note)
        """ # nopep8
        return self._cards[0].get_value("lamda")

    @lamda.setter
    def lamda(self, value: float) -> None:
        """Set the lamda property."""
        self._cards[0].set_value("lamda", value)

    @property
    def fbuck(self) -> typing.Optional[float]:
        """Get or set the Initial buckling load (optional ¨C see note. If used, should be positive)
        """ # nopep8
        return self._cards[0].get_value("fbuck")

    @fbuck.setter
    def fbuck(self, value: float) -> None:
        """Set the fbuck property."""
        self._cards[0].set_value("fbuck", value)

    @property
    def fbuck2(self) -> typing.Optional[float]:
        """Get or set the Optional extra term in initial buckling load ¨C see note.
        """ # nopep8
        return self._cards[0].get_value("fbuck2")

    @fbuck2.setter
    def fbuck2(self, value: float) -> None:
        """Set the fbuck2 property."""
        self._cards[0].set_value("fbuck2", value)

    @property
    def ccbrf(self) -> typing.Optional[float]:
        """Get or set the Reduction factor on initial buckling load for cyclic behavior.
        """ # nopep8
        return self._cards[1].get_value("ccbrf")

    @ccbrf.setter
    def ccbrf(self, value: float) -> None:
        """Set the ccbrf property."""
        self._cards[1].set_value("ccbrf", value)

    @property
    def bcur(self) -> typing.Optional[float]:
        """Get or set the Optional load curve giving compressive buckling load (y-axis) versus compressive strain (x-axis - both positive).
        """ # nopep8
        return self._cards[1].get_value("bcur")

    @bcur.setter
    def bcur(self, value: float) -> None:
        """Set the bcur property."""
        self._cards[1].set_value("bcur", value)

    @property
    def ts1(self) -> float:
        """Get or set the Tensile axial strain thresholds 1.
        """ # nopep8
        return self._cards[2].get_value("ts1")

    @ts1.setter
    def ts1(self, value: float) -> None:
        """Set the ts1 property."""
        self._cards[2].set_value("ts1", value)

    @property
    def ts2(self) -> float:
        """Get or set the Tensile axial strain thresholds 2.
        """ # nopep8
        return self._cards[2].get_value("ts2")

    @ts2.setter
    def ts2(self, value: float) -> None:
        """Set the ts2 property."""
        self._cards[2].set_value("ts2", value)

    @property
    def ts3(self) -> float:
        """Get or set the Tensile axial strain thresholds 3
        """ # nopep8
        return self._cards[2].get_value("ts3")

    @ts3.setter
    def ts3(self, value: float) -> None:
        """Set the ts3 property."""
        self._cards[2].set_value("ts3", value)

    @property
    def ts4(self) -> float:
        """Get or set the Tensile axial strain thresholds 4
        """ # nopep8
        return self._cards[2].get_value("ts4")

    @ts4.setter
    def ts4(self, value: float) -> None:
        """Set the ts4 property."""
        self._cards[2].set_value("ts4", value)

    @property
    def cs1(self) -> float:
        """Get or set the Compressive axial strain thresholds 1
        """ # nopep8
        return self._cards[2].get_value("cs1")

    @cs1.setter
    def cs1(self, value: float) -> None:
        """Set the cs1 property."""
        self._cards[2].set_value("cs1", value)

    @property
    def cs2(self) -> float:
        """Get or set the Compressive axial strain thresholds 2
        """ # nopep8
        return self._cards[2].get_value("cs2")

    @cs2.setter
    def cs2(self, value: float) -> None:
        """Set the cs2 property."""
        self._cards[2].set_value("cs2", value)

    @property
    def cs3(self) -> float:
        """Get or set the Compressive axial strain thresholds 3
        """ # nopep8
        return self._cards[2].get_value("cs3")

    @cs3.setter
    def cs3(self, value: float) -> None:
        """Set the cs3 property."""
        self._cards[2].set_value("cs3", value)

    @property
    def cs4(self) -> float:
        """Get or set the Compressive axial strain thresholds 4.
        """ # nopep8
        return self._cards[2].get_value("cs4")

    @cs4.setter
    def cs4(self, value: float) -> None:
        """Set the cs4 property."""
        self._cards[2].set_value("cs4", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

