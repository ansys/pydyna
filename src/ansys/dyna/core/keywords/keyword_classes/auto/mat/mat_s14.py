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

"""Module providing the MatS14 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATS14_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("a14", float, 10, 10, None),
    FieldSchema("b14", float, 20, 10, None),
    FieldSchema("c14", float, 30, 10, None),
    FieldSchema("d14", float, 40, 10, None),
    FieldSchema("e14", float, 50, 10, None),
    FieldSchema("lcid", int, 60, 10, None),
    FieldSchema("psd", float, 70, 10, None),
)

class MatS14(KeywordBase):
    """DYNA MAT_S14 keyword"""

    keyword = "MAT"
    subkeyword = "S14"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatS14 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATS14_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatS14.option_specs[0],
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
        """Get or set the Material number. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def a14(self) -> typing.Optional[float]:
        """Get or set the Material coefficient A
        """ # nopep8
        return self._cards[0].get_value("a14")

    @a14.setter
    def a14(self, value: float) -> None:
        """Set the a14 property."""
        self._cards[0].set_value("a14", value)

    @property
    def b14(self) -> typing.Optional[float]:
        """Get or set the Material coefficient B
        """ # nopep8
        return self._cards[0].get_value("b14")

    @b14.setter
    def b14(self, value: float) -> None:
        """Set the b14 property."""
        self._cards[0].set_value("b14", value)

    @property
    def c14(self) -> typing.Optional[float]:
        """Get or set the Material coefficient C
        """ # nopep8
        return self._cards[0].get_value("c14")

    @c14.setter
    def c14(self, value: float) -> None:
        """Set the c14 property."""
        self._cards[0].set_value("c14", value)

    @property
    def d14(self) -> typing.Optional[float]:
        """Get or set the Material coefficient D
        """ # nopep8
        return self._cards[0].get_value("d14")

    @d14.setter
    def d14(self, value: float) -> None:
        """Set the d14 property."""
        self._cards[0].set_value("d14", value)

    @property
    def e14(self) -> typing.Optional[float]:
        """Get or set the Material coefficient E
        """ # nopep8
        return self._cards[0].get_value("e14")

    @e14.setter
    def e14(self, value: float) -> None:
        """Set the e14 property."""
        self._cards[0].set_value("e14", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Loadcurve id referencing the maximum strength envelope curve
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def psd(self) -> typing.Optional[float]:
        """Get or set the Sustained strength reduction factor
        """ # nopep8
        return self._cards[0].get_value("psd")

    @psd.setter
    def psd(self, value: float) -> None:
        """Set the psd property."""
        self._cards[0].set_value("psd", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

