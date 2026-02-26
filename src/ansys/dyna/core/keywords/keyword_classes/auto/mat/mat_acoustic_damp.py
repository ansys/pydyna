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

"""Module providing the MatAcousticDamp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATACOUSTICDAMP_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("cee", float, 20, 10, None),
    FieldSchema("beta", float, 30, 10, 0.0),
)

_MATACOUSTICDAMP_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("vdc", float, 60, 10, 0.0),
    FieldSchema("beta2", float, 70, 10, 0.0),
)

_MATACOUSTICDAMP_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAcousticDamp(KeywordBase):
    """DYNA MAT_ACOUSTIC_DAMP keyword"""

    keyword = "MAT"
    subkeyword = "ACOUSTIC_DAMP"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatAcousticDamp class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATACOUSTICDAMP_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATACOUSTICDAMP_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAcousticDamp.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATACOUSTICDAMP_OPTION0_CARD0,
                        **kwargs,
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
    def cee(self) -> typing.Optional[float]:
        """Get or set the Sound speed.
        """ # nopep8
        return self._cards[0].get_value("cee")

    @cee.setter
    def cee(self, value: float) -> None:
        """Set the cee property."""
        self._cards[0].set_value("cee", value)

    @property
    def beta(self) -> float:
        """Get or set the Linear bulk viscosity coefficient.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def vdc(self) -> float:
        """Get or set the Volumetric drag coefficient.
        """ # nopep8
        return self._cards[1].get_value("vdc")

    @vdc.setter
    def vdc(self, value: float) -> None:
        """Set the vdc property."""
        self._cards[1].set_value("vdc", value)

    @property
    def beta2(self) -> float:
        """Get or set the Quadratic bulk viscosity coefficient.
        """ # nopep8
        return self._cards[1].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[1].set_value("beta2", value)

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

