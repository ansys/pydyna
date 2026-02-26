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

"""Module providing the MatBlatz_KoFoam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATBLATZ_KOFOAM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("g", float, 20, 10, None),
    FieldSchema("ref", float, 30, 10, 0.0),
)

_MATBLATZ_KOFOAM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatBlatz_KoFoam(KeywordBase):
    """DYNA MAT_BLATZ-KO_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "BLATZ-KO_FOAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatBlatz_KoFoam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATBLATZ_KOFOAM_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatBlatz_KoFoam.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATBLATZ_KOFOAM_OPTION0_CARD0,
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor, see *INITIAL_FOAM_REFERENCE_GEOMETRY (only 8-noded solid elements with one point integration):
        EQ.0.0: off (default),
        EQ.1.0: on.
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ref must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("ref", value)

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

