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

"""Module providing the Mat129 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT129_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("k", float, 20, 10, None),
    FieldSchema("c", float, 30, 10, None),
    FieldSchema("delta", float, 40, 10, None),
    FieldSchema("alpha", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MAT129_CARD1 = (
    FieldSchema("c1", float, 0, 10, None),
    FieldSchema("c2", float, 10, 10, None),
    FieldSchema("lcid", float, 20, 10, 0.0),
    FieldSchema("tramp", float, 30, 10, None),
    FieldSchema("nt", float, 40, 10, 6.0),
)

_MAT129_CARD2 = (
    FieldSchema("gi", float, 0, 10, None),
    FieldSchema("betai", float, 10, 10, None),
)

class Mat129(KeywordBase):
    """DYNA MAT_129 keyword"""

    keyword = "MAT"
    subkeyword = "129"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat129 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT129_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT129_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT129_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat129.option_specs[0],
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Material coefficient.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def delta(self) -> typing.Optional[float]:
        """Get or set the delta, material coefficient.
        """ # nopep8
        return self._cards[0].get_value("delta")

    @delta.setter
    def delta(self, value: float) -> None:
        """Set the delta property."""
        self._cards[0].set_value("delta", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the alpha, material coefficient.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the beta, material coefficient.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Material coefficient.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Material coefficient.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def lcid(self) -> float:
        """Get or set the Optional load curve ID of relaxation curve.
        If constants beta-i are determined via a least squares fit. This model ignores the constant stress.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: float) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for loading.
        """ # nopep8
        return self._cards[1].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        """Set the tramp property."""
        self._cards[1].set_value("tramp", value)

    @property
    def nt(self) -> float:
        """Get or set the Number of Prony series terms in optional fit. Default is 6. Possibly 3-5 are recommended, since each term used adds significantly to the cost. Always check the results of the fit in the output file. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once a satisfactory fit has been achieved it is recommended that the coefficients which are written into the output file be input in future runs.
        """ # nopep8
        return self._cards[1].get_value("nt")

    @nt.setter
    def nt(self, value: float) -> None:
        """Set the nt property."""
        self._cards[1].set_value("nt", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Optional shear relaxation modulus for the  first term. Define up to six cards.
        """ # nopep8
        return self._cards[2].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        """Set the gi property."""
        self._cards[2].set_value("gi", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the Optional decay constant if ith term. Define up to six cards.
        """ # nopep8
        return self._cards[2].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        """Set the betai property."""
        self._cards[2].set_value("betai", value)

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

