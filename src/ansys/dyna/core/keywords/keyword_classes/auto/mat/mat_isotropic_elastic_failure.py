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

"""Module providing the MatIsotropicElasticFailure class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATISOTROPICELASTICFAILURE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("g", float, 20, 10, None),
    FieldSchema("sigy", float, 30, 10, None),
    FieldSchema("etan", float, 40, 10, None),
    FieldSchema("bulk", float, 50, 10, None),
)

_MATISOTROPICELASTICFAILURE_CARD1 = (
    FieldSchema("epf", float, 0, 10, None),
    FieldSchema("prf", float, 10, 10, None),
    FieldSchema("rem", float, 20, 10, None),
    FieldSchema("trem", float, 30, 10, None),
)

class MatIsotropicElasticFailure(KeywordBase):
    """DYNA MAT_ISOTROPIC_ELASTIC_FAILURE keyword"""

    keyword = "MAT"
    subkeyword = "ISOTROPIC_ELASTIC_FAILURE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatIsotropicElasticFailure class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATISOTROPICELASTICFAILURE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATISOTROPICELASTICFAILURE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatIsotropicElasticFailure.option_specs[0],
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus.
        """ # nopep8
        return self._cards[0].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[0].set_value("etan", value)

    @property
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus, K.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        """Set the bulk property."""
        self._cards[0].set_value("bulk", value)

    @property
    def epf(self) -> typing.Optional[float]:
        """Get or set the Plastic failure strain.
        """ # nopep8
        return self._cards[1].get_value("epf")

    @epf.setter
    def epf(self, value: float) -> None:
        """Set the epf property."""
        self._cards[1].set_value("epf", value)

    @property
    def prf(self) -> typing.Optional[float]:
        """Get or set the Failure pressure (<= 0.0) (default = 0.0).
        """ # nopep8
        return self._cards[1].get_value("prf")

    @prf.setter
    def prf(self, value: float) -> None:
        """Set the prf property."""
        self._cards[1].set_value("prf", value)

    @property
    def rem(self) -> typing.Optional[float]:
        """Get or set the Element erosion option:
        EQ.0.0: failed element eroded after failure,
        NE.0.0: element is kept, no removal except by Dt below.
        """ # nopep8
        return self._cards[1].get_value("rem")

    @rem.setter
    def rem(self, value: float) -> None:
        """Set the rem property."""
        self._cards[1].set_value("rem", value)

    @property
    def trem(self) -> typing.Optional[float]:
        """Get or set the Dt for element removal:
        EQ.0.0: Dt is not considered (default),
        GT.0.0: element eroded if element time step size falls below Dt.
        """ # nopep8
        return self._cards[1].get_value("trem")

    @trem.setter
    def trem(self, value: float) -> None:
        """Set the trem property."""
        self._cards[1].set_value("trem", value)

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

