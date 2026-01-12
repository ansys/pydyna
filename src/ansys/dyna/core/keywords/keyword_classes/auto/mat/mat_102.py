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

"""Module providing the Mat102 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT102_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("t", float, 40, 10, None),
    FieldSchema("hc", float, 50, 10, None),
    FieldSchema("vp", float, 60, 10, 0.0),
)

_MAT102_CARD1 = (
    FieldSchema("alpha", float, 0, 10, None),
    FieldSchema("n", float, 10, 10, None),
    FieldSchema("a", float, 20, 10, None),
    FieldSchema("q", float, 30, 10, None),
    FieldSchema("g", float, 40, 10, None),
    FieldSchema("epso", float, 50, 10, None),
    FieldSchema("lcq", float, 60, 10, None),
)

_MAT102_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat102(KeywordBase):
    """DYNA MAT_102 keyword"""

    keyword = "MAT"
    subkeyword = "102"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat102 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT102_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT102_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat102.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT102_OPTION0_CARD0,
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
    def t(self) -> typing.Optional[float]:
        """Get or set the Initial temperature.
        """ # nopep8
        return self._cards[0].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[0].set_value("t", value)

    @property
    def hc(self) -> typing.Optional[float]:
        """Get or set the Heat generation coefficient.
        """ # nopep8
        return self._cards[0].get_value("hc")

    @hc.setter
    def hc(self, value: float) -> None:
        """Set the hc property."""
        self._cards[0].set_value("hc", value)

    @property
    def vp(self) -> float:
        """Get or set the Formula for rate effects:
        EQ.0.0 Scale yield stress (default),
        EQ.1.0: Viscoplastic formulation.
        """ # nopep8
        return self._cards[0].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        """Set the vp property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""vp must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("vp", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Not to be confused with coefficient of thermal expansion
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[1].set_value("q", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[1].set_value("g", value)

    @property
    def epso(self) -> typing.Optional[float]:
        """Get or set the Minimum strain rate used is the model.
        """ # nopep8
        return self._cards[1].get_value("epso")

    @epso.setter
    def epso(self, value: float) -> None:
        """Set the epso property."""
        self._cards[1].set_value("epso", value)

    @property
    def lcq(self) -> typing.Optional[float]:
        """Get or set the ID of curve specifying parameter Q:
        GT.0:	Q as function of plastic strain.
        LT.0 : Q as function of temperature
        """ # nopep8
        return self._cards[1].get_value("lcq")

    @lcq.setter
    def lcq(self, value: float) -> None:
        """Set the lcq property."""
        self._cards[1].set_value("lcq", value)

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

