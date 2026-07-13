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

"""Module providing the MatIspg04 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATISPG04_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("b", float, 20, 10, None),
    FieldSchema("sften", float, 30, 10, None),
    FieldSchema("tau", int, 40, 10, None),
    FieldSchema("n", float, 50, 10, None),
)

_MATISPG04_CARD1 = (
    FieldSchema("tref", float, 0, 10, None),
)

_MATISPG04_CARD2 = (
    FieldSchema("alphag", float, 0, 10, None),
    FieldSchema("c1", int, 10, 10, None),
    FieldSchema("c2", float, 20, 10, None),
)

_MATISPG04_CARD3 = (
    FieldSchema("m_ks", float, 0, 10, None),
    FieldSchema("n_ks", int, 10, 10, None),
    FieldSchema("a1", float, 20, 10, None),
    FieldSchema("a2", float, 30, 10, None),
    FieldSchema("t1", float, 40, 10, None),
    FieldSchema("t2", float, 50, 10, None),
)

_MATISPG04_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatIspg04(KeywordBase):
    """DYNA MAT_ISPG_04 keyword"""

    keyword = "MAT"
    subkeyword = "ISPG_04"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatIspg04 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATISPG04_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATISPG04_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATISPG04_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATISPG04_CARD3,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatIspg04._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATISPG04_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Coefficient for calculating the zero-shear viscosity of the fluid, B
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def sften(self) -> typing.Optional[float]:
        """Get or set the Surface tension coefficient of the fluid
        """ # nopep8
        return self._cards[0].get_value("sften")

    @sften.setter
    def sften(self, value: float) -> None:
        """Set the sften property."""
        self._cards[0].set_value("sften", value)

    @property
    def tau(self) -> typing.Optional[int]:
        """Get or set the Shear stress at the transition from Newtonian to non-Newtonian flow
        """ # nopep8
        return self._cards[0].get_value("tau")

    @tau.setter
    def tau(self, value: int) -> None:
        """Set the tau property."""
        self._cards[0].set_value("tau", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Power-law index, n
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for calculating the zero-shear viscosity, T_ref
        """ # nopep8
        return self._cards[1].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[1].set_value("tref", value)

    @property
    def alphag(self) -> typing.Optional[float]:
        """Get or set the Gel point at which flow is no longer possible, a_g
        """ # nopep8
        return self._cards[2].get_value("alphag")

    @alphag.setter
    def alphag(self, value: float) -> None:
        """Set the alphag property."""
        self._cards[2].set_value("alphag", value)

    @property
    def c1(self) -> typing.Optional[int]:
        """Get or set the Experimental constant, c_1
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: int) -> None:
        """Set the c1 property."""
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Experimental constant, c_2
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[2].set_value("c2", value)

    @property
    def m_ks(self) -> typing.Optional[float]:
        """Get or set the Reaction order in the Kamal-Sourour model, m_ks
        """ # nopep8
        return self._cards[3].get_value("m_ks")

    @m_ks.setter
    def m_ks(self, value: float) -> None:
        """Set the m_ks property."""
        self._cards[3].set_value("m_ks", value)

    @property
    def n_ks(self) -> typing.Optional[int]:
        """Get or set the Reaction order in the Kamal-Sourour model, n_ks
        """ # nopep8
        return self._cards[3].get_value("n_ks")

    @n_ks.setter
    def n_ks(self, value: int) -> None:
        """Set the n_ks property."""
        self._cards[3].set_value("n_ks", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Fitted rate coefficient, A_1
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Fitted rate coefficient, A_2
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Activation temperature, T_1
        """ # nopep8
        return self._cards[3].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[3].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Activation temperature, T_2
        """ # nopep8
        return self._cards[3].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[3].set_value("t2", value)

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

