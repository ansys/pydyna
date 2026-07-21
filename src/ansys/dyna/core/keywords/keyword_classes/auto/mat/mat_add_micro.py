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

"""Module providing the MatAddMicro class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_MATADDMICRO_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("mictyp", int, 10, 10, 1),
    FieldSchema("nump", int, 20, 10, 16),
)

_MATADDMICRO_CARD1 = (
    FieldSchema("q1", float, 0, 10, None),
    FieldSchema("a1", float, 10, 10, None),
    FieldSchema("m1", float, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("q2", float, 70, 10, None),
)

_MATADDMICRO_CARD2 = (
    FieldSchema("a2", float, 0, 10, None),
    FieldSchema("m2", float, 10, 10, None),
    FieldSchema("h2", float, 20, 10, None),
    FieldSchema("q3", float, 30, 10, None),
    FieldSchema("a3", float, 40, 10, None),
    FieldSchema("m3", float, 50, 10, None),
    FieldSchema("betad", float, 60, 10, None),
    FieldSchema("d0", float, 70, 10, None),
)

_MATADDMICRO_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAddMicro(KeywordBase):
    """DYNA MAT_ADD_MICRO keyword"""

    keyword = "MAT"
    subkeyword = "ADD_MICRO"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "mid": LinkType.MAT,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAddMicro class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATADDMICRO_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDMICRO_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDMICRO_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatAddMicro._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATADDMICRO_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID for which this keyword applies
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def mictyp(self) -> int:
        """Get or set the Microstructure evolution model:
        EQ.1 : Sommitch’s Johnson - Mehl - Avrami - Kolmogorov(JMAK) model
        """ # nopep8
        return self._cards[0].get_value("mictyp")

    @mictyp.setter
    def mictyp(self, value: int) -> None:
        """Set the mictyp property."""
        self._cards[0].set_value("mictyp", value)

    @property
    def nump(self) -> int:
        """Get or set the Number of material constants for the microstructure evolution model
        """ # nopep8
        return self._cards[0].get_value("nump")

    @nump.setter
    def nump(self, value: int) -> None:
        """Set the nump property."""
        self._cards[0].set_value("nump", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Material constant Q_1 needed for determining critical strain
        """ # nopep8
        return self._cards[1].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        """Set the q1 property."""
        self._cards[1].set_value("q1", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Material constant a_1 needed for determining critical strain
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[1].set_value("a1", value)

    @property
    def m1(self) -> typing.Optional[float]:
        """Get or set the Material constant m_1 needed for determining critical strain
        """ # nopep8
        return self._cards[1].get_value("m1")

    @m1.setter
    def m1(self, value: float) -> None:
        """Set the m1 property."""
        self._cards[1].set_value("m1", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Material constant Q_2 needed for determining the strain at 50% recrystallization
        """ # nopep8
        return self._cards[1].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        """Set the q2 property."""
        self._cards[1].set_value("q2", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Material constant a_2 needed for determining the strain at 50% recrystallization
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[2].set_value("a2", value)

    @property
    def m2(self) -> typing.Optional[float]:
        """Get or set the Material constant m_2 needed for determining the strain at 50% recrystallization
        """ # nopep8
        return self._cards[2].get_value("m2")

    @m2.setter
    def m2(self, value: float) -> None:
        """Set the m2 property."""
        self._cards[2].set_value("m2", value)

    @property
    def h2(self) -> typing.Optional[float]:
        """Get or set the Material constant h_2 needed for determining the strain at 50% recrystallization
        """ # nopep8
        return self._cards[2].get_value("h2")

    @h2.setter
    def h2(self, value: float) -> None:
        """Set the h2 property."""
        self._cards[2].set_value("h2", value)

    @property
    def q3(self) -> typing.Optional[float]:
        """Get or set the Material constant Q_3 needed for determining the dynamic recrystallization grain size
        """ # nopep8
        return self._cards[2].get_value("q3")

    @q3.setter
    def q3(self, value: float) -> None:
        """Set the q3 property."""
        self._cards[2].set_value("q3", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Material constant a_3 needed for determining the dynamic recrystallization grain size
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[2].set_value("a3", value)

    @property
    def m3(self) -> typing.Optional[float]:
        """Get or set the Material constant m_3 needed for determining the dynamic recrystallization grain size
        """ # nopep8
        return self._cards[2].get_value("m3")

    @m3.setter
    def m3(self, value: float) -> None:
        """Set the m3 property."""
        self._cards[2].set_value("m3", value)

    @property
    def betad(self) -> typing.Optional[float]:
        """Get or set the Material constant β_d for determining the volume fraction of dynamic recrystallization
        """ # nopep8
        return self._cards[2].get_value("betad")

    @betad.setter
    def betad(self, value: float) -> None:
        """Set the betad property."""
        self._cards[2].set_value("betad", value)

    @property
    def d0(self) -> typing.Optional[float]:
        """Get or set the Initial grain size, d_0
        """ # nopep8
        return self._cards[2].get_value("d0")

    @d0.setter
    def d0(self, value: float) -> None:
        """Set the d0 property."""
        self._cards[2].set_value("d0", value)

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

    @property
    def mid_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid:
                return kwd
        return None

    @mid_link.setter
    def mid_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid."""
        self.mid = value.mid

