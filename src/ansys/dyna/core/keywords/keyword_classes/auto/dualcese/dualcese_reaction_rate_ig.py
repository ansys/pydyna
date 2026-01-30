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

"""Module providing the DualceseReactionRateIg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEREACTIONRATEIG_CARD0 = (
    FieldSchema("react_id", int, 0, 10, None),
    FieldSchema("ign", float, 10, 10, None),
    FieldSchema("aa", float, 20, 10, None),
    FieldSchema("bb", float, 30, 10, None),
    FieldSchema("xx", float, 40, 10, None),
    FieldSchema("grow1", float, 50, 10, None),
    FieldSchema("cc", float, 60, 10, None),
    FieldSchema("dd", float, 70, 10, None),
)

_DUALCESEREACTIONRATEIG_CARD1 = (
    FieldSchema("yy", float, 0, 10, None),
    FieldSchema("grow2", float, 10, 10, None),
    FieldSchema("ee", float, 20, 10, None),
    FieldSchema("gg", float, 30, 10, None),
    FieldSchema("zz", float, 40, 10, None),
    FieldSchema("igmax", float, 50, 10, None),
    FieldSchema("g1max", float, 60, 10, None),
    FieldSchema("g2max", float, 70, 10, None),
)

class DualceseReactionRateIg(KeywordBase):
    """DYNA DUALCESE_REACTION_RATE_IG keyword"""

    keyword = "DUALCESE"
    subkeyword = "REACTION_RATE_IG"

    def __init__(self, **kwargs):
        """Initialize the DualceseReactionRateIg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEREACTIONRATEIG_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DUALCESEREACTIONRATEIG_CARD1,
                **kwargs,
            ),        ]
    @property
    def react_id(self) -> typing.Optional[int]:
        """Get or set the ID of reaction rate law
        """ # nopep8
        return self._cards[0].get_value("react_id")

    @react_id.setter
    def react_id(self, value: int) -> None:
        """Set the react_id property."""
        self._cards[0].set_value("react_id", value)

    @property
    def ign(self) -> typing.Optional[float]:
        """Get or set the Reaction ignition term parameter i
        """ # nopep8
        return self._cards[0].get_value("ign")

    @ign.setter
    def ign(self, value: float) -> None:
        """Set the ign property."""
        self._cards[0].set_value("ign", value)

    @property
    def aa(self) -> typing.Optional[float]:
        """Get or set the Reaction ignition term parameter
        """ # nopep8
        return self._cards[0].get_value("aa")

    @aa.setter
    def aa(self, value: float) -> None:
        """Set the aa property."""
        self._cards[0].set_value("aa", value)

    @property
    def bb(self) -> typing.Optional[float]:
        """Get or set the Reaction ignition term parameter
        """ # nopep8
        return self._cards[0].get_value("bb")

    @bb.setter
    def bb(self, value: float) -> None:
        """Set the bb property."""
        self._cards[0].set_value("bb", value)

    @property
    def xx(self) -> typing.Optional[float]:
        """Get or set the Reaction ignition term parameter
        """ # nopep8
        return self._cards[0].get_value("xx")

    @xx.setter
    def xx(self, value: float) -> None:
        """Set the xx property."""
        self._cards[0].set_value("xx", value)

    @property
    def grow1(self) -> typing.Optional[float]:
        """Get or set the Reaction growth term parameter
        """ # nopep8
        return self._cards[0].get_value("grow1")

    @grow1.setter
    def grow1(self, value: float) -> None:
        """Set the grow1 property."""
        self._cards[0].set_value("grow1", value)

    @property
    def cc(self) -> typing.Optional[float]:
        """Get or set the Reaction growth term parameter
        """ # nopep8
        return self._cards[0].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        """Set the cc property."""
        self._cards[0].set_value("cc", value)

    @property
    def dd(self) -> typing.Optional[float]:
        """Get or set the Reaction growth term parameter
        """ # nopep8
        return self._cards[0].get_value("dd")

    @dd.setter
    def dd(self, value: float) -> None:
        """Set the dd property."""
        self._cards[0].set_value("dd", value)

    @property
    def yy(self) -> typing.Optional[float]:
        """Get or set the Reaction growth term parameterr
        """ # nopep8
        return self._cards[1].get_value("yy")

    @yy.setter
    def yy(self, value: float) -> None:
        """Set the yy property."""
        self._cards[1].set_value("yy", value)

    @property
    def grow2(self) -> typing.Optional[float]:
        """Get or set the Reaction completion term parameter
        """ # nopep8
        return self._cards[1].get_value("grow2")

    @grow2.setter
    def grow2(self, value: float) -> None:
        """Set the grow2 property."""
        self._cards[1].set_value("grow2", value)

    @property
    def ee(self) -> typing.Optional[float]:
        """Get or set the Reaction completion term parameter
        """ # nopep8
        return self._cards[1].get_value("ee")

    @ee.setter
    def ee(self, value: float) -> None:
        """Set the ee property."""
        self._cards[1].set_value("ee", value)

    @property
    def gg(self) -> typing.Optional[float]:
        """Get or set the Reaction completion term parameter
        """ # nopep8
        return self._cards[1].get_value("gg")

    @gg.setter
    def gg(self, value: float) -> None:
        """Set the gg property."""
        self._cards[1].set_value("gg", value)

    @property
    def zz(self) -> typing.Optional[float]:
        """Get or set the Reaction completion term parameter
        """ # nopep8
        return self._cards[1].get_value("zz")

    @zz.setter
    def zz(self, value: float) -> None:
        """Set the zz property."""
        self._cards[1].set_value("zz", value)

    @property
    def igmax(self) -> typing.Optional[float]:
        """Get or set the Maximum mass fraction of the product for reaction ignition term
        """ # nopep8
        return self._cards[1].get_value("igmax")

    @igmax.setter
    def igmax(self, value: float) -> None:
        """Set the igmax property."""
        self._cards[1].set_value("igmax", value)

    @property
    def g1max(self) -> typing.Optional[float]:
        """Get or set the Maximum mass fraction of the product for reaction ignition term
        """ # nopep8
        return self._cards[1].get_value("g1max")

    @g1max.setter
    def g1max(self, value: float) -> None:
        """Set the g1max property."""
        self._cards[1].set_value("g1max", value)

    @property
    def g2max(self) -> typing.Optional[float]:
        """Get or set the Maximum for reaction completion term
        """ # nopep8
        return self._cards[1].get_value("g2max")

    @g2max.setter
    def g2max(self, value: float) -> None:
        """Set the g2max property."""
        self._cards[1].set_value("g2max", value)

