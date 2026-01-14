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

"""Module providing the MatSpringGeneralNonlinear class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_MATSPRINGGENERALNONLINEAR_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("lcdl", int, 10, 10, None),
    FieldSchema("lcdu", int, 20, 10, None),
    FieldSchema("beta", float, 30, 10, None),
    FieldSchema("tyi", float, 40, 10, None),
    FieldSchema("cyi", float, 50, 10, None),
)

_MATSPRINGGENERALNONLINEAR_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatSpringGeneralNonlinear(KeywordBase):
    """DYNA MAT_SPRING_GENERAL_NONLINEAR keyword"""

    keyword = "MAT"
    subkeyword = "SPRING_GENERAL_NONLINEAR"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcdl": LinkType.DEFINE_CURVE_OR_TABLE,
        "lcdu": LinkType.DEFINE_CURVE_OR_TABLE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatSpringGeneralNonlinear class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSPRINGGENERALNONLINEAR_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatSpringGeneralNonlinear.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATSPRINGGENERALNONLINEAR_OPTION0_CARD0,
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
    def lcdl(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID giving force/torque as a function of displacement/rotation (curve) or as a function of velocity and displacement/rotation (table) for loading;
        """ # nopep8
        return self._cards[0].get_value("lcdl")

    @lcdl.setter
    def lcdl(self, value: int) -> None:
        """Set the lcdl property."""
        self._cards[0].set_value("lcdl", value)

    @property
    def lcdu(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID giving force/torque as a function of displacement/rotation (curve) or as a function of velocity and displacement/rotation (table) for unloading
        """ # nopep8
        return self._cards[0].get_value("lcdu")

    @lcdu.setter
    def lcdu(self, value: int) -> None:
        """Set the lcdu property."""
        self._cards[0].set_value("lcdu", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter:
        EQ.0.0: tensile and compressive yield with strain softening (negative or zero slope allowed in the force versus disp. load curves),
        NE.0.0: kinematic hardening without strain softening,
        EQ.1.0: isotropic hardening without strain softening.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def tyi(self) -> typing.Optional[float]:
        """Get or set the Initial yield force in tension ( > 0).
        """ # nopep8
        return self._cards[0].get_value("tyi")

    @tyi.setter
    def tyi(self, value: float) -> None:
        """Set the tyi property."""
        self._cards[0].set_value("tyi", value)

    @property
    def cyi(self) -> typing.Optional[float]:
        """Get or set the Initial yield force in compression ( < 0).
        """ # nopep8
        return self._cards[0].get_value("cyi")

    @cyi.setter
    def cyi(self, value: float) -> None:
        """Set the cyi property."""
        self._cards[0].set_value("cyi", value)

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

    @property
    def lcdl_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for lcdl."""
        if self.deck is None:
            return None
        field_value = self.lcdl
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @lcdl_link.setter
    def lcdl_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for lcdl."""
        if hasattr(value, "lcid"):
            self.lcdl = value.lcid
        elif hasattr(value, "tbid"):
            self.lcdl = value.tbid

    @property
    def lcdu_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for lcdu."""
        if self.deck is None:
            return None
        field_value = self.lcdu
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @lcdu_link.setter
    def lcdu_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for lcdu."""
        if hasattr(value, "lcid"):
            self.lcdu = value.lcid
        elif hasattr(value, "tbid"):
            self.lcdu = value.tbid

