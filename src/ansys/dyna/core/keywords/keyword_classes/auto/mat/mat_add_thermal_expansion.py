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

"""Module providing the MatAddThermalExpansion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATADDTHERMALEXPANSION_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("mult", float, 20, 10, 1.0),
    FieldSchema("lcid", int, 30, 10, None),
    FieldSchema("multy", float, 40, 10, 1.0),
    FieldSchema("lcid", int, 50, 10, None),
    FieldSchema("multz", float, 60, 10, 1.0),
    FieldSchema("tref", float, 70, 10, 0.0),
)

_MATADDTHERMALEXPANSION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAddThermalExpansion(KeywordBase):
    """DYNA MAT_ADD_THERMAL_EXPANSION keyword"""

    keyword = "MAT"
    subkeyword = "ADD_THERMAL_EXPANSION"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "lcid": LinkType.DEFINE_CURVE,
        "lcid": LinkType.DEFINE_CURVE,
        "id": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAddThermalExpansion class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATADDTHERMALEXPANSION_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatAddThermalExpansion._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATADDTHERMALEXPANSION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part or material ID for which the thermal expansion property applies:
        GT.0: Part ID
        LT.0: Material ID given by abs(ID)
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the LCID.GT.0: Load curve ID defining thermal expansion coefficient as a function of temperature LCID.
        EQ.0: Thermal expansion coefficient given by constant MULT
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def mult(self) -> float:
        """Get or set the Scale factor scaling load curve given by LCID
        """ # nopep8
        return self._cards[0].get_value("mult")

    @mult.setter
    def mult(self, value: float) -> None:
        """Set the mult property."""
        self._cards[0].set_value("mult", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining thermal expansion coefficient in local y-direction as a function of temperature. If zero, the thermal  expansion coefficient in local y-direction given by constant MULTY, if MULTY=0 as well, the properties in x-direction are used.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def multy(self) -> float:
        """Get or set the Scale factor scaling load curve given by LCIDY
        """ # nopep8
        return self._cards[0].get_value("multy")

    @multy.setter
    def multy(self, value: float) -> None:
        """Set the multy property."""
        self._cards[0].set_value("multy", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining thermal expansion coefficient in local z-direction as a function of temperature. If zero, the thermal  expansion coefficient in local z-direction given by constant MULTZ, if MULTZ=0 as well, the properties in x-direction are used.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def multz(self) -> float:
        """Get or set the Scale factor scaling load curve given by LCIDZ
        """ # nopep8
        return self._cards[0].get_value("multz")

    @multz.setter
    def multz(self, value: float) -> None:
        """Set the multz property."""
        self._cards[0].set_value("multz", value)

    @property
    def tref(self) -> float:
        """Get or set the Reference temperature. A nonzero value activates the secant approach, see Remarks below.
        """ # nopep8
        return self._cards[0].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[0].set_value("tref", value)

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
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

    @property
    def id_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given id."""
        return self._get_link_by_attr("PART", "pid", self.id, "parts")

