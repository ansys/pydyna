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

"""Module providing the MatAddSocExpansion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATADDSOCEXPANSION_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, 0),
    FieldSchema("mult", float, 20, 10, None),
    FieldSchema("lcidy", int, 30, 10, None),
    FieldSchema("multy", float, 40, 10, None),
    FieldSchema("lcidz", int, 50, 10, None),
    FieldSchema("multz", float, 60, 10, None),
)

_MATADDSOCEXPANSION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAddSocExpansion(KeywordBase):
    """DYNA MAT_ADD_SOC_EXPANSION keyword"""

    keyword = "MAT"
    subkeyword = "ADD_SOC_EXPANSION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "lcidy": LinkType.DEFINE_CURVE,
        "lcidz": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAddSocExpansion class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATADDSOCEXPANSION_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAddSocExpansion.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATADDSOCEXPANSION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for which the SOC expansion property applies
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> int:
        """Get or set the For isotropic material models, LCID is the load curve ID defining the SOC expansion coefficient as a function of state of charge. In this case, LCIDY, MULTY, LCIDZ, and MULTZ are ignored. For anisotropic material models, LCID and MULT define the SOC expansion coefficient in the local material a-direction. In either case, if LCID is zero, the SOC expansion coefficient is constant and equal to MULT
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def mult(self) -> typing.Optional[float]:
        """Get or set the Scale factor scaling load curve given by LCID
        """ # nopep8
        return self._cards[0].get_value("mult")

    @mult.setter
    def mult(self, value: float) -> None:
        """Set the mult property."""
        self._cards[0].set_value("mult", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the SOC expansion coefficient in the local material b-direction as a function of state of charge. If zero, the SOC expansion coefficient in the local material b-direction is constant and equal to MULTY. If MULTY=0 as well, LCID and MULT specify the SOC expansion coefficient in the local material b-direction
        """ # nopep8
        return self._cards[0].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        """Set the lcidy property."""
        self._cards[0].set_value("lcidy", value)

    @property
    def multy(self) -> typing.Optional[float]:
        """Get or set the Scale factor scaling load curve given by LCIDY
        """ # nopep8
        return self._cards[0].get_value("multy")

    @multy.setter
    def multy(self, value: float) -> None:
        """Set the multy property."""
        self._cards[0].set_value("multy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the SOC expansion coefficient in the local material c-direction as a function of state of charge. If zero, the SOC expansion coefficient in the local material c-direction is constant and equal to MULTZ. If MULTZ=0 as well, LCID and MULT specify the SOC expansion coefficient in the local material c-direction
        """ # nopep8
        return self._cards[0].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        """Set the lcidz property."""
        self._cards[0].set_value("lcidz", value)

    @property
    def multz(self) -> typing.Optional[float]:
        """Get or set the Scale factor scaling load curve given by LCIDZ
        """ # nopep8
        return self._cards[0].get_value("multz")

    @multz.setter
    def multz(self, value: float) -> None:
        """Set the multz property."""
        self._cards[0].set_value("multz", value)

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
    def lcid_link(self) -> DefineCurve:
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
    def lcidy_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidy:
                return kwd
        return None

    @lcidy_link.setter
    def lcidy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidy."""
        self.lcidy = value.lcid

    @property
    def lcidz_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidz."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidz:
                return kwd
        return None

    @lcidz_link.setter
    def lcidz_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidz."""
        self.lcidz = value.lcid

