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

"""Module providing the MatCwm class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATCWM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("lcem", int, 20, 10, None),
    FieldSchema("lcpr", int, 30, 10, None),
    FieldSchema("lcsy", int, 40, 10, None),
    FieldSchema("lchr", int, 50, 10, None),
    FieldSchema("lcat", int, 60, 10, None),
    FieldSchema("beta", float, 70, 10, None),
)

_MATCWM_CARD1 = (
    FieldSchema("tastart", float, 0, 10, None),
    FieldSchema("taend", float, 10, 10, None),
    FieldSchema("tlstart", float, 20, 10, None),
    FieldSchema("tlend", float, 30, 10, None),
    FieldSchema("eghost", float, 40, 10, None),
    FieldSchema("pghost", float, 50, 10, None),
    FieldSchema("aghost", float, 60, 10, None),
)

_MATCWM_CARD2 = (
    FieldSchema("t2phase", float, 0, 10, None),
    FieldSchema("t1phase", float, 10, 10, None),
)

_MATCWM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatCwm(KeywordBase):
    """DYNA MAT_CWM keyword"""

    keyword = "MAT"
    subkeyword = "CWM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcem": LinkType.DEFINE_CURVE,
        "lcpr": LinkType.DEFINE_CURVE,
        "lcsy": LinkType.DEFINE_CURVE,
        "lchr": LinkType.DEFINE_CURVE,
        "lcat": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatCwm class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATCWM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCWM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCWM_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatCwm.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATCWM_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.
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
    def lcem(self) -> typing.Optional[int]:
        """Get or set the Load curve for Young's modulus as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lcem")

    @lcem.setter
    def lcem(self, value: int) -> None:
        """Set the lcem property."""
        self._cards[0].set_value("lcem", value)

    @property
    def lcpr(self) -> typing.Optional[int]:
        """Get or set the Load curve for Poisson's ratio as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lcpr")

    @lcpr.setter
    def lcpr(self, value: int) -> None:
        """Set the lcpr property."""
        self._cards[0].set_value("lcpr", value)

    @property
    def lcsy(self) -> typing.Optional[int]:
        """Get or set the Load curve for yield stress as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lcsy")

    @lcsy.setter
    def lcsy(self, value: int) -> None:
        """Set the lcsy property."""
        self._cards[0].set_value("lcsy", value)

    @property
    def lchr(self) -> typing.Optional[int]:
        """Get or set the Load curve for hardening modulus as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lchr")

    @lchr.setter
    def lchr(self, value: int) -> None:
        """Set the lchr property."""
        self._cards[0].set_value("lchr", value)

    @property
    def lcat(self) -> typing.Optional[int]:
        """Get or set the Load curve for thermal expansion coefficient as function of temperature.
        """ # nopep8
        return self._cards[0].get_value("lcat")

    @lcat.setter
    def lcat(self, value: int) -> None:
        """Set the lcat property."""
        self._cards[0].set_value("lcat", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Fraction isotropic hardening between 0 and 1
        EQ.0: Kinematic hardening
        EQ.1: Isotropic hardening.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def tastart(self) -> typing.Optional[float]:
        """Get or set the Annealing temperature start.
        """ # nopep8
        return self._cards[1].get_value("tastart")

    @tastart.setter
    def tastart(self, value: float) -> None:
        """Set the tastart property."""
        self._cards[1].set_value("tastart", value)

    @property
    def taend(self) -> typing.Optional[float]:
        """Get or set the Annealing temperature end.
        """ # nopep8
        return self._cards[1].get_value("taend")

    @taend.setter
    def taend(self, value: float) -> None:
        """Set the taend property."""
        self._cards[1].set_value("taend", value)

    @property
    def tlstart(self) -> typing.Optional[float]:
        """Get or set the Birth temperature start.
        """ # nopep8
        return self._cards[1].get_value("tlstart")

    @tlstart.setter
    def tlstart(self, value: float) -> None:
        """Set the tlstart property."""
        self._cards[1].set_value("tlstart", value)

    @property
    def tlend(self) -> typing.Optional[float]:
        """Get or set the Birth temperature end.
        """ # nopep8
        return self._cards[1].get_value("tlend")

    @tlend.setter
    def tlend(self, value: float) -> None:
        """Set the tlend property."""
        self._cards[1].set_value("tlend", value)

    @property
    def eghost(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for ghost (quiet) material.
        """ # nopep8
        return self._cards[1].get_value("eghost")

    @eghost.setter
    def eghost(self, value: float) -> None:
        """Set the eghost property."""
        self._cards[1].set_value("eghost", value)

    @property
    def pghost(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for ghost (quiet) material.
        """ # nopep8
        return self._cards[1].get_value("pghost")

    @pghost.setter
    def pghost(self, value: float) -> None:
        """Set the pghost property."""
        self._cards[1].set_value("pghost", value)

    @property
    def aghost(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient for ghost (quiet) material.
        """ # nopep8
        return self._cards[1].get_value("aghost")

    @aghost.setter
    def aghost(self, value: float) -> None:
        """Set the aghost property."""
        self._cards[1].set_value("aghost", value)

    @property
    def t2phase(self) -> typing.Optional[float]:
        """Get or set the Temperature at which phase change commences.
        """ # nopep8
        return self._cards[2].get_value("t2phase")

    @t2phase.setter
    def t2phase(self, value: float) -> None:
        """Set the t2phase property."""
        self._cards[2].set_value("t2phase", value)

    @property
    def t1phase(self) -> typing.Optional[float]:
        """Get or set the Temperature at which phase change ends.
        """ # nopep8
        return self._cards[2].get_value("t1phase")

    @t1phase.setter
    def t1phase(self, value: float) -> None:
        """Set the t1phase property."""
        self._cards[2].set_value("t1phase", value)

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
    def lcem_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcem."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcem:
                return kwd
        return None

    @lcem_link.setter
    def lcem_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcem."""
        self.lcem = value.lcid

    @property
    def lcpr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcpr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcpr:
                return kwd
        return None

    @lcpr_link.setter
    def lcpr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcpr."""
        self.lcpr = value.lcid

    @property
    def lcsy_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsy."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsy:
                return kwd
        return None

    @lcsy_link.setter
    def lcsy_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsy."""
        self.lcsy = value.lcid

    @property
    def lchr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lchr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lchr:
                return kwd
        return None

    @lchr_link.setter
    def lchr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lchr."""
        self.lchr = value.lcid

    @property
    def lcat_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcat."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcat:
                return kwd
        return None

    @lcat_link.setter
    def lcat_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcat."""
        self.lcat = value.lcid

