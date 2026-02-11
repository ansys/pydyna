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

"""Module providing the LoadVibroAcoustic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADVIBROACOUSTIC_CARD0 = (
    FieldSchema("nmode", float, 0, 10, None),
    FieldSchema("texpos", float, 10, 10, 1.0),
    FieldSchema("tscale", float, 20, 10, 0.0),
    FieldSchema("temper", float, 30, 10, None),
    FieldSchema("dampro", float, 40, 10, None),
    FieldSchema("damptype", float, 50, 10, None),
    FieldSchema("spltype", float, 60, 10, None),
)

_LOADVIBROACOUSTIC_CARD1 = (
    FieldSchema("lddamp", int, 0, 10, None),
    FieldSchema("ldspl", int, 10, 10, None),
    FieldSchema("ldvel", int, 20, 10, None),
    FieldSchema("ldflw", int, 30, 10, None),
    FieldSchema("ldspn", int, 40, 10, None),
)

class LoadVibroAcoustic(KeywordBase):
    """DYNA LOAD_VIBRO_ACOUSTIC keyword"""

    keyword = "LOAD"
    subkeyword = "VIBRO_ACOUSTIC"
    _link_fields = {
        "lddamp": LinkType.DEFINE_CURVE,
        "ldspl": LinkType.DEFINE_CURVE,
        "ldvel": LinkType.DEFINE_CURVE,
        "ldflw": LinkType.DEFINE_CURVE,
        "ldspn": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadVibroAcoustic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADVIBROACOUSTIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADVIBROACOUSTIC_CARD1,
                **kwargs,
            ),        ]
    @property
    def nmode(self) -> typing.Optional[float]:
        """Get or set the Number of normal vibration modes employed for coupling with excitation pressure field
        """ # nopep8
        return self._cards[0].get_value("nmode")

    @nmode.setter
    def nmode(self, value: float) -> None:
        """Set the nmode property."""
        self._cards[0].set_value("nmode", value)

    @property
    def texpos(self) -> float:
        """Get or set the Esposure time
        """ # nopep8
        return self._cards[0].get_value("texpos")

    @texpos.setter
    def texpos(self, value: float) -> None:
        """Set the texpos property."""
        self._cards[0].set_value("texpos", value)

    @property
    def tscale(self) -> float:
        """Get or set the Time scale
        """ # nopep8
        return self._cards[0].get_value("tscale")

    @tscale.setter
    def tscale(self, value: float) -> None:
        """Set the tscale property."""
        self._cards[0].set_value("tscale", value)

    @property
    def temper(self) -> typing.Optional[float]:
        """Get or set the Temperature
        """ # nopep8
        return self._cards[0].get_value("temper")

    @temper.setter
    def temper(self, value: float) -> None:
        """Set the temper property."""
        self._cards[0].set_value("temper", value)

    @property
    def dampro(self) -> typing.Optional[float]:
        """Get or set the Damping ratio
        """ # nopep8
        return self._cards[0].get_value("dampro")

    @dampro.setter
    def dampro(self, value: float) -> None:
        """Set the dampro property."""
        self._cards[0].set_value("dampro", value)

    @property
    def damptype(self) -> typing.Optional[float]:
        """Get or set the Type of damping (=1, broadband; =2, modal damping)
        """ # nopep8
        return self._cards[0].get_value("damptype")

    @damptype.setter
    def damptype(self, value: float) -> None:
        """Set the damptype property."""
        self._cards[0].set_value("damptype", value)

    @property
    def spltype(self) -> typing.Optional[float]:
        """Get or set the Type of SPL input (=1, prs; =2, spl)
        """ # nopep8
        return self._cards[0].get_value("spltype")

    @spltype.setter
    def spltype(self, value: float) -> None:
        """Set the spltype property."""
        self._cards[0].set_value("spltype", value)

    @property
    def lddamp(self) -> typing.Optional[int]:
        """Get or set the Load curve for damping ratio (if non-constant)
        """ # nopep8
        return self._cards[1].get_value("lddamp")

    @lddamp.setter
    def lddamp(self, value: int) -> None:
        """Set the lddamp property."""
        self._cards[1].set_value("lddamp", value)

    @property
    def ldspl(self) -> typing.Optional[int]:
        """Get or set the Load curve for PSD or SPL value vs. frequency
        """ # nopep8
        return self._cards[1].get_value("ldspl")

    @ldspl.setter
    def ldspl(self, value: int) -> None:
        """Set the ldspl property."""
        self._cards[1].set_value("ldspl", value)

    @property
    def ldvel(self) -> typing.Optional[int]:
        """Get or set the Load curve for phase velocity
        """ # nopep8
        return self._cards[1].get_value("ldvel")

    @ldvel.setter
    def ldvel(self, value: int) -> None:
        """Set the ldvel property."""
        self._cards[1].set_value("ldvel", value)

    @property
    def ldflw(self) -> typing.Optional[int]:
        """Get or set the Load curve for exponential decay for TBL in flow-wise direction
        """ # nopep8
        return self._cards[1].get_value("ldflw")

    @ldflw.setter
    def ldflw(self, value: int) -> None:
        """Set the ldflw property."""
        self._cards[1].set_value("ldflw", value)

    @property
    def ldspn(self) -> typing.Optional[int]:
        """Get or set the Load curve for exponential decay for TBL in span-wise direction
        """ # nopep8
        return self._cards[1].get_value("ldspn")

    @ldspn.setter
    def ldspn(self, value: int) -> None:
        """Set the ldspn property."""
        self._cards[1].set_value("ldspn", value)

    @property
    def lddamp_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lddamp."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lddamp:
                return kwd
        return None

    @lddamp_link.setter
    def lddamp_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lddamp."""
        self.lddamp = value.lcid

    @property
    def ldspl_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ldspl."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ldspl:
                return kwd
        return None

    @ldspl_link.setter
    def ldspl_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ldspl."""
        self.ldspl = value.lcid

    @property
    def ldvel_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ldvel."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ldvel:
                return kwd
        return None

    @ldvel_link.setter
    def ldvel_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ldvel."""
        self.ldvel = value.lcid

    @property
    def ldflw_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ldflw."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ldflw:
                return kwd
        return None

    @ldflw_link.setter
    def ldflw_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ldflw."""
        self.ldflw = value.lcid

    @property
    def ldspn_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ldspn."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ldspn:
                return kwd
        return None

    @ldspn_link.setter
    def ldspn_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ldspn."""
        self.ldspn = value.lcid

