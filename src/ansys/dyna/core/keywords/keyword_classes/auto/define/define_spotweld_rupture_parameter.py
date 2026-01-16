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

"""Module providing the DefineSpotweldRuptureParameter class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DEFINESPOTWELDRUPTUREPARAMETER_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
)

_DEFINESPOTWELDRUPTUREPARAMETER_CARD1 = (
    FieldSchema("c11", float, 0, 10, None),
    FieldSchema("c12", float, 10, 10, None),
    FieldSchema("c13", float, 20, 10, None),
    FieldSchema("n11", float, 30, 10, None),
    FieldSchema("n12", float, 40, 10, None),
    FieldSchema("n13", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("sig_pf", float, 70, 10, None),
)

_DEFINESPOTWELDRUPTUREPARAMETER_CARD2 = (
    FieldSchema("c21", float, 0, 10, None),
    FieldSchema("c22", float, 10, 10, None),
    FieldSchema("c23", float, 20, 10, None),
    FieldSchema("n2", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("sig_nf", float, 70, 10, None),
)

_DEFINESPOTWELDRUPTUREPARAMETER_CARD3 = (
    FieldSchema("lcdpa", int, 0, 10, 0),
    FieldSchema("lcdpm", int, 10, 10, 0),
    FieldSchema("lcdps", int, 20, 10, 0),
    FieldSchema("lcdna", int, 30, 10, 0),
    FieldSchema("lcdnm", int, 40, 10, 0),
    FieldSchema("lcdns", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("nsmt", int, 70, 10, 0),
)

_DEFINESPOTWELDRUPTUREPARAMETER_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSpotweldRuptureParameter(KeywordBase):
    """DYNA DEFINE_SPOTWELD_RUPTURE_PARAMETER keyword"""

    keyword = "DEFINE"
    subkeyword = "SPOTWELD_RUPTURE_PARAMETER"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcdpa": LinkType.DEFINE_CURVE,
        "lcdpm": LinkType.DEFINE_CURVE,
        "lcdps": LinkType.DEFINE_CURVE,
        "lcdna": LinkType.DEFINE_CURVE,
        "lcdnm": LinkType.DEFINE_CURVE,
        "lcdns": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSpotweldRuptureParameter class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPOTWELDRUPTUREPARAMETER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESPOTWELDRUPTUREPARAMETER_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESPOTWELDRUPTUREPARAMETER_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESPOTWELDRUPTUREPARAMETER_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineSpotweldRuptureParameter.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPOTWELDRUPTUREPARAMETER_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for the attached shell..
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def c11(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[1].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        """Set the c11 property."""
        self._cards[1].set_value("c11", value)

    @property
    def c12(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[1].get_value("c12")

    @c12.setter
    def c12(self, value: float) -> None:
        """Set the c12 property."""
        self._cards[1].set_value("c12", value)

    @property
    def c13(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[1].get_value("c13")

    @c13.setter
    def c13(self, value: float) -> None:
        """Set the c13 property."""
        self._cards[1].set_value("c13", value)

    @property
    def n11(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[1].get_value("n11")

    @n11.setter
    def n11(self, value: float) -> None:
        """Set the n11 property."""
        self._cards[1].set_value("n11", value)

    @property
    def n12(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[1].get_value("n12")

    @n12.setter
    def n12(self, value: float) -> None:
        """Set the n12 property."""
        self._cards[1].set_value("n12", value)

    @property
    def n13(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[1].get_value("n13")

    @n13.setter
    def n13(self, value: float) -> None:
        """Set the n13 property."""
        self._cards[1].set_value("n13", value)

    @property
    def sig_pf(self) -> typing.Optional[float]:
        """Get or set the Nugget pull-out stress.
        """ # nopep8
        return self._cards[1].get_value("sig_pf")

    @sig_pf.setter
    def sig_pf(self, value: float) -> None:
        """Set the sig_pf property."""
        self._cards[1].set_value("sig_pf", value)

    @property
    def c21(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[2].get_value("c21")

    @c21.setter
    def c21(self, value: float) -> None:
        """Set the c21 property."""
        self._cards[2].set_value("c21", value)

    @property
    def c22(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[2].get_value("c22")

    @c22.setter
    def c22(self, value: float) -> None:
        """Set the c22 property."""
        self._cards[2].set_value("c22", value)

    @property
    def c23(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[2].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        """Set the c23 property."""
        self._cards[2].set_value("c23", value)

    @property
    def n2(self) -> typing.Optional[float]:
        """Get or set the Parameters for model.
        """ # nopep8
        return self._cards[2].get_value("n2")

    @n2.setter
    def n2(self, value: float) -> None:
        """Set the n2 property."""
        self._cards[2].set_value("n2", value)

    @property
    def sig_nf(self) -> typing.Optional[float]:
        """Get or set the Nugget fracture stress.
        """ # nopep8
        return self._cards[2].get_value("sig_nf")

    @sig_nf.setter
    def sig_nf(self, value: float) -> None:
        """Set the sig_nf property."""
        self._cards[2].set_value("sig_nf", value)

    @property
    def lcdpa(self) -> int:
        """Get or set the Curve ID defining dynamic scale factor of spot weld axial load rate for nugget pull-out mode.
        """ # nopep8
        return self._cards[3].get_value("lcdpa")

    @lcdpa.setter
    def lcdpa(self, value: int) -> None:
        """Set the lcdpa property."""
        self._cards[3].set_value("lcdpa", value)

    @property
    def lcdpm(self) -> int:
        """Get or set the Curve ID defining dynamic scale factor of spot weld moment
        load rate for nugget pull-out mode.
        """ # nopep8
        return self._cards[3].get_value("lcdpm")

    @lcdpm.setter
    def lcdpm(self, value: int) -> None:
        """Set the lcdpm property."""
        self._cards[3].set_value("lcdpm", value)

    @property
    def lcdps(self) -> int:
        """Get or set the Curve ID defining dynamic scale factor of spot weld shear load
        rate for nugget pull-out mode.
        """ # nopep8
        return self._cards[3].get_value("lcdps")

    @lcdps.setter
    def lcdps(self, value: int) -> None:
        """Set the lcdps property."""
        self._cards[3].set_value("lcdps", value)

    @property
    def lcdna(self) -> int:
        """Get or set the Curve ID defining dynamic scale factor of spot weld axial load
        rate for nugget fracture mode.
        """ # nopep8
        return self._cards[3].get_value("lcdna")

    @lcdna.setter
    def lcdna(self, value: int) -> None:
        """Set the lcdna property."""
        self._cards[3].set_value("lcdna", value)

    @property
    def lcdnm(self) -> int:
        """Get or set the Curve ID defining dynamic scale factor of spot weld moment
        load rate for nugget fracture mode.
        """ # nopep8
        return self._cards[3].get_value("lcdnm")

    @lcdnm.setter
    def lcdnm(self, value: int) -> None:
        """Set the lcdnm property."""
        self._cards[3].set_value("lcdnm", value)

    @property
    def lcdns(self) -> int:
        """Get or set the Curve ID defining dynamic scale factor of spot weld shear load
        rate for nugget fracture mode
        """ # nopep8
        return self._cards[3].get_value("lcdns")

    @lcdns.setter
    def lcdns(self, value: int) -> None:
        """Set the lcdns property."""
        self._cards[3].set_value("lcdns", value)

    @property
    def nsmt(self) -> int:
        """Get or set the The number of time steps used for averaging the resultant rates
        for the dynamic scale factors.
        """ # nopep8
        return self._cards[3].get_value("nsmt")

    @nsmt.setter
    def nsmt(self, value: int) -> None:
        """Set the nsmt property."""
        self._cards[3].set_value("nsmt", value)

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

    @property
    def lcdpa_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdpa."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdpa:
                return kwd
        return None

    @lcdpa_link.setter
    def lcdpa_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdpa."""
        self.lcdpa = value.lcid

    @property
    def lcdpm_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdpm."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdpm:
                return kwd
        return None

    @lcdpm_link.setter
    def lcdpm_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdpm."""
        self.lcdpm = value.lcid

    @property
    def lcdps_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdps."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdps:
                return kwd
        return None

    @lcdps_link.setter
    def lcdps_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdps."""
        self.lcdps = value.lcid

    @property
    def lcdna_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdna."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdna:
                return kwd
        return None

    @lcdna_link.setter
    def lcdna_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdna."""
        self.lcdna = value.lcid

    @property
    def lcdnm_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdnm."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdnm:
                return kwd
        return None

    @lcdnm_link.setter
    def lcdnm_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdnm."""
        self.lcdnm = value.lcid

    @property
    def lcdns_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdns."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdns:
                return kwd
        return None

    @lcdns_link.setter
    def lcdns_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdns."""
        self.lcdns = value.lcid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

