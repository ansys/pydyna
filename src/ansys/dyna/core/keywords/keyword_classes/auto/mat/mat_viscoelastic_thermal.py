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

"""Module providing the MatViscoelasticThermal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATVISCOELASTICTHERMAL_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("bulk", float, 20, 10, None),
    FieldSchema("pcf", float, 30, 10, None),
    FieldSchema("ef", float, 40, 10, None),
    FieldSchema("tref", float, 50, 10, None),
    FieldSchema("a", float, 60, 10, None),
    FieldSchema("b", float, 70, 10, None),
)

_MATVISCOELASTICTHERMAL_CARD1 = (
    FieldSchema("lcid", int, 0, 10, None),
    FieldSchema("nt", int, 10, 10, None),
    FieldSchema("bstart", float, 20, 10, None),
    FieldSchema("tramp", float, 30, 10, None),
    FieldSchema("lcidk", int, 40, 10, None),
    FieldSchema("ntk", int, 50, 10, None),
    FieldSchema("bstartk", float, 60, 10, None),
    FieldSchema("trampk", float, 70, 10, None),
)

_MATVISCOELASTICTHERMAL_CARD2 = (
    FieldSchema("gi", float, 0, 10, None),
    FieldSchema("betai", float, 10, 10, None),
    FieldSchema("ki", float, 20, 10, None),
    FieldSchema("betaki", float, 30, 10, None),
)

_MATVISCOELASTICTHERMAL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatViscoelasticThermal(KeywordBase):
    """DYNA MAT_VISCOELASTIC_THERMAL keyword"""

    keyword = "MAT"
    subkeyword = "VISCOELASTIC_THERMAL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "lcidk": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatViscoelasticThermal class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATVISCOELASTICTHERMAL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATVISCOELASTICTHERMAL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATVISCOELASTICTHERMAL_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatViscoelasticThermal.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATVISCOELASTICTHERMAL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.  A unique number or label must be specified.
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
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Elastic bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        """Set the bulk property."""
        self._cards[0].set_value("bulk", value)

    @property
    def pcf(self) -> typing.Optional[float]:
        """Get or set the Tensile pressure elimination flag for solid elements only.  If set to unity tensile pressures are set to zero.
        """ # nopep8
        return self._cards[0].get_value("pcf")

    @pcf.setter
    def pcf(self, value: float) -> None:
        """Set the pcf property."""
        self._cards[0].set_value("pcf", value)

    @property
    def ef(self) -> typing.Optional[float]:
        """Get or set the Elastic flag (if equal 1, the layer is elastic. If 0 the layer is viscoelastic).
        """ # nopep8
        return self._cards[0].get_value("ef")

    @ef.setter
    def ef(self, value: float) -> None:
        """Set the ef property."""
        self._cards[0].set_value("ef", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for shift function (must be greater than zero).
        """ # nopep8
        return self._cards[0].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[0].set_value("tref", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Coefficient for the Arrhenius and the Williams-Landau-Ferry shift functions..
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Coefficient for the Williams-Landau-Ferry shift function
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for deviatoric behavior if constants, Gi, and  i are determined via a least squares fit.  This relaxation curve is shown below
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def nt(self) -> typing.Optional[int]:
        """Get or set the Number of terms in shear fit.  If zero the default is 6.  Fewer than NT terms will be used if the fit produces one or more negative shear moduli.  Currently, the maximum number is set to 6.
        """ # nopep8
        return self._cards[1].get_value("nt")

    @nt.setter
    def nt(self, value: int) -> None:
        """Set the nt property."""
        self._cards[1].set_value("nt", value)

    @property
    def bstart(self) -> typing.Optional[float]:
        """Get or set the In the fit,  1  is set to zero,  2  is set to BSTART,  3  is 10 times  2,  4 is 100 times greater than  3 , and so on.  If zero, BSTART is determined by an iterative trial and error scheme.
        """ # nopep8
        return self._cards[1].get_value("bstart")

    @bstart.setter
    def bstart(self, value: float) -> None:
        """Set the bstart property."""
        self._cards[1].set_value("bstart", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for loading.
        """ # nopep8
        return self._cards[1].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        """Set the tramp property."""
        self._cards[1].set_value("tramp", value)

    @property
    def lcidk(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for bulk behavior if constants, Ki, and   i  are determined via a least squares fit.  This relaxation curve is shown below
        """ # nopep8
        return self._cards[1].get_value("lcidk")

    @lcidk.setter
    def lcidk(self, value: int) -> None:
        """Set the lcidk property."""
        self._cards[1].set_value("lcidk", value)

    @property
    def ntk(self) -> typing.Optional[int]:
        """Get or set the Number of terms desired in bulk fit.  If zero the default is 6.  Currently, the maximum number is set to 6.
        """ # nopep8
        return self._cards[1].get_value("ntk")

    @ntk.setter
    def ntk(self, value: int) -> None:
        """Set the ntk property."""
        self._cards[1].set_value("ntk", value)

    @property
    def bstartk(self) -> typing.Optional[float]:
        """Get or set the In the fit,   1  is set to zero,   2  is set to BSTARTK,   3  is 10 times   2,   4 is 100 times greater than   3 , and so on.  If zero, BSTARTK is determined by an iterative trial and error scheme.
        """ # nopep8
        return self._cards[1].get_value("bstartk")

    @bstartk.setter
    def bstartk(self, value: float) -> None:
        """Set the bstartk property."""
        self._cards[1].set_value("bstartk", value)

    @property
    def trampk(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for bulk loading.
        """ # nopep8
        return self._cards[1].get_value("trampk")

    @trampk.setter
    def trampk(self, value: float) -> None:
        """Set the trampk property."""
        self._cards[1].set_value("trampk", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Optional shear relaxation modulus for the ith term
        """ # nopep8
        return self._cards[2].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        """Set the gi property."""
        self._cards[2].set_value("gi", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the Optional shear decay constant for the ith term
        """ # nopep8
        return self._cards[2].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        """Set the betai property."""
        self._cards[2].set_value("betai", value)

    @property
    def ki(self) -> typing.Optional[float]:
        """Get or set the Optional bulk relaxation modulus for the ith term
        """ # nopep8
        return self._cards[2].get_value("ki")

    @ki.setter
    def ki(self, value: float) -> None:
        """Set the ki property."""
        self._cards[2].set_value("ki", value)

    @property
    def betaki(self) -> typing.Optional[float]:
        """Get or set the Optional bulk decay constant for the ith term
        """ # nopep8
        return self._cards[2].get_value("betaki")

    @betaki.setter
    def betaki(self, value: float) -> None:
        """Set the betaki property."""
        self._cards[2].set_value("betaki", value)

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
    def lcidk_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidk."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidk:
                return kwd
        return None

    @lcidk_link.setter
    def lcidk_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidk."""
        self.lcidk = value.lcid

