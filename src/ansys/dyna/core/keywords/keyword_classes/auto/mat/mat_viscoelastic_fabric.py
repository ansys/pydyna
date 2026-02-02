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

"""Module providing the MatViscoelasticFabric class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATVISCOELASTICFABRIC_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("bulk", float, 20, 10, None),
    FieldSchema("unused", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("cse", float, 60, 10, 0.0),
)

_MATVISCOELASTICFABRIC_CARD1 = (
    FieldSchema("lcid", int, 0, 10, None),
    FieldSchema("nt", int, 10, 10, None),
    FieldSchema("bstart", float, 20, 10, None),
    FieldSchema("tramp", float, 30, 10, None),
    FieldSchema("lcidk", int, 40, 10, None),
    FieldSchema("ntk", int, 50, 10, None),
    FieldSchema("bstartk", float, 60, 10, None),
    FieldSchema("trampk", float, 70, 10, None),
)

_MATVISCOELASTICFABRIC_CARD2 = (
    FieldSchema("gi", float, 0, 10, None),
    FieldSchema("betai", float, 10, 10, None),
    FieldSchema("ki", float, 20, 10, None),
    FieldSchema("betaki", float, 30, 10, None),
)

_MATVISCOELASTICFABRIC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatViscoelasticFabric(KeywordBase):
    """DYNA MAT_VISCOELASTIC_FABRIC keyword"""

    keyword = "MAT"
    subkeyword = "VISCOELASTIC_FABRIC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "lcidk": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatViscoelasticFabric class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATVISCOELASTICFABRIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATVISCOELASTICFABRIC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATVISCOELASTICFABRIC_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatViscoelasticFabric.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATVISCOELASTICFABRIC_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
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
        """Get or set the Elastic constant bulk modulus. If the bulk behavior is
        viscoelastic, then this modulus is used in determining the contact
        interface stiffness only.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        """Set the bulk property."""
        self._cards[0].set_value("bulk", value)

    @property
    def cse(self) -> float:
        """Get or set the Compressive stress flag (default = 0.0).
        EQ.0.0: don't eliminate compressive stresses
        EQ.1.0: eliminate compressive stresses.
        """ # nopep8
        return self._cards[0].get_value("cse")

    @cse.setter
    def cse(self, value: float) -> None:
        """Set the cse property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""cse must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("cse", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID if constants, Gi, and βi are determined via a least
        squares fit. This relaxation curve is shown below.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def nt(self) -> typing.Optional[int]:
        """Get or set the Number of terms in shear fit. If zero the default is 6. Currently,
        the maximum number is set to 6..
        """ # nopep8
        return self._cards[1].get_value("nt")

    @nt.setter
    def nt(self, value: int) -> None:
        """Set the nt property."""
        self._cards[1].set_value("nt", value)

    @property
    def bstart(self) -> typing.Optional[float]:
        """Get or set the In the fit, β1 is set to zero, β2 is set to BSTART, β3 is 10 times β2,
        β4 is 10 times β3 , and so on. If zero, BSTART = 0.01..
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
        """Get or set the Load curve ID for bulk behavior if constants, Ki, and βki are
        determined via a least squares fit. This relaxation curve is shown below..
        """ # nopep8
        return self._cards[1].get_value("lcidk")

    @lcidk.setter
    def lcidk(self, value: int) -> None:
        """Set the lcidk property."""
        self._cards[1].set_value("lcidk", value)

    @property
    def ntk(self) -> typing.Optional[int]:
        """Get or set the Number of terms desired in bulk fit. If zero the default is 6.
        Currently, the maximum number is set to 6.
        """ # nopep8
        return self._cards[1].get_value("ntk")

    @ntk.setter
    def ntk(self, value: int) -> None:
        """Set the ntk property."""
        self._cards[1].set_value("ntk", value)

    @property
    def bstartk(self) -> typing.Optional[float]:
        """Get or set the In the fit, βk1 is set to zero, βk2 is set to BSTARTK, βk3 is 10
        times βk2, βk4 is 10 times βk3 , and so on. If zero,
        BSTARTK = 0.01.
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
        """Get or set the Optional shear relaxation modulus for the ith term.
        """ # nopep8
        return self._cards[2].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        """Set the gi property."""
        self._cards[2].set_value("gi", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the Optional shear decay constant for the ith term.
        """ # nopep8
        return self._cards[2].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        """Set the betai property."""
        self._cards[2].set_value("betai", value)

    @property
    def ki(self) -> typing.Optional[float]:
        """Get or set the Optional bulk relaxation modulus for the ith term.
        """ # nopep8
        return self._cards[2].get_value("ki")

    @ki.setter
    def ki(self, value: float) -> None:
        """Set the ki property."""
        self._cards[2].set_value("ki", value)

    @property
    def betaki(self) -> typing.Optional[float]:
        """Get or set the Optional bulk decay constant for the ith term.
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

