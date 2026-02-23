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

"""Module providing the MatNonlinearElasticDiscreteBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATNONLINEARELASTICDISCRETEBEAM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("lcidtr", int, 20, 10, None),
    FieldSchema("lcidts", int, 30, 10, None),
    FieldSchema("lcidtt", int, 40, 10, None),
    FieldSchema("lcidrr", int, 50, 10, None),
    FieldSchema("lcidrs", int, 60, 10, None),
    FieldSchema("lcidrt", int, 70, 10, None),
)

_MATNONLINEARELASTICDISCRETEBEAM_CARD1 = (
    FieldSchema("lcidtdr", int, 0, 10, None),
    FieldSchema("lcidtds", int, 10, 10, None),
    FieldSchema("lcidtdt", int, 20, 10, None),
    FieldSchema("lcidrdr", int, 30, 10, None),
    FieldSchema("lcidrds", int, 40, 10, None),
    FieldSchema("lcidrdt", int, 50, 10, None),
)

_MATNONLINEARELASTICDISCRETEBEAM_CARD2 = (
    FieldSchema("for_", float, 0, 10, None, "for"),
    FieldSchema("fos", float, 10, 10, None),
    FieldSchema("fot", float, 20, 10, None),
    FieldSchema("mor", float, 30, 10, None),
    FieldSchema("mos", float, 40, 10, None),
    FieldSchema("mot", float, 50, 10, None),
)

_MATNONLINEARELASTICDISCRETEBEAM_CARD3 = (
    FieldSchema("ffailr", float, 0, 10, None),
    FieldSchema("ffails", float, 10, 10, None),
    FieldSchema("ffailt", float, 20, 10, None),
    FieldSchema("mfailr", float, 30, 10, None),
    FieldSchema("mfails", float, 40, 10, None),
    FieldSchema("mfailt", float, 50, 10, None),
)

_MATNONLINEARELASTICDISCRETEBEAM_CARD4 = (
    FieldSchema("ufailr", float, 0, 10, None),
    FieldSchema("ufails", float, 10, 10, None),
    FieldSchema("ufailt", float, 20, 10, None),
    FieldSchema("tfailr", float, 30, 10, None),
    FieldSchema("tfails", float, 40, 10, None),
    FieldSchema("tfailt", float, 50, 10, None),
)

_MATNONLINEARELASTICDISCRETEBEAM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatNonlinearElasticDiscreteBeam(KeywordBase):
    """DYNA MAT_NONLINEAR_ELASTIC_DISCRETE_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "NONLINEAR_ELASTIC_DISCRETE_BEAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcidtr": LinkType.DEFINE_CURVE,
        "lcidts": LinkType.DEFINE_CURVE,
        "lcidtt": LinkType.DEFINE_CURVE,
        "lcidrr": LinkType.DEFINE_CURVE,
        "lcidrs": LinkType.DEFINE_CURVE,
        "lcidrt": LinkType.DEFINE_CURVE,
        "lcidtdr": LinkType.DEFINE_CURVE,
        "lcidtds": LinkType.DEFINE_CURVE,
        "lcidtdt": LinkType.DEFINE_CURVE,
        "lcidrdr": LinkType.DEFINE_CURVE,
        "lcidrds": LinkType.DEFINE_CURVE,
        "lcidrdt": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatNonlinearElasticDiscreteBeam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATNONLINEARELASTICDISCRETEBEAM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARELASTICDISCRETEBEAM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARELASTICDISCRETEBEAM_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARELASTICDISCRETEBEAM_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATNONLINEARELASTICDISCRETEBEAM_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatNonlinearElasticDiscreteBeam.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATNONLINEARELASTICDISCRETEBEAM_OPTION0_CARD0,
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
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def lcidtr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local r-axis versus relative translational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidtr")

    @lcidtr.setter
    def lcidtr(self, value: int) -> None:
        """Set the lcidtr property."""
        self._cards[0].set_value("lcidtr", value)

    @property
    def lcidts(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local s-axis versus relative translational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidts")

    @lcidts.setter
    def lcidts(self, value: int) -> None:
        """Set the lcidts property."""
        self._cards[0].set_value("lcidts", value)

    @property
    def lcidtt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local t-axis versus relative translational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidtt")

    @lcidtt.setter
    def lcidtt(self, value: int) -> None:
        """Set the lcidtt property."""
        self._cards[0].set_value("lcidtt", value)

    @property
    def lcidrr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational moment resultant about local r-axis versus relative rotational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidrr")

    @lcidrr.setter
    def lcidrr(self, value: int) -> None:
        """Set the lcidrr property."""
        self._cards[0].set_value("lcidrr", value)

    @property
    def lcidrs(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational moment resultant about local s-axis versus relative rotational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidrs")

    @lcidrs.setter
    def lcidrs(self, value: int) -> None:
        """Set the lcidrs property."""
        self._cards[0].set_value("lcidrs", value)

    @property
    def lcidrt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational moment resultant about local t-axis versus relative rotational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidrt")

    @lcidrt.setter
    def lcidrt(self, value: int) -> None:
        """Set the lcidrt property."""
        self._cards[0].set_value("lcidrt", value)

    @property
    def lcidtdr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local r-axis versus relative translational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidtdr")

    @lcidtdr.setter
    def lcidtdr(self, value: int) -> None:
        """Set the lcidtdr property."""
        self._cards[1].set_value("lcidtdr", value)

    @property
    def lcidtds(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local s-axis versus relative translational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidtds")

    @lcidtds.setter
    def lcidtds(self, value: int) -> None:
        """Set the lcidtds property."""
        self._cards[1].set_value("lcidtds", value)

    @property
    def lcidtdt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local t-axis versus relative translational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidtdt")

    @lcidtdt.setter
    def lcidtdt(self, value: int) -> None:
        """Set the lcidtdt property."""
        self._cards[1].set_value("lcidtdt", value)

    @property
    def lcidrdr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local r-axis versus relative rotational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidrdr")

    @lcidrdr.setter
    def lcidrdr(self, value: int) -> None:
        """Set the lcidrdr property."""
        self._cards[1].set_value("lcidrdr", value)

    @property
    def lcidrds(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local s-axis versus relative rotational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidrds")

    @lcidrds.setter
    def lcidrds(self, value: int) -> None:
        """Set the lcidrds property."""
        self._cards[1].set_value("lcidrds", value)

    @property
    def lcidrdt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local t-axis versus relative rotational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidrdt")

    @lcidrdt.setter
    def lcidrdt(self, value: int) -> None:
        """Set the lcidrdt property."""
        self._cards[1].set_value("lcidrdt", value)

    @property
    def for_(self) -> typing.Optional[float]:
        """Get or set the Preload force in r-direction
        """ # nopep8
        return self._cards[2].get_value("for_")

    @for_.setter
    def for_(self, value: float) -> None:
        """Set the for_ property."""
        self._cards[2].set_value("for_", value)

    @property
    def fos(self) -> typing.Optional[float]:
        """Get or set the Preload force in s-direction
        """ # nopep8
        return self._cards[2].get_value("fos")

    @fos.setter
    def fos(self, value: float) -> None:
        """Set the fos property."""
        self._cards[2].set_value("fos", value)

    @property
    def fot(self) -> typing.Optional[float]:
        """Get or set the Preload force in t-direction
        """ # nopep8
        return self._cards[2].get_value("fot")

    @fot.setter
    def fot(self, value: float) -> None:
        """Set the fot property."""
        self._cards[2].set_value("fot", value)

    @property
    def mor(self) -> typing.Optional[float]:
        """Get or set the Preload moment about r-axis
        """ # nopep8
        return self._cards[2].get_value("mor")

    @mor.setter
    def mor(self, value: float) -> None:
        """Set the mor property."""
        self._cards[2].set_value("mor", value)

    @property
    def mos(self) -> typing.Optional[float]:
        """Get or set the Preload moment about s-axis
        """ # nopep8
        return self._cards[2].get_value("mos")

    @mos.setter
    def mos(self, value: float) -> None:
        """Set the mos property."""
        self._cards[2].set_value("mos", value)

    @property
    def mot(self) -> typing.Optional[float]:
        """Get or set the Preload moment about t-axis
        """ # nopep8
        return self._cards[2].get_value("mot")

    @mot.setter
    def mot(self, value: float) -> None:
        """Set the mot property."""
        self._cards[2].set_value("mot", value)

    @property
    def ffailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding force, Fr, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("ffailr")

    @ffailr.setter
    def ffailr(self, value: float) -> None:
        """Set the ffailr property."""
        self._cards[3].set_value("ffailr", value)

    @property
    def ffails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding force, Fs, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("ffails")

    @ffails.setter
    def ffails(self, value: float) -> None:
        """Set the ffails property."""
        self._cards[3].set_value("ffails", value)

    @property
    def ffailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding force, Ft, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("ffailt")

    @ffailt.setter
    def ffailt(self, value: float) -> None:
        """Set the ffailt property."""
        self._cards[3].set_value("ffailt", value)

    @property
    def mfailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding moment, Mr, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("mfailr")

    @mfailr.setter
    def mfailr(self, value: float) -> None:
        """Set the mfailr property."""
        self._cards[3].set_value("mfailr", value)

    @property
    def mfails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding moment, Ms, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("mfails")

    @mfails.setter
    def mfails(self, value: float) -> None:
        """Set the mfails property."""
        self._cards[3].set_value("mfails", value)

    @property
    def mfailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding moment, Mt, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("mfailt")

    @mfailt.setter
    def mfailt(self, value: float) -> None:
        """Set the mfailt property."""
        self._cards[3].set_value("mfailt", value)

    @property
    def ufailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding displacement, Ur, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("ufailr")

    @ufailr.setter
    def ufailr(self, value: float) -> None:
        """Set the ufailr property."""
        self._cards[4].set_value("ufailr", value)

    @property
    def ufails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding displacement, Us, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("ufails")

    @ufails.setter
    def ufails(self, value: float) -> None:
        """Set the ufails property."""
        self._cards[4].set_value("ufails", value)

    @property
    def ufailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding displacement, Ut, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("ufailt")

    @ufailt.setter
    def ufailt(self, value: float) -> None:
        """Set the ufailt property."""
        self._cards[4].set_value("ufailt", value)

    @property
    def tfailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding rotation, Qr, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("tfailr")

    @tfailr.setter
    def tfailr(self, value: float) -> None:
        """Set the tfailr property."""
        self._cards[4].set_value("tfailr", value)

    @property
    def tfails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding rotation, Qs, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("tfails")

    @tfails.setter
    def tfails(self, value: float) -> None:
        """Set the tfails property."""
        self._cards[4].set_value("tfails", value)

    @property
    def tfailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding rotation, Qt, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("tfailt")

    @tfailt.setter
    def tfailt(self, value: float) -> None:
        """Set the tfailt property."""
        self._cards[4].set_value("tfailt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcidtr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidtr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidtr:
                return kwd
        return None

    @lcidtr_link.setter
    def lcidtr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidtr."""
        self.lcidtr = value.lcid

    @property
    def lcidts_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidts."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidts:
                return kwd
        return None

    @lcidts_link.setter
    def lcidts_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidts."""
        self.lcidts = value.lcid

    @property
    def lcidtt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidtt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidtt:
                return kwd
        return None

    @lcidtt_link.setter
    def lcidtt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidtt."""
        self.lcidtt = value.lcid

    @property
    def lcidrr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidrr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidrr:
                return kwd
        return None

    @lcidrr_link.setter
    def lcidrr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidrr."""
        self.lcidrr = value.lcid

    @property
    def lcidrs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidrs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidrs:
                return kwd
        return None

    @lcidrs_link.setter
    def lcidrs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidrs."""
        self.lcidrs = value.lcid

    @property
    def lcidrt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidrt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidrt:
                return kwd
        return None

    @lcidrt_link.setter
    def lcidrt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidrt."""
        self.lcidrt = value.lcid

    @property
    def lcidtdr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidtdr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidtdr:
                return kwd
        return None

    @lcidtdr_link.setter
    def lcidtdr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidtdr."""
        self.lcidtdr = value.lcid

    @property
    def lcidtds_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidtds."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidtds:
                return kwd
        return None

    @lcidtds_link.setter
    def lcidtds_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidtds."""
        self.lcidtds = value.lcid

    @property
    def lcidtdt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidtdt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidtdt:
                return kwd
        return None

    @lcidtdt_link.setter
    def lcidtdt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidtdt."""
        self.lcidtdt = value.lcid

    @property
    def lcidrdr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidrdr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidrdr:
                return kwd
        return None

    @lcidrdr_link.setter
    def lcidrdr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidrdr."""
        self.lcidrdr = value.lcid

    @property
    def lcidrds_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidrds."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidrds:
                return kwd
        return None

    @lcidrds_link.setter
    def lcidrds_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidrds."""
        self.lcidrds = value.lcid

    @property
    def lcidrdt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidrdt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidrdt:
                return kwd
        return None

    @lcidrdt_link.setter
    def lcidrdt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidrdt."""
        self.lcidrdt = value.lcid

