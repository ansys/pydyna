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

"""Module providing the AleStructuredPointSource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_ALESTRUCTUREDPOINTSOURCE_CARD0 = (
    FieldSchema("ammgnm", str, 0, 10, None),
    FieldSchema("lcvel", int, 10, 10, None),
    FieldSchema("lct", int, 20, 10, None),
    FieldSchema("norif", int, 30, 10, None),
)

_ALESTRUCTUREDPOINTSOURCE_CARD1 = (
    FieldSchema("lcm1", int, 0, 10, None),
    FieldSchema("lcm2", int, 10, 10, None),
    FieldSchema("lcm3", int, 20, 10, None),
    FieldSchema("lcm4", int, 30, 10, None),
    FieldSchema("lcm5", int, 40, 10, None),
    FieldSchema("lcm6", int, 50, 10, None),
    FieldSchema("lcm7", int, 60, 10, None),
    FieldSchema("lcm8", int, 70, 10, None),
)

_ALESTRUCTUREDPOINTSOURCE_CARD2 = (
    FieldSchema("nidi", int, 0, 10, None),
    FieldSchema("vdi", int, 10, 10, None),
    FieldSchema("ani", int, 20, 10, None),
)

class AleStructuredPointSource(KeywordBase):
    """DYNA ALE_STRUCTURED_POINT_SOURCE keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_POINT_SOURCE"
    _link_fields = {
        "nidi": LinkType.NODE,
        "lcvel": LinkType.DEFINE_CURVE,
        "lct": LinkType.DEFINE_CURVE,
        "lcm1": LinkType.DEFINE_CURVE,
        "lcm2": LinkType.DEFINE_CURVE,
        "lcm3": LinkType.DEFINE_CURVE,
        "lcm4": LinkType.DEFINE_CURVE,
        "lcm5": LinkType.DEFINE_CURVE,
        "lcm6": LinkType.DEFINE_CURVE,
        "lcm7": LinkType.DEFINE_CURVE,
        "lcm8": LinkType.DEFINE_CURVE,
        "vdi": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the AleStructuredPointSource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDPOINTSOURCE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDPOINTSOURCE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDPOINTSOURCE_CARD2,
                **kwargs,
            ),
        ]
    @property
    def ammgnm(self) -> typing.Optional[str]:
        """Get or set the ALE multi-material group (AMMG) name. This specifies the AMMG of the inlet gas. See *ALE_STRUCTURED_MULTI-MATERIAL_GROUP, specifically Remark Error! Reference source not found. of that keyword.
        """ # nopep8
        return self._cards[0].get_value("ammgnm")

    @ammgnm.setter
    def ammgnm(self, value: str) -> None:
        """Set the ammgnm property."""
        self._cards[0].set_value("ammgnm", value)

    @property
    def lcvel(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for a curve giving the gas inlet velocity as a function of time. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("lcvel")

    @lcvel.setter
    def lcvel(self, value: int) -> None:
        """Set the lcvel property."""
        self._cards[0].set_value("lcvel", value)

    @property
    def lct(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for a curve giving the gas inlet temperature as a function of time.
        """ # nopep8
        return self._cards[0].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        """Set the lct property."""
        self._cards[0].set_value("lct", value)

    @property
    def norif(self) -> typing.Optional[int]:
        """Get or set the Number of orifices.
        """ # nopep8
        return self._cards[0].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        """Set the norif property."""
        self._cards[0].set_value("norif", value)

    @property
    def lcm1(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcm1")

    @lcm1.setter
    def lcm1(self, value: int) -> None:
        """Set the lcm1 property."""
        self._cards[1].set_value("lcm1", value)

    @property
    def lcm2(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcm2")

    @lcm2.setter
    def lcm2(self, value: int) -> None:
        """Set the lcm2 property."""
        self._cards[1].set_value("lcm2", value)

    @property
    def lcm3(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcm3")

    @lcm3.setter
    def lcm3(self, value: int) -> None:
        """Set the lcm3 property."""
        self._cards[1].set_value("lcm3", value)

    @property
    def lcm4(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcm4")

    @lcm4.setter
    def lcm4(self, value: int) -> None:
        """Set the lcm4 property."""
        self._cards[1].set_value("lcm4", value)

    @property
    def lcm5(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcm5")

    @lcm5.setter
    def lcm5(self, value: int) -> None:
        """Set the lcm5 property."""
        self._cards[1].set_value("lcm5", value)

    @property
    def lcm6(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcm6")

    @lcm6.setter
    def lcm6(self, value: int) -> None:
        """Set the lcm6 property."""
        self._cards[1].set_value("lcm6", value)

    @property
    def lcm7(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcm7")

    @lcm7.setter
    def lcm7(self, value: int) -> None:
        """Set the lcm7 property."""
        self._cards[1].set_value("lcm7", value)

    @property
    def lcm8(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcm8")

    @lcm8.setter
    def lcm8(self, value: int) -> None:
        """Set the lcm8 property."""
        self._cards[1].set_value("lcm8", value)

    @property
    def nidi(self) -> typing.Optional[int]:
        """Get or set the The mass flow rate load curve ID of each gas component.  See Remark 2.
        """ # nopep8
        return self._cards[2].get_value("nidi")

    @nidi.setter
    def nidi(self, value: int) -> None:
        """Set the nidi property."""
        self._cards[2].set_value("nidi", value)

    @property
    def vdi(self) -> typing.Optional[int]:
        """Get or set the Vector ID defining of gas inflow direction at nozzle i.
        """ # nopep8
        return self._cards[2].get_value("vdi")

    @vdi.setter
    def vdi(self, value: int) -> None:
        """Set the vdi property."""
        self._cards[2].set_value("vdi", value)

    @property
    def ani(self) -> typing.Optional[int]:
        """Get or set the Area of nozzle i. See Remark 3.
        """ # nopep8
        return self._cards[2].get_value("ani")

    @ani.setter
    def ani(self, value: int) -> None:
        """Set the ani property."""
        self._cards[2].set_value("ani", value)

    @property
    def nidi_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nidi."""
        return self._get_link_by_attr("NODE", "nid", self.nidi, "parts")

    @property
    def lcvel_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcvel."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcvel:
                return kwd
        return None

    @lcvel_link.setter
    def lcvel_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcvel."""
        self.lcvel = value.lcid

    @property
    def lct_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lct."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lct:
                return kwd
        return None

    @lct_link.setter
    def lct_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lct."""
        self.lct = value.lcid

    @property
    def lcm1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcm1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcm1:
                return kwd
        return None

    @lcm1_link.setter
    def lcm1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcm1."""
        self.lcm1 = value.lcid

    @property
    def lcm2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcm2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcm2:
                return kwd
        return None

    @lcm2_link.setter
    def lcm2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcm2."""
        self.lcm2 = value.lcid

    @property
    def lcm3_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcm3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcm3:
                return kwd
        return None

    @lcm3_link.setter
    def lcm3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcm3."""
        self.lcm3 = value.lcid

    @property
    def lcm4_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcm4."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcm4:
                return kwd
        return None

    @lcm4_link.setter
    def lcm4_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcm4."""
        self.lcm4 = value.lcid

    @property
    def lcm5_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcm5."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcm5:
                return kwd
        return None

    @lcm5_link.setter
    def lcm5_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcm5."""
        self.lcm5 = value.lcid

    @property
    def lcm6_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcm6."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcm6:
                return kwd
        return None

    @lcm6_link.setter
    def lcm6_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcm6."""
        self.lcm6 = value.lcid

    @property
    def lcm7_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcm7."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcm7:
                return kwd
        return None

    @lcm7_link.setter
    def lcm7_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcm7."""
        self.lcm7 = value.lcid

    @property
    def lcm8_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcm8."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcm8:
                return kwd
        return None

    @lcm8_link.setter
    def lcm8_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcm8."""
        self.lcm8 = value.lcid

    @property
    def vdi_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vdi."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vdi:
                return kwd
        return None

    @vdi_link.setter
    def vdi_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vdi."""
        self.vdi = value.vid

