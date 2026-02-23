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

"""Module providing the DefineHazProperties class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DEFINEHAZPROPERTIES_CARD0 = (
    FieldSchema("id_haz", int, 0, 10, 0),
    FieldSchema("iop", int, 10, 10, 0),
    FieldSchema("pid", int, 20, 10, 0),
    FieldSchema("pid_typ", int, 30, 10, 0),
)

_DEFINEHAZPROPERTIES_CARD1 = (
    FieldSchema("iss", int, 0, 10, 0),
    FieldSchema("ifs", int, 10, 10, 0),
    FieldSchema("isb", int, 20, 10, 0),
    FieldSchema("ifb", int, 30, 10, 0),
    FieldSchema("isc", int, 40, 10, 0),
    FieldSchema("ifc", int, 50, 10, 0),
    FieldSchema("isw", int, 60, 10, 0),
    FieldSchema("ifw", int, 70, 10, 0),
)

_DEFINEHAZPROPERTIES_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineHazProperties(KeywordBase):
    """DYNA DEFINE_HAZ_PROPERTIES keyword"""

    keyword = "DEFINE"
    subkeyword = "HAZ_PROPERTIES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "iss": LinkType.DEFINE_CURVE,
        "ifs": LinkType.DEFINE_CURVE,
        "isb": LinkType.DEFINE_CURVE,
        "ifb": LinkType.DEFINE_CURVE,
        "isc": LinkType.DEFINE_CURVE,
        "ifc": LinkType.DEFINE_CURVE,
        "isw": LinkType.DEFINE_CURVE,
        "ifw": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineHazProperties class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEHAZPROPERTIES_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEHAZPROPERTIES_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineHazProperties.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEHAZPROPERTIES_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id_haz(self) -> int:
        """Get or set the Property set ID. A unique ID number must be used.
        """ # nopep8
        return self._cards[0].get_value("id_haz")

    @id_haz.setter
    def id_haz(self, value: int) -> None:
        """Set the id_haz property."""
        self._cards[0].set_value("id_haz", value)

    @property
    def iop(self) -> int:
        """Get or set the Activity flag. If IOP = 0, then the scaling is not applied, and if IOP = 1, the scaling is active.
        """ # nopep8
        return self._cards[0].get_value("iop")

    @iop.setter
    def iop(self, value: int) -> None:
        """Set the iop property."""
        if value not in [0, 1, None]:
            raise Exception("""iop must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iop", value)

    @property
    def pid(self) -> int:
        """Get or set the Part or part set ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def pid_typ(self) -> int:
        """Get or set the PID type. PID_TYP = 0 indicates that PID is a *PART ID, and PID_TYP = 1, a part set..
        """ # nopep8
        return self._cards[0].get_value("pid_typ")

    @pid_typ.setter
    def pid_typ(self, value: int) -> None:
        """Set the pid_typ property."""
        if value not in [0, 1, None]:
            raise Exception("""pid_typ must be `None` or one of {0,1}.""")
        self._cards[0].set_value("pid_typ", value)

    @property
    def iss(self) -> int:
        """Get or set the Curve ID for scaling the yield stress based on the distance to the closest solid element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("iss")

    @iss.setter
    def iss(self, value: int) -> None:
        """Set the iss property."""
        self._cards[1].set_value("iss", value)

    @property
    def ifs(self) -> int:
        """Get or set the Curve ID for scaling the failure strain based on the distance to the	closest solid element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("ifs")

    @ifs.setter
    def ifs(self, value: int) -> None:
        """Set the ifs property."""
        self._cards[1].set_value("ifs", value)

    @property
    def isb(self) -> int:
        """Get or set the Curve ID for scaling the yield stress based on the distance to the closest beam element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("isb")

    @isb.setter
    def isb(self, value: int) -> None:
        """Set the isb property."""
        self._cards[1].set_value("isb", value)

    @property
    def ifb(self) -> int:
        """Get or set the Curve ID for scaling the failure strain based on the distance to the closest beam element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("ifb")

    @ifb.setter
    def ifb(self, value: int) -> None:
        """Set the ifb property."""
        self._cards[1].set_value("ifb", value)

    @property
    def isc(self) -> int:
        """Get or set the Curve ID for scaling the yield stress based on the distance to the closest constrained spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("isc")

    @isc.setter
    def isc(self, value: int) -> None:
        """Set the isc property."""
        self._cards[1].set_value("isc", value)

    @property
    def ifc(self) -> int:
        """Get or set the Curve ID for scaling the failure strain based on the distance to the closest constrained spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("ifc")

    @ifc.setter
    def ifc(self, value: int) -> None:
        """Set the ifc property."""
        self._cards[1].set_value("ifc", value)

    @property
    def isw(self) -> int:
        """Get or set the Curve ID for scaling the yield stress based on the distance to the closest tailor welded blank node.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("isw")

    @isw.setter
    def isw(self, value: int) -> None:
        """Set the isw property."""
        self._cards[1].set_value("isw", value)

    @property
    def ifw(self) -> int:
        """Get or set the Curve ID for scaling the failure strain based on the distance to the tailor welded blank node.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
        """ # nopep8
        return self._cards[1].get_value("ifw")

    @ifw.setter
    def ifw(self, value: int) -> None:
        """Set the ifw property."""
        self._cards[1].set_value("ifw", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def iss_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for iss."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.iss:
                return kwd
        return None

    @iss_link.setter
    def iss_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for iss."""
        self.iss = value.lcid

    @property
    def ifs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ifs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ifs:
                return kwd
        return None

    @ifs_link.setter
    def ifs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ifs."""
        self.ifs = value.lcid

    @property
    def isb_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for isb."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.isb:
                return kwd
        return None

    @isb_link.setter
    def isb_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for isb."""
        self.isb = value.lcid

    @property
    def ifb_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ifb."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ifb:
                return kwd
        return None

    @ifb_link.setter
    def ifb_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ifb."""
        self.ifb = value.lcid

    @property
    def isc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for isc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.isc:
                return kwd
        return None

    @isc_link.setter
    def isc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for isc."""
        self.isc = value.lcid

    @property
    def ifc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ifc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ifc:
                return kwd
        return None

    @ifc_link.setter
    def ifc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ifc."""
        self.ifc = value.lcid

    @property
    def isw_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for isw."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.isw:
                return kwd
        return None

    @isw_link.setter
    def isw_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for isw."""
        self.isw = value.lcid

    @property
    def ifw_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for ifw."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ifw:
                return kwd
        return None

    @ifw_link.setter
    def ifw_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ifw."""
        self.ifw = value.lcid

