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

"""Module providing the LoadThermalVariableShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADTHERMALVARIABLESHELL_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("eid", int, 10, 10, None),
)

_LOADTHERMALVARIABLESHELL_CARD1 = (
    FieldSchema("tbase", float, 0, 10, None),
    FieldSchema("tscale", float, 10, 10, None),
    FieldSchema("tcurve", int, 20, 10, None),
    FieldSchema("tcurdr", int, 30, 10, None),
    FieldSchema("zco", float, 40, 10, None),
)

class LoadThermalVariableShell(KeywordBase):
    """DYNA LOAD_THERMAL_VARIABLE_SHELL keyword"""

    keyword = "LOAD"
    subkeyword = "THERMAL_VARIABLE_SHELL"
    _link_fields = {
        "eid": LinkType.ELEMENT_SHELL,
        "tcurve": LinkType.DEFINE_CURVE,
        "tcurdr": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadThermalVariableShell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADTHERMALVARIABLESHELL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADTHERMALVARIABLESHELL_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the load ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Shell ID
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def tbase(self) -> typing.Optional[float]:
        """Get or set the Base temperature
        """ # nopep8
        return self._cards[1].get_value("tbase")

    @tbase.setter
    def tbase(self, value: float) -> None:
        """Set the tbase property."""
        self._cards[1].set_value("tbase", value)

    @property
    def tscale(self) -> typing.Optional[float]:
        """Get or set the Scale factor on temperature from load curve
        """ # nopep8
        return self._cards[1].get_value("tscale")

    @tscale.setter
    def tscale(self, value: float) -> None:
        """Set the tscale property."""
        self._cards[1].set_value("tscale", value)

    @property
    def tcurve(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for temperature vs time
        """ # nopep8
        return self._cards[1].get_value("tcurve")

    @tcurve.setter
    def tcurve(self, value: int) -> None:
        """Set the tcurve property."""
        self._cards[1].set_value("tcurve", value)

    @property
    def tcurdr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID used during dynamic relaxation
        """ # nopep8
        return self._cards[1].get_value("tcurdr")

    @tcurdr.setter
    def tcurdr(self, value: int) -> None:
        """Set the tcurdr property."""
        self._cards[1].set_value("tcurdr", value)

    @property
    def zco(self) -> typing.Optional[float]:
        """Get or set the Relative coordinate through-thickness (-1.0 to +1.0)
        """ # nopep8
        return self._cards[1].get_value("zco")

    @zco.setter
    def zco(self, value: float) -> None:
        """Set the zco property."""
        self._cards[1].set_value("zco", value)

    @property
    def eid_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid, "parts")

    @property
    def tcurve_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for tcurve."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.tcurve:
                return kwd
        return None

    @tcurve_link.setter
    def tcurve_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for tcurve."""
        self.tcurve = value.lcid

    @property
    def tcurdr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for tcurdr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.tcurdr:
                return kwd
        return None

    @tcurdr_link.setter
    def tcurdr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for tcurdr."""
        self.tcurdr = value.lcid

