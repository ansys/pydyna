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

"""Module providing the LoadThermalVariableElementBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADTHERMALVARIABLEELEMENTBEAM_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("ts", float, 10, 10, None),
    FieldSchema("tb", float, 20, 10, None),
    FieldSchema("lcid", int, 30, 10, None),
)

class LoadThermalVariableElementBeam(KeywordBase):
    """DYNA LOAD_THERMAL_VARIABLE_ELEMENT_BEAM keyword"""

    keyword = "LOAD"
    subkeyword = "THERMAL_VARIABLE_ELEMENT_BEAM"
    _link_fields = {
        "eid": LinkType.ELEMENT_BEAM,
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadThermalVariableElementBeam class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADTHERMALVARIABLEELEMENTBEAM_CARD0,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def ts(self) -> typing.Optional[float]:
        """Get or set the Scaled temperature
        """ # nopep8
        return self._cards[0].get_value("ts")

    @ts.setter
    def ts(self, value: float) -> None:
        """Set the ts property."""
        self._cards[0].set_value("ts", value)

    @property
    def tb(self) -> typing.Optional[float]:
        """Get or set the Base temperature
        """ # nopep8
        return self._cards[0].get_value("tb")

    @tb.setter
    def tb(self, value: float) -> None:
        """Set the tb property."""
        self._cards[0].set_value("tb", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining a scale factor that multiplies the scaled temperature	as a function of time, (see *DEFINE_CURVE).
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def eid_link(self) -> KeywordBase:
        """Get the ELEMENT keyword containing the given eid."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid, "parts")

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

