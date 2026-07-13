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

"""Module providing the EmCircuitSource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMCIRCUITSOURCE_CARD0 = (
    FieldSchema("circid", int, 0, 10, None),
    FieldSchema("circtyp", int, 10, 10, 1),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("r_f", float, 30, 10, None, "r/f"),
    FieldSchema("t_a", float, 40, 10, None, "t/a"),
    FieldSchema("t0", float, 50, 10, 0.0),
)

_EMCIRCUITSOURCE_CARD1 = (
    FieldSchema("sidcurr", int, 0, 10, None),
    FieldSchema("sidvin", int, 10, 10, None),
    FieldSchema("sidvout", int, 20, 10, None),
    FieldSchema("partid", int, 30, 10, None),
    FieldSchema("ifreqst", int, 40, 10, 1),
)

class EmCircuitSource(KeywordBase):
    """DYNA EM_CIRCUIT_SOURCE keyword"""

    keyword = "EM"
    subkeyword = "CIRCUIT_SOURCE"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "sidcurr": LinkType.SET_SEGMENT,
        "sidvin": LinkType.SET_SEGMENT,
        "sidvout": LinkType.SET_SEGMENT,
        "partid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmCircuitSource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCIRCUITSOURCE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMCIRCUITSOURCE_CARD1,
                **kwargs,
            ),
        ]
    @property
    def circid(self) -> typing.Optional[int]:
        """Get or set the Circuit ID.
        """ # nopep8
        return self._cards[0].get_value("circid")

    @circid.setter
    def circid(self, value: int) -> None:
        """Set the circid property."""
        self._cards[0].set_value("circid", value)

    @property
    def circtyp(self) -> int:
        """Get or set the Circuit type:
        EQ.1: Imposed current as a function of time specified by a load curve.
        EQ.2: Imposed voltage as a function of times by a load curve.
        EQ.11: Imposed current defined by an amplitude A, frequency F and initial time t0: I = Asin[2*PI*F*(t-t0)].
        """ # nopep8
        return self._cards[0].get_value("circtyp")

    @circtyp.setter
    def circtyp(self, value: int) -> None:
        """Set the circtyp property."""
        if value not in [1, 2, 11, None]:
            raise Exception("""circtyp must be `None` or one of {1,2,11}.""")
        self._cards[0].set_value("circtyp", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve Id for CIRCTYP=1,2
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def r_f(self) -> typing.Optional[float]:
        """Get or set the Value of the frequency for CIRCTYP = 11.To have the frequency specified as a function of time with a load curve, a negative value can be entered with the absolute value corresponding to the load curve ID.
        Value of the resistance when using an imposed voltage (CIRCTYPE = 2).
        """ # nopep8
        return self._cards[0].get_value("r_f")

    @r_f.setter
    def r_f(self, value: float) -> None:
        """Set the r_f property."""
        self._cards[0].set_value("r_f", value)

    @property
    def t_a(self) -> typing.Optional[float]:
        """Get or set the Value of the amplitude for CIRCTYP = 11. To have the amplitude specified as a function of time with a load curve, a negative value can be entered with the absolute value corresponding to the load curve ID.
        Number of turns when using an imposed voltage (CIRCTYPE = 2).
        """ # nopep8
        return self._cards[0].get_value("t_a")

    @t_a.setter
    def t_a(self, value: float) -> None:
        """Set the t_a property."""
        self._cards[0].set_value("t_a", value)

    @property
    def t0(self) -> float:
        """Get or set the Value of the initial time, t_0, for CIRCTYP = 11.
        Phase time shift for the frequency domain solvers(EMSOL = 4, 7, and 9 * EM_CONTROL) when CIRCTYP = 1 or 2.
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[0].set_value("t0", value)

    @property
    def sidcurr(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for the current. It uses the orientation given by the normal of the segments. To use the opposite orientation, use a '- ' (minus) sign in front of the segment set ID or reorient the face normals.
        """ # nopep8
        return self._cards[1].get_value("sidcurr")

    @sidcurr.setter
    def sidcurr(self, value: int) -> None:
        """Set the sidcurr property."""
        self._cards[1].set_value("sidcurr", value)

    @property
    def sidvin(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for input voltage when CIRCTYP = 2 or for input current when CIRCTYP = 1 or 11. This field must be left blank if the stranded conductor is a torus shape.
        """ # nopep8
        return self._cards[1].get_value("sidvin")

    @sidvin.setter
    def sidvin(self, value: int) -> None:
        """Set the sidvin property."""
        self._cards[1].set_value("sidvin", value)

    @property
    def sidvout(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for output voltage when CIRCTYP = 2 or for output current when CIRCTYP = 1 or 11. This field must be left blank if the stranded conductor is a torus shape.
        """ # nopep8
        return self._cards[1].get_value("sidvout")

    @sidvout.setter
    def sidvout(self, value: int) -> None:
        """Set the sidvout property."""
        self._cards[1].set_value("sidvout", value)

    @property
    def partid(self) -> typing.Optional[int]:
        """Get or set the Part ID associated with the circuit.
        """ # nopep8
        return self._cards[1].get_value("partid")

    @partid.setter
    def partid(self, value: int) -> None:
        """Set the partid property."""
        self._cards[1].set_value("partid", value)

    @property
    def ifreqst(self) -> int:
        """Get or set the Frequency for recomputing the source terms. The source terms are recalculated every IFREQST time steps. By default, the source terms are recomputed every EM time step.
        LT.0: | IFREQST | is a load curve ID giving the frequency for recomputing as a function of time.
        """ # nopep8
        return self._cards[1].get_value("ifreqst")

    @ifreqst.setter
    def ifreqst(self, value: int) -> None:
        """Set the ifreqst property."""
        self._cards[1].set_value("ifreqst", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
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
    def sidcurr_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for sidcurr."""
        return self._get_set_link("SEGMENT", self.sidcurr)

    @sidcurr_link.setter
    def sidcurr_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for sidcurr."""
        self.sidcurr = value.sid

    @property
    def sidvin_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for sidvin."""
        return self._get_set_link("SEGMENT", self.sidvin)

    @sidvin_link.setter
    def sidvin_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for sidvin."""
        self.sidvin = value.sid

    @property
    def sidvout_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for sidvout."""
        return self._get_set_link("SEGMENT", self.sidvout)

    @sidvout_link.setter
    def sidvout_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for sidvout."""
        self.sidvout = value.sid

    @property
    def partid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given partid."""
        return self._get_link_by_attr("PART", "pid", self.partid, "parts")

