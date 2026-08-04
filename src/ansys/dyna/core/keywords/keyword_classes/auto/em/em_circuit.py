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

"""Module providing the EmCircuit class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMCIRCUIT_CARD0 = (
    FieldSchema("circid", int, 0, 10, None),
    FieldSchema("circtyp", int, 10, 10, 1),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("r_f", float, 30, 10, None, "r/f"),
    FieldSchema("l_a", float, 40, 10, None, "l/a"),
    FieldSchema("c_t0", float, 50, 10, None, "c/t0"),
    FieldSchema("v0", float, 60, 10, None),
    FieldSchema("t0", float, 70, 10, 0.0),
)

_EMCIRCUIT_CARD1 = (
    FieldSchema("sidcurr", int, 0, 10, None),
    FieldSchema("sidvin", int, 10, 10, None),
    FieldSchema("sidvout", int, 20, 10, None),
    FieldSchema("pid", int, 30, 10, None),
)

_EMCIRCUIT_CARD2 = (
    FieldSchema("vcut", float, 0, 10, 0.0),
    FieldSchema("r3", float, 10, 10, 0.0),
    FieldSchema("l3", float, 20, 10, 0.0),
    FieldSchema("r2", float, 30, 10, 0.0),
)

class EmCircuit(KeywordBase):
    """DYNA EM_CIRCUIT keyword"""

    keyword = "EM"
    subkeyword = "CIRCUIT"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "sidcurr": LinkType.SET_SEGMENT,
        "sidvin": LinkType.SET_SEGMENT,
        "sidvout": LinkType.SET_SEGMENT,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmCircuit class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCIRCUIT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMCIRCUIT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMCIRCUIT_CARD2,
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
        EQ.1: Imposed current as a function of time set by a load curve.
        EQ.2: Imposed voltage as a function of time specified by a load curve.
        EQ.3: R,L,C,V0 circuit.
        EQ.11: Imposed current defined by an amplitude A, frequency F and initial time t0: I = Asin[2*PI*F*(t-t0)].
        EQ.12: Imposed voltage defined by an amplitude A, frequency F and initial time t0: V = Asin[2*PI*F*(t-t0)].
        EQ.21: Imposed current defined by a load curve over one period and a frequency F.
        EQ.22: Imposed voltage defined by a load curve over one period and a frequency F.
        EQ.30: Modified version of R,L,C, V0 circuit that includes a diode. See Figure 0-1.
        """ # nopep8
        return self._cards[0].get_value("circtyp")

    @circtyp.setter
    def circtyp(self, value: int) -> None:
        """Set the circtyp property."""
        if value not in [1, 2, 3, 11, 12, 21, 22, 30, None]:
            raise Exception("""circtyp must be `None` or one of {1,2,3,11,12,21,22,30}.""")
        self._cards[0].set_value("circtyp", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve Id for CIRCTYP=1,2,21 and 22.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def r_f(self) -> typing.Optional[float]:
        """Get or set the Value of the circuit resistance for CIRCTYP = 3 and 30. For CIRCTYP = 30, this part of the circuit is labeled R1 in Figure 0-1.
        Value of the frequency for CIRCTYP = 11, 12, 21, and 22. For CIRCTYP = 11 and 12, to have the frequency specified as a function
        of time with a load curve, a negative value can be entered with the absolute value corresponding to the load curve ID.
        """ # nopep8
        return self._cards[0].get_value("r_f")

    @r_f.setter
    def r_f(self, value: float) -> None:
        """Set the r_f property."""
        self._cards[0].set_value("r_f", value)

    @property
    def l_a(self) -> typing.Optional[float]:
        """Get or set the Value of the circuit inductance for CIRCTYP = 3 and 30. For CIRCTYP = 30, this part of the circuit is labeled L1 in Figure 0-1.
        Value of the amplitude for CIRCTYP = 11 and 12. To have the amplitude specified as a function of time with a load curve, a negative
        value can be entered with the absolute value corresponding to the load curve ID.
        """ # nopep8
        return self._cards[0].get_value("l_a")

    @l_a.setter
    def l_a(self, value: float) -> None:
        """Set the l_a property."""
        self._cards[0].set_value("l_a", value)

    @property
    def c_t0(self) -> typing.Optional[float]:
        """Get or set the Value of the circuit capacity for CIRCTYP = 3 and 30. For CIRCTYP = 30, this part of the circuit is labeled C1 in Figure 0-1.
        Value of the initial time t_0 for CIRCTYP = 11 and 12. For the frequency domain solvers(EMSOL = 4 and 9 on * EM_CONTROL), T0 specifies the phase time shift for CIRCTYP = 1 and 2.
        """ # nopep8
        return self._cards[0].get_value("c_t0")

    @c_t0.setter
    def c_t0(self, value: float) -> None:
        """Set the c_t0 property."""
        self._cards[0].set_value("c_t0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Value of the circuit initial voltage for CIRCTYP = 3 and 30
        """ # nopep8
        return self._cards[0].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[0].set_value("v0", value)

    @property
    def t0(self) -> float:
        """Get or set the Starting time for CIRCTYP = 3. The default is the beginning of the run.
        """ # nopep8
        return self._cards[0].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[0].set_value("t0", value)

    @property
    def sidcurr(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for the current. It uses the orientation given by the normal of the segments. To use the opposite orientation, use a '- ' (minus) sign in front of the segment set ID.
        CIRCTYP.EQ.1 / 11 / 21: The current is imposed through this segment set.
        CIRCTYP.EQ.3: The current needed by the circuit equations is measured through this segment set.
        CIRCTYP.EQ.2 / 12 / 22: Optional segment set that the current is forced through.See Remark 2
        """ # nopep8
        return self._cards[1].get_value("sidcurr")

    @sidcurr.setter
    def sidcurr(self, value: int) -> None:
        """Set the sidcurr property."""
        self._cards[1].set_value("sidcurr", value)

    @property
    def sidvin(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for input voltage or input current when CIRCTYP.EQ.2/3/12/22 or CIRCTYP.EQ.1/11/21 respectively. It is considered to be oriented as going into the structural mesh, irrespective of the orientation of the segment.
        """ # nopep8
        return self._cards[1].get_value("sidvin")

    @sidvin.setter
    def sidvin(self, value: int) -> None:
        """Set the sidvin property."""
        self._cards[1].set_value("sidvin", value)

    @property
    def sidvout(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for output voltage or output current when CIRCTYP.EQ.2/3/12/22 or CIRCTYP.EQ.1/11/21 repecitively. It is considered to be oriented as going out of the structural mesh, irrespective of the orientation of the segment
        """ # nopep8
        return self._cards[1].get_value("sidvout")

    @sidvout.setter
    def sidvout(self, value: int) -> None:
        """Set the sidvout property."""
        self._cards[1].set_value("sidvout", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID associated with the circuit. It is only mandatory when including the second line of *EM_ROTATION_AXIS for the EM 2D axisymmetric solver.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def vcut(self) -> float:
        """Get or set the Cut off voltage for the diode, allowing the current to flow or not. Setting this field to a very large negative value
        effectively means that the diode circuit branch is never triggered, causing CIRCTYP = 30 to revert to an R, L, C circuit with behavior similar to CIRCTYP = 3. See Figure 0-1
        """ # nopep8
        return self._cards[2].get_value("vcut")

    @vcut.setter
    def vcut(self, value: float) -> None:
        """Set the vcut property."""
        self._cards[2].set_value("vcut", value)

    @property
    def r3(self) -> float:
        """Get or set the Optional circuit resistance and inductance values for the different branches for CIRCTYP = 30. See Figure 0-1.
        """ # nopep8
        return self._cards[2].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        """Set the r3 property."""
        self._cards[2].set_value("r3", value)

    @property
    def l3(self) -> float:
        """Get or set the Optional circuit resistance and inductance values for the different branches for CIRCTYP = 30. See Figure 0-1
        """ # nopep8
        return self._cards[2].get_value("l3")

    @l3.setter
    def l3(self, value: float) -> None:
        """Set the l3 property."""
        self._cards[2].set_value("l3", value)

    @property
    def r2(self) -> float:
        """Get or set the Optional circuit resistance and inductance values for the different branches for CIRCTYP = 30. See Figure 0-1.
        """ # nopep8
        return self._cards[2].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[2].set_value("r2", value)

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
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

