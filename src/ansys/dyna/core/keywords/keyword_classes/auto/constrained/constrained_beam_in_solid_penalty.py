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

"""Module providing the ConstrainedBeamInSolidPenalty class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONSTRAINEDBEAMINSOLIDPENALTY_CARD0 = (
    FieldSchema("coupid", int, 0, 10, None),
    FieldSchema("title", str, 10, 70, None),
)

_CONSTRAINEDBEAMINSOLIDPENALTY_CARD1 = (
    FieldSchema("bsid", int, 0, 10, None),
    FieldSchema("ssid", int, 10, 10, None),
    FieldSchema("bstyp", int, 20, 10, 0),
    FieldSchema("sstyp", int, 30, 10, 0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("ncoup_", int, 60, 10, None, "ncoup "),
    FieldSchema("cdir", int, 70, 10, None),
)

_CONSTRAINEDBEAMINSOLIDPENALTY_CARD2 = (
    FieldSchema("start", float, 0, 10, 0.0),
    FieldSchema("end", float, 10, 10, 1e+20),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("axfor_", int, 30, 10, None, "axfor "),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("pssf", float, 50, 10, 0.1),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("xint", float, 70, 10, 1e+16),
)

_CONSTRAINEDBEAMINSOLIDPENALTY_CARD3 = (
    FieldSchema("bondc", int, 0, 10, None),
    FieldSchema("barea", float, 10, 10, None),
    FieldSchema("fcm", float, 20, 10, None),
    FieldSchema("s1", float, 30, 10, None),
    FieldSchema("s2", float, 40, 10, None),
    FieldSchema("clear", float, 50, 10, None),
    FieldSchema("alpha", float, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

class ConstrainedBeamInSolidPenalty(KeywordBase):
    """DYNA CONSTRAINED_BEAM_IN_SOLID_PENALTY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "BEAM_IN_SOLID_PENALTY"

    def __init__(self, **kwargs):
        """Initialize the ConstrainedBeamInSolidPenalty class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDBEAMINSOLIDPENALTY_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDBEAMINSOLIDPENALTY_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDBEAMINSOLIDPENALTY_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDBEAMINSOLIDPENALTY_CARD3,
                **kwargs,
            ),
        ]
    @property
    def coupid(self) -> typing.Optional[int]:
        """Get or set the Coupling card ID number
        """ # nopep8
        return self._cards[0].get_value("coupid")

    @coupid.setter
    def coupid(self, value: int) -> None:
        """Set the coupid property."""
        self._cards[0].set_value("coupid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the A description of this coupling definition
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def bsid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID of the Lagrangian beam structure(see *PART, *SET_PART)
        """ # nopep8
        return self._cards[1].get_value("bsid")

    @bsid.setter
    def bsid(self, value: int) -> None:
        """Set the bsid property."""
        self._cards[1].set_value("bsid", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID of the Lagrangian solid elements or thick shell element(see *PART, *SET_PART)
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[1].set_value("ssid", value)

    @property
    def bstyp(self) -> int:
        """Get or set the Set type of BSID
        EQ.0: part set ID (PSID).
        EQ.1: part ID (PID).
        """ # nopep8
        return self._cards[1].get_value("bstyp")

    @bstyp.setter
    def bstyp(self, value: int) -> None:
        """Set the bstyp property."""
        if value not in [0, 1, None]:
            raise Exception("""bstyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("bstyp", value)

    @property
    def sstyp(self) -> int:
        """Get or set the Set type of SSID
        EQ.0: part set ID (PSID).
        EQ.1: part ID (PID).
        """ # nopep8
        return self._cards[1].get_value("sstyp")

    @sstyp.setter
    def sstyp(self, value: int) -> None:
        """Set the sstyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sstyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("sstyp", value)

    @property
    def ncoup_(self) -> typing.Optional[int]:
        """Get or set the Number of coupling points generated in one beam element. If set to 0, coupling only happens at beam nodes. Otherwise, coupling is done at both the beam nodes and those automatically generated coupling points.NCOUP > 0 works only in explicit analysis; it will be set to zero in implicit analysis.
        """ # nopep8
        return self._cards[1].get_value("ncoup_")

    @ncoup_.setter
    def ncoup_(self, value: int) -> None:
        """Set the ncoup_ property."""
        self._cards[1].set_value("ncoup_", value)

    @property
    def cdir(self) -> typing.Optional[int]:
        """Get or set the Coupling direction.
        EQ.0: default, constraint applied along all directions.
        EQ.1: Constraint only applied along normal directions; along the beam axial direction there is no constraint
        """ # nopep8
        return self._cards[1].get_value("cdir")

    @cdir.setter
    def cdir(self, value: int) -> None:
        """Set the cdir property."""
        self._cards[1].set_value("cdir", value)

    @property
    def start(self) -> float:
        """Get or set the Start time to activate the coupling
        LT.0: Start time is set to |START|.  When negative, start time is followed during the dynamic relaxation phase of the calculation.  After dynamic relaxation has completed, coupling is activated regardless of the value of END.EQ.0: Start time is inactive, meaning coupling is always active
        GT.0: If END = -9999, START is interpreted as the curve or table ID defining multiple pairs of start - time and end - time.Otherwise, if END > 0, the start time applies duringand after dynamic relaxation.
        """ # nopep8
        return self._cards[2].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        """Set the start property."""
        self._cards[2].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the End time to deactive the coupling
        LT.0: If END = -9999, START is interpreted as the curve or table ID defining multiple pairs of start-time and end-time.  Otherwise, negative END indicates that coupling is inactive during dynamic relaxation.  After dynamic relaxation the start and end times are followed and set to |START| and |END|, respectively.EQ.0: END defaults to 1020.
        GT.0: END sets the time at which the coupling is deactivated.
        """ # nopep8
        return self._cards[2].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        """Set the end property."""
        self._cards[2].set_value("end", value)

    @property
    def axfor_(self) -> typing.Optional[int]:
        """Get or set the Function that calculates the coupling force in the beam axial direction. Only available in constraint form with CDIR = 1.
        GE.0 and LT.900: OFF
        LT.0: |AXFOR| is the function ID in *DEFINE_FUNCTION
        EQ.999: General bond stress-slip relationship,which follows Bulletin 65 of fib Model Code for Concrete Structures 2010.
        GT.1000: Debonding law ID, lawid, in the user defined subroutine rebar_bondslip_get_force().
        """ # nopep8
        return self._cards[2].get_value("axfor_")

    @axfor_.setter
    def axfor_(self, value: int) -> None:
        """Set the axfor_ property."""
        self._cards[2].set_value("axfor_", value)

    @property
    def pssf(self) -> float:
        """Get or set the Penalty spring stiffness scale factor. Only available in penalty form.
        """ # nopep8
        return self._cards[2].get_value("pssf")

    @pssf.setter
    def pssf(self, value: float) -> None:
        """Set the pssf property."""
        self._cards[2].set_value("pssf", value)

    @property
    def xint(self) -> float:
        """Get or set the Interval distance. This field is designed to deal with beam elements with wide length variations.  Coupling points are generated at an interval of length equal to XINT.  Hence the number of coupling points in a beam element is no longer a fixed number (NCOUP) but instead variable, depending on the length of the beam element.This field can be used together with NCOUP.In that case, we will take the larger number of coupling points from these two options in each element
        """ # nopep8
        return self._cards[2].get_value("xint")

    @xint.setter
    def xint(self, value: float) -> None:
        """Set the xint property."""
        self._cards[2].set_value("xint", value)

    @property
    def bondc(self) -> typing.Optional[int]:
        """Get or set the Bond condition flag. BONDC consists of a two-digit integer, BONDC = [BA]:
        BONDC = B10 + A
        The 1s digit controls the transverse reinforcement:
        A.EQ.0: Unconfined
        A.EQ.1: Stirrups
        The 10s digit controls the bond condition:
        B.EQ.0: Good bond condition
        B.EQ.1: All other bond condition
        """ # nopep8
        return self._cards[3].get_value("bondc")

    @bondc.setter
    def bondc(self, value: int) -> None:
        """Set the bondc property."""
        self._cards[3].set_value("bondc", value)

    @property
    def barea(self) -> typing.Optional[float]:
        """Get or set the Beam area
        """ # nopep8
        return self._cards[3].get_value("barea")

    @barea.setter
    def barea(self, value: float) -> None:
        """Set the barea property."""
        self._cards[3].set_value("barea", value)

    @property
    def fcm(self) -> typing.Optional[float]:
        """Get or set the Concrete compression strength
        """ # nopep8
        return self._cards[3].get_value("fcm")

    @fcm.setter
    def fcm(self, value: float) -> None:
        """Set the fcm property."""
        self._cards[3].set_value("fcm", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the Slip at maximum stress (pull-out)
        """ # nopep8
        return self._cards[3].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        """Set the s1 property."""
        self._cards[3].set_value("s1", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the Slip atthe end of stress plateau (pull-out)
        """ # nopep8
        return self._cards[3].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        """Set the s2 property."""
        self._cards[3].set_value("s2", value)

    @property
    def clear(self) -> typing.Optional[float]:
        """Get or set the Clearance distance between ribs
        """ # nopep8
        return self._cards[3].get_value("clear")

    @clear.setter
    def clear(self, value: float) -> None:
        """Set the clear property."""
        self._cards[3].set_value("clear", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the , see Bulletin 65 of fib Model Code for Concrete Structures 2010
        """ # nopep8
        return self._cards[3].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[3].set_value("alpha", value)

