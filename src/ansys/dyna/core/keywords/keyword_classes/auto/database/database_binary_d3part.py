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

"""Module providing the DatabaseBinaryD3Part class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DATABASEBINARYD3PART_CARD0 = (
    FieldSchema("dt", float, 0, 10, None),
    FieldSchema("lcdt", int, 10, 10, None),
    FieldSchema("beam", int, 20, 10, 0),
    FieldSchema("npltc", int, 30, 10, None),
    FieldSchema("psetid", int, 40, 10, None),
)

_DATABASEBINARYD3PART_CARD1 = (
    FieldSchema("hsetid", int, 0, 10, 0),
    FieldSchema("bsetid", int, 10, 10, 0),
    FieldSchema("ssetid", int, 20, 10, 0),
    FieldSchema("tsetid", int, 30, 10, 0),
)

class DatabaseBinaryD3Part(KeywordBase):
    """DYNA DATABASE_BINARY_D3PART keyword"""

    keyword = "DATABASE"
    subkeyword = "BINARY_D3PART"
    _link_fields = {
        "lcdt": LinkType.DEFINE_CURVE,
        "bsetid": LinkType.SET_BEAM,
        "psetid": LinkType.SET_PART,
        "hsetid": LinkType.SET_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseBinaryD3Part class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEBINARYD3PART_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DATABASEBINARYD3PART_CARD1,
                active_func=lambda: self._cards[1].has_nondefault_values(),
                **kwargs,
            ),        ]
    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the This field defines the time interval between output states, DT, for all options except D3DUMP, RUNRSF, and D3DRLF.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def lcdt(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID specifying the output time interval as a function of time. This variable is only available for options D3PLOT, D3PART, D3THDT, INTFOR and BLSTFOR.
        """ # nopep8
        return self._cards[0].get_value("lcdt")

    @lcdt.setter
    def lcdt(self, value: int) -> None:
        """Set the lcdt property."""
        self._cards[0].set_value("lcdt", value)

    @property
    def beam(self) -> int:
        """Get or set the Discrete element option flag (*DATABASE_‌BINARY_‌D3PLOT only):
        EQ.0:	Discrete spring and damper elements are added to the d3plot database where they are displayed as beam elements.The discrete elements’ global x, global y, global zand resultant forces(moments) and change in length(rotation) are written to the database where LS - PrePost(incorrectly) labels them as though they were beam quantities, such as axial force, S - shear resultant, T - shear resultant, etc.
        EQ.1 : No discrete spring, damperand seatbelt elements are added to the d3plot database.This option is useful when translating old LS - DYNA input decks to KEYWORD input.In older input decks there is no requirement that beam and spring elements have unique IDs,and beam elements may be created for the springand dampers with identical IDs to existing beam elements causing a fatal error.However, this option comes with some limitationsand, therefore, should be used with caution.
        Contact interfaces which are based on part IDs of seatbelt elements will not be properly generated if this option is used.
        DEFORMABLE_TO_RIGID will not work if PID refers to discrete, damper, or seatbelt elements.
        EQ.2 : Discrete spring and damper elements are added to the d3plot database where they are displayed as beam elements(similar to option 0).In this option the element resultant force is written to its first database position allowing beam axial forces and spring resultant forces to be plotted at the same time.This can be useful during some post - processing applications.
        This flag, set in* DATABASE_BINARY_D3PLOT, also affects the display of discrete elements in several other databases, such as d3drlfand d3part.
        """ # nopep8
        return self._cards[0].get_value("beam")

    @beam.setter
    def beam(self, value: int) -> None:
        """Set the beam property."""
        self._cards[0].set_value("beam", value)

    @property
    def npltc(self) -> typing.Optional[int]:
        """Get or set the DT=ENDTIM/NPLTC.  Applies to D3PLOT, D3PART, DEMFOR, and INTFOR options only.  This overrides the DT specified in the first field. ENDTIM is specified in *CONTROL_TERMINATION
        """ # nopep8
        return self._cards[0].get_value("npltc")

    @npltc.setter
    def npltc(self, value: int) -> None:
        """Set the npltc property."""
        self._cards[0].set_value("npltc", value)

    @property
    def psetid(self) -> typing.Optional[int]:
        """Get or set the Part set ID for D3PART and D3PLOT options only.  See *SET_‌PART.  Parts in PSETID will excluded in the d3plot database.  Only parts in PSETID are included in the d3part database.
        """ # nopep8
        return self._cards[0].get_value("psetid")

    @psetid.setter
    def psetid(self, value: int) -> None:
        """Set the psetid property."""
        self._cards[0].set_value("psetid", value)

    @property
    def hsetid(self) -> int:
        """Get or set the Set ID of a *SET_SOLID for which data will be output to d3part.
        """ # nopep8
        return self._cards[1].get_value("hsetid")

    @hsetid.setter
    def hsetid(self, value: int) -> None:
        """Set the hsetid property."""
        self._cards[1].set_value("hsetid", value)

    @property
    def bsetid(self) -> int:
        """Get or set the Set ID of a *SET_BEAM for which data will be output to d3part.
        """ # nopep8
        return self._cards[1].get_value("bsetid")

    @bsetid.setter
    def bsetid(self, value: int) -> None:
        """Set the bsetid property."""
        self._cards[1].set_value("bsetid", value)

    @property
    def ssetid(self) -> int:
        """Get or set the Set ID of *SET_SHELL for which data will be output to d3part.
        """ # nopep8
        return self._cards[1].get_value("ssetid")

    @ssetid.setter
    def ssetid(self, value: int) -> None:
        """Set the ssetid property."""
        self._cards[1].set_value("ssetid", value)

    @property
    def tsetid(self) -> int:
        """Get or set the Set ID of *SET_TSHELL for which data will be output to d3part.
        """ # nopep8
        return self._cards[1].get_value("tsetid")

    @tsetid.setter
    def tsetid(self, value: int) -> None:
        """Set the tsetid property."""
        self._cards[1].set_value("tsetid", value)

    @property
    def lcdt_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdt:
                return kwd
        return None

    @lcdt_link.setter
    def lcdt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdt."""
        self.lcdt = value.lcid

    @property
    def bsetid_link(self) -> KeywordBase:
        """Get the SET_BEAM_* keyword for bsetid."""
        return self._get_set_link("BEAM", self.bsetid)

    @bsetid_link.setter
    def bsetid_link(self, value: KeywordBase) -> None:
        """Set the SET_BEAM_* keyword for bsetid."""
        self.bsetid = value.sid

    @property
    def psetid_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psetid."""
        return self._get_set_link("PART", self.psetid)

    @psetid_link.setter
    def psetid_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psetid."""
        self.psetid = value.sid

    @property
    def hsetid_link(self) -> KeywordBase:
        """Get the SET_SOLID_* keyword for hsetid."""
        return self._get_set_link("SOLID", self.hsetid)

    @hsetid_link.setter
    def hsetid_link(self, value: KeywordBase) -> None:
        """Set the SET_SOLID_* keyword for hsetid."""
        self.hsetid = value.sid

