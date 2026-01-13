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

"""Module providing the LsoTimeSequence class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LSOTIMESEQUENCE_CARD0 = (
    FieldSchema("solver_name", str, 0, 20, "MECH"),
)

_LSOTIMESEQUENCE_CARD1 = (
    FieldSchema("dt", float, 0, 10, 0.0),
    FieldSchema("lcdt", int, 10, 10, 0),
    FieldSchema("lcopt", int, 20, 10, 1),
    FieldSchema("npltc", int, 30, 10, 0),
    FieldSchema("tbeg", float, 40, 10, 0.0),
    FieldSchema("tend", float, 50, 10, 0.0),
)

_LSOTIMESEQUENCE_CARD2 = (
    FieldSchema("domid1", int, 0, 10, None),
    FieldSchema("domid2", int, 10, 10, None),
    FieldSchema("domid3", int, 20, 10, None),
    FieldSchema("domid4", int, 30, 10, None),
    FieldSchema("domid5", int, 40, 10, None),
    FieldSchema("domid6", int, 50, 10, None),
    FieldSchema("domid7", int, 60, 10, None),
    FieldSchema("domid8", int, 70, 10, None),
)

_LSOTIMESEQUENCE_CARD3 = (
    FieldSchema("global_var", str, 0, 80, None),
)

class LsoTimeSequence(KeywordBase):
    """DYNA LSO_TIME_SEQUENCE keyword"""

    keyword = "LSO"
    subkeyword = "TIME_SEQUENCE"
    _link_fields = {
        "lcdt": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LsoTimeSequence class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LSOTIMESEQUENCE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LSOTIMESEQUENCE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LSOTIMESEQUENCE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LSOTIMESEQUENCE_CARD3,
                **kwargs,
            ),        ]
    @property
    def solver_name(self) -> str:
        """Get or set the Selects the solver from which data is output in this time sequence.
        """ # nopep8
        return self._cards[0].get_value("solver_name")

    @solver_name.setter
    def solver_name(self, value: str) -> None:
        """Set the solver_name property."""
        if value not in ["MECH", "EM", "CESE", "ICFD", None]:
            raise Exception("""solver_name must be `None` or one of {"MECH","EM","CESE","ICFD"}.""")
        self._cards[0].set_value("solver_name", value)

    @property
    def dt(self) -> float:
        """Get or set the Time interval between outputs.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[1].set_value("dt", value)

    @property
    def lcdt(self) -> int:
        """Get or set the Optional load curve ID specifying the time interval between dumps.
        """ # nopep8
        return self._cards[1].get_value("lcdt")

    @lcdt.setter
    def lcdt(self, value: int) -> None:
        """Set the lcdt property."""
        self._cards[1].set_value("lcdt", value)

    @property
    def lcopt(self) -> int:
        """Get or set the Flag to govern behavior of plot frequency load curve.
        = 1: At the time each plot is generated, the load curve value is added to the current time to determine the next plot time (this is the default behavior).
        = 2: At the time each plot is generated, the next plot time T is computed so that T = the current time plus the load curve value at the time T.
        =3: A plot is generated for each ordinate point in the load curve definition. The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[1].get_value("lcopt")

    @lcopt.setter
    def lcopt(self, value: int) -> None:
        """Set the lcopt property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""lcopt must be `None` or one of {1,2,3}.""")
        self._cards[1].set_value("lcopt", value)

    @property
    def npltc(self) -> int:
        """Get or set the DT=ENDTIM/NPLTC overrides the DT specified in the first field.
        """ # nopep8
        return self._cards[1].get_value("npltc")

    @npltc.setter
    def npltc(self, value: int) -> None:
        """Set the npltc property."""
        self._cards[1].set_value("npltc", value)

    @property
    def tbeg(self) -> float:
        """Get or set the The problem time at which to begin writing output to this time sequence.
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        """Set the tbeg property."""
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the The problem time at which to terminate writing output to this time sequence.
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[1].set_value("tend", value)

    @property
    def domid1(self) -> typing.Optional[int]:
        """Get or set the Output set ID defining the domain over which variable output is
        to be performed in this time sequence. Each DOMID refers to the
        domain identified in an *LSO_DOMAIN keyword card.
        """ # nopep8
        return self._cards[2].get_value("domid1")

    @domid1.setter
    def domid1(self, value: int) -> None:
        """Set the domid1 property."""
        self._cards[2].set_value("domid1", value)

    @property
    def domid2(self) -> typing.Optional[int]:
        """Get or set the Output set ID defining the domain over which variable output is
        to be performed in this time sequence. Each DOMID refers to the
        domain identified in an *LSO_DOMAIN keyword card.
        """ # nopep8
        return self._cards[2].get_value("domid2")

    @domid2.setter
    def domid2(self, value: int) -> None:
        """Set the domid2 property."""
        self._cards[2].set_value("domid2", value)

    @property
    def domid3(self) -> typing.Optional[int]:
        """Get or set the Output set ID defining the domain over which variable output is
        to be performed in this time sequence. Each DOMID refers to the
        domain identified in an *LSO_DOMAIN keyword card.
        """ # nopep8
        return self._cards[2].get_value("domid3")

    @domid3.setter
    def domid3(self, value: int) -> None:
        """Set the domid3 property."""
        self._cards[2].set_value("domid3", value)

    @property
    def domid4(self) -> typing.Optional[int]:
        """Get or set the Output set ID defining the domain over which variable output is
        to be performed in this time sequence. Each DOMID refers to the
        domain identified in an *LSO_DOMAIN keyword card.
        """ # nopep8
        return self._cards[2].get_value("domid4")

    @domid4.setter
    def domid4(self, value: int) -> None:
        """Set the domid4 property."""
        self._cards[2].set_value("domid4", value)

    @property
    def domid5(self) -> typing.Optional[int]:
        """Get or set the Output set ID defining the domain over which variable output is
        to be performed in this time sequence. Each DOMID refers to the
        domain identified in an *LSO_DOMAIN keyword card.
        """ # nopep8
        return self._cards[2].get_value("domid5")

    @domid5.setter
    def domid5(self, value: int) -> None:
        """Set the domid5 property."""
        self._cards[2].set_value("domid5", value)

    @property
    def domid6(self) -> typing.Optional[int]:
        """Get or set the Output set ID defining the domain over which variable output is
        to be performed in this time sequence. Each DOMID refers to the
        domain identified in an *LSO_DOMAIN keyword card.
        """ # nopep8
        return self._cards[2].get_value("domid6")

    @domid6.setter
    def domid6(self, value: int) -> None:
        """Set the domid6 property."""
        self._cards[2].set_value("domid6", value)

    @property
    def domid7(self) -> typing.Optional[int]:
        """Get or set the Output set ID defining the domain over which variable output is
        to be performed in this time sequence. Each DOMID refers to the
        domain identified in an *LSO_DOMAIN keyword card.
        """ # nopep8
        return self._cards[2].get_value("domid7")

    @domid7.setter
    def domid7(self, value: int) -> None:
        """Set the domid7 property."""
        self._cards[2].set_value("domid7", value)

    @property
    def domid8(self) -> typing.Optional[int]:
        """Get or set the Output set ID defining the domain over which variable output is
        to be performed in this time sequence. Each DOMID refers to the
        domain identified in an *LSO_DOMAIN keyword card.
        """ # nopep8
        return self._cards[2].get_value("domid8")

    @domid8.setter
    def domid8(self, value: int) -> None:
        """Set the domid8 property."""
        self._cards[2].set_value("domid8", value)

    @property
    def global_var(self) -> typing.Optional[str]:
        """Get or set the The name of a global output variable computed by SOLVER_NAME.
        This variable must have a single value (scalar, vector, or
        tensor), and therefore does not depend upon any DOMID. Any
        number of such variables may be specified with a given time
        sequence. These variables are listed as having  global  domain
        for SOLVER_NAME in a separate document. This document
        (LSO_VARIABLES.TXT) is created by running the command: LSDYNA
        print_lso_doc.
        """ # nopep8
        return self._cards[3].get_value("global_var")

    @global_var.setter
    def global_var(self, value: str) -> None:
        """Set the global_var property."""
        self._cards[3].set_value("global_var", value)

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

