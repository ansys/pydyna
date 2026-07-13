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

"""Module providing the DualceseEnsightTimeseq class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DUALCESEENSIGHTTIMESEQ_CARD0 = (
    FieldSchema("tseqid", int, 0, 10, None),
    FieldSchema("tseqtype", str, 10, 10, "NODE"),
    FieldSchema("dt", int, 20, 10, None),
    FieldSchema("lcdt", int, 30, 10, None),
    FieldSchema("lcopt", int, 40, 10, None),
    FieldSchema("npltc", int, 50, 10, None),
    FieldSchema("tbeg", float, 60, 10, None),
    FieldSchema("tend", float, 70, 10, None),
)

_DUALCESEENSIGHTTIMESEQ_CARD1 = (
    FieldSchema("domid1", int, 0, 10, None),
    FieldSchema("domid2", int, 10, 10, None),
    FieldSchema("domid3", int, 20, 10, None),
    FieldSchema("domid4", int, 30, 10, None),
    FieldSchema("domid5", int, 40, 10, None),
    FieldSchema("domid6", int, 50, 10, None),
    FieldSchema("domid7", int, 60, 10, None),
    FieldSchema("domid8", int, 70, 10, None),
)

class DualceseEnsightTimeseq(KeywordBase):
    """DYNA DUALCESE_ENSIGHT_TIMESEQ keyword"""

    keyword = "DUALCESE"
    subkeyword = "ENSIGHT_TIMESEQ"
    _link_fields = {
        "lcdt": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DualceseEnsightTimeseq class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTTIMESEQ_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEENSIGHTTIMESEQ_CARD1,
                **kwargs,
            ),
        ]
    @property
    def tseqid(self) -> typing.Optional[int]:
        """Get or set the ID of this time sequence
        """ # nopep8
        return self._cards[0].get_value("tseqid")

    @tseqid.setter
    def tseqid(self, value: int) -> None:
        """Set the tseqid property."""
        self._cards[0].set_value("tseqid", value)

    @property
    def tseqtype(self) -> str:
        """Get or set the Type of time sequence. These are the geometric objects on which the data will be output.
        Q.ELEMENT:	The data is output for the elements in the domains.The DOMAIN_TYPE(see * DUALCESE_ENSIGHT_DOMAIN) for the domains must be ELEMENT or ELEMENT_SET.
        EQ.FSI : The data is output for the FSI nodes in the domains.The DOMAIN_TYPE(see * DUALCESE_ENSIGHT_DOMAIN) for the domains must be DUALCESE_FSI_INTERFACE.
        EQ.FSI_FACE : The data is output for the FSI faces in the domains.The DOMAIN_TYPE(see * DUALCESE_ENSIGHT_DOMAIN) for the domains must be DUALCESE_FSI_FACES.
        EQ.NODE : The data is output for the nodes in the domains, and the DOMAIN_TYPE(see * DUALCESE_ENSIGHT_DOMAIN) for the domains must be NODE or NODE_SET.This is the default.
        EQ.TIMEHIST : The data for the variables for each domain is output as a single value(or three values for vectors) for each output time.This time sequence is available for every value of DOMAIN_TYPE(see * DUALCESE_ENSIGHT_DOMAIN), but REDUCT must not be blank.
        """ # nopep8
        return self._cards[0].get_value("tseqtype")

    @tseqtype.setter
    def tseqtype(self, value: str) -> None:
        """Set the tseqtype property."""
        if value not in ["NODE", "ELEMENT", "FSI", "FSI_FACE", "TIMEHIST", None]:
            raise Exception("""tseqtype must be `None` or one of {"NODE","ELEMENT","FSI","FSI_FACE","TIMEHIST"}.""")
        self._cards[0].set_value("tseqtype", value)

    @property
    def dt(self) -> typing.Optional[int]:
        """Get or set the Time interval between outputs(ignored if LCDT or NPLTC is nonzero).
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: int) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def lcdt(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID specifying the time interval between dumps.
        """ # nopep8
        return self._cards[0].get_value("lcdt")

    @lcdt.setter
    def lcdt(self, value: int) -> None:
        """Set the lcdt property."""
        self._cards[0].set_value("lcdt", value)

    @property
    def lcopt(self) -> typing.Optional[int]:
        """Get or set the Flag to govern the behavior of the plot frequency load curve, LCDT:
        EQ.1:	At the time each plot is generated, the load curve value is added to the current time to determine the next plot time(this is the default behavior).
        EQ.2 : At the time each plot is generated, the next plot time T is computed so that T equals the current time plus the load curve value at the time T.
        EQ.3 : A plot is generated for each ordinate point in the load curve definition.The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[0].get_value("lcopt")

    @lcopt.setter
    def lcopt(self, value: int) -> None:
        """Set the lcopt property."""
        self._cards[0].set_value("lcopt", value)

    @property
    def npltc(self) -> typing.Optional[int]:
        """Get or set the DT = ENDTIM/NPLTC. ENDTIM is set on *CONTROL_TERMINATION. This calculated value of DT overrides DT specified in the first field.
        """ # nopep8
        return self._cards[0].get_value("npltc")

    @npltc.setter
    def npltc(self, value: int) -> None:
        """Set the npltc property."""
        self._cards[0].set_value("npltc", value)

    @property
    def tbeg(self) -> typing.Optional[float]:
        """Get or set the Problem time at which to begin writing output to this time sequence
        """ # nopep8
        return self._cards[0].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        """Set the tbeg property."""
        self._cards[0].set_value("tbeg", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the Problem time at which to terminate writing output to this time sequence
        """ # nopep8
        return self._cards[0].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[0].set_value("tend", value)

    @property
    def domid1(self) -> typing.Optional[int]:
        """Get or set the Domain ID specifying the domain over which variable output is to be performed in this time sequence. Each DOMID refers to the domain ID in an instance of *DUALCESE_ENSIGHT_?DOMAIN.
        """ # nopep8
        return self._cards[1].get_value("domid1")

    @domid1.setter
    def domid1(self, value: int) -> None:
        """Set the domid1 property."""
        self._cards[1].set_value("domid1", value)

    @property
    def domid2(self) -> typing.Optional[int]:
        """Get or set the Domain ID specifying the domain over which variable output is to be performed in this time sequence. Each DOMID refers to the domain ID in an instance of *DUALCESE_ENSIGHT_?DOMAIN.
        """ # nopep8
        return self._cards[1].get_value("domid2")

    @domid2.setter
    def domid2(self, value: int) -> None:
        """Set the domid2 property."""
        self._cards[1].set_value("domid2", value)

    @property
    def domid3(self) -> typing.Optional[int]:
        """Get or set the Domain ID specifying the domain over which variable output is to be performed in this time sequence. Each DOMID refers to the domain ID in an instance of *DUALCESE_ENSIGHT_?DOMAIN.
        """ # nopep8
        return self._cards[1].get_value("domid3")

    @domid3.setter
    def domid3(self, value: int) -> None:
        """Set the domid3 property."""
        self._cards[1].set_value("domid3", value)

    @property
    def domid4(self) -> typing.Optional[int]:
        """Get or set the Domain ID specifying the domain over which variable output is to be performed in this time sequence. Each DOMID refers to the domain ID in an instance of *DUALCESE_ENSIGHT_?DOMAIN.
        """ # nopep8
        return self._cards[1].get_value("domid4")

    @domid4.setter
    def domid4(self, value: int) -> None:
        """Set the domid4 property."""
        self._cards[1].set_value("domid4", value)

    @property
    def domid5(self) -> typing.Optional[int]:
        """Get or set the Domain ID specifying the domain over which variable output is to be performed in this time sequence. Each DOMID refers to the domain ID in an instance of *DUALCESE_ENSIGHT_?DOMAIN.
        """ # nopep8
        return self._cards[1].get_value("domid5")

    @domid5.setter
    def domid5(self, value: int) -> None:
        """Set the domid5 property."""
        self._cards[1].set_value("domid5", value)

    @property
    def domid6(self) -> typing.Optional[int]:
        """Get or set the Domain ID specifying the domain over which variable output is to be performed in this time sequence. Each DOMID refers to the domain ID in an instance of *DUALCESE_ENSIGHT_?DOMAIN.
        """ # nopep8
        return self._cards[1].get_value("domid6")

    @domid6.setter
    def domid6(self, value: int) -> None:
        """Set the domid6 property."""
        self._cards[1].set_value("domid6", value)

    @property
    def domid7(self) -> typing.Optional[int]:
        """Get or set the Domain ID specifying the domain over which variable output is to be performed in this time sequence. Each DOMID refers to the domain ID in an instance of *DUALCESE_ENSIGHT_?DOMAIN.
        """ # nopep8
        return self._cards[1].get_value("domid7")

    @domid7.setter
    def domid7(self, value: int) -> None:
        """Set the domid7 property."""
        self._cards[1].set_value("domid7", value)

    @property
    def domid8(self) -> typing.Optional[int]:
        """Get or set the Domain ID specifying the domain over which variable output is to be performed in this time sequence. Each DOMID refers to the domain ID in an instance of *DUALCESE_ENSIGHT_?DOMAIN.
        """ # nopep8
        return self._cards[1].get_value("domid8")

    @domid8.setter
    def domid8(self, value: int) -> None:
        """Set the domid8 property."""
        self._cards[1].set_value("domid8", value)

    @property
    def lcdt_link(self) -> typing.Optional[DefineCurve]:
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

