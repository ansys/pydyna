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

"""Module providing the EmBoundaryPrescribed class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMBOUNDARYPRESCRIBED_CARD0 = (
    FieldSchema("bpid", int, 0, 10, None),
    FieldSchema("bptype", int, 10, 10, 1),
    FieldSchema("settype", int, 20, 10, 1),
    FieldSchema("setid", int, 30, 10, None),
    FieldSchema("val", float, 40, 10, 0.0),
    FieldSchema("lcid", int, 50, 10, None),
)

_EMBOUNDARYPRESCRIBED_CARD1 = (
    FieldSchema("birtht", float, 0, 10, 0.0),
    FieldSchema("deatht", float, 10, 10, 1e+28),
)

class EmBoundaryPrescribed(KeywordBase):
    """DYNA EM_BOUNDARY_PRESCRIBED keyword"""

    keyword = "EM"
    subkeyword = "BOUNDARY_PRESCRIBED"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmBoundaryPrescribed class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMBOUNDARYPRESCRIBED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMBOUNDARYPRESCRIBED_CARD1,
                **kwargs,
            ),        ]
    @property
    def bpid(self) -> typing.Optional[int]:
        """Get or set the ID of the Prescribed boundary.
        .
        """ # nopep8
        return self._cards[0].get_value("bpid")

    @bpid.setter
    def bpid(self, value: int) -> None:
        """Set the bpid property."""
        self._cards[0].set_value("bpid", value)

    @property
    def bptype(self) -> int:
        """Get or set the Boundary Prescribed type:
        EQ.1:Short (Scalar Potential set to 0.)
        EQ.2:Prescribed Resistance (Robin B.C).
        EQ.3:Prescribed Scalar Potential (Dirichlet B.C)
        EQ.4:Prescribed Current Density (Neumann B.C).
        """ # nopep8
        return self._cards[0].get_value("bptype")

    @bptype.setter
    def bptype(self, value: int) -> None:
        """Set the bptype property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""bptype must be `None` or one of {1,2,3,4}.""")
        self._cards[0].set_value("bptype", value)

    @property
    def settype(self) -> int:
        """Get or set the Set type:
        EQ.1:Segment Set.
        EQ.2: Node Set.
        EQ.3: Fluid part. See *ICFD_PART.
        """ # nopep8
        return self._cards[0].get_value("settype")

    @settype.setter
    def settype(self, value: int) -> None:
        """Set the settype property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""settype must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("settype", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the set ID
        .
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[0].set_value("setid", value)

    @property
    def val(self) -> float:
        """Get or set the Value of the Resistance, current density or potential depending on BPTYPE.Ignored if LCID is defined
        .
        """ # nopep8
        return self._cards[0].get_value("val")

    @val.setter
    def val(self, value: float) -> None:
        """Set the val property."""
        self._cards[0].set_value("val", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the value of the resistance, voltage or current function of time
        .
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def birtht(self) -> float:
        """Get or set the Birth times for that prescribed boundary.

        """ # nopep8
        return self._cards[1].get_value("birtht")

    @birtht.setter
    def birtht(self, value: float) -> None:
        """Set the birtht property."""
        self._cards[1].set_value("birtht", value)

    @property
    def deatht(self) -> float:
        """Get or set the Death times for that prescribed boundary.

        """ # nopep8
        return self._cards[1].get_value("deatht")

    @deatht.setter
    def deatht(self, value: float) -> None:
        """Set the deatht property."""
        self._cards[1].set_value("deatht", value)

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

