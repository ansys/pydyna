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

"""Module providing the DatabaseIsphhtc class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DATABASEISPHHTC_CARD0 = (
    FieldSchema("dt", float, 0, 10, None),
    FieldSchema("binary", int, 10, 10, None),
    FieldSchema("lcdt", int, 20, 10, None),
    FieldSchema("ioopt", int, 30, 10, None),
    FieldSchema("pset", int, 40, 10, None),
)

class DatabaseIsphhtc(KeywordBase):
    """DYNA DATABASE_ISPHHTC keyword"""

    keyword = "DATABASE"
    subkeyword = "ISPHHTC"
    _link_fields = {
        "lcdt": LinkType.DEFINE_CURVE,
        "pset": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseIsphhtc class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASEISPHHTC_CARD0,
                **kwargs,
            ),
        ]
    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time interval between output states. If DT is zero, the output will be skipped even if LCDT is defined.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def binary(self) -> typing.Optional[int]:
        """Get or set the Currently unused. The output format is currently a profile file that can be directly imported inside Ansys Fluent or Ansys Mechanical
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        """Set the binary property."""
        self._cards[0].set_value("binary", value)

    @property
    def lcdt(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID specifying the output time interval as a function of time.
        """ # nopep8
        return self._cards[0].get_value("lcdt")

    @lcdt.setter
    def lcdt(self, value: int) -> None:
        """Set the lcdt property."""
        self._cards[0].set_value("lcdt", value)

    @property
    def ioopt(self) -> typing.Optional[int]:
        """Get or set the This input field governs how the plot state frequency is determined from curve LCDT:
        EQ.1: When a plot is generated at time t_n, the next plot time t_(n + 1) is computed as
        t_(n + 1) = t_n + LCDT(t_n)  .
        This is the default behavior.
        EQ.2: When a plot is generated at time t_n, the next plot time t_(n + 1) is computed as
        t_(n + 1) = t_n + LCDT(t_(n + 1))  .
        EQ.3: A plot is generated for each abscissa point in the load curve definition.The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[0].get_value("ioopt")

    @ioopt.setter
    def ioopt(self, value: int) -> None:
        """Set the ioopt property."""
        self._cards[0].set_value("ioopt", value)

    @property
    def pset(self) -> typing.Optional[int]:
        """Get or set the Optional part set. If defined, only parts contained in this part set will be included in the HTC output file.
        """ # nopep8
        return self._cards[0].get_value("pset")

    @pset.setter
    def pset(self, value: int) -> None:
        """Set the pset property."""
        self._cards[0].set_value("pset", value)

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

    @property
    def pset_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_PART_* keyword for pset."""
        return self._get_set_link("PART", self.pset)

    @pset_link.setter
    def pset_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for pset."""
        self.pset = value.sid

