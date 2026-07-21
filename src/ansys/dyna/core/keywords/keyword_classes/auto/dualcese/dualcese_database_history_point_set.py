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

"""Module providing the DualceseDatabaseHistoryPointSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_DUALCESEDATABASEHISTORYPOINTSET_CARD0 = (
    FieldSchema("pntsid", int, 0, 10, None),
    FieldSchema("dt", float, 10, 10, 0.0),
    FieldSchema("lcur", int, 20, 10, 0),
    FieldSchema("ioopt", int, 30, 10, 0),
)

class DualceseDatabaseHistoryPointSet(KeywordBase):
    """DYNA DUALCESE_DATABASE_HISTORY_POINT_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "DATABASE_HISTORY_POINT_SET"
    _link_fields = {
        "lcur": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the DualceseDatabaseHistoryPointSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEDATABASEHISTORYPOINTSET_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pntsid(self) -> typing.Optional[int]:
        """Get or set the ID of a dual CESE point set (see *DUALCESE_POINTSET).
        """ # nopep8
        return self._cards[0].get_value("pntsid")

    @pntsid.setter
    def pntsid(self, value: int) -> None:
        """Set the pntsid property."""
        self._cards[0].set_value("pntsid", value)

    @property
    def dt(self) -> float:
        """Get or set the Time interval between outputs. If DT is zero, no output is generated.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def lcur(self) -> int:
        """Get or set the Optional curve ID specifying the time interval between outputs. Use *DEFINE_CURVE to define the curve. The abscissa is time and the ordinate is time interval between outputs.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        """Set the lcur property."""
        self._cards[0].set_value("lcur", value)

    @property
    def ioopt(self) -> int:
        """Get or set the Flag to govern behavior of the output frequency load curve defined by LCUR:
        EQ.1: When output is generated at time t_n, the next output time  t_(n + 1) is computed as(2 / 2) t_(n + 1) = t_n + LCUR (t_n)  .This is the default behavior.
        EQ.2: When output is generated at time t_n, the next output time t_(n + 1) is computed as(2 / 2) t_(n + 1) = t_n + LCUR (t_(n + 1))  .
        EQ.3: Output is generated for each abscissa point in the load curve definition.The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[0].get_value("ioopt")

    @ioopt.setter
    def ioopt(self, value: int) -> None:
        """Set the ioopt property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""ioopt must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("ioopt", value)

    @property
    def lcur_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcur."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcur:
                return kwd
        return None

    @lcur_link.setter
    def lcur_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcur."""
        self.lcur = value.lcid

