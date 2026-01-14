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

"""Module providing the ElementSeatbeltSlipring class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ELEMENTSEATBELTSLIPRING_CARD0 = (
    FieldSchema("sbsrid", int, 0, 10, 0),
    FieldSchema("sbid1", int, 10, 10, 0),
    FieldSchema("sbid2", int, 20, 10, 0),
    FieldSchema("fc", float, 30, 10, 0.0),
    FieldSchema("sbrnid", int, 40, 10, 0),
    FieldSchema("ltime", float, 50, 10, 1e+20),
    FieldSchema("fcs", float, 60, 10, 0.0),
    FieldSchema("onid", int, 70, 10, None),
)

_ELEMENTSEATBELTSLIPRING_CARD1 = (
    FieldSchema("k", float, 0, 10, None),
    FieldSchema("funcid_", int, 10, 10, None, "funcid "),
    FieldSchema("direct", int, 20, 10, None),
    FieldSchema("dc", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("lcnffd", int, 50, 10, 0),
    FieldSchema("lcnffs", int, 60, 10, 0),
)

class ElementSeatbeltSlipring(KeywordBase):
    """DYNA ELEMENT_SEATBELT_SLIPRING keyword"""

    keyword = "ELEMENT"
    subkeyword = "SEATBELT_SLIPRING"

    def __init__(self, **kwargs):
        """Initialize the ElementSeatbeltSlipring class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSEATBELTSLIPRING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSEATBELTSLIPRING_CARD1,
                **kwargs,
            ),        ]
    @property
    def sbsrid(self) -> int:
        """Get or set the Slipring ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("sbsrid")

    @sbsrid.setter
    def sbsrid(self, value: int) -> None:
        """Set the sbsrid property."""
        self._cards[0].set_value("sbsrid", value)

    @property
    def sbid1(self) -> int:
        """Get or set the Seat belt element 1 ID
        """ # nopep8
        return self._cards[0].get_value("sbid1")

    @sbid1.setter
    def sbid1(self, value: int) -> None:
        """Set the sbid1 property."""
        self._cards[0].set_value("sbid1", value)

    @property
    def sbid2(self) -> int:
        """Get or set the Seat belt element 2 ID
        """ # nopep8
        return self._cards[0].get_value("sbid2")

    @sbid2.setter
    def sbid2(self, value: int) -> None:
        """Set the sbid2 property."""
        self._cards[0].set_value("sbid2", value)

    @property
    def fc(self) -> float:
        """Get or set the Coulomb friction coefficient,<0 when |FC| refers to a curve which defines dynamic friction coefficient as a function of time
        """ # nopep8
        return self._cards[0].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        """Set the fc property."""
        self._cards[0].set_value("fc", value)

    @property
    def sbrnid(self) -> int:
        """Get or set the Slip ring node, NID
        """ # nopep8
        return self._cards[0].get_value("sbrnid")

    @sbrnid.setter
    def sbrnid(self, value: int) -> None:
        """Set the sbrnid property."""
        self._cards[0].set_value("sbrnid", value)

    @property
    def ltime(self) -> float:
        """Get or set the Slip ring lockup time. After this time no material is moved from one side of the slip ring to the other. This option is not active during dynamic relaxation.
        """ # nopep8
        return self._cards[0].get_value("ltime")

    @ltime.setter
    def ltime(self, value: float) -> None:
        """Set the ltime property."""
        self._cards[0].set_value("ltime", value)

    @property
    def fcs(self) -> float:
        """Get or set the Optional static Coulomb friction coefficient.; <0 when |FCS| refers to a curve which defines static friction coefficient as a function of time
        """ # nopep8
        return self._cards[0].get_value("fcs")

    @fcs.setter
    def fcs(self, value: float) -> None:
        """Set the fcs property."""
        self._cards[0].set_value("fcs", value)

    @property
    def onid(self) -> typing.Optional[int]:
        """Get or set the Optional orientation node ID used to define the skew angle, (see maunal Figure 0-1 and Figure 0-4).  If ONID undefined, the skew angle is assumed to be 0.0.
        """ # nopep8
        return self._cards[0].get_value("onid")

    @onid.setter
    def onid(self, value: int) -> None:
        """Set the onid property."""
        self._cards[0].set_value("onid", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Optional coefficient for determining the Coulomb friction coefficient related to angle alpha
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def funcid_(self) -> typing.Optional[int]:
        """Get or set the Function ID to determine friction coefficient
        """ # nopep8
        return self._cards[1].get_value("funcid_")

    @funcid_.setter
    def funcid_(self, value: int) -> None:
        """Set the funcid_ property."""
        self._cards[1].set_value("funcid_", value)

    @property
    def direct(self) -> typing.Optional[int]:
        """Get or set the DIRECT: = 12 if the belt is only allowed to slip along the direction from SBID1 to SBID2
        =21 if the belt is only allowed to slip along the direction from SBID2 to SBID1
        =0 if the belt can move along both directions.
        """ # nopep8
        return self._cards[1].get_value("direct")

    @direct.setter
    def direct(self, value: int) -> None:
        """Set the direct property."""
        self._cards[1].set_value("direct", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Optional decay constant to allow a smooth transition between the static and dynamic friction coefficients,
        """ # nopep8
        return self._cards[1].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[1].set_value("dc", value)

    @property
    def lcnffd(self) -> int:
        """Get or set the Optional curve for normal-force-dependent Coulomb dynamic friction
        coefficient. When defined, the dynamic friction coefficient becomes
        FC+fLCNFFD(Fn), where fLCNFFD(Fn) is the function value of LCNFFD when
        contact force equals Fn,The normal direction is defined as the average of the directions of attached elements SBID1 and SBID2.  The normal force, or contact force, F_n is the summation of T_1  and T_2, the  forces of attached elements,  along the normal direction
        """ # nopep8
        return self._cards[1].get_value("lcnffd")

    @lcnffd.setter
    def lcnffd(self, value: int) -> None:
        """Set the lcnffd property."""
        self._cards[1].set_value("lcnffd", value)

    @property
    def lcnffs(self) -> int:
        """Get or set the Optional curve for normal-force-dependent Coulomb static friction
        coefficient. When defined, the static friction coefficient becomes
        FCS+fLCNFFS(Fn), where fLCNFFS(Fn) is the function value of LCNFFS when
        contact force equals Fn,
        """ # nopep8
        return self._cards[1].get_value("lcnffs")

    @lcnffs.setter
    def lcnffs(self, value: int) -> None:
        """Set the lcnffs property."""
        self._cards[1].set_value("lcnffs", value)

