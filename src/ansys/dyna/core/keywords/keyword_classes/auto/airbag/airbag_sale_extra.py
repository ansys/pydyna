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

"""Module providing the AirbagSaleExtra class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_AIRBAGSALEEXTRA_CARD0 = (
    FieldSchema("sid1", int, 0, 10, None),
    FieldSchema("stype1", int, 10, 10, 0),
    FieldSchema("sid2", int, 20, 10, None),
    FieldSchema("stype2", int, 30, 10, 0),
)

_AIRBAGSALEEXTRA_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unit", int, 10, 10, 0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("tatm", float, 30, 10, 293.0),
    FieldSchema("patm", float, 40, 10, None),
    FieldSchema("nvent", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("tsw", float, 70, 10, 10000000000.0),
)

_AIRBAGSALEEXTRA_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("ngas", int, 10, 10, None),
    FieldSchema("norif", int, 20, 10, None),
)

_AIRBAGSALEEXTRA_CARD3 = (
    FieldSchema("sid1", int, 0, 10, None),
    FieldSchema("stype1", int, 10, 10, 0),
    FieldSchema("c23", float, 20, 10, 1.0),
)

_AIRBAGSALEEXTRA_CARD4 = (
    FieldSchema("pair", float, 0, 10, None),
    FieldSchema("tair", float, 10, 10, None),
    FieldSchema("xmair", float, 20, 10, None),
    FieldSchema("aair", float, 30, 10, None),
    FieldSchema("bair", float, 40, 10, None),
    FieldSchema("cair", float, 50, 10, None),
)

_AIRBAGSALEEXTRA_CARD5 = (
    FieldSchema("lcmi", int, 0, 10, None),
    FieldSchema("lcti", int, 10, 10, None),
    FieldSchema("xmi", float, 20, 10, None),
    FieldSchema("ai", float, 30, 10, None),
    FieldSchema("bi", float, 40, 10, None),
    FieldSchema("ci", float, 50, 10, None),
)

_AIRBAGSALEEXTRA_CARD6 = (
    FieldSchema("iflip", int, 0, 10, None),
    FieldSchema("lcvel", int, 10, 10, None),
    FieldSchema("pmax", float, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("ilock", int, 50, 10, None),
    FieldSchema("ilvl", int, 60, 10, None),
    FieldSchema("decay", float, 70, 10, None),
)

class AirbagSaleExtra(KeywordBase):
    """DYNA AIRBAG_SALE_EXTRA keyword"""

    keyword = "AIRBAG"
    subkeyword = "SALE_EXTRA"

    def __init__(self, **kwargs):
        """Initialize the AirbagSaleExtra class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGSALEEXTRA_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGSALEEXTRA_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGSALEEXTRA_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGSALEEXTRA_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGSALEEXTRA_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGSALEEXTRA_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _AIRBAGSALEEXTRA_CARD6,
                **kwargs,
            ),
        ]
    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining the complete airbag.
        """ # nopep8
        return self._cards[0].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        """Set the sid1 property."""
        self._cards[0].set_value("sid1", value)

    @property
    def stype1(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set
        """ # nopep8
        return self._cards[0].get_value("stype1")

    @stype1.setter
    def stype1(self, value: int) -> None:
        """Set the stype1 property."""
        if value not in [0, 1, None]:
            raise Exception("""stype1 must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype1", value)

    @property
    def sid2(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining the internal parts of the airbag.
        """ # nopep8
        return self._cards[0].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        """Set the sid2 property."""
        self._cards[0].set_value("sid2", value)

    @property
    def stype2(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set
        """ # nopep8
        return self._cards[0].get_value("stype2")

    @stype2.setter
    def stype2(self, value: int) -> None:
        """Set the stype2 property."""
        if value not in [0, 1, None]:
            raise Exception("""stype2 must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype2", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit system:
        EQ.0: Kg - mm - ms - K
        EQ.1: SI
        EQ.2: tonne - mm - s - K
        """ # nopep8
        return self._cards[1].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        """Set the unit property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""unit must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("unit", value)

    @property
    def tatm(self) -> float:
        """Get or set the Atmospheric temperature
        """ # nopep8
        return self._cards[1].get_value("tatm")

    @tatm.setter
    def tatm(self, value: float) -> None:
        """Set the tatm property."""
        self._cards[1].set_value("tatm", value)

    @property
    def patm(self) -> typing.Optional[float]:
        """Get or set the Atmospheric pressure
        """ # nopep8
        return self._cards[1].get_value("patm")

    @patm.setter
    def patm(self, value: float) -> None:
        """Set the patm property."""
        self._cards[1].set_value("patm", value)

    @property
    def nvent(self) -> int:
        """Get or set the Number of vent hole parts or part sets. See Remark 5
        """ # nopep8
        return self._cards[1].get_value("nvent")

    @nvent.setter
    def nvent(self, value: int) -> None:
        """Set the nvent property."""
        self._cards[1].set_value("nvent", value)

    @property
    def tsw(self) -> float:
        """Get or set the Time at which the algorithm switches to control volumes.
        """ # nopep8
        return self._cards[1].get_value("tsw")

    @tsw.setter
    def tsw(self, value: float) -> None:
        """Set the tsw property."""
        self._cards[1].set_value("tsw", value)

    @property
    def ngas(self) -> typing.Optional[int]:
        """Get or set the Number of gas components
        """ # nopep8
        return self._cards[2].get_value("ngas")

    @ngas.setter
    def ngas(self, value: int) -> None:
        """Set the ngas property."""
        self._cards[2].set_value("ngas", value)

    @property
    def norif(self) -> typing.Optional[int]:
        """Get or set the Number of orifices
        """ # nopep8
        return self._cards[2].get_value("norif")

    @norif.setter
    def norif(self, value: int) -> None:
        """Set the norif property."""
        self._cards[2].set_value("norif", value)

    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID defining vent holes.
        """ # nopep8
        return self._cards[3].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        """Set the sid1 property."""
        self._cards[3].set_value("sid1", value)

    @property
    def stype1(self) -> int:
        """Get or set the Set type:
        EQ.0: Part
        EQ.1: Part set
        EQ.2: Segment set
        """ # nopep8
        return self._cards[3].get_value("stype1")

    @stype1.setter
    def stype1(self, value: int) -> None:
        """Set the stype1 property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""stype1 must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("stype1", value)

    @property
    def c23(self) -> float:
        """Get or set the Vent hole coefficient, a parameter of Wang-Nefske leakage.
        """ # nopep8
        return self._cards[3].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        """Set the c23 property."""
        self._cards[3].set_value("c23", value)

    @property
    def pair(self) -> typing.Optional[float]:
        """Get or set the Initial pressure inside the bag (optional if PATM is set)
        """ # nopep8
        return self._cards[4].get_value("pair")

    @pair.setter
    def pair(self, value: float) -> None:
        """Set the pair property."""
        self._cards[4].set_value("pair", value)

    @property
    def tair(self) -> typing.Optional[float]:
        """Get or set the Initial temperature inside the bag (optional if TATM is set)
        """ # nopep8
        return self._cards[4].get_value("tair")

    @tair.setter
    def tair(self, value: float) -> None:
        """Set the tair property."""
        self._cards[4].set_value("tair", value)

    @property
    def xmair(self) -> typing.Optional[float]:
        """Get or set the Molar mass of the gas initially inside the bag
        """ # nopep8
        return self._cards[4].get_value("xmair")

    @xmair.setter
    def xmair(self, value: float) -> None:
        """Set the xmair property."""
        self._cards[4].set_value("xmair", value)

    @property
    def aair(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters
        """ # nopep8
        return self._cards[4].get_value("aair")

    @aair.setter
    def aair(self, value: float) -> None:
        """Set the aair property."""
        self._cards[4].set_value("aair", value)

    @property
    def bair(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters
        """ # nopep8
        return self._cards[4].get_value("bair")

    @bair.setter
    def bair(self, value: float) -> None:
        """Set the bair property."""
        self._cards[4].set_value("bair", value)

    @property
    def cair(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters
        """ # nopep8
        return self._cards[4].get_value("cair")

    @cair.setter
    def cair(self, value: float) -> None:
        """Set the cair property."""
        self._cards[4].set_value("cair", value)

    @property
    def lcmi(self) -> typing.Optional[int]:
        """Get or set the Mass flow rate curve for gas component i
        """ # nopep8
        return self._cards[5].get_value("lcmi")

    @lcmi.setter
    def lcmi(self, value: int) -> None:
        """Set the lcmi property."""
        self._cards[5].set_value("lcmi", value)

    @property
    def lcti(self) -> typing.Optional[int]:
        """Get or set the Temperature load curve for gas component i
        """ # nopep8
        return self._cards[5].get_value("lcti")

    @lcti.setter
    def lcti(self, value: int) -> None:
        """Set the lcti property."""
        self._cards[5].set_value("lcti", value)

    @property
    def xmi(self) -> typing.Optional[float]:
        """Get or set the Molar mass of gas component i.
        """ # nopep8
        return self._cards[5].get_value("xmi")

    @xmi.setter
    def xmi(self, value: float) -> None:
        """Set the xmi property."""
        self._cards[5].set_value("xmi", value)

    @property
    def ai(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i
        """ # nopep8
        return self._cards[5].get_value("ai")

    @ai.setter
    def ai(self, value: float) -> None:
        """Set the ai property."""
        self._cards[5].set_value("ai", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i
        """ # nopep8
        return self._cards[5].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[5].set_value("bi", value)

    @property
    def ci(self) -> typing.Optional[float]:
        """Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i
        """ # nopep8
        return self._cards[5].get_value("ci")

    @ci.setter
    def ci(self, value: float) -> None:
        """Set the ci property."""
        self._cards[5].set_value("ci", value)

    @property
    def iflip(self) -> typing.Optional[int]:
        """Get or set the Flag to flip the normal vectors for airbag shell parts. *ALE_STRUCTURED_FSI expects shell segment normal vectors to point inward, toward the inflator gas to which it couples. See Remark 2
        """ # nopep8
        return self._cards[6].get_value("iflip")

    @iflip.setter
    def iflip(self, value: int) -> None:
        """Set the iflip property."""
        self._cards[6].set_value("iflip", value)

    @property
    def lcvel(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for a curve giving the gas inlet velocity as a function of time. See Remark 3.
        """ # nopep8
        return self._cards[6].get_value("lcvel")

    @lcvel.setter
    def lcvel(self, value: int) -> None:
        """Set the lcvel property."""
        self._cards[6].set_value("lcvel", value)

    @property
    def pmax(self) -> typing.Optional[float]:
        """Get or set the Maximum impact pressure. This is used to set penalty stiffness. See Remark 4
        """ # nopep8
        return self._cards[6].get_value("pmax")

    @pmax.setter
    def pmax(self, value: float) -> None:
        """Set the pmax property."""
        self._cards[6].set_value("pmax", value)

    @property
    def ilock(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for specifying offset distance. This offset distance is used to avoid 'contact locking' between airbag fabric from different chambers. See Remark 7.
        """ # nopep8
        return self._cards[6].get_value("ilock")

    @ilock.setter
    def ilock(self, value: int) -> None:
        """Set the ilock property."""
        self._cards[6].set_value("ilock", value)

    @property
    def ilvl(self) -> typing.Optional[int]:
        """Get or set the Number of airbag segments to look downstream from the gas front.  ILVL and DECAY are the two flags needed to activate a mechanism to pre-open the closed airbag tube in front of the gas front to allow more gas flow. See Remark 8.
        """ # nopep8
        return self._cards[6].get_value("ilvl")

    @ilvl.setter
    def ilvl(self, value: int) -> None:
        """Set the ilvl property."""
        self._cards[6].set_value("ilvl", value)

    @property
    def decay(self) -> typing.Optional[float]:
        """Get or set the Decaying factor. See Remark 8
        """ # nopep8
        return self._cards[6].get_value("decay")

    @decay.setter
    def decay(self, value: float) -> None:
        """Set the decay property."""
        self._cards[6].set_value("decay", value)

