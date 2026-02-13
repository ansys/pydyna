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

"""Module providing the LoadBlastEnhanced class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_LOADBLASTENHANCED_CARD0 = (
    FieldSchema("bid", int, 0, 10, None),
    FieldSchema("m", float, 10, 10, 0.0),
    FieldSchema("xbo", float, 20, 10, 0.0),
    FieldSchema("ybo", float, 30, 10, 0.0),
    FieldSchema("zbo", float, 40, 10, 0.0),
    FieldSchema("tbo", float, 50, 10, 0.0),
    FieldSchema("unit", int, 60, 10, 2),
    FieldSchema("blast", int, 70, 10, 2),
)

_LOADBLASTENHANCED_CARD1 = (
    FieldSchema("cfm", float, 0, 10, 0.0),
    FieldSchema("cfl", float, 10, 10, 0.0),
    FieldSchema("cft", float, 20, 10, 0.0),
    FieldSchema("cfp", float, 30, 10, 0.0),
    FieldSchema("nidbo", int, 40, 10, None),
    FieldSchema("death", float, 50, 10, 1e+20),
    FieldSchema("negphs", int, 60, 10, 0),
)

class LoadBlastEnhanced(KeywordBase):
    """DYNA LOAD_BLAST_ENHANCED keyword"""

    keyword = "LOAD"
    subkeyword = "BLAST_ENHANCED"
    _link_fields = {
        "nidbo": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadBlastEnhanced class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADBLASTENHANCED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADBLASTENHANCED_CARD1,
                **kwargs,
            ),        ]
    @property
    def bid(self) -> typing.Optional[int]:
        """Get or set the Blast ID.  A unique number must be defined for each blast source (charge).  Multiple charges may be defined, however, interaction of the waves in air is not considered.
        """ # nopep8
        return self._cards[0].get_value("bid")

    @bid.setter
    def bid(self, value: int) -> None:
        """Set the bid property."""
        self._cards[0].set_value("bid", value)

    @property
    def m(self) -> float:
        """Get or set the Equivalent mass of TNT.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[0].set_value("m", value)

    @property
    def xbo(self) -> float:
        """Get or set the x-coordinate of charge center.
        """ # nopep8
        return self._cards[0].get_value("xbo")

    @xbo.setter
    def xbo(self, value: float) -> None:
        """Set the xbo property."""
        self._cards[0].set_value("xbo", value)

    @property
    def ybo(self) -> float:
        """Get or set the y-coordinate of charge center.
        """ # nopep8
        return self._cards[0].get_value("ybo")

    @ybo.setter
    def ybo(self, value: float) -> None:
        """Set the ybo property."""
        self._cards[0].set_value("ybo", value)

    @property
    def zbo(self) -> float:
        """Get or set the z-coordinate of charge center.
        """ # nopep8
        return self._cards[0].get_value("zbo")

    @zbo.setter
    def zbo(self, value: float) -> None:
        """Set the zbo property."""
        self._cards[0].set_value("zbo", value)

    @property
    def tbo(self) -> float:
        """Get or set the Time of detonation.
        """ # nopep8
        return self._cards[0].get_value("tbo")

    @tbo.setter
    def tbo(self, value: float) -> None:
        """Set the tbo property."""
        self._cards[0].set_value("tbo", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit conversion flag.    EQ.1:  feet, pound-mass, seconds, psi    EQ.2:  meters, kilograms, seconds, Pascals (default)    EQ.3:  inch, dozens of slugs, seconds, psi    EQ.4:  centimeters, grams, microseconds, Megabars    EQ.5:  user conversions will be supplied(see Card 2).    EQ.6: kilogram, millimeter, millisecond, GPa    EQ.7: metric ton, millimeter, second, Mpa    EQ.8: gram, millimeter, millisecond, MPa
        """ # nopep8
        return self._cards[0].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        """Set the unit property."""
        if value not in [2, 1, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""unit must be `None` or one of {2,1,3,4,5,6,7,8}.""")
        self._cards[0].set_value("unit", value)

    @property
    def blast(self) -> int:
        """Get or set the Type of blast source    EQ.1:  hemispherical surface burst - charge is located on or very near the ground surface, initial shock wave is reflected and reinforced by the ground    EQ.2:  spherical free-air burst (default) - no amplification of the initial shock wave due to interaction with the ground surface    EQ.3:  air burst - moving non-sperhical warhead    EQ.4:  air burst with ground reflection - initial shock wave impinges on the ground surface and is reinforced by the reflected wave to produce a Mach stem.
        """ # nopep8
        return self._cards[0].get_value("blast")

    @blast.setter
    def blast(self, value: int) -> None:
        """Set the blast property."""
        if value not in [2, 1, 3, 4, None]:
            raise Exception("""blast must be `None` or one of {2,1,3,4}.""")
        self._cards[0].set_value("blast", value)

    @property
    def cfm(self) -> float:
        """Get or set the Conversion factor - pounds per LS-DYNA mass unit.
        """ # nopep8
        return self._cards[1].get_value("cfm")

    @cfm.setter
    def cfm(self, value: float) -> None:
        """Set the cfm property."""
        self._cards[1].set_value("cfm", value)

    @property
    def cfl(self) -> float:
        """Get or set the Conversion factor - feet per LS-DYNA length units.
        """ # nopep8
        return self._cards[1].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        """Set the cfl property."""
        self._cards[1].set_value("cfl", value)

    @property
    def cft(self) -> float:
        """Get or set the Conversion factor - milliseconds per LS-DYNA time unit.
        """ # nopep8
        return self._cards[1].get_value("cft")

    @cft.setter
    def cft(self, value: float) -> None:
        """Set the cft property."""
        self._cards[1].set_value("cft", value)

    @property
    def cfp(self) -> float:
        """Get or set the Conversion factor - psi per LS-DYNA pressure unit.
        """ # nopep8
        return self._cards[1].get_value("cfp")

    @cfp.setter
    def cfp(self, value: float) -> None:
        """Set the cfp property."""
        self._cards[1].set_value("cfp", value)

    @property
    def nidbo(self) -> typing.Optional[int]:
        """Get or set the Optional node ID representing the charge center.  If defined then XBO, YBO and XBO are ignored.
        """ # nopep8
        return self._cards[1].get_value("nidbo")

    @nidbo.setter
    def nidbo(self, value: int) -> None:
        """Set the nidbo property."""
        self._cards[1].set_value("nidbo", value)

    @property
    def death(self) -> float:
        """Get or set the Death time.  Blast pressures are deactivated at this time.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[1].set_value("death", value)

    @property
    def negphs(self) -> int:
        """Get or set the Treament of negative phase.
        EQ.0:  negative dictated by the Friedlander equation.
        EQ.1:  negative phase ignored as in ConWep.
        """ # nopep8
        return self._cards[1].get_value("negphs")

    @negphs.setter
    def negphs(self, value: int) -> None:
        """Set the negphs property."""
        if value not in [0, 1, None]:
            raise Exception("""negphs must be `None` or one of {0,1}.""")
        self._cards[1].set_value("negphs", value)

    @property
    def nidbo_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nidbo."""
        return self._get_link_by_attr("NODE", "nid", self.nidbo, "parts")

