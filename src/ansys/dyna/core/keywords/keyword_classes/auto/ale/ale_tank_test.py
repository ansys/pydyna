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

"""Module providing the AleTankTest class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALETANKTEST_CARD0 = (
    FieldSchema("mdotlc", int, 0, 10, 0),
    FieldSchema("tankvol", float, 10, 10, 0.0),
    FieldSchema("pamb", float, 20, 10, 0.0),
    FieldSchema("pfinal", float, 30, 10, 0.0),
    FieldSchema("machlim", float, 40, 10, 0.0),
    FieldSchema("velmax", float, 50, 10, 0.0),
    FieldSchema("aorif", float, 60, 10, 0.0),
)

_ALETANKTEST_CARD1 = (
    FieldSchema("ammgidg", int, 0, 10, 0),
    FieldSchema("ammgida", int, 10, 10, 0),
    FieldSchema("numpnt", int, 20, 10, 50),
)

class AleTankTest(KeywordBase):
    """DYNA ALE_TANK_TEST keyword"""

    keyword = "ALE"
    subkeyword = "TANK_TEST"

    def __init__(self, **kwargs):
        """Initialize the AleTankTest class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALETANKTEST_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALETANKTEST_CARD1,
                **kwargs,
            ),        ]
    @property
    def mdotlc(self) -> int:
        """Get or set the LCID for mass flow rate as a function of time. This may be obtained directly from the control-volume type input data.
        """ # nopep8
        return self._cards[0].get_value("mdotlc")

    @mdotlc.setter
    def mdotlc(self, value: int) -> None:
        """Set the mdotlc property."""
        self._cards[0].set_value("mdotlc", value)

    @property
    def tankvol(self) -> float:
        """Get or set the Volume of the tank used in a tank test from which the tank pressure is measured, and the m(t) and Tgas(t) are computed from this tank pressure data.
        """ # nopep8
        return self._cards[0].get_value("tankvol")

    @tankvol.setter
    def tankvol(self, value: float) -> None:
        """Set the tankvol property."""
        self._cards[0].set_value("tankvol", value)

    @property
    def pamb(self) -> float:
        """Get or set the The pressure inside the tank before jetting (usually 1 bar).
        """ # nopep8
        return self._cards[0].get_value("pamb")

    @pamb.setter
    def pamb(self, value: float) -> None:
        """Set the pamb property."""
        self._cards[0].set_value("pamb", value)

    @property
    def pfinal(self) -> float:
        """Get or set the The final equilibrated pressure inside the tank from the tank test.
        """ # nopep8
        return self._cards[0].get_value("pfinal")

    @pfinal.setter
    def pfinal(self, value: float) -> None:
        """Set the pfinal property."""
        self._cards[0].set_value("pfinal", value)

    @property
    def machlim(self) -> float:
        """Get or set the A limiting MACH number for the gass at the throat (MACH=1 preferred).
        """ # nopep8
        return self._cards[0].get_value("machlim")

    @machlim.setter
    def machlim(self, value: float) -> None:
        """Set the machlim property."""
        self._cards[0].set_value("machlim", value)

    @property
    def velmax(self) -> float:
        """Get or set the Maximum allowable gas velocity across the inflator orifice (not preferred).
        """ # nopep8
        return self._cards[0].get_value("velmax")

    @velmax.setter
    def velmax(self, value: float) -> None:
        """Set the velmax property."""
        self._cards[0].set_value("velmax", value)

    @property
    def aorif(self) -> float:
        """Get or set the Total inflator orifice area (optional, only needed if the *SECTION_POINT_SOURCE card is not used).
        """ # nopep8
        return self._cards[0].get_value("aorif")

    @aorif.setter
    def aorif(self, value: float) -> None:
        """Set the aorif property."""
        self._cards[0].set_value("aorif", value)

    @property
    def ammgidg(self) -> int:
        """Get or set the The ALE multi-material group ID (AMMGID) of the gas.
        """ # nopep8
        return self._cards[1].get_value("ammgidg")

    @ammgidg.setter
    def ammgidg(self, value: int) -> None:
        """Set the ammgidg property."""
        self._cards[1].set_value("ammgidg", value)

    @property
    def ammgida(self) -> int:
        """Get or set the The ALE multi-material group ID (AMMGID) of the air
        """ # nopep8
        return self._cards[1].get_value("ammgida")

    @ammgida.setter
    def ammgida(self, value: int) -> None:
        """Set the ammgida property."""
        self._cards[1].set_value("ammgida", value)

    @property
    def numpnt(self) -> int:
        """Get or set the The number of points in m(t) and Tgas(t) curves. If NUMPNT=0, defaults to 50 points.
        """ # nopep8
        return self._cards[1].get_value("numpnt")

    @numpnt.setter
    def numpnt(self, value: int) -> None:
        """Set the numpnt property."""
        self._cards[1].set_value("numpnt", value)

