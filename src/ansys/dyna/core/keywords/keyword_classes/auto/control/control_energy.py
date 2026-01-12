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

"""Module providing the ControlEnergy class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLENERGY_CARD0 = (
    FieldSchema("hgen", int, 0, 10, 1),
    FieldSchema("rwen", int, 10, 10, 2),
    FieldSchema("slnten", int, 20, 10, 1),
    FieldSchema("rylen", int, 30, 10, 1),
    FieldSchema("irgen", int, 40, 10, 2),
    FieldSchema("maten", int, 50, 10, 1),
    FieldSchema("drlen", int, 60, 10, 1),
    FieldSchema("disen", int, 70, 10, 1),
)

class ControlEnergy(KeywordBase):
    """DYNA CONTROL_ENERGY keyword"""

    keyword = "CONTROL"
    subkeyword = "ENERGY"

    def __init__(self, **kwargs):
        """Initialize the ControlEnergy class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLENERGY_CARD0,
                **kwargs,
            ),        ]
    @property
    def hgen(self) -> int:
        """Get or set the Hourglass energy calculation option.
        EQ.1: hourglass energy is not computed (default),
        EQ.2: hourglass energy is computed and included in the energy balance.
        """ # nopep8
        return self._cards[0].get_value("hgen")

    @hgen.setter
    def hgen(self, value: int) -> None:
        """Set the hgen property."""
        if value not in [1, 2, None]:
            raise Exception("""hgen must be `None` or one of {1,2}.""")
        self._cards[0].set_value("hgen", value)

    @property
    def rwen(self) -> int:
        """Get or set the Stonewall energy dissipation option:
        EQ.1: energy dissipation is not computed,
        EQ.2: energy dissipation is computed and included in the energy balance (default).
        """ # nopep8
        return self._cards[0].get_value("rwen")

    @rwen.setter
    def rwen(self, value: int) -> None:
        """Set the rwen property."""
        if value not in [2, 1, None]:
            raise Exception("""rwen must be `None` or one of {2,1}.""")
        self._cards[0].set_value("rwen", value)

    @property
    def slnten(self) -> int:
        """Get or set the Sliding interface energy dissipation option:
        EQ.1: energy dissipation is not computed,
        EQ.2: energy dissipation is computed and included in the energy balance.
        """ # nopep8
        return self._cards[0].get_value("slnten")

    @slnten.setter
    def slnten(self, value: int) -> None:
        """Set the slnten property."""
        if value not in [1, 2, None]:
            raise Exception("""slnten must be `None` or one of {1,2}.""")
        self._cards[0].set_value("slnten", value)

    @property
    def rylen(self) -> int:
        """Get or set the Rayleigh energy dissipation option (damping energy dissipation):
        EQ.1: energy dissipation is not computed (default),
        EQ.2: energy dissipation is computed and included in the energy balance.
        """ # nopep8
        return self._cards[0].get_value("rylen")

    @rylen.setter
    def rylen(self, value: int) -> None:
        """Set the rylen property."""
        if value not in [1, 2, None]:
            raise Exception("""rylen must be `None` or one of {1,2}.""")
        self._cards[0].set_value("rylen", value)

    @property
    def irgen(self) -> int:
        """Get or set the Initial reference geometry energy option (included in internal energy, resulting from *INITIAL_FOAM_REFERENCE_GEOMETRY):
        EQ.1:	initial reference geometry energy is not computed,
        EQ.2:	initial reference geometry energy is computed and included in the energy balance as part of the internal energy (default).
        """ # nopep8
        return self._cards[0].get_value("irgen")

    @irgen.setter
    def irgen(self, value: int) -> None:
        """Set the irgen property."""
        if value not in [2, 1, None]:
            raise Exception("""irgen must be `None` or one of {2,1}.""")
        self._cards[0].set_value("irgen", value)

    @property
    def maten(self) -> int:
        """Get or set the Detailed material energies option. For a choice of material models (currently supported are 3, 4, 15, 19, 24, 63,81, 82, 98, 104, 105, 106, 107, 123, 124, 188, 224, 225, 240, and 251 for shell and solid elements), internal energy is additionally split into elastic, plastic and damage portions:
        EQ.1:	detailed material energies are not computed(default).
        EQ.2 : detailed material energies are computed and reported as mat_energy_elastic, mat_energy_plastic,and mat_energy_ damage in ASCII file glstatand matsum
        """ # nopep8
        return self._cards[0].get_value("maten")

    @maten.setter
    def maten(self, value: int) -> None:
        """Set the maten property."""
        if value not in [1, 2, None]:
            raise Exception("""maten must be `None` or one of {1,2}.""")
        self._cards[0].set_value("maten", value)

    @property
    def drlen(self) -> int:
        """Get or set the Drilling energy calculation option, for implicit and with use of DRCPSID/DRCPRM on *CONTROL_SHELL:
        EQ.1:	Drilling energy is not computed(default).
        EQ.2 : Drilling energy is computed and included in the energy balance.The drilling energies are reported in the ASCII file glstat, see* DATABASE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("drlen")

    @drlen.setter
    def drlen(self, value: int) -> None:
        """Set the drlen property."""
        if value not in [1, 2, None]:
            raise Exception("""drlen must be `None` or one of {1,2}.""")
        self._cards[0].set_value("drlen", value)

    @property
    def disen(self) -> int:
        """Get or set the Dissipation energy calculation option, for implicit:
        EQ.1:	Dissipated energy is not computed(default).
        EQ.2 : Dissipated kinetic and internal energy is computed and included in the energy balance.The dissipation energies are reported in the ASCII file glstat, see* DATABASE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("disen")

    @disen.setter
    def disen(self, value: int) -> None:
        """Set the disen property."""
        if value not in [1, 2, None]:
            raise Exception("""disen must be `None` or one of {1,2}.""")
        self._cards[0].set_value("disen", value)

