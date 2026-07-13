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

"""Module providing the ControlPoreAir class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLPOREAIR_CARD0 = (
    FieldSchema("air_rho", float, 0, 10, None),
    FieldSchema("air_p", float, 10, 10, None),
    FieldSchema("eterm", float, 20, 10, None),
    FieldSchema("anamsg", int, 30, 10, 0),
    FieldSchema("optapa", int, 40, 10, 0),
    FieldSchema("pa_dr", int, 50, 10, 0),
)

class ControlPoreAir(KeywordBase):
    """DYNA CONTROL_PORE_AIR keyword"""

    keyword = "CONTROL"
    subkeyword = "PORE_AIR"

    def __init__(self, **kwargs):
        """Initialize the ControlPoreAir class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLPOREAIR_CARD0,
                **kwargs,
            ),
        ]
    @property
    def air_rho(self) -> typing.Optional[float]:
        """Get or set the Density of atmospheric air, = 1.184 kg/m3 at 25 C
        """ # nopep8
        return self._cards[0].get_value("air_rho")

    @air_rho.setter
    def air_rho(self, value: float) -> None:
        """Set the air_rho property."""
        self._cards[0].set_value("air_rho", value)

    @property
    def air_p(self) -> typing.Optional[float]:
        """Get or set the Pressure of atmospheric air, = 101.325 kPa at 25 C
        """ # nopep8
        return self._cards[0].get_value("air_p")

    @air_p.setter
    def air_p(self, value: float) -> None:
        """Set the air_p property."""
        self._cards[0].set_value("air_p", value)

    @property
    def eterm(self) -> typing.Optional[float]:
        """Get or set the Event termination time. The default is ENDTIM of *CONTROL_TERMINATION.  If ETERM is defined and smaller than ENDTIM, the simublation terminates, by default, when the simulation time reaches ETERM.  However, OTAPA below provides options for continuing the simulation beyond ETERM.
        """ # nopep8
        return self._cards[0].get_value("eterm")

    @eterm.setter
    def eterm(self, value: float) -> None:
        """Set the eterm property."""
        self._cards[0].set_value("eterm", value)

    @property
    def anamsg(self) -> int:
        """Get or set the Flag to turn off the printing of pore air analysis status message,
        including the analysis time, the node with the highest pressure change.
        EQ. 0 Status messages are printed, the default value.
        EQ. 1 Status messages are not printed
        """ # nopep8
        return self._cards[0].get_value("anamsg")

    @anamsg.setter
    def anamsg(self, value: int) -> None:
        """Set the anamsg property."""
        if value not in [0, 1, None]:
            raise Exception("""anamsg must be `None` or one of {0,1}.""")
        self._cards[0].set_value("anamsg", value)

    @property
    def optapa(self) -> int:
        """Get or set the Option when 0.0 < ETERM < ENDTIM:
        EQ.0: The simulation terminates when time reaches ETERM.
        EQ.1: The simulation continues beyond time ETERM.However, pore air pressure is not updated after ETERM,and the last updated pore air pressure applies for the rest of the simulation.
        EQ.2: The simulation continues beyond time ETERM, but pore air pressure does not apply after time ETERM.
        """ # nopep8
        return self._cards[0].get_value("optapa")

    @optapa.setter
    def optapa(self, value: int) -> None:
        """Set the optapa property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""optapa must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("optapa", value)

    @property
    def pa_dr(self) -> int:
        """Get or set the Flag to turn on pore air flow analysis during dynamic relaxation.  Starting with R13, the pore air analysis is disabled by default during dynamic relaxation. This flag enables reproducing results comparable to R12 or earlier versions in which pore air analysis is active during the dynamic relaxation phase.
        EQ.0:	Disable pore air analysis is disabled during dynamic relaxation.
        EQ.1:	Enable pore air analysis during dynamic relaxation
        """ # nopep8
        return self._cards[0].get_value("pa_dr")

    @pa_dr.setter
    def pa_dr(self, value: int) -> None:
        """Set the pa_dr property."""
        if value not in [0, 1, None]:
            raise Exception("""pa_dr must be `None` or one of {0,1}.""")
        self._cards[0].set_value("pa_dr", value)

