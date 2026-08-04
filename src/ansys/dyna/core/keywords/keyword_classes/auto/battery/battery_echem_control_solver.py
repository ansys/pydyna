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

"""Module providing the BatteryEchemControlSolver class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYECHEMCONTROLSOLVER_CARD0 = (
    FieldSchema("imodel", int, 0, 10, None),
    FieldSchema("idim", int, 10, 10, None),
    FieldSchema("ncycle", int, 20, 10, 1),
    FieldSchema("iage", int, 30, 10, None),
    FieldSchema("itra", int, 40, 10, 0),
    FieldSchema("igas", int, 50, 10, 0),
    FieldSchema("ip2d", int, 60, 10, 0),
    FieldSchema("iporo", int, 70, 10, 0),
)

_BATTERYECHEMCONTROLSOLVER_CARD1 = (
    FieldSchema("cmode", int, 0, 10, 1),
    FieldSchema("ctype", int, 10, 10, 0),
    FieldSchema("cend", int, 20, 10, None),
    FieldSchema("tcut", float, 30, 10, None),
    FieldSchema("vcut", float, 40, 10, 2.0),
    FieldSchema("curr", float, 50, 10, None),
    FieldSchema("taper", float, 60, 10, None),
    FieldSchema("cellid", int, 70, 10, None),
)

class BatteryEchemControlSolver(KeywordBase):
    """DYNA BATTERY_ECHEM_CONTROL_SOLVER keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_CONTROL_SOLVER"

    def __init__(self, **kwargs):
        """Initialize the BatteryEchemControlSolver class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMCONTROLSOLVER_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BATTERYECHEMCONTROLSOLVER_CARD1,
                **kwargs,
            ),
        ]
    @property
    def imodel(self) -> typing.Optional[int]:
        """Get or set the Sets the battery model:
        EQ.1 : 6 Equation P2D : ITRA,IAGE and IGAS must be set to 0.
        EQ.2 : 10 or 15 - Equation Model.With IGAS = 0 and IAGE = 1, the model includes ageing effects(10 Eq model) while with IGAS = 1 and IAGE = 1, the model includes both ageing and gas generation effects.See Remark 1
        """ # nopep8
        return self._cards[0].get_value("imodel")

    @imodel.setter
    def imodel(self, value: int) -> None:
        """Set the imodel property."""
        self._cards[0].set_value("imodel", value)

    @property
    def idim(self) -> typing.Optional[int]:
        """Get or set the Sets the geometric dimension:
        EQ.1:	Standalone 1D model:
        EQ.101:	1D models with EM-thermo-mechanical coupling (available for IMODEL = 1 and 2). See Remark 2
        """ # nopep8
        return self._cards[0].get_value("idim")

    @idim.setter
    def idim(self, value: int) -> None:
        """Set the idim property."""
        self._cards[0].set_value("idim", value)

    @property
    def ncycle(self) -> int:
        """Get or set the Number of cycles to run. The default value is 1 cycle. The maximunm value for this parameter is 100
        """ # nopep8
        return self._cards[0].get_value("ncycle")

    @ncycle.setter
    def ncycle(self, value: int) -> None:
        """Set the ncycle property."""
        self._cards[0].set_value("ncycle", value)

    @property
    def iage(self) -> typing.Optional[int]:
        """Get or set the Ageing model:
        EQ.0 : Off
        EQ.1 : On
        """ # nopep8
        return self._cards[0].get_value("iage")

    @iage.setter
    def iage(self, value: int) -> None:
        """Set the iage property."""
        self._cards[0].set_value("iage", value)

    @property
    def itra(self) -> int:
        """Get or set the Ageing heat flag (see Remark 3):
        EQ.0:Off
        EQ.1: On
        """ # nopep8
        return self._cards[0].get_value("itra")

    @itra.setter
    def itra(self, value: int) -> None:
        """Set the itra property."""
        if value not in [0, 1, None]:
            raise Exception("""itra must be `None` or one of {0,1}.""")
        self._cards[0].set_value("itra", value)

    @property
    def igas(self) -> int:
        """Get or set the Gas generation model flag (see Remark 2):
        EQ.0:Off
        EQ.1: On
        EQ.2:	On and venting model activated
        """ # nopep8
        return self._cards[0].get_value("igas")

    @igas.setter
    def igas(self, value: int) -> None:
        """Set the igas property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""igas must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("igas", value)

    @property
    def ip2d(self) -> int:
        """Get or set the Radial diffusion scheme:
        EQ.0 : Superposition
        EQ.1 : BDF finite differences.
        """ # nopep8
        return self._cards[0].get_value("ip2d")

    @ip2d.setter
    def ip2d(self, value: int) -> None:
        """Set the ip2d property."""
        if value not in [0, 1, None]:
            raise Exception("""ip2d must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ip2d", value)

    @property
    def iporo(self) -> int:
        """Get or set the Porosity feedback when IAGING=1:
        EQ.0 : Off
        EQ.1 : On
        """ # nopep8
        return self._cards[0].get_value("iporo")

    @iporo.setter
    def iporo(self, value: int) -> None:
        """Set the iporo property."""
        if value not in [0, 1, None]:
            raise Exception("""iporo must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iporo", value)

    @property
    def cmode(self) -> int:
        """Get or set the Battery running mode flag:
        EQ.0: Galvanostatic run(imposed current)
        EQ.1: Potentiostatic run run (current chosen so Voltage is held)
        EQ.2 : CC - CV run(imposed current switching to constant voltage mode once VCUT is reached)
        EQ.3 : Constant Resistant run(current chose so Resistance is held)
        """ # nopep8
        return self._cards[1].get_value("cmode")

    @cmode.setter
    def cmode(self, value: int) -> None:
        """Set the cmode property."""
        if value not in [1, 0, 2, 3, None]:
            raise Exception("""cmode must be `None` or one of {1,0,2,3}.""")
        self._cards[1].set_value("cmode", value)

    @property
    def ctype(self) -> int:
        """Get or set the Boundary condition type:
        EQ.0:Constant
        EQ.1: Variable VCUT, CURR values point to load curve ID function of time
        """ # nopep8
        return self._cards[1].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        """Set the ctype property."""
        if value not in [0, 1, None]:
            raise Exception("""ctype must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ctype", value)

    @property
    def cend(self) -> typing.Optional[int]:
        """Get or set the Cause of battery cycle termination:
        EQ.1: Cycle run for a given time period.
        EQ.2: Cycle run until a given cut - off voltage.
        """ # nopep8
        return self._cards[1].get_value("cend")

    @cend.setter
    def cend(self, value: int) -> None:
        """Set the cend property."""
        self._cards[1].set_value("cend", value)

    @property
    def tcut(self) -> typing.Optional[float]:
        """Get or set the Total running time for the cycle.(units: s)
        """ # nopep8
        return self._cards[1].get_value("tcut")

    @tcut.setter
    def tcut(self, value: float) -> None:
        """Set the tcut property."""
        self._cards[1].set_value("tcut", value)

    @property
    def vcut(self) -> float:
        """Get or set the Cut-off voltage for the cycle or holding voltage depending on CMODE .(units: V)
        """ # nopep8
        return self._cards[1].get_value("vcut")

    @vcut.setter
    def vcut(self, value: float) -> None:
        """Set the vcut property."""
        self._cards[1].set_value("vcut", value)

    @property
    def curr(self) -> typing.Optional[float]:
        """Get or set the Cycle operating current in the case of constant current. Load curve ID in case of variable current. Initial current guess if CMODE=1 or 3. (units: A.m^(-2))
        """ # nopep8
        return self._cards[1].get_value("curr")

    @curr.setter
    def curr(self, value: float) -> None:
        """Set the curr property."""
        self._cards[1].set_value("curr", value)

    @property
    def taper(self) -> typing.Optional[float]:
        """Get or set the Taper current. Below this absolute current value, the cycle is terminated
        """ # nopep8
        return self._cards[1].get_value("taper")

    @taper.setter
    def taper(self, value: float) -> None:
        """Set the taper property."""
        self._cards[1].set_value("taper", value)

    @property
    def cellid(self) -> typing.Optional[int]:
        """Get or set the Optional Cell ID that links this particular cycle to a specific Cell
        """ # nopep8
        return self._cards[1].get_value("cellid")

    @cellid.setter
    def cellid(self, value: int) -> None:
        """Set the cellid property."""
        self._cards[1].set_value("cellid", value)

