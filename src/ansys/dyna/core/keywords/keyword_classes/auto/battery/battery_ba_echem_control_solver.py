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

"""Module providing the BatteryBaEchemControlSolver class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BATTERYBAECHEMCONTROLSOLVER_CARD0 = (
    FieldSchema("imodel", int, 0, 10, None),
    FieldSchema("igeom", int, 10, 10, None),
    FieldSchema("ncycle", int, 20, 10, 1),
    FieldSchema("aging", int, 30, 10, 1),
    FieldSchema("tra", int, 40, 10, 0),
    FieldSchema("gas", int, 50, 10, 0),
    FieldSchema("esolid", int, 60, 10, 0),
    FieldSchema("unused", int, 70, 10, None),
)

_BATTERYBAECHEMCONTROLSOLVER_CARD1 = (
    FieldSchema("irun", int, 0, 10, None),
    FieldSchema("lcur", int, 10, 10, None),
    FieldSchema("curv", float, 20, 10, None),
    FieldSchema("ctime", float, 30, 10, 0.0),
    FieldSchema("vcut", float, 40, 10, 0.0),
)

class BatteryBaEchemControlSolver(KeywordBase):
    """DYNA BATTERY_BA_ECHEM_CONTROL_SOLVER keyword"""

    keyword = "BATTERY"
    subkeyword = "BA_ECHEM_CONTROL_SOLVER"

    def __init__(self, **kwargs):
        """Initialize the BatteryBaEchemControlSolver class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BATTERYBAECHEMCONTROLSOLVER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BATTERYBAECHEMCONTROLSOLVER_CARD1,
                **kwargs,
            ),        ]
    @property
    def imodel(self) -> typing.Optional[int]:
        """Get or set the Sets the battery model.
        EQ.1:	A single insertion model
        EQ.2 : Dual insertion model
        """ # nopep8
        return self._cards[0].get_value("imodel")

    @imodel.setter
    def imodel(self, value: int) -> None:
        """Set the imodel property."""
        self._cards[0].set_value("imodel", value)

    @property
    def igeom(self) -> typing.Optional[int]:
        """Get or set the Sets the geometric dimension:
        EQ.1:	1D Electrochemical model
        EQ.11 : 1D Aging and Thermal Runaway model
        EQ.101 : 1D ECTM model
        EQ.111 : 1D ECTM with A & T model
        """ # nopep8
        return self._cards[0].get_value("igeom")

    @igeom.setter
    def igeom(self, value: int) -> None:
        """Set the igeom property."""
        self._cards[0].set_value("igeom", value)

    @property
    def ncycle(self) -> int:
        """Get or set the The number of cycles to run. Default is 1 cycle
        """ # nopep8
        return self._cards[0].get_value("ncycle")

    @ncycle.setter
    def ncycle(self, value: int) -> None:
        """Set the ncycle property."""
        self._cards[0].set_value("ncycle", value)

    @property
    def aging(self) -> int:
        """Get or set the Aging model. 1 for ON, and 0 for OFF
        """ # nopep8
        return self._cards[0].get_value("aging")

    @aging.setter
    def aging(self, value: int) -> None:
        """Set the aging property."""
        self._cards[0].set_value("aging", value)

    @property
    def tra(self) -> int:
        """Get or set the Thermal runaway model. 1for ON, and 0 for OFF.
        """ # nopep8
        return self._cards[0].get_value("tra")

    @tra.setter
    def tra(self, value: int) -> None:
        """Set the tra property."""
        self._cards[0].set_value("tra", value)

    @property
    def gas(self) -> int:
        """Get or set the Gas generation model (scheduled)
        """ # nopep8
        return self._cards[0].get_value("gas")

    @gas.setter
    def gas(self, value: int) -> None:
        """Set the gas property."""
        self._cards[0].set_value("gas", value)

    @property
    def esolid(self) -> int:
        """Get or set the The concentration of the electrode particle model
        EQ.0: Superposition method.
        EQ.1 : Full equation method.
        """ # nopep8
        return self._cards[0].get_value("esolid")

    @esolid.setter
    def esolid(self, value: int) -> None:
        """Set the esolid property."""
        self._cards[0].set_value("esolid", value)

    @property
    def irun(self) -> typing.Optional[int]:
        """Get or set the Battery simulation cycle termination criterion
        EQ.1:	The current cycle runs for a given time
        EQ.2 : The current cycle runs until the cell voltage reaches VCUT
        """ # nopep8
        return self._cards[1].get_value("irun")

    @irun.setter
    def irun(self, value: int) -> None:
        """Set the irun property."""
        self._cards[1].set_value("irun", value)

    @property
    def lcur(self) -> typing.Optional[int]:
        """Get or set the Running current.
        EQ.0:	Constant current.
        EQ.1 : Variable current
        """ # nopep8
        return self._cards[1].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        """Set the lcur property."""
        self._cards[1].set_value("lcur", value)

    @property
    def curv(self) -> typing.Optional[float]:
        """Get or set the Current value to run
        """ # nopep8
        return self._cards[1].get_value("curv")

    @curv.setter
    def curv(self, value: float) -> None:
        """Set the curv property."""
        self._cards[1].set_value("curv", value)

    @property
    def ctime(self) -> float:
        """Get or set the Running time for the cycle
        """ # nopep8
        return self._cards[1].get_value("ctime")

    @ctime.setter
    def ctime(self, value: float) -> None:
        """Set the ctime property."""
        self._cards[1].set_value("ctime", value)

    @property
    def vcut(self) -> float:
        """Get or set the A voltage to terminate
        """ # nopep8
        return self._cards[1].get_value("vcut")

    @vcut.setter
    def vcut(self, value: float) -> None:
        """Set the vcut property."""
        self._cards[1].set_value("vcut", value)

