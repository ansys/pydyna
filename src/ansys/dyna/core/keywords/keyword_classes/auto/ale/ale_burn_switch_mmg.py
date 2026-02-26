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

"""Module providing the AleBurnSwitchMmg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALEBURNSWITCHMMG_CARD0 = (
    FieldSchema("mmgfr", int, 0, 10, None),
    FieldSchema("mmgto", int, 10, 10, None),
    FieldSchema("nvarline", int, 20, 10, 0),
)

_ALEBURNSWITCHMMG_CARD1 = (
    FieldSchema("react", int, 0, 10, 0),
)

_ALEBURNSWITCHMMG_CARD2 = (
    FieldSchema("igni", int, 0, 10, 0),
    FieldSchema("igniv", int, 10, 10, 0),
    FieldSchema("ignivf", int, 20, 10, 0),
)

_ALEBURNSWITCHMMG_CARD3 = (
    FieldSchema("var", int, 0, 10, 0),
    FieldSchema("var", int, 10, 10, 0),
    FieldSchema("var", int, 20, 10, 0),
    FieldSchema("var", int, 30, 10, 0),
    FieldSchema("var", int, 40, 10, 0),
    FieldSchema("var", int, 50, 10, 0),
    FieldSchema("var", int, 60, 10, 0),
    FieldSchema("var", int, 70, 10, 0),
)

_ALEBURNSWITCHMMG_CARD4 = (
    FieldSchema("par", float, 0, 10, 0.0),
    FieldSchema("par", float, 10, 10, 0.0),
    FieldSchema("par", float, 20, 10, 0.0),
    FieldSchema("par", float, 30, 10, 0.0),
    FieldSchema("par", float, 40, 10, 0.0),
    FieldSchema("par", float, 50, 10, 0.0),
    FieldSchema("par", float, 60, 10, 0.0),
    FieldSchema("par", float, 70, 10, 0.0),
)

class AleBurnSwitchMmg(KeywordBase):
    """DYNA ALE_BURN_SWITCH_MMG keyword"""

    keyword = "ALE"
    subkeyword = "BURN_SWITCH_MMG"

    def __init__(self, **kwargs):
        """Initialize the AleBurnSwitchMmg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD4,
                **kwargs,
            ),        ]
    @property
    def mmgfr(self) -> typing.Optional[int]:
        """Get or set the ALE multi-material-group (explosive) before the switch.
        """ # nopep8
        return self._cards[0].get_value("mmgfr")

    @mmgfr.setter
    def mmgfr(self, value: int) -> None:
        """Set the mmgfr property."""
        self._cards[0].set_value("mmgfr", value)

    @property
    def mmgto(self) -> typing.Optional[int]:
        """Get or set the ALE multi-material-group (explosion product) after the switch.
        """ # nopep8
        return self._cards[0].get_value("mmgto")

    @mmgto.setter
    def mmgto(self, value: int) -> None:
        """Set the mmgto property."""
        self._cards[0].set_value("mmgto", value)

    @property
    def nvarline(self) -> int:
        """Get or set the Number of lines with arguments in the functions REACT, IGNI and IGNIV.
        """ # nopep8
        return self._cards[0].get_value("nvarline")

    @nvarline.setter
    def nvarline(self, value: int) -> None:
        """Set the nvarline property."""
        self._cards[0].set_value("nvarline", value)

    @property
    def react(self) -> int:
        """Get or set the ID of the *DEFINE_FUNCTION function controlling the reaction rate.
        This function determines the explosive volume fraction to be switched.
        """ # nopep8
        return self._cards[1].get_value("react")

    @react.setter
    def react(self, value: int) -> None:
        """Set the react property."""
        self._cards[1].set_value("react", value)

    @property
    def igni(self) -> int:
        """Get or set the ID of the *DEFINE_FUNCTION function controlling the conditions of ignition.
        """ # nopep8
        return self._cards[2].get_value("igni")

    @igni.setter
    def igni(self, value: int) -> None:
        """Set the igni property."""
        self._cards[2].set_value("igni", value)

    @property
    def igniv(self) -> int:
        """Get or set the ID of the *DEFINE_FUNCTION function computing the ignition front speed.  See Remark 1.
        """ # nopep8
        return self._cards[2].get_value("igniv")

    @igniv.setter
    def igniv(self, value: int) -> None:
        """Set the igniv property."""
        self._cards[2].set_value("igniv", value)

    @property
    def ignivf(self) -> int:
        """Get or set the Flag that activates computing the ignition front as a material interface between MMGFR and MMGTO.
        This flag will be automatically activated if both IGNI and IGNIVF are undefined (see Remark 2).
        EQ.0:	not activated
        EQ.1:	activated.
        """ # nopep8
        return self._cards[2].get_value("ignivf")

    @ignivf.setter
    def ignivf(self, value: int) -> None:
        """Set the ignivf property."""
        if value not in [0, 1, None]:
            raise Exception("""ignivf must be `None` or one of {0,1}.""")
        self._cards[2].set_value("ignivf", value)

    @property
    def var(self) -> int:
        """Get or set the Variable rank in the following list (see Remark 3):
        EQ.1:	-stress for MMGFR
        EQ.2:	-stress for MMGFR
        EQ.3:	-stress for MMGFR
        EQ.4:	-stress for MMGFR
        EQ.5:	-stress for MMGFR
        EQ.6:	-stress for MMGFR
        EQ.7:	plastic strain for MMGFR
        EQ.8:	internal energy for MMGFR
        EQ.9:	bulk viscosity for MMGFR
        EQ.10:	volume from previous cycle for MMGFR
        GE.11 and LE.20:	other auxiliary variables for MMGFR
        GE.21 and LE.40:	auxiliary variables for MMGTO (-stress, …)
        EQ.41:	mass for MMGFR
        EQ.42:	mass for MMGTO
        EQ.43:	volume fraction for MMGFR
        EQ.44:	volume fraction for MMGTO
        EQ.45:	material volume for MMGFR
        EQ.46:	material volume for MMGTO
        EQ.47:	time step
        EQ.48:	time
        EQ.49:	cycle
        GE.50 and LE.57:	-positions of the ALE nodes
        GE.58 and LE.65:	-positions of the ALE nodes
        GE.66 and LE.73:	-positions of the ALE nodes
        GE.74 and LE.81:	-velocities of the ALE nodes
        GE.82 and LE.89:	-velocities of the ALE nodes
        GE.90 and LE.97:	-velocities of the ALE nodes
        GE.98 and LE.105:	-accelerations of the ALE nodes
        GE.106 and LE.113:	-accelerations of the ALE nodes
        GE.114 and LE.121:	-accelerations of the ALE nodes
        GE.122 and LE.129:	masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[3].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Variable rank in the following list (see Remark 3):
        EQ.1:	-stress for MMGFR
        EQ.2:	-stress for MMGFR
        EQ.3:	-stress for MMGFR
        EQ.4:	-stress for MMGFR
        EQ.5:	-stress for MMGFR
        EQ.6:	-stress for MMGFR
        EQ.7:	plastic strain for MMGFR
        EQ.8:	internal energy for MMGFR
        EQ.9:	bulk viscosity for MMGFR
        EQ.10:	volume from previous cycle for MMGFR
        GE.11 and LE.20:	other auxiliary variables for MMGFR
        GE.21 and LE.40:	auxiliary variables for MMGTO (-stress, …)
        EQ.41:	mass for MMGFR
        EQ.42:	mass for MMGTO
        EQ.43:	volume fraction for MMGFR
        EQ.44:	volume fraction for MMGTO
        EQ.45:	material volume for MMGFR
        EQ.46:	material volume for MMGTO
        EQ.47:	time step
        EQ.48:	time
        EQ.49:	cycle
        GE.50 and LE.57:	-positions of the ALE nodes
        GE.58 and LE.65:	-positions of the ALE nodes
        GE.66 and LE.73:	-positions of the ALE nodes
        GE.74 and LE.81:	-velocities of the ALE nodes
        GE.82 and LE.89:	-velocities of the ALE nodes
        GE.90 and LE.97:	-velocities of the ALE nodes
        GE.98 and LE.105:	-accelerations of the ALE nodes
        GE.106 and LE.113:	-accelerations of the ALE nodes
        GE.114 and LE.121:	-accelerations of the ALE nodes
        GE.122 and LE.129:	masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[3].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Variable rank in the following list (see Remark 3):
        EQ.1:	-stress for MMGFR
        EQ.2:	-stress for MMGFR
        EQ.3:	-stress for MMGFR
        EQ.4:	-stress for MMGFR
        EQ.5:	-stress for MMGFR
        EQ.6:	-stress for MMGFR
        EQ.7:	plastic strain for MMGFR
        EQ.8:	internal energy for MMGFR
        EQ.9:	bulk viscosity for MMGFR
        EQ.10:	volume from previous cycle for MMGFR
        GE.11 and LE.20:	other auxiliary variables for MMGFR
        GE.21 and LE.40:	auxiliary variables for MMGTO (-stress, …)
        EQ.41:	mass for MMGFR
        EQ.42:	mass for MMGTO
        EQ.43:	volume fraction for MMGFR
        EQ.44:	volume fraction for MMGTO
        EQ.45:	material volume for MMGFR
        EQ.46:	material volume for MMGTO
        EQ.47:	time step
        EQ.48:	time
        EQ.49:	cycle
        GE.50 and LE.57:	-positions of the ALE nodes
        GE.58 and LE.65:	-positions of the ALE nodes
        GE.66 and LE.73:	-positions of the ALE nodes
        GE.74 and LE.81:	-velocities of the ALE nodes
        GE.82 and LE.89:	-velocities of the ALE nodes
        GE.90 and LE.97:	-velocities of the ALE nodes
        GE.98 and LE.105:	-accelerations of the ALE nodes
        GE.106 and LE.113:	-accelerations of the ALE nodes
        GE.114 and LE.121:	-accelerations of the ALE nodes
        GE.122 and LE.129:	masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[3].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Variable rank in the following list (see Remark 3):
        EQ.1:	-stress for MMGFR
        EQ.2:	-stress for MMGFR
        EQ.3:	-stress for MMGFR
        EQ.4:	-stress for MMGFR
        EQ.5:	-stress for MMGFR
        EQ.6:	-stress for MMGFR
        EQ.7:	plastic strain for MMGFR
        EQ.8:	internal energy for MMGFR
        EQ.9:	bulk viscosity for MMGFR
        EQ.10:	volume from previous cycle for MMGFR
        GE.11 and LE.20:	other auxiliary variables for MMGFR
        GE.21 and LE.40:	auxiliary variables for MMGTO (-stress, …)
        EQ.41:	mass for MMGFR
        EQ.42:	mass for MMGTO
        EQ.43:	volume fraction for MMGFR
        EQ.44:	volume fraction for MMGTO
        EQ.45:	material volume for MMGFR
        EQ.46:	material volume for MMGTO
        EQ.47:	time step
        EQ.48:	time
        EQ.49:	cycle
        GE.50 and LE.57:	-positions of the ALE nodes
        GE.58 and LE.65:	-positions of the ALE nodes
        GE.66 and LE.73:	-positions of the ALE nodes
        GE.74 and LE.81:	-velocities of the ALE nodes
        GE.82 and LE.89:	-velocities of the ALE nodes
        GE.90 and LE.97:	-velocities of the ALE nodes
        GE.98 and LE.105:	-accelerations of the ALE nodes
        GE.106 and LE.113:	-accelerations of the ALE nodes
        GE.114 and LE.121:	-accelerations of the ALE nodes
        GE.122 and LE.129:	masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[3].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Variable rank in the following list (see Remark 3):
        EQ.1:	-stress for MMGFR
        EQ.2:	-stress for MMGFR
        EQ.3:	-stress for MMGFR
        EQ.4:	-stress for MMGFR
        EQ.5:	-stress for MMGFR
        EQ.6:	-stress for MMGFR
        EQ.7:	plastic strain for MMGFR
        EQ.8:	internal energy for MMGFR
        EQ.9:	bulk viscosity for MMGFR
        EQ.10:	volume from previous cycle for MMGFR
        GE.11 and LE.20:	other auxiliary variables for MMGFR
        GE.21 and LE.40:	auxiliary variables for MMGTO (-stress, …)
        EQ.41:	mass for MMGFR
        EQ.42:	mass for MMGTO
        EQ.43:	volume fraction for MMGFR
        EQ.44:	volume fraction for MMGTO
        EQ.45:	material volume for MMGFR
        EQ.46:	material volume for MMGTO
        EQ.47:	time step
        EQ.48:	time
        EQ.49:	cycle
        GE.50 and LE.57:	-positions of the ALE nodes
        GE.58 and LE.65:	-positions of the ALE nodes
        GE.66 and LE.73:	-positions of the ALE nodes
        GE.74 and LE.81:	-velocities of the ALE nodes
        GE.82 and LE.89:	-velocities of the ALE nodes
        GE.90 and LE.97:	-velocities of the ALE nodes
        GE.98 and LE.105:	-accelerations of the ALE nodes
        GE.106 and LE.113:	-accelerations of the ALE nodes
        GE.114 and LE.121:	-accelerations of the ALE nodes
        GE.122 and LE.129:	masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[3].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Variable rank in the following list (see Remark 3):
        EQ.1:	-stress for MMGFR
        EQ.2:	-stress for MMGFR
        EQ.3:	-stress for MMGFR
        EQ.4:	-stress for MMGFR
        EQ.5:	-stress for MMGFR
        EQ.6:	-stress for MMGFR
        EQ.7:	plastic strain for MMGFR
        EQ.8:	internal energy for MMGFR
        EQ.9:	bulk viscosity for MMGFR
        EQ.10:	volume from previous cycle for MMGFR
        GE.11 and LE.20:	other auxiliary variables for MMGFR
        GE.21 and LE.40:	auxiliary variables for MMGTO (-stress, …)
        EQ.41:	mass for MMGFR
        EQ.42:	mass for MMGTO
        EQ.43:	volume fraction for MMGFR
        EQ.44:	volume fraction for MMGTO
        EQ.45:	material volume for MMGFR
        EQ.46:	material volume for MMGTO
        EQ.47:	time step
        EQ.48:	time
        EQ.49:	cycle
        GE.50 and LE.57:	-positions of the ALE nodes
        GE.58 and LE.65:	-positions of the ALE nodes
        GE.66 and LE.73:	-positions of the ALE nodes
        GE.74 and LE.81:	-velocities of the ALE nodes
        GE.82 and LE.89:	-velocities of the ALE nodes
        GE.90 and LE.97:	-velocities of the ALE nodes
        GE.98 and LE.105:	-accelerations of the ALE nodes
        GE.106 and LE.113:	-accelerations of the ALE nodes
        GE.114 and LE.121:	-accelerations of the ALE nodes
        GE.122 and LE.129:	masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[3].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Variable rank in the following list (see Remark 3):
        EQ.1:	-stress for MMGFR
        EQ.2:	-stress for MMGFR
        EQ.3:	-stress for MMGFR
        EQ.4:	-stress for MMGFR
        EQ.5:	-stress for MMGFR
        EQ.6:	-stress for MMGFR
        EQ.7:	plastic strain for MMGFR
        EQ.8:	internal energy for MMGFR
        EQ.9:	bulk viscosity for MMGFR
        EQ.10:	volume from previous cycle for MMGFR
        GE.11 and LE.20:	other auxiliary variables for MMGFR
        GE.21 and LE.40:	auxiliary variables for MMGTO (-stress, …)
        EQ.41:	mass for MMGFR
        EQ.42:	mass for MMGTO
        EQ.43:	volume fraction for MMGFR
        EQ.44:	volume fraction for MMGTO
        EQ.45:	material volume for MMGFR
        EQ.46:	material volume for MMGTO
        EQ.47:	time step
        EQ.48:	time
        EQ.49:	cycle
        GE.50 and LE.57:	-positions of the ALE nodes
        GE.58 and LE.65:	-positions of the ALE nodes
        GE.66 and LE.73:	-positions of the ALE nodes
        GE.74 and LE.81:	-velocities of the ALE nodes
        GE.82 and LE.89:	-velocities of the ALE nodes
        GE.90 and LE.97:	-velocities of the ALE nodes
        GE.98 and LE.105:	-accelerations of the ALE nodes
        GE.106 and LE.113:	-accelerations of the ALE nodes
        GE.114 and LE.121:	-accelerations of the ALE nodes
        GE.122 and LE.129:	masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[3].set_value("var", value)

    @property
    def var(self) -> int:
        """Get or set the Variable rank in the following list (see Remark 3):
        EQ.1:	-stress for MMGFR
        EQ.2:	-stress for MMGFR
        EQ.3:	-stress for MMGFR
        EQ.4:	-stress for MMGFR
        EQ.5:	-stress for MMGFR
        EQ.6:	-stress for MMGFR
        EQ.7:	plastic strain for MMGFR
        EQ.8:	internal energy for MMGFR
        EQ.9:	bulk viscosity for MMGFR
        EQ.10:	volume from previous cycle for MMGFR
        GE.11 and LE.20:	other auxiliary variables for MMGFR
        GE.21 and LE.40:	auxiliary variables for MMGTO (-stress, …)
        EQ.41:	mass for MMGFR
        EQ.42:	mass for MMGTO
        EQ.43:	volume fraction for MMGFR
        EQ.44:	volume fraction for MMGTO
        EQ.45:	material volume for MMGFR
        EQ.46:	material volume for MMGTO
        EQ.47:	time step
        EQ.48:	time
        EQ.49:	cycle
        GE.50 and LE.57:	-positions of the ALE nodes
        GE.58 and LE.65:	-positions of the ALE nodes
        GE.66 and LE.73:	-positions of the ALE nodes
        GE.74 and LE.81:	-velocities of the ALE nodes
        GE.82 and LE.89:	-velocities of the ALE nodes
        GE.90 and LE.97:	-velocities of the ALE nodes
        GE.98 and LE.105:	-accelerations of the ALE nodes
        GE.106 and LE.113:	-accelerations of the ALE nodes
        GE.114 and LE.121:	-accelerations of the ALE nodes
        GE.122 and LE.129:	masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[3].set_value("var", value)

    @property
    def par(self) -> float:
        """Get or set the User define routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par")

    @par.setter
    def par(self, value: float) -> None:
        """Set the par property."""
        self._cards[4].set_value("par", value)

    @property
    def par(self) -> float:
        """Get or set the User define routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par")

    @par.setter
    def par(self, value: float) -> None:
        """Set the par property."""
        self._cards[4].set_value("par", value)

    @property
    def par(self) -> float:
        """Get or set the User define routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par")

    @par.setter
    def par(self, value: float) -> None:
        """Set the par property."""
        self._cards[4].set_value("par", value)

    @property
    def par(self) -> float:
        """Get or set the User define routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par")

    @par.setter
    def par(self, value: float) -> None:
        """Set the par property."""
        self._cards[4].set_value("par", value)

    @property
    def par(self) -> float:
        """Get or set the User define routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par")

    @par.setter
    def par(self, value: float) -> None:
        """Set the par property."""
        self._cards[4].set_value("par", value)

    @property
    def par(self) -> float:
        """Get or set the User define routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par")

    @par.setter
    def par(self, value: float) -> None:
        """Set the par property."""
        self._cards[4].set_value("par", value)

    @property
    def par(self) -> float:
        """Get or set the User define routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par")

    @par.setter
    def par(self, value: float) -> None:
        """Set the par property."""
        self._cards[4].set_value("par", value)

    @property
    def par(self) -> float:
        """Get or set the User define routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par")

    @par.setter
    def par(self, value: float) -> None:
        """Set the par property."""
        self._cards[4].set_value("par", value)

