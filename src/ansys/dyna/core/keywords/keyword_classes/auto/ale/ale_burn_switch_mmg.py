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
    FieldSchema("var1", int, 0, 10, 0),
    FieldSchema("var2", int, 10, 10, 0),
    FieldSchema("var3", int, 20, 10, 0),
    FieldSchema("var4", int, 30, 10, 0),
    FieldSchema("var5", int, 40, 10, 0),
    FieldSchema("var6", int, 50, 10, 0),
    FieldSchema("var7", int, 60, 10, 0),
    FieldSchema("var8", int, 70, 10, 0),
)

_ALEBURNSWITCHMMG_CARD4 = (
    FieldSchema("par1", float, 0, 10, 0.0),
    FieldSchema("par2", float, 10, 10, 0.0),
    FieldSchema("par3", float, 20, 10, 0.0),
    FieldSchema("par4", float, 30, 10, 0.0),
    FieldSchema("par5", float, 40, 10, 0.0),
    FieldSchema("par6", float, 50, 10, 0.0),
    FieldSchema("par7", float, 60, 10, 0.0),
    FieldSchema("par8", float, 70, 10, 0.0),
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
            ),
            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ALEBURNSWITCHMMG_CARD4,
                **kwargs,
            ),
        ]
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
        """Get or set the Flag that activates computing the ignition front as a material interface between MMGFR and MMGTO. This flag will automatically turn if both IGNI and IGNIVF are undefined (see Remark 2).
        EQ.0: not activated
        EQ.1: activated.
        """ # nopep8
        return self._cards[2].get_value("ignivf")

    @ignivf.setter
    def ignivf(self, value: int) -> None:
        """Set the ignivf property."""
        if value not in [0, 1, None]:
            raise Exception("""ignivf must be `None` or one of {0,1}.""")
        self._cards[2].set_value("ignivf", value)

    @property
    def var1(self) -> int:
        """Get or set the Variable that provide the arguments for REACT, IGNI, and IGNIV (see Remark 3):
        EQ.1: -stress for MMGFR
        EQ.2: -stress for MMGFR
        EQ.3: -stress for MMGFR
        EQ.4: -stress for MMGFR
        EQ.5: -stress for MMGFR
        EQ.6: -stress for MMGFR
        EQ.7: plastic strain for MMGFR
        EQ.8: internal energy for MMGFR
        EQ.9: bulk viscosity for MMGFR
        EQ.10: volume from previous cycle for MMGFR
        GE.11 and LE.20: other auxiliary variables for MMGFR
        GE.21 and LE.40: auxiliary variables for MMGTO (-stress,)
        EQ.41: mass for MMGFR
        EQ.42: mass for MMGTO
        EQ.43: volume fraction for MMGFR
        EQ.44: volume fraction for MMGTO
        EQ.45: material volume for MMGFR
        EQ.46: material volume for MMGTO
        EQ.47: time step
        EQ.48: time
        EQ.49: cycle
        GE.50 and LE.57: -positions of the ALE nodes
        GE.58 and LE.65: -positions of the ALE nodes
        GE.66 and LE.73: -positions of the ALE nodes
        GE.74 and LE.81: -velocities of the ALE nodes
        GE.82 and LE.89: -velocities of the ALE nodes
        GE.90 and LE.97: -velocities of the ALE nodes
        GE.98 and LE.105: -accelerations of the ALE nodes
        GE.106 and LE.113: -accelerations of the ALE nodes
        GE.114 and LE.121: -accelerations of the ALE nodes
        GE.122 and LE.129: masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var1")

    @var1.setter
    def var1(self, value: int) -> None:
        """Set the var1 property."""
        self._cards[3].set_value("var1", value)

    @property
    def var2(self) -> int:
        """Get or set the Variable that provide the arguments for REACT, IGNI, and IGNIV (see Remark 3):
        EQ.1: -stress for MMGFR
        EQ.2: -stress for MMGFR
        EQ.3: -stress for MMGFR
        EQ.4: -stress for MMGFR
        EQ.5: -stress for MMGFR
        EQ.6: -stress for MMGFR
        EQ.7: plastic strain for MMGFR
        EQ.8: internal energy for MMGFR
        EQ.9: bulk viscosity for MMGFR
        EQ.10: volume from previous cycle for MMGFR
        GE.11 and LE.20: other auxiliary variables for MMGFR
        GE.21 and LE.40: auxiliary variables for MMGTO (-stress,)
        EQ.41: mass for MMGFR
        EQ.42: mass for MMGTO
        EQ.43: volume fraction for MMGFR
        EQ.44: volume fraction for MMGTO
        EQ.45: material volume for MMGFR
        EQ.46: material volume for MMGTO
        EQ.47: time step
        EQ.48: time
        EQ.49: cycle
        GE.50 and LE.57: -positions of the ALE nodes
        GE.58 and LE.65: -positions of the ALE nodes
        GE.66 and LE.73: -positions of the ALE nodes
        GE.74 and LE.81: -velocities of the ALE nodes
        GE.82 and LE.89: -velocities of the ALE nodes
        GE.90 and LE.97: -velocities of the ALE nodes
        GE.98 and LE.105: -accelerations of the ALE nodes
        GE.106 and LE.113: -accelerations of the ALE nodes
        GE.114 and LE.121: -accelerations of the ALE nodes
        GE.122 and LE.129: masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var2")

    @var2.setter
    def var2(self, value: int) -> None:
        """Set the var2 property."""
        self._cards[3].set_value("var2", value)

    @property
    def var3(self) -> int:
        """Get or set the Variable that provide the arguments for REACT, IGNI, and IGNIV (see Remark 3):
        EQ.1: -stress for MMGFR
        EQ.2: -stress for MMGFR
        EQ.3: -stress for MMGFR
        EQ.4: -stress for MMGFR
        EQ.5: -stress for MMGFR
        EQ.6: -stress for MMGFR
        EQ.7: plastic strain for MMGFR
        EQ.8: internal energy for MMGFR
        EQ.9: bulk viscosity for MMGFR
        EQ.10: volume from previous cycle for MMGFR
        GE.11 and LE.20: other auxiliary variables for MMGFR
        GE.21 and LE.40: auxiliary variables for MMGTO (-stress,)
        EQ.41: mass for MMGFR
        EQ.42: mass for MMGTO
        EQ.43: volume fraction for MMGFR
        EQ.44: volume fraction for MMGTO
        EQ.45: material volume for MMGFR
        EQ.46: material volume for MMGTO
        EQ.47: time step
        EQ.48: time
        EQ.49: cycle
        GE.50 and LE.57: -positions of the ALE nodes
        GE.58 and LE.65: -positions of the ALE nodes
        GE.66 and LE.73: -positions of the ALE nodes
        GE.74 and LE.81: -velocities of the ALE nodes
        GE.82 and LE.89: -velocities of the ALE nodes
        GE.90 and LE.97: -velocities of the ALE nodes
        GE.98 and LE.105: -accelerations of the ALE nodes
        GE.106 and LE.113: -accelerations of the ALE nodes
        GE.114 and LE.121: -accelerations of the ALE nodes
        GE.122 and LE.129: masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var3")

    @var3.setter
    def var3(self, value: int) -> None:
        """Set the var3 property."""
        self._cards[3].set_value("var3", value)

    @property
    def var4(self) -> int:
        """Get or set the Variable that provide the arguments for REACT, IGNI, and IGNIV (see Remark 3):
        EQ.1: -stress for MMGFR
        EQ.2: -stress for MMGFR
        EQ.3: -stress for MMGFR
        EQ.4: -stress for MMGFR
        EQ.5: -stress for MMGFR
        EQ.6: -stress for MMGFR
        EQ.7: plastic strain for MMGFR
        EQ.8: internal energy for MMGFR
        EQ.9: bulk viscosity for MMGFR
        EQ.10: volume from previous cycle for MMGFR
        GE.11 and LE.20: other auxiliary variables for MMGFR
        GE.21 and LE.40: auxiliary variables for MMGTO (-stress,)
        EQ.41: mass for MMGFR
        EQ.42: mass for MMGTO
        EQ.43: volume fraction for MMGFR
        EQ.44: volume fraction for MMGTO
        EQ.45: material volume for MMGFR
        EQ.46: material volume for MMGTO
        EQ.47: time step
        EQ.48: time
        EQ.49: cycle
        GE.50 and LE.57: -positions of the ALE nodes
        GE.58 and LE.65: -positions of the ALE nodes
        GE.66 and LE.73: -positions of the ALE nodes
        GE.74 and LE.81: -velocities of the ALE nodes
        GE.82 and LE.89: -velocities of the ALE nodes
        GE.90 and LE.97: -velocities of the ALE nodes
        GE.98 and LE.105: -accelerations of the ALE nodes
        GE.106 and LE.113: -accelerations of the ALE nodes
        GE.114 and LE.121: -accelerations of the ALE nodes
        GE.122 and LE.129: masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var4")

    @var4.setter
    def var4(self, value: int) -> None:
        """Set the var4 property."""
        self._cards[3].set_value("var4", value)

    @property
    def var5(self) -> int:
        """Get or set the Variable that provide the arguments for REACT, IGNI, and IGNIV (see Remark 3):
        EQ.1: -stress for MMGFR
        EQ.2: -stress for MMGFR
        EQ.3: -stress for MMGFR
        EQ.4: -stress for MMGFR
        EQ.5: -stress for MMGFR
        EQ.6: -stress for MMGFR
        EQ.7: plastic strain for MMGFR
        EQ.8: internal energy for MMGFR
        EQ.9: bulk viscosity for MMGFR
        EQ.10: volume from previous cycle for MMGFR
        GE.11 and LE.20: other auxiliary variables for MMGFR
        GE.21 and LE.40: auxiliary variables for MMGTO (-stress,)
        EQ.41: mass for MMGFR
        EQ.42: mass for MMGTO
        EQ.43: volume fraction for MMGFR
        EQ.44: volume fraction for MMGTO
        EQ.45: material volume for MMGFR
        EQ.46: material volume for MMGTO
        EQ.47: time step
        EQ.48: time
        EQ.49: cycle
        GE.50 and LE.57: -positions of the ALE nodes
        GE.58 and LE.65: -positions of the ALE nodes
        GE.66 and LE.73: -positions of the ALE nodes
        GE.74 and LE.81: -velocities of the ALE nodes
        GE.82 and LE.89: -velocities of the ALE nodes
        GE.90 and LE.97: -velocities of the ALE nodes
        GE.98 and LE.105: -accelerations of the ALE nodes
        GE.106 and LE.113: -accelerations of the ALE nodes
        GE.114 and LE.121: -accelerations of the ALE nodes
        GE.122 and LE.129: masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var5")

    @var5.setter
    def var5(self, value: int) -> None:
        """Set the var5 property."""
        self._cards[3].set_value("var5", value)

    @property
    def var6(self) -> int:
        """Get or set the Variable that provide the arguments for REACT, IGNI, and IGNIV (see Remark 3):
        EQ.1: -stress for MMGFR
        EQ.2: -stress for MMGFR
        EQ.3: -stress for MMGFR
        EQ.4: -stress for MMGFR
        EQ.5: -stress for MMGFR
        EQ.6: -stress for MMGFR
        EQ.7: plastic strain for MMGFR
        EQ.8: internal energy for MMGFR
        EQ.9: bulk viscosity for MMGFR
        EQ.10: volume from previous cycle for MMGFR
        GE.11 and LE.20: other auxiliary variables for MMGFR
        GE.21 and LE.40: auxiliary variables for MMGTO (-stress,)
        EQ.41: mass for MMGFR
        EQ.42: mass for MMGTO
        EQ.43: volume fraction for MMGFR
        EQ.44: volume fraction for MMGTO
        EQ.45: material volume for MMGFR
        EQ.46: material volume for MMGTO
        EQ.47: time step
        EQ.48: time
        EQ.49: cycle
        GE.50 and LE.57: -positions of the ALE nodes
        GE.58 and LE.65: -positions of the ALE nodes
        GE.66 and LE.73: -positions of the ALE nodes
        GE.74 and LE.81: -velocities of the ALE nodes
        GE.82 and LE.89: -velocities of the ALE nodes
        GE.90 and LE.97: -velocities of the ALE nodes
        GE.98 and LE.105: -accelerations of the ALE nodes
        GE.106 and LE.113: -accelerations of the ALE nodes
        GE.114 and LE.121: -accelerations of the ALE nodes
        GE.122 and LE.129: masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var6")

    @var6.setter
    def var6(self, value: int) -> None:
        """Set the var6 property."""
        self._cards[3].set_value("var6", value)

    @property
    def var7(self) -> int:
        """Get or set the Variable that provide the arguments for REACT, IGNI, and IGNIV (see Remark 3):
        EQ.1: -stress for MMGFR
        EQ.2: -stress for MMGFR
        EQ.3: -stress for MMGFR
        EQ.4: -stress for MMGFR
        EQ.5: -stress for MMGFR
        EQ.6: -stress for MMGFR
        EQ.7: plastic strain for MMGFR
        EQ.8: internal energy for MMGFR
        EQ.9: bulk viscosity for MMGFR
        EQ.10: volume from previous cycle for MMGFR
        GE.11 and LE.20: other auxiliary variables for MMGFR
        GE.21 and LE.40: auxiliary variables for MMGTO (-stress,)
        EQ.41: mass for MMGFR
        EQ.42: mass for MMGTO
        EQ.43: volume fraction for MMGFR
        EQ.44: volume fraction for MMGTO
        EQ.45: material volume for MMGFR
        EQ.46: material volume for MMGTO
        EQ.47: time step
        EQ.48: time
        EQ.49: cycle
        GE.50 and LE.57: -positions of the ALE nodes
        GE.58 and LE.65: -positions of the ALE nodes
        GE.66 and LE.73: -positions of the ALE nodes
        GE.74 and LE.81: -velocities of the ALE nodes
        GE.82 and LE.89: -velocities of the ALE nodes
        GE.90 and LE.97: -velocities of the ALE nodes
        GE.98 and LE.105: -accelerations of the ALE nodes
        GE.106 and LE.113: -accelerations of the ALE nodes
        GE.114 and LE.121: -accelerations of the ALE nodes
        GE.122 and LE.129: masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var7")

    @var7.setter
    def var7(self, value: int) -> None:
        """Set the var7 property."""
        self._cards[3].set_value("var7", value)

    @property
    def var8(self) -> int:
        """Get or set the Variable that provide the arguments for REACT, IGNI, and IGNIV (see Remark 3):
        EQ.1: -stress for MMGFR
        EQ.2: -stress for MMGFR
        EQ.3: -stress for MMGFR
        EQ.4: -stress for MMGFR
        EQ.5: -stress for MMGFR
        EQ.6: -stress for MMGFR
        EQ.7: plastic strain for MMGFR
        EQ.8: internal energy for MMGFR
        EQ.9: bulk viscosity for MMGFR
        EQ.10: volume from previous cycle for MMGFR
        GE.11 and LE.20: other auxiliary variables for MMGFR
        GE.21 and LE.40: auxiliary variables for MMGTO (-stress,)
        EQ.41: mass for MMGFR
        EQ.42: mass for MMGTO
        EQ.43: volume fraction for MMGFR
        EQ.44: volume fraction for MMGTO
        EQ.45: material volume for MMGFR
        EQ.46: material volume for MMGTO
        EQ.47: time step
        EQ.48: time
        EQ.49: cycle
        GE.50 and LE.57: -positions of the ALE nodes
        GE.58 and LE.65: -positions of the ALE nodes
        GE.66 and LE.73: -positions of the ALE nodes
        GE.74 and LE.81: -velocities of the ALE nodes
        GE.82 and LE.89: -velocities of the ALE nodes
        GE.90 and LE.97: -velocities of the ALE nodes
        GE.98 and LE.105: -accelerations of the ALE nodes
        GE.106 and LE.113: -accelerations of the ALE nodes
        GE.114 and LE.121: -accelerations of the ALE nodes
        GE.122 and LE.129: masses of the ALE nodes.
        """ # nopep8
        return self._cards[3].get_value("var8")

    @var8.setter
    def var8(self, value: int) -> None:
        """Set the var8 property."""
        self._cards[3].set_value("var8", value)

    @property
    def par1(self) -> float:
        """Get or set the User-defined routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par1")

    @par1.setter
    def par1(self, value: float) -> None:
        """Set the par1 property."""
        self._cards[4].set_value("par1", value)

    @property
    def par2(self) -> float:
        """Get or set the User-defined routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par2")

    @par2.setter
    def par2(self, value: float) -> None:
        """Set the par2 property."""
        self._cards[4].set_value("par2", value)

    @property
    def par3(self) -> float:
        """Get or set the User-defined routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par3")

    @par3.setter
    def par3(self, value: float) -> None:
        """Set the par3 property."""
        self._cards[4].set_value("par3", value)

    @property
    def par4(self) -> float:
        """Get or set the User-defined routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par4")

    @par4.setter
    def par4(self, value: float) -> None:
        """Set the par4 property."""
        self._cards[4].set_value("par4", value)

    @property
    def par5(self) -> float:
        """Get or set the User-defined routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par5")

    @par5.setter
    def par5(self, value: float) -> None:
        """Set the par5 property."""
        self._cards[4].set_value("par5", value)

    @property
    def par6(self) -> float:
        """Get or set the User-defined routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par6")

    @par6.setter
    def par6(self, value: float) -> None:
        """Set the par6 property."""
        self._cards[4].set_value("par6", value)

    @property
    def par7(self) -> float:
        """Get or set the User-defined routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par7")

    @par7.setter
    def par7(self, value: float) -> None:
        """Set the par7 property."""
        self._cards[4].set_value("par7", value)

    @property
    def par8(self) -> float:
        """Get or set the User-defined routines parameters.
        """ # nopep8
        return self._cards[4].get_value("par8")

    @par8.setter
    def par8(self, value: float) -> None:
        """Set the par8 property."""
        self._cards[4].set_value("par8", value)

