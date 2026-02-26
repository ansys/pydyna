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

"""Module providing the IcfdControlOutputVar class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLOUTPUTVAR_CARD0 = (
    FieldSchema("vel", int, 0, 10, 0),
    FieldSchema("avgvel", int, 10, 10, 0),
    FieldSchema("vort", int, 20, 10, 0),
)

_ICFDCONTROLOUTPUTVAR_CARD1 = (
    FieldSchema("pre", int, 0, 10, 0),
    FieldSchema("preavg", int, 10, 10, 0),
    FieldSchema("lset", int, 20, 10, 0),
    FieldSchema("oc", int, 30, 10, 0),
    FieldSchema("cfl", int, 40, 10, 0),
)

_ICFDCONTROLOUTPUTVAR_CARD2 = (
    FieldSchema("temp", int, 0, 10, 0),
    FieldSchema("tempavg", int, 10, 10, 0),
)

_ICFDCONTROLOUTPUTVAR_CARD3 = (
    FieldSchema("kp", int, 0, 10, 0),
    FieldSchema("ep", int, 10, 10, 0),
    FieldSchema("mut", int, 20, 10, 0),
    FieldSchema("int_", int, 30, 10, 0, "int"),
    FieldSchema("cmu", int, 40, 10, 0),
)

class IcfdControlOutputVar(KeywordBase):
    """DYNA ICFD_CONTROL_OUTPUT_VAR keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_OUTPUT_VAR"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlOutputVar class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLOUTPUTVAR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLOUTPUTVAR_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLOUTPUTVAR_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLOUTPUTVAR_CARD3,
                **kwargs,
            ),        ]
    @property
    def vel(self) -> int:
        """Get or set the Velocity :
        EQ.0:	Is output.
        EQ.1:	Is not output.
        """ # nopep8
        return self._cards[0].get_value("vel")

    @vel.setter
    def vel(self, value: int) -> None:
        """Set the vel property."""
        if value not in [0, 1, None]:
            raise Exception("""vel must be `None` or one of {0,1}.""")
        self._cards[0].set_value("vel", value)

    @property
    def avgvel(self) -> int:
        """Get or set the average velocity :
        EQ.0:	Is output.
        EQ.1:	Is not output.
        """ # nopep8
        return self._cards[0].get_value("avgvel")

    @avgvel.setter
    def avgvel(self, value: int) -> None:
        """Set the avgvel property."""
        if value not in [0, 1, None]:
            raise Exception("""avgvel must be `None` or one of {0,1}.""")
        self._cards[0].set_value("avgvel", value)

    @property
    def vort(self) -> int:
        """Get or set the vorticity :
        EQ.0:	Is output.
        EQ.1:	Is not output.
        """ # nopep8
        return self._cards[0].get_value("vort")

    @vort.setter
    def vort(self, value: int) -> None:
        """Set the vort property."""
        if value not in [0, 1, None]:
            raise Exception("""vort must be `None` or one of {0,1}.""")
        self._cards[0].set_value("vort", value)

    @property
    def pre(self) -> int:
        """Get or set the pressure:
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("pre")

    @pre.setter
    def pre(self, value: int) -> None:
        """Set the pre property."""
        if value not in [0, 1, None]:
            raise Exception("""pre must be `None` or one of {0,1}.""")
        self._cards[1].set_value("pre", value)

    @property
    def preavg(self) -> int:
        """Get or set the average pressure, levelset, Q criterion, CFL number :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("preavg")

    @preavg.setter
    def preavg(self, value: int) -> None:
        """Set the preavg property."""
        if value not in [0, 1, None]:
            raise Exception("""preavg must be `None` or one of {0,1}.""")
        self._cards[1].set_value("preavg", value)

    @property
    def lset(self) -> int:
        """Get or set the levelset :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("lset")

    @lset.setter
    def lset(self, value: int) -> None:
        """Set the lset property."""
        if value not in [0, 1, None]:
            raise Exception("""lset must be `None` or one of {0,1}.""")
        self._cards[1].set_value("lset", value)

    @property
    def oc(self) -> int:
        """Get or set the Q criterion:
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("oc")

    @oc.setter
    def oc(self, value: int) -> None:
        """Set the oc property."""
        if value not in [0, 1, None]:
            raise Exception("""oc must be `None` or one of {0,1}.""")
        self._cards[1].set_value("oc", value)

    @property
    def cfl(self) -> int:
        """Get or set the CFL number :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("cfl")

    @cfl.setter
    def cfl(self, value: int) -> None:
        """Set the cfl property."""
        if value not in [0, 1, None]:
            raise Exception("""cfl must be `None` or one of {0,1}.""")
        self._cards[1].set_value("cfl", value)

    @property
    def temp(self) -> int:
        """Get or set the Temperature :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[2].get_value("temp")

    @temp.setter
    def temp(self, value: int) -> None:
        """Set the temp property."""
        if value not in [0, 1, None]:
            raise Exception("""temp must be `None` or one of {0,1}.""")
        self._cards[2].set_value("temp", value)

    @property
    def tempavg(self) -> int:
        """Get or set the average temperature  :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[2].get_value("tempavg")

    @tempavg.setter
    def tempavg(self, value: int) -> None:
        """Set the tempavg property."""
        if value not in [0, 1, None]:
            raise Exception("""tempavg must be `None` or one of {0,1}.""")
        self._cards[2].set_value("tempavg", value)

    @property
    def kp(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("kp")

    @kp.setter
    def kp(self, value: int) -> None:
        """Set the kp property."""
        if value not in [0, 1, None]:
            raise Exception("""kp must be `None` or one of {0,1}.""")
        self._cards[3].set_value("kp", value)

    @property
    def ep(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("ep")

    @ep.setter
    def ep(self, value: int) -> None:
        """Set the ep property."""
        if value not in [0, 1, None]:
            raise Exception("""ep must be `None` or one of {0,1}.""")
        self._cards[3].set_value("ep", value)

    @property
    def mut(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("mut")

    @mut.setter
    def mut(self, value: int) -> None:
        """Set the mut property."""
        if value not in [0, 1, None]:
            raise Exception("""mut must be `None` or one of {0,1}.""")
        self._cards[3].set_value("mut", value)

    @property
    def int_(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("int_")

    @int_.setter
    def int_(self, value: int) -> None:
        """Set the int_ property."""
        if value not in [0, 1, None]:
            raise Exception("""int_ must be `None` or one of {0,1}.""")
        self._cards[3].set_value("int_", value)

    @property
    def cmu(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("cmu")

    @cmu.setter
    def cmu(self, value: int) -> None:
        """Set the cmu property."""
        if value not in [0, 1, None]:
            raise Exception("""cmu must be `None` or one of {0,1}.""")
        self._cards[3].set_value("cmu", value)

