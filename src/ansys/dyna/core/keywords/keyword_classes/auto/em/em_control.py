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

"""Module providing the EmControl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMCONTROL_CARD0 = (
    FieldSchema("emsol", int, 0, 10, -1),
    FieldSchema("numls", int, 10, 10, 100),
    FieldSchema("macrodt", float, 20, 10, None),
    FieldSchema("dimtype", int, 30, 10, 0),
    FieldSchema("nperio", int, 40, 10, 2),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("ncylfem", int, 60, 10, 5000),
    FieldSchema("ncylbem", int, 70, 10, 5000),
)

class EmControl(KeywordBase):
    """DYNA EM_CONTROL keyword"""

    keyword = "EM"
    subkeyword = "CONTROL"

    def __init__(self, **kwargs):
        """Initialize the EmControl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTROL_CARD0,
                **kwargs,
            ),        ]
    @property
    def emsol(self) -> int:
        """Get or set the Electromagnetism solver selector:
        EQ.-1:Turns the EM solver off after reading the EM keywords.
        EQ.1:eddy current solver.
        EQ.2:induced heating solver.
        EQ.3:resistive heating solver.
        EQ.11:Electrophysiology monodomain.
        EQ.12:Electrophysiology bidomain.
        EQ.13:Electrophysiology monodmain coupled with bidomain.

        """ # nopep8
        return self._cards[0].get_value("emsol")

    @emsol.setter
    def emsol(self, value: int) -> None:
        """Set the emsol property."""
        if value not in [-1, 1, 2, 3, 11, 12, 13, None]:
            raise Exception("""emsol must be `None` or one of {-1,1,2,3,11,12,13}.""")
        self._cards[0].set_value("emsol", value)

    @property
    def numls(self) -> int:
        """Get or set the Number of local EM steps in A whole period for EMSOL=2 If a negative value is entered, it will give the number of local EM steps as a function of the macro time
        """ # nopep8
        return self._cards[0].get_value("numls")

    @numls.setter
    def numls(self, value: int) -> None:
        """Set the numls property."""
        self._cards[0].set_value("numls", value)

    @property
    def macrodt(self) -> typing.Optional[float]:
        """Get or set the Macro timestep when EMSOL = 2.
        """ # nopep8
        return self._cards[0].get_value("macrodt")

    @macrodt.setter
    def macrodt(self, value: float) -> None:
        """Set the macrodt property."""
        self._cards[0].set_value("macrodt", value)

    @property
    def dimtype(self) -> int:
        """Get or set the EM dimension type:
        EQ.0:3D solve.
        EQ.1:2D planar with 4-zero thickness shell elements.
        EQ.3:2D axisymmetric (Y axis only) with zero thickness elements.
        """ # nopep8
        return self._cards[0].get_value("dimtype")

    @dimtype.setter
    def dimtype(self, value: int) -> None:
        """Set the dimtype property."""
        if value not in [0, 1, 3, None]:
            raise Exception("""dimtype must be `None` or one of {0,1,3}.""")
        self._cards[0].set_value("dimtype", value)

    @property
    def nperio(self) -> int:
        """Get or set the Number of periods for which the last is used to calculate the average Joule heat rate when EMSOL=2. NPERIO=2 means that two periods of NUMLS steps will be calculated. Only the last period of NPERIO is used for the average Joule heat calculation
        """ # nopep8
        return self._cards[0].get_value("nperio")

    @nperio.setter
    def nperio(self, value: int) -> None:
        """Set the nperio property."""
        self._cards[0].set_value("nperio", value)

    @property
    def ncylfem(self) -> int:
        """Get or set the Number of electromagnetism cycles between the recomputation of EM-FEM matrices,If a negative value is entered, then the absolute value refers to a load curve giving the number of electromagnetism cysles as function of time.
        """ # nopep8
        return self._cards[0].get_value("ncylfem")

    @ncylfem.setter
    def ncylfem(self, value: int) -> None:
        """Set the ncylfem property."""
        self._cards[0].set_value("ncylfem", value)

    @property
    def ncylbem(self) -> int:
        """Get or set the Number of electromagnetism cycles between the recomputation of EM-BEM matrices,If a negative value is entered, then the absolute value refers to a load curve giving the number of electomagnetism cycles as function of time.
        """ # nopep8
        return self._cards[0].get_value("ncylbem")

    @ncylbem.setter
    def ncylbem(self, value: int) -> None:
        """Set the ncylbem property."""
        self._cards[0].set_value("ncylbem", value)

