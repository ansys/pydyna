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
    FieldSchema("emsol", int, 0, 10, None),
    FieldSchema("numls", int, 10, 10, None),
    FieldSchema("biot", int, 20, 10, 0),
    FieldSchema("dimtype", int, 30, 10, 0),
    FieldSchema("nperio", int, 40, 10, 2),
    FieldSchema("eps0sf", float, 50, 10, 1.0),
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
            ),
        ]
    @property
    def emsol(self) -> typing.Optional[int]:
        """Get or set the Electromagnetism solver selector:
        EQ.-1: Turns the EM solver off after reading the EM keywords.
        EQ.1: Eddy current and magnetostatics solver
        EQ.2: Periodic inductive heating solver(see Remark 3)
        EQ.3: Resistive heating solver
        EQ.4: Frequency - based Eddy current solver(see Remark 3)
        EQ.5: Periodic resistive heating solver(see Remark 3)
        EQ.7: Helmholtz wave equation solver (see Remark 4)
        EQ.8: Quasistatic electrostatics solver(see Remark 5)
        EQ.9 : Radiofrequency(RF) Heating solver(see Remark 5)
        EQ.11: Electrophysiology monodomain
        EQ.12: Electrophysiology bidomain
        EQ.13: Electrophysiology monodomain coupled with bidomain
        EQ.14: Pure eikonal model. Activation times are computed and output in VTK format to the / vtk directory. See *EM_EP_EIKONAL.
        EQ.15: Reaction eikonal(RE) model based on[1]. See *EM_EP_EIKONAL.
        EQ.16: Reaction eikonal (RE+) model based on [1]. See *EM_EP_EIKONAL.
        """ # nopep8
        return self._cards[0].get_value("emsol")

    @emsol.setter
    def emsol(self, value: int) -> None:
        """Set the emsol property."""
        if value not in [-1, 1, 2, 3, 4, 5, 7, 8, 9, 11, 12, 13, 14, 15, 16, None]:
            raise Exception("""emsol must be `None` or one of {-1,1,2,3,4,5,7,8,9,11,12,13,14,15,16}.""")
        self._cards[0].set_value("emsol", value)

    @property
    def numls(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("numls")

    @numls.setter
    def numls(self, value: int) -> None:
        """Set the numls property."""
        self._cards[0].set_value("numls", value)

    @property
    def biot(self) -> int:
        """Get or set the Biot-Savart type calculations:
        EQ.0: No extra Biot-Savart type calculations
        EQ.1: When the resistive heat solver is selected(EMSOL = 3), this option triggers a Lorentz force calculation in beam elements that have been defined as conductors.For a magnetostatic solve(EMSOL = 1), this option causes the output of a binary file embiotout(see Remark 7).
        EQ.2 : This option causing reading in the embiotout binary file for the Biot - Savart solve.See Remark 7.
        """ # nopep8
        return self._cards[0].get_value("biot")

    @biot.setter
    def biot(self, value: int) -> None:
        """Set the biot property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""biot must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("biot", value)

    @property
    def dimtype(self) -> int:
        """Get or set the EM dimension type:
        EQ.0:3D solve.
        EQ.1:2D planar with 4-zero thickness shell elements.
        EQ.2:2D axisymmetric (Y axis only) with zero thickness elements.
        """ # nopep8
        return self._cards[0].get_value("dimtype")

    @dimtype.setter
    def dimtype(self, value: int) -> None:
        """Set the dimtype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""dimtype must be `None` or one of {0,1,2}.""")
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
    def eps0sf(self) -> float:
        """Get or set the Optional scale sactor on Permittivity ?_0 . See Remark 6.
        """ # nopep8
        return self._cards[0].get_value("eps0sf")

    @eps0sf.setter
    def eps0sf(self, value: float) -> None:
        """Set the eps0sf property."""
        self._cards[0].set_value("eps0sf", value)

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

