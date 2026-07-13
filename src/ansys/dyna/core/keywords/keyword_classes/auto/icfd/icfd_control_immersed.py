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

"""Module providing the IcfdControlImmersed class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLIMMERSED_CARD0 = (
    FieldSchema("immth", int, 0, 10, 0),
    FieldSchema("issta", int, 10, 10, 0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("nitbs", int, 30, 10, None),
)

_ICFDCONTROLIMMERSED_CARD1 = (
    FieldSchema("pid", int, 0, 10, None),
)

class IcfdControlImmersed(KeywordBase):
    """DYNA ICFD_CONTROL_IMMERSED keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_IMMERSED"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlImmersed class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLIMMERSED_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLIMMERSED_CARD1,
                **kwargs,
            ),
        ]
    @property
    def immth(self) -> int:
        """Get or set the Method used to represent the immersed interface. By default, the solver assumes that all interfaces of interest are body-fitted.
        EQ.0: Immersed methods are not used in the model.
        EQ.1: Method based on a discontinuous FEM approximation at the interface
        EQ.2: Method based on the Resistive Immersed Implicit Surface(RIIS) method.This option needs DEM particles on the surface of immersed structural parts. See *DEFINE_DE_MESH_SURFACE.Note that prior to R17, using this method required *ICFD_CONTROL_DEM_COUPLING to specify the coupling between the DEM particles and fluid. With R17, *ICFD_CONTROL_IMMERSED_FSI controls the coupling for all immersed interfaces, and *ICFD_CONTROL_DEM_COUPLING is no longer needed
        EQ.4:	Method based on the Resistive Immersed Implicit Surface (RIIS) method. The fluid solver detects structural parts and computes the interfaces automatically.
        """ # nopep8
        return self._cards[0].get_value("immth")

    @immth.setter
    def immth(self, value: int) -> None:
        """Set the immth property."""
        if value not in [0, 1, 2, 4, None]:
            raise Exception("""immth must be `None` or one of {0,1,2,4}.""")
        self._cards[0].set_value("immth", value)

    @property
    def issta(self) -> int:
        """Get or set the Flag for specifying whether the immersed interface remains unchanged (static) during the simulation:
        EQ.0:The immersed interface is dynamic. Frequent interface computations are needed.
        EQ.1:The immersed interface is static. The interface is computed at the first time step or after re-meshing steps only. This option saves computation time.
        """ # nopep8
        return self._cards[0].get_value("issta")

    @issta.setter
    def issta(self, value: int) -> None:
        """Set the issta property."""
        if value not in [0, 1, None]:
            raise Exception("""issta must be `None` or one of {0,1}.""")
        self._cards[0].set_value("issta", value)

    @property
    def nitbs(self) -> typing.Optional[int]:
        """Get or set the Indicate how often the interface is computed. The default value depends on the mechanical solver (explicit or implicit) for the structure.  For the explicit mechanical solver, the default is every 50 time steps, while for implict, the default is every time step
        """ # nopep8
        return self._cards[0].get_value("nitbs")

    @nitbs.setter
    def nitbs(self, value: int) -> None:
        """Set the nitbs property."""
        self._cards[0].set_value("nitbs", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the structural element that is used to compute the immersed interface. If no PID is defined in this card, the solver uses all the structural parts.This field is only available for IMMTH = 1 and 4.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

