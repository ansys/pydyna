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

"""Module providing the IcfdControlAdapt class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLADAPT_CARD0 = (
    FieldSchema("minh", float, 0, 10, None),
    FieldSchema("maxh", float, 10, 10, None),
    FieldSchema("err", float, 20, 10, 1.0),
    FieldSchema("mth", int, 30, 10, 1),
    FieldSchema("nit", int, 40, 10, 0),
    FieldSchema("var", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("kis", int, 70, 10, 0),
)

_ICFDCONTROLADAPT_CARD1 = (
    FieldSchema("dt", float, 0, 10, None),
)

class IcfdControlAdapt(KeywordBase):
    """DYNA ICFD_CONTROL_ADAPT keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_ADAPT"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlAdapt class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLADAPT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLADAPT_CARD1,
                **kwargs,
            ),
        ]
    @property
    def minh(self) -> typing.Optional[float]:
        """Get or set the Minimum mesh size allowed to the mesh generator. The resulting mesh will not have an element smaller than MINH even if the minimum size does not satisfy the maximum error.
        """ # nopep8
        return self._cards[0].get_value("minh")

    @minh.setter
    def minh(self, value: float) -> None:
        """Set the minh property."""
        self._cards[0].set_value("minh", value)

    @property
    def maxh(self) -> typing.Optional[float]:
        """Get or set the Maximum mesh size.
        """ # nopep8
        return self._cards[0].get_value("maxh")

    @maxh.setter
    def maxh(self, value: float) -> None:
        """Set the maxh property."""
        self._cards[0].set_value("maxh", value)

    @property
    def err(self) -> float:
        """Get or set the Maximum perceptual error allowed in the whole domain.
        """ # nopep8
        return self._cards[0].get_value("err")

    @err.setter
    def err(self, value: float) -> None:
        """Set the err property."""
        self._cards[0].set_value("err", value)

    @property
    def mth(self) -> int:
        """Get or set the Specify if the mesh size is computed based on function error or gradient error:
        EQ.1: Function error
        EQ.2: Gradient error
        EQ.3: Relative error
        EQ.4: Gradient relative error
        """ # nopep8
        return self._cards[0].get_value("mth")

    @mth.setter
    def mth(self, value: int) -> None:
        """Set the mth property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""mth must be `None` or one of {1,2,3,4}.""")
        self._cards[0].set_value("mth", value)

    @property
    def nit(self) -> int:
        """Get or set the Number of iterations before a remeshing is forced:
        LT.0: | NIT | is a load curve ID giving the number of iterations before a remeshing as a function of time.
        EQ.0: Do not remesh.
        GT.0: Number of iterations before a forced remeshing.
        """ # nopep8
        return self._cards[0].get_value("nit")

    @nit.setter
    def nit(self, value: int) -> None:
        """Set the nit property."""
        self._cards[0].set_value("nit", value)

    @property
    def var(self) -> int:
        """Get or set the Specify which variable is taken into account for the error calculation:
        LT.0: Adaptive meshing is based only on the level-set function. Elements of size MINH are targeted over a length |VAR| on each side of the interface.
        EQ.0: Velocity, pressureand level-set function are taken into account.
        EQ.1: Remove the level-set function from the error calculation.
        EQ.2: Remove the pressure from the error calculation.
        EQ.3: Remove both the pressure and the level-set function from the error calculation.Only the fluid velocity will, therefore, remain.
        EQ.5: For immersed interface methods, adapt the mesh only at the interface of the immersed surfaces.
        EQ.10:	For species transport problems, use species concentrations and the velocity field for the error calculation.
        """ # nopep8
        return self._cards[0].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        self._cards[0].set_value("var", value)

    @property
    def kis(self) -> int:
        """Get or set the Keep initial mesh size:
        EQ.0: Turned Off. The remeshing process will ignore the initial mesh size in the volume.
        EQ.1: Turned on. Whenever a remeshing occurs, the new local mesh size will not be allowed to be substantially coarser than the one from the previous mesh.The object is to diminish the excessive coarsening that can occur between two remeshing
        """ # nopep8
        return self._cards[0].get_value("kis")

    @kis.setter
    def kis(self, value: int) -> None:
        """Set the kis property."""
        if value not in [0, 1, None]:
            raise Exception("""kis must be `None` or one of {0,1}.""")
        self._cards[0].set_value("kis", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Optional time step to control the remeshing frequency. A negative value points to a load curve ID giving the remeshing time step as a function of time.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[1].set_value("dt", value)

