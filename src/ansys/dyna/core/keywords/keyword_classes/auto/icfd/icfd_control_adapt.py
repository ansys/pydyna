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
    FieldSchema("mth", int, 30, 10, 0),
    FieldSchema("nit", int, 40, 10, 0),
    FieldSchema("var", int, 50, 10, 0),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("kis", int, 70, 10, 0),
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
            ),        ]
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
        """Get or set the Specify if the mesh size is computed based on function error or gradient error. EQ. 0: Function error. EQ. 1: Gradient error.
        """ # nopep8
        return self._cards[0].get_value("mth")

    @mth.setter
    def mth(self, value: int) -> None:
        """Set the mth property."""
        if value not in [0, 1, None]:
            raise Exception("""mth must be `None` or one of {0,1}.""")
        self._cards[0].set_value("mth", value)

    @property
    def nit(self) -> int:
        """Get or set the Number of iterations before a remeshing is forced:
        GT.0:	Number of iterations before a forced remeshing
        EQ.0 : Do not remesh
        LT.0 : |NIT| is a load curve ID giving the number iterations before a remeshing as a function of time
        """ # nopep8
        return self._cards[0].get_value("nit")

    @nit.setter
    def nit(self, value: int) -> None:
        """Set the nit property."""
        self._cards[0].set_value("nit", value)

    @property
    def var(self) -> int:
        """Get or set the Specify which variable is taken into account for the error calculation:
        EQ.0:	Velocity, pressure and levelset function are taken into account.
        EQ.1:	Remove the levelset function from the error calculation.
        EQ.2: Remove the pressure from the error calculation.
        EQ.3: Remove both pressure and levelset function. Only the fluid velocity will therefore remain.
        """ # nopep8
        return self._cards[0].get_value("var")

    @var.setter
    def var(self, value: int) -> None:
        """Set the var property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""var must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("var", value)

    @property
    def kis(self) -> int:
        """Get or set the Keep initial mesh size:
        EQ.0:	Turned Off.The remeshing process will ignore the initial mesh size in the volume.
        EQ.1 : Turned on.Whenever a remeshing occurs, the new local mesh size will not be allowed to be substantially coarser than the one from the previous mesh.The object is to diminish the excessive coarsening that can occur between two remeshes
        """ # nopep8
        return self._cards[0].get_value("kis")

    @kis.setter
    def kis(self, value: int) -> None:
        """Set the kis property."""
        if value not in [0, 1, None]:
            raise Exception("""kis must be `None` or one of {0,1}.""")
        self._cards[0].set_value("kis", value)

