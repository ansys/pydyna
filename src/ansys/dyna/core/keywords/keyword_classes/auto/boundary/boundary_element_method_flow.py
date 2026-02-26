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

"""Module providing the BoundaryElementMethodFlow class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYELEMENTMETHODFLOW_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("vx", float, 10, 10, None),
    FieldSchema("vy", float, 20, 10, None),
    FieldSchema("vz", float, 30, 10, None),
    FieldSchema("ro", float, 40, 10, None),
    FieldSchema("pstatic", float, 50, 10, 0.0),
    FieldSchema("mach", float, 60, 10, 0.0),
)

class BoundaryElementMethodFlow(KeywordBase):
    """DYNA BOUNDARY_ELEMENT_METHOD_FLOW keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ELEMENT_METHOD_FLOW"

    def __init__(self, **kwargs):
        """Initialize the BoundaryElementMethodFlow class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYELEMENTMETHODFLOW_CARD0,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Shell set ID for the set of shell elements which define the surface of the bodies of interest (see *SET_SHELL). The nodes of these shells should be ordered so that the shell normals point into the fluid.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the x-component of the free-stream fluid velocity.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the y-component of the free-stream fluid velocity.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the z-component of the free-stream fluid velocity.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def pstatic(self) -> float:
        """Get or set the Fluid static pressure.
        """ # nopep8
        return self._cards[0].get_value("pstatic")

    @pstatic.setter
    def pstatic(self, value: float) -> None:
        """Set the pstatic property."""
        self._cards[0].set_value("pstatic", value)

    @property
    def mach(self) -> float:
        """Get or set the Free-stream Mach number.
        """ # nopep8
        return self._cards[0].get_value("mach")

    @mach.setter
    def mach(self, value: float) -> None:
        """Set the mach property."""
        self._cards[0].set_value("mach", value)

