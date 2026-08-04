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

"""Module providing the BoundaryFluidm class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_BOUNDARYFLUIDM_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("rhof", float, 10, 10, None),
    FieldSchema("ieigen", int, 20, 10, None),
    FieldSchema("icurv", int, 30, 10, None),
    FieldSchema("besimf", int, 30, 10, 0),
)

class BoundaryFluidm(KeywordBase):
    """DYNA BOUNDARY_FLUIDM keyword"""

    keyword = "BOUNDARY"
    subkeyword = "FLUIDM"
    _link_fields = {
        "ssid": LinkType.SET_SEGMENT,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundaryFluidm class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUIDM_CARD0,
                **kwargs,
            ),
        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID (see *SET_SEGMENT) for the structural faces on which the fluid boundary mass is to be calculated
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def rhof(self) -> typing.Optional[float]:
        """Get or set the Fluid mass density in the same units as the structural model
        """ # nopep8
        return self._cards[0].get_value("rhof")

    @rhof.setter
    def rhof(self, value: float) -> None:
        """Set the rhof property."""
        self._cards[0].set_value("rhof", value)

    @property
    def ieigen(self) -> typing.Optional[int]:
        """Get or set the Eigenvalue solution flag:
        EQ.0: The fluid mass will be used directly in the calculation of the in - fluid structural modes of vibration.
        EQ.1: A superelement for the fluid - added mass matrix will be written to file FLUIDM.DMIG,and then the solution will stop.This is only advisable for very small matrices.
        """ # nopep8
        return self._cards[0].get_value("ieigen")

    @ieigen.setter
    def ieigen(self, value: int) -> None:
        """Set the ieigen property."""
        self._cards[0].set_value("ieigen", value)

    @property
    def icurv(self) -> typing.Optional[int]:
        """Get or set the Curvature usage flag:
        EQ.0: If no segment radii of curvature are included in the segment set, they will be estimated from the angle between the neighboring segments.
        EQ.1: Disable estimating the radii of curvature.
        """ # nopep8
        return self._cards[0].get_value("icurv")

    @icurv.setter
    def icurv(self, value: int) -> None:
        """Set the icurv property."""
        self._cards[0].set_value("icurv", value)

    @property
    def besimf(self) -> int:
        """Get or set the Boundary element solution method for external fluid mass:
        EQ.0: LAPACK direct matrix solution
        EQ.1: Block low rank(BLR) direct solution without pivoting
        EQ.2: Pseudo - block GMRES iterative solution
        """ # nopep8
        return self._cards[0].get_value("besimf")

    @besimf.setter
    def besimf(self, value: int) -> None:
        """Set the besimf property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""besimf must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("besimf", value)

    @property
    def ssid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_SEGMENT_* keyword for ssid."""
        return self._get_set_link("SEGMENT", self.ssid)

    @ssid_link.setter
    def ssid_link(self, value: KeywordBase) -> None:
        """Set the SET_SEGMENT_* keyword for ssid."""
        self.ssid = value.sid

