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

"""Module providing the ControlRemeshing class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlRemeshing(KeywordBase):
    """DYNA CONTROL_REMESHING keyword"""

    keyword = "CONTROL"
    subkeyword = "REMESHING"

    def __init__(self, **kwargs):
        """Initialize the ControlRemeshing class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "rmin",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "rmax",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "vf_loss",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "mfrac",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dt_min",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "icurv",
                        int,
                        50,
                        10,
                        4,
                        **kwargs,
                    ),
                    Field(
                        "iadp10",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "sefang",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def rmin(self) -> typing.Optional[float]:
        """Get or set the Minimum edge length for the surface mesh surrounding the parts which should be remeshed.
        """ # nopep8
        return self._cards[0].get_value("rmin")

    @rmin.setter
    def rmin(self, value: float) -> None:
        """Set the rmin property."""
        self._cards[0].set_value("rmin", value)

    @property
    def rmax(self) -> typing.Optional[float]:
        """Get or set the Maximum edge length for the surface mesh surrounding the parts which should be remeshed.
        """ # nopep8
        return self._cards[0].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        """Set the rmax property."""
        self._cards[0].set_value("rmax", value)

    @property
    def vf_loss(self) -> float:
        """Get or set the Volume fraction loss required in a type 10/13 tetrahedral elements to trigger a remesh.  For the type 13 solid elements, the pressures are computed at the nodal points; therefore, it is possible for overall volume to be conserved but for individual tetrahedrons to experience a significant volume loss or gain.  The volume loss can lead to numerical problems.  Recommended values for VF_‌LOSS in the range of 0.10 to 0.30 may be reasonable.
        """ # nopep8
        return self._cards[0].get_value("vf_loss")

    @vf_loss.setter
    def vf_loss(self, value: float) -> None:
        """Set the vf_loss property."""
        self._cards[0].set_value("vf_loss", value)

    @property
    def mfrac(self) -> float:
        """Get or set the Mass ratio gain during mass scaling required for triggering a remesh. For a one percent increase in mass, set MFRAC=0.01.
        """ # nopep8
        return self._cards[0].get_value("mfrac")

    @mfrac.setter
    def mfrac(self, value: float) -> None:
        """Set the mfrac property."""
        self._cards[0].set_value("mfrac", value)

    @property
    def dt_min(self) -> float:
        """Get or set the Time-step size required for triggering a remesh. This option is checked before mass scaling is applied and the time step size reset.
        """ # nopep8
        return self._cards[0].get_value("dt_min")

    @dt_min.setter
    def dt_min(self, value: float) -> None:
        """Set the dt_min property."""
        self._cards[0].set_value("dt_min", value)

    @property
    def icurv(self) -> int:
        """Get or set the Define number of element along the radius in the adaptivity.
        """ # nopep8
        return self._cards[0].get_value("icurv")

    @icurv.setter
    def icurv(self, value: int) -> None:
        """Set the icurv property."""
        self._cards[0].set_value("icurv", value)

    @property
    def iadp10(self) -> int:
        """Get or set the Not used.
        """ # nopep8
        return self._cards[0].get_value("iadp10")

    @iadp10.setter
    def iadp10(self, value: int) -> None:
        """Set the iadp10 property."""
        self._cards[0].set_value("iadp10", value)

    @property
    def sefang(self) -> float:
        """Get or set the Define angular mesh size in 3-D axisymmetric remeshing.
        """ # nopep8
        return self._cards[0].get_value("sefang")

    @sefang.setter
    def sefang(self, value: float) -> None:
        """Set the sefang property."""
        self._cards[0].set_value("sefang", value)

