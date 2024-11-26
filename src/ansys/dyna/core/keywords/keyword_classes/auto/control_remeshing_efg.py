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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlRemeshingEfg(KeywordBase):
    """DYNA CONTROL_REMESHING_EFG keyword"""

    keyword = "CONTROL"
    subkeyword = "REMESHING_EFG"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "rmin",
                        float,
                        0,
                        10,
                        kwargs.get("rmin")
                    ),
                    Field(
                        "rmax",
                        float,
                        10,
                        10,
                        kwargs.get("rmax")
                    ),
                    Field(
                        "vf_loss",
                        float,
                        20,
                        10,
                        kwargs.get("vf_loss", 1.0)
                    ),
                    Field(
                        "mfrac",
                        float,
                        30,
                        10,
                        kwargs.get("mfrac", 0.0)
                    ),
                    Field(
                        "dt_min",
                        float,
                        40,
                        10,
                        kwargs.get("dt_min", 0.0)
                    ),
                    Field(
                        "icurv",
                        int,
                        50,
                        10,
                        kwargs.get("icurv", 4)
                    ),
                    Field(
                        "iadp10",
                        int,
                        60,
                        10,
                        kwargs.get("iadp10", 0)
                    ),
                    Field(
                        "sefang",
                        float,
                        70,
                        10,
                        kwargs.get("sefang", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ivt",
                        int,
                        0,
                        10,
                        kwargs.get("ivt", 1)
                    ),
                    Field(
                        "iat",
                        int,
                        10,
                        10,
                        kwargs.get("iat", 0)
                    ),
                    Field(
                        "iaat",
                        int,
                        20,
                        10,
                        kwargs.get("iaat", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "mm",
                        int,
                        40,
                        10,
                        kwargs.get("mm", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iat1",
                        float,
                        0,
                        10,
                        kwargs.get("iat1", 1.0E+20)
                    ),
                    Field(
                        "iat2",
                        float,
                        10,
                        10,
                        kwargs.get("iat2", 1.0E+20)
                    ),
                    Field(
                        "iat3",
                        float,
                        20,
                        10,
                        kwargs.get("iat3", 1.0E+20)
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
        self._cards[0].set_value("rmin", value)

    @property
    def rmax(self) -> typing.Optional[float]:
        """Get or set the Maximum edge length for the surface mesh surrounding the parts which should be remeshed.
        """ # nopep8
        return self._cards[0].get_value("rmax")

    @rmax.setter
    def rmax(self, value: float) -> None:
        self._cards[0].set_value("rmax", value)

    @property
    def vf_loss(self) -> float:
        """Get or set the Volume fraction loss required in a type 10/13 tetrahedral elements to trigger a remesh.  For the type 13 solid elements, the pressures are computed at the nodal points; therefore, it is possible for overall volume to be conserved but for individual tetrahedrons to experience a significant volume loss or gain.  The volume loss can lead to numerical problems.  Recommended values for VF_‌LOSS in the range of 0.10 to 0.30 may be reasonable.
        """ # nopep8
        return self._cards[0].get_value("vf_loss")

    @vf_loss.setter
    def vf_loss(self, value: float) -> None:
        self._cards[0].set_value("vf_loss", value)

    @property
    def mfrac(self) -> float:
        """Get or set the Mass ratio gain during mass scaling required for triggering a remesh. For a one percent increase in mass, set MFRAC=0.01.
        """ # nopep8
        return self._cards[0].get_value("mfrac")

    @mfrac.setter
    def mfrac(self, value: float) -> None:
        self._cards[0].set_value("mfrac", value)

    @property
    def dt_min(self) -> float:
        """Get or set the Time-step size required for triggering a remesh. This option is checked before mass scaling is applied and the time step size reset.
        """ # nopep8
        return self._cards[0].get_value("dt_min")

    @dt_min.setter
    def dt_min(self, value: float) -> None:
        self._cards[0].set_value("dt_min", value)

    @property
    def icurv(self) -> int:
        """Get or set the Define number of element along the radius in the adaptivity.
        """ # nopep8
        return self._cards[0].get_value("icurv")

    @icurv.setter
    def icurv(self, value: int) -> None:
        self._cards[0].set_value("icurv", value)

    @property
    def iadp10(self) -> int:
        """Get or set the Not used.
        """ # nopep8
        return self._cards[0].get_value("iadp10")

    @iadp10.setter
    def iadp10(self, value: int) -> None:
        self._cards[0].set_value("iadp10", value)

    @property
    def sefang(self) -> float:
        """Get or set the Define angular mesh size in 3-D axisymmetric remeshing.
        """ # nopep8
        return self._cards[0].get_value("sefang")

    @sefang.setter
    def sefang(self, value: float) -> None:
        self._cards[0].set_value("sefang", value)

    @property
    def ivt(self) -> int:
        """Get or set the Internal variable transfer in adaptive EFG
        EQ.1: Moving Least square approximation with Kronecker-delta property
        EQ.-1: Moving Least square approximation without Kronecker-delta property.
        EQ.2: Partition of unity approximation with Kronecker-delta property.
        EQ.-2: Partition of unity approximation without Kronecker-delta property.
        """ # nopep8
        return self._cards[1].get_value("ivt")

    @ivt.setter
    def ivt(self, value: int) -> None:
        if value not in [1, -1, 2, -2]:
            raise Exception("""ivt must be one of {1,-1,2,-2}""")
        self._cards[1].set_value("ivt", value)

    @property
    def iat(self) -> int:
        """Get or set the Interactive adaptivity
        EQ. 0: No interactive adaptivity.
        EQ. 1: Interactive adaptivity combined with predefined adaptivity.
        EQ. 2: Purely interactive adaptivity. The time interval between two successive adaptive steps is bounded by ADPFREQ.
        EQ. 3: Purely interactive adaptivity.
        """ # nopep8
        return self._cards[1].get_value("iat")

    @iat.setter
    def iat(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""iat must be one of {0,1,2,3}""")
        self._cards[1].set_value("iat", value)

    @property
    def iaat(self) -> int:
        """Get or set the Interactive adaptivity adjustable tolerance
        EQ. 0: The tolerance to trigger interactive adaptivity is not adjusted.
        EQ. 1: The tolerance is adjusted in run-time to avoid over-activation.
        """ # nopep8
        return self._cards[1].get_value("iaat")

    @iaat.setter
    def iaat(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iaat must be one of {0,1}""")
        self._cards[1].set_value("iaat", value)

    @property
    def mm(self) -> int:
        """Get or set the Interactive adaptive remeshing with monotonic resizing:
        EQ.1:	the adaptive remeshing cannot coarsen a mesh.The current implementation only supports IAT = 1, 2, 3 and IER = 0.
        """ # nopep8
        return self._cards[1].get_value("mm")

    @mm.setter
    def mm(self, value: int) -> None:
        self._cards[1].set_value("mm", value)

    @property
    def iat1(self) -> float:
        """Get or set the Define the tolerance of shear distortion indicator for interactive adaptivity. (0.1~0.5 is recommended)
        """ # nopep8
        return self._cards[2].get_value("iat1")

    @iat1.setter
    def iat1(self, value: float) -> None:
        self._cards[2].set_value("iat1", value)

    @property
    def iat2(self) -> float:
        """Get or set the Define the tolerance of unbalanced nodal distribution indicator for interactive adaptivity. (RMAX/RMIN is recommended)
        """ # nopep8
        return self._cards[2].get_value("iat2")

    @iat2.setter
    def iat2(self, value: float) -> None:
        self._cards[2].set_value("iat2", value)

    @property
    def iat3(self) -> float:
        """Get or set the Define the tolerance of volumetric change indicator for interactive adaptivity.
        """ # nopep8
        return self._cards[2].get_value("iat3")

    @iat3.setter
    def iat3(self, value: float) -> None:
        self._cards[2].set_value("iat3", value)

