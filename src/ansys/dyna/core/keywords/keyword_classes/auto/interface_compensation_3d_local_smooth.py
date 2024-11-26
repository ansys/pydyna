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

class InterfaceCompensation3DLocalSmooth(KeywordBase):
    """DYNA INTERFACE_COMPENSATION_3D_LOCAL_SMOOTH keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION_3D_LOCAL_SMOOTH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "method",
                        int,
                        0,
                        10,
                        kwargs.get("method", 6)
                    ),
                    Field(
                        "sl",
                        float,
                        10,
                        10,
                        kwargs.get("sl", 5.0)
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        kwargs.get("sf", 0.75)
                    ),
                    Field(
                        "elref",
                        int,
                        30,
                        10,
                        kwargs.get("elref", 1)
                    ),
                    Field(
                        "psidp",
                        float,
                        40,
                        10,
                        kwargs.get("psidp")
                    ),
                    Field(
                        "undct",
                        float,
                        50,
                        10,
                        kwargs.get("undct")
                    ),
                    Field(
                        "angle",
                        float,
                        60,
                        10,
                        kwargs.get("angle", 0.0)
                    ),
                    Field(
                        "nlinear",
                        int,
                        70,
                        10,
                        kwargs.get("nlinear", 1)
                    ),
                ],
            ),
        ]

    @property
    def method(self) -> int:
        """Get or set the There are several extrapolation methods for the addendum and binder outside of trim lines, see Remarks.
        """ # nopep8
        return self._cards[0].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        self._cards[0].set_value("method", value)

    @property
    def sl(self) -> float:
        """Get or set the The smooth level parameter controls the smoothness of the modified
        surfaces. A large value makes the surface smoother. Typically the value ranges from 5 to 10. If spring back is large, the transition
        region is expected to be large. However, by using a smaller value of SL, the region of transition can be reduced.
        """ # nopep8
        return self._cards[0].get_value("sl")

    @sl.setter
    def sl(self, value: float) -> None:
        self._cards[0].set_value("sl", value)

    @property
    def sf(self) -> float:
        """Get or set the Shape compensation scale factor. The value scales the spring back
        amount of the blank and the scaled amount is used to compensate	the tooling.
        GT.0: compensate in the opposite direction of the spring back;
        LT.0: compensate in the punch moving direction (for undercut).
        This scale factor scales how much of the shape deviation is
        compensated. For example, if 10 mm of spring back is predicted,
        and the scale factor is chosen as 0.75, then the compensation in the
        opposite direction will only be 7.5 mm.
        Experience shows that the best scale factor for reaching a converged
        solution (within part tolerance) is case dependent. In some cases, a
        scale factor range of 0.5 to 0.75 is best; while in others, larger values
        are indicated. Sometimes, the best value can be larger than 1.1.
        Note that within an automatic compensation loop, this factor does		not need to be varied.
        Since it is impossible to choose the best value for each application up
        front 0.75 is recommended for the first attempt. If the spring back
        cannot be effectively compensated and the calculation diverges, the
        factor can be moved upward or downward to obtain a converged
        solution, or more iterations must be used with the initial trial value
        to compensate the remaining shape deviation.
        For channel shaped parts that have a twisting mode of spring back,
        the scale factor is more important. It was found that a small change
        of the tool shape might change the twisting mode. If this occurs,
        using a small value (<0.5) is suggested.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def elref(self) -> int:
        """Get or set the Element refinement option:
        EQ.1: special element refinement is used with the tool elements (default);
        EQ.2: special element refinement is turned off.
        """ # nopep8
        return self._cards[0].get_value("elref")

    @elref.setter
    def elref(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""elref must be one of {1,2}""")
        self._cards[0].set_value("elref", value)

    @property
    def psidp(self) -> typing.Optional[float]:
        """Get or set the Define the part set ID for primary parts of the tooling.  Properly choosing the parts for the primary side is important since it affects what kinds of modifications will be made to the tooling. Usually, only one side of the tool will be chosen as the primary side, and the modifications made to the other side (secondary side) depend solely on the changes in the primary side.  This specification allows the two sides to be coupled while maintaining a constant (tool) gap between the two sides.  If both sides are chosen to be primary, the gap between the two sides might change and become inhomogeneous.
        When using METHOD 7, the choice of primary side will affect the result when applied to three-piece draw models.  At this time, when the punch and binder are chosen as the primary side, the binder region will not be changed.  Otherwise, when the die is chosen as primary side, the binder will be changed since the changes extend to the edges of the primary tool
        """ # nopep8
        return self._cards[0].get_value("psidp")

    @psidp.setter
    def psidp(self, value: float) -> None:
        self._cards[0].set_value("psidp", value)

    @property
    def undct(self) -> typing.Optional[float]:
        """Get or set the Tool undercut treatment option:
        EQ.0: no check (default);
        EQ.1: check and fix undercut.
        """ # nopep8
        return self._cards[0].get_value("undct")

    @undct.setter
    def undct(self, value: float) -> None:
        self._cards[0].set_value("undct", value)

    @property
    def angle(self) -> float:
        """Get or set the An angle defining the undercut.
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        self._cards[0].set_value("angle", value)

    @property
    def nlinear(self) -> int:
        """Get or set the Activate nonlinear extrapolation.
        """ # nopep8
        return self._cards[0].get_value("nlinear")

    @nlinear.setter
    def nlinear(self, value: int) -> None:
        self._cards[0].set_value("nlinear", value)

