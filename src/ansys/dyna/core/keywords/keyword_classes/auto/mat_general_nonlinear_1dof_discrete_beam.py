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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatGeneralNonlinear1DofDiscreteBeam(KeywordBase):
    """DYNA MAT_GENERAL_NONLINEAR_1DOF_DISCRETE_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "GENERAL_NONLINEAR_1DOF_DISCRETE_BEAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "k",
                        float,
                        20,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "iunld",
                        int,
                        30,
                        10,
                        kwargs.get("iunld")
                    ),
                    Field(
                        "offset",
                        float,
                        40,
                        10,
                        kwargs.get("offset")
                    ),
                    Field(
                        "dampf",
                        float,
                        50,
                        10,
                        kwargs.get("dampf")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidt",
                        int,
                        0,
                        10,
                        kwargs.get("lcidt")
                    ),
                    Field(
                        "lcidtu",
                        int,
                        10,
                        10,
                        kwargs.get("lcidtu")
                    ),
                    Field(
                        "lcidtd",
                        int,
                        20,
                        10,
                        kwargs.get("lcidtd")
                    ),
                    Field(
                        "lcidte",
                        int,
                        30,
                        10,
                        kwargs.get("lcidte")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "utfail",
                        float,
                        0,
                        10,
                        kwargs.get("utfail")
                    ),
                    Field(
                        "ucfail",
                        float,
                        10,
                        10,
                        kwargs.get("ucfail")
                    ),
                    Field(
                        "iu",
                        float,
                        20,
                        10,
                        kwargs.get("iu")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatGeneralNonlinear1DofDiscreteBeam.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.  A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Translational stiffness for unloading option 2.0.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def iunld(self) -> typing.Optional[int]:
        """Get or set the Unloading option (Also see Figure 20.34 in the User's manual):
        EQ.0.0:  Loading and unloading follow loading curve
        EQ.1.0:  Loading follows loading curve, unloading follows loading curve.  (Also see Figure 20.35 in the User's Manual).  The unloading curve ID if defined is ignored.
        EQ.2.0:  Loading follows loading curve, unloading follows unloading stiffness, K,  to the unloading curve.  The loading and unloading curves intersect at the origin of the axes.
        EQ.3.0:  Quadratic unloading from peak displacement value to permanent set.
        """ # nopep8
        return self._cards[0].get_value("iunld")

    @iunld.setter
    def iunld(self, value: int) -> None:
        self._cards[0].set_value("iunld", value)

    @property
    def offset(self) -> typing.Optional[float]:
        """Get or set the Offset to determine a permanent set upon unloading if the UNLDOPT=3.0.  The permanent sets in compression and tension are equal to the product of this offset value and the maximum compressive and tensile displacements, respectively.
        """ # nopep8
        return self._cards[0].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[0].set_value("offset", value)

    @property
    def dampf(self) -> typing.Optional[float]:
        """Get or set the Damping factor for stability.  Values in the neighborhood of unity are recommended.  This damping factor is properly scaled to eliminate time step size dependency.  Also, it is active if and only if the local stiffness is defined.
        """ # nopep8
        return self._cards[0].get_value("dampf")

    @dampf.setter
    def dampf(self, value: float) -> None:
        self._cards[0].set_value("dampf", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along the axis versus relative translational displacement.  If zero, no stiffness related forces are generated for this degree of freedom.  The loading curves must be defined from the most negative displacement to the most positive displacement.  The force does not need to increase montonically for the loading curve.  The curves in this input are extrapolated when the displacement range falls outside the curve definition.
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        self._cards[1].set_value("lcidt", value)

    @property
    def lcidtu(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along the axis versus relative translational displacement during unloading.  The force values defined by this curve must increase monotonically from the most negative displacement to the most positive displacement.  For UNLDOPT=1.0, the slope of this curve must equal or exceed the loading curve for stability reasons.  This is not the case for UNLDOPT=2.0.   For loading and unloading to follow the same path simply set LCIDTU=LCIDT.
        """ # nopep8
        return self._cards[1].get_value("lcidtu")

    @lcidtu.setter
    def lcidtu(self, value: int) -> None:
        self._cards[1].set_value("lcidtu", value)

    @property
    def lcidtd(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local the axis versus relative translational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidtd")

    @lcidtd.setter
    def lcidtd(self, value: int) -> None:
        self._cards[1].set_value("lcidtd", value)

    @property
    def lcidte(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force scale factor versus relative displacement in along axis.
        """ # nopep8
        return self._cards[1].get_value("lcidte")

    @lcidte.setter
    def lcidte(self, value: int) -> None:
        self._cards[1].set_value("lcidte", value)

    @property
    def utfail(self) -> typing.Optional[float]:
        """Get or set the Optional, translational displacement at failure in tension.  If zero, failure in tension is not considered.
        """ # nopep8
        return self._cards[2].get_value("utfail")

    @utfail.setter
    def utfail(self, value: float) -> None:
        self._cards[2].set_value("utfail", value)

    @property
    def ucfail(self) -> typing.Optional[float]:
        """Get or set the Optional, translational displacement at failure in compression.  If zero, failure in compression is not considered.
        """ # nopep8
        return self._cards[2].get_value("ucfail")

    @ucfail.setter
    def ucfail(self, value: float) -> None:
        self._cards[2].set_value("ucfail", value)

    @property
    def iu(self) -> typing.Optional[float]:
        """Get or set the Initial translational displacement along axis.
        """ # nopep8
        return self._cards[2].get_value("iu")

    @iu.setter
    def iu(self, value: float) -> None:
        self._cards[2].set_value("iu", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

