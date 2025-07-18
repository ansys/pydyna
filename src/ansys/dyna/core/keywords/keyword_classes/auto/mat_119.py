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

"""Module providing the Mat119 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat119(KeywordBase):
    """DYNA MAT_119 keyword"""

    keyword = "MAT"
    subkeyword = "119"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat119 class."""
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "kt",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "kr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iunld",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "offset",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dampf",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iflag",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidtr",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidts",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidtt",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrr",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrs",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrt",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidtur",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidtus",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidtut",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrur",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrus",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrut",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidtdr",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidtds",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidtdt",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrdr",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrds",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrdt",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidter",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidtes",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidtet",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidrer",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidres",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcidret",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "utfailr",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "utfails",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "utfailt",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "wtfailr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "wtfails",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "wtfailt",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fcrit",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ucfailr",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ucfails",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ucfailt",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "wcfailr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "wcfails",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "wcfailt",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iur",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ius",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iut",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iwr",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iws",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iwt",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat119.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def kt(self) -> typing.Optional[float]:
        """Get or set the Translational stiffness for IUNLD = 2.0.  However, if IFLAG = 2, then it is the translational stiffness for unloading along the local r-axis.
        If left blank, a value calculated by LS-DYNA will be used.
        """ # nopep8
        return self._cards[0].get_value("kt")

    @kt.setter
    def kt(self, value: float) -> None:
        """Set the kt property."""
        self._cards[0].set_value("kt", value)

    @property
    def kr(self) -> typing.Optional[float]:
        """Get or set the Rotational stiffness for IUNLD = 2.0.  However, if IFLAG =‌ 2, then KR is the rotational stiffness for unloading along the local r-axis.
        If left blank, a value calculated by LS-DYNA will be used
        """ # nopep8
        return self._cards[0].get_value("kr")

    @kr.setter
    def kr(self, value: float) -> None:
        """Set the kr property."""
        self._cards[0].set_value("kr", value)

    @property
    def iunld(self) -> typing.Optional[int]:
        """Get or set the Unloading option (see Figure 0-1):
        EQ.0.0:	loading and unloading follow loading curve
        EQ.1.0:	loading follows loading curve, unloading follows unloading curve. The unloading curve ID if undefined is taken as the loading curve.
        EQ.2.0:	loading follows loading curve, unloading follows unloading stiffness, KT or KR, to the unloading curve.
        The loading and unloading curves may only intersect at the origin of the axes.
        EQ.3.0:	quadratic unloading from peak displacement value to a permanent offset.
        """ # nopep8
        return self._cards[0].get_value("iunld")

    @iunld.setter
    def iunld(self, value: int) -> None:
        """Set the iunld property."""
        self._cards[0].set_value("iunld", value)

    @property
    def offset(self) -> typing.Optional[float]:
        """Get or set the Offset factor between 0 and 1.0 to determine permanent set upon unloading if the UNLDOPT=3.0. The permanent sets in compression and tension are equal to the product of this offset value and the maximum compressive and tensile displacements, respectively.
        """ # nopep8
        return self._cards[0].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        """Set the offset property."""
        self._cards[0].set_value("offset", value)

    @property
    def dampf(self) -> typing.Optional[float]:
        """Get or set the Damping factor for stability.  Values in the neighborhood of unity are recommended. This damping factor is properly scaled to eliminate time step size dependency. Also, it is active if and only if the local stiffness is defined.
        """ # nopep8
        return self._cards[0].get_value("dampf")

    @dampf.setter
    def dampf(self, value: float) -> None:
        """Set the dampf property."""
        self._cards[0].set_value("dampf", value)

    @property
    def iflag(self) -> int:
        """Get or set the Formulation flag:
        EQ.0:	displacement formulation which is used in all other models
        EQ.1:	linear strain formulation.  The displacements and velocities are divided by the initial length of the beam.
        EQ.2:	a displacement formulation to simulate the buckling behavior of crushable frames.
        """ # nopep8
        return self._cards[0].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        """Set the iflag property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iflag must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("iflag", value)

    @property
    def lcidtr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local r-axis versus relative translational displacement.
        If zero, no stiffness related forces are generated for this degree of freedom. The loading curves must be defined from the most negative displacement to the most positive displacement.  The force does not need to increase montonically. The curves in this input are linearly extrapolated when the displacement range falls outside the curve definition.
        """ # nopep8
        return self._cards[1].get_value("lcidtr")

    @lcidtr.setter
    def lcidtr(self, value: int) -> None:
        """Set the lcidtr property."""
        self._cards[1].set_value("lcidtr", value)

    @property
    def lcidts(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local s-axis versus relative translational displacement.
        """ # nopep8
        return self._cards[1].get_value("lcidts")

    @lcidts.setter
    def lcidts(self, value: int) -> None:
        """Set the lcidts property."""
        self._cards[1].set_value("lcidts", value)

    @property
    def lcidtt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local t-axis versus relative translational displacement.
        """ # nopep8
        return self._cards[1].get_value("lcidtt")

    @lcidtt.setter
    def lcidtt(self, value: int) -> None:
        """Set the lcidtt property."""
        self._cards[1].set_value("lcidtt", value)

    @property
    def lcidrr(self) -> typing.Optional[int]:
        """Get or set the Load curve for rotational moment resultant about the local r-axis:
        IFLAG.NE.2:	load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement
        IFLAG.EQ.2:	load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement at node 2.
        """ # nopep8
        return self._cards[1].get_value("lcidrr")

    @lcidrr.setter
    def lcidrr(self, value: int) -> None:
        """Set the lcidrr property."""
        self._cards[1].set_value("lcidrr", value)

    @property
    def lcidrs(self) -> typing.Optional[int]:
        """Get or set the Load curve for rotational moment resultant about local r-axis:
        IFLAG.NE.2:	load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement
        IFLAG.EQ.2:	load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement at node 2.
        """ # nopep8
        return self._cards[1].get_value("lcidrs")

    @lcidrs.setter
    def lcidrs(self, value: int) -> None:
        """Set the lcidrs property."""
        self._cards[1].set_value("lcidrs", value)

    @property
    def lcidrt(self) -> typing.Optional[int]:
        """Get or set the Load curve for rotational moment resultant about local -axis:
        IFLAG.NE.2:	load curve ID defining rotational moment resultant about local s-axis as a function of relative rotational displacement
        IFLAG.EQ.2:	load curve ID defining rotational moment resultant about local s-axis as a function of relative rotational displacement at node 2.
        """ # nopep8
        return self._cards[1].get_value("lcidrt")

    @lcidrt.setter
    def lcidrt(self, value: int) -> None:
        """Set the lcidrt property."""
        self._cards[1].set_value("lcidrt", value)

    @property
    def lcidtur(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local r-axis as a function of relative translational displacement during unloading.
        The force values defined by this curve must increase monotonically from the most negative displacement to the most positive displacement.
        For IUNLD = 1.0, the slope of this curve must equal or exceed the loading curve for stability reasons.  This is not the case for IUNLD = 2.0.
        For loading and unloading to follow the same path simply set LCIDTUR = LCIDTR.  For options IUNLD = 0.0 or 3.0 the unloading curve is not required.
        For IUNLD = 2.0, if LCIDTUR is left blank or zero, the default is to use the same curve for unloading as for loading.
        """ # nopep8
        return self._cards[2].get_value("lcidtur")

    @lcidtur.setter
    def lcidtur(self, value: int) -> None:
        """Set the lcidtur property."""
        self._cards[2].set_value("lcidtur", value)

    @property
    def lcidtus(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local s-axis as a function of relative translational displacement during unloading (IFLAG = 0 or 1 only).
        """ # nopep8
        return self._cards[2].get_value("lcidtus")

    @lcidtus.setter
    def lcidtus(self, value: int) -> None:
        """Set the lcidtus property."""
        self._cards[2].set_value("lcidtus", value)

    @property
    def lcidtut(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local t-axis as a function of relative translational displacement during unloading (IFLAG = 0 or 1 only).
        """ # nopep8
        return self._cards[2].get_value("lcidtut")

    @lcidtut.setter
    def lcidtut(self, value: int) -> None:
        """Set the lcidtut property."""
        self._cards[2].set_value("lcidtut", value)

    @property
    def lcidrur(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement during unloading.
        """ # nopep8
        return self._cards[2].get_value("lcidrur")

    @lcidrur.setter
    def lcidrur(self, value: int) -> None:
        """Set the lcidrur property."""
        self._cards[2].set_value("lcidrur", value)

    @property
    def lcidrus(self) -> typing.Optional[int]:
        """Get or set the Load curve for rotational moment resultant about local s-axis:
        IFLAG.NE.2:	load curve ID defining rotational moment resultant about local s-axis as a function of relative rotational displacement during unloading
        IFLAG.EQ.2:	load curve ID defining rotational moment resultant about local s-axis as a function of relative rotational displacement during unloading at node 2.
        """ # nopep8
        return self._cards[2].get_value("lcidrus")

    @lcidrus.setter
    def lcidrus(self, value: int) -> None:
        """Set the lcidrus property."""
        self._cards[2].set_value("lcidrus", value)

    @property
    def lcidrut(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational moment resultant about local -axis:
        IFLAG.NE.2:	load curve ID defining rotational moment resultant about local -axis as a function of relative rotational displacement during unloading.  If zero, no viscous forces are generated for this degree of freedom
        IFLAG.EQ.2:	load curve ID defining rotational moment resultant about local -axis as a function of relative rotational displacement during unloading at node 2.
        """ # nopep8
        return self._cards[2].get_value("lcidrut")

    @lcidrut.setter
    def lcidrut(self, value: int) -> None:
        """Set the lcidrut property."""
        self._cards[2].set_value("lcidrut", value)

    @property
    def lcidtdr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local r-
        axis as a function of relative translational velocity.
        """ # nopep8
        return self._cards[3].get_value("lcidtdr")

    @lcidtdr.setter
    def lcidtdr(self, value: int) -> None:
        """Set the lcidtdr property."""
        self._cards[3].set_value("lcidtdr", value)

    @property
    def lcidtds(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local 
        s-axis as a function relative translational velocity.
        """ # nopep8
        return self._cards[3].get_value("lcidtds")

    @lcidtds.setter
    def lcidtds(self, value: int) -> None:
        """Set the lcidtds property."""
        self._cards[3].set_value("lcidtds", value)

    @property
    def lcidtdt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local 
        t-axis as a function of relative translational velocity.
        """ # nopep8
        return self._cards[3].get_value("lcidtdt")

    @lcidtdt.setter
    def lcidtdt(self, value: int) -> None:
        """Set the lcidtdt property."""
        self._cards[3].set_value("lcidtdt", value)

    @property
    def lcidrdr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local 
        r-axis as a function of relative rotational velocity.
        """ # nopep8
        return self._cards[3].get_value("lcidrdr")

    @lcidrdr.setter
    def lcidrdr(self, value: int) -> None:
        """Set the lcidrdr property."""
        self._cards[3].set_value("lcidrdr", value)

    @property
    def lcidrds(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local 
        s-axis as a function of relative rotational velocity.
        """ # nopep8
        return self._cards[3].get_value("lcidrds")

    @lcidrds.setter
    def lcidrds(self, value: int) -> None:
        """Set the lcidrds property."""
        self._cards[3].set_value("lcidrds", value)

    @property
    def lcidrdt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local 
        t-axis as a function of relative rotational velocity.
        """ # nopep8
        return self._cards[3].get_value("lcidrdt")

    @lcidrdt.setter
    def lcidrdt(self, value: int) -> None:
        """Set the lcidrdt property."""
        self._cards[3].set_value("lcidrdt", value)

    @property
    def lcidter(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local r-direction.
        """ # nopep8
        return self._cards[4].get_value("lcidter")

    @lcidter.setter
    def lcidter(self, value: int) -> None:
        """Set the lcidter property."""
        self._cards[4].set_value("lcidter", value)

    @property
    def lcidtes(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local s-direction.
        """ # nopep8
        return self._cards[4].get_value("lcidtes")

    @lcidtes.setter
    def lcidtes(self, value: int) -> None:
        """Set the lcidtes property."""
        self._cards[4].set_value("lcidtes", value)

    @property
    def lcidtet(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local t-direction.
        """ # nopep8
        return self._cards[4].get_value("lcidtet")

    @lcidtet.setter
    def lcidtet(self, value: int) -> None:
        """Set the lcidtet property."""
        self._cards[4].set_value("lcidtet", value)

    @property
    def lcidrer(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local r-rotation.
        """ # nopep8
        return self._cards[4].get_value("lcidrer")

    @lcidrer.setter
    def lcidrer(self, value: int) -> None:
        """Set the lcidrer property."""
        self._cards[4].set_value("lcidrer", value)

    @property
    def lcidres(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local s-rotation.
        """ # nopep8
        return self._cards[4].get_value("lcidres")

    @lcidres.setter
    def lcidres(self, value: int) -> None:
        """Set the lcidres property."""
        self._cards[4].set_value("lcidres", value)

    @property
    def lcidret(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local t-rotation.
        """ # nopep8
        return self._cards[4].get_value("lcidret")

    @lcidret.setter
    def lcidret(self, value: int) -> None:
        """Set the lcidret property."""
        self._cards[4].set_value("lcidret", value)

    @property
    def utfailr(self) -> typing.Optional[float]:
        """Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Ur, is not considered in the failure calculation.
        """ # nopep8
        return self._cards[5].get_value("utfailr")

    @utfailr.setter
    def utfailr(self, value: float) -> None:
        """Set the utfailr property."""
        self._cards[5].set_value("utfailr", value)

    @property
    def utfails(self) -> typing.Optional[float]:
        """Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Us, is not considered in the failure calculation.
        """ # nopep8
        return self._cards[5].get_value("utfails")

    @utfails.setter
    def utfails(self, value: float) -> None:
        """Set the utfails property."""
        self._cards[5].set_value("utfails", value)

    @property
    def utfailt(self) -> typing.Optional[float]:
        """Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Ut, is not considered in the failure calculation.
        """ # nopep8
        return self._cards[5].get_value("utfailt")

    @utfailt.setter
    def utfailt(self, value: float) -> None:
        """Set the utfailt property."""
        self._cards[5].set_value("utfailt", value)

    @property
    def wtfailr(self) -> typing.Optional[float]:
        """Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-r, is not considered in the failure calculation.
        """ # nopep8
        return self._cards[5].get_value("wtfailr")

    @wtfailr.setter
    def wtfailr(self, value: float) -> None:
        """Set the wtfailr property."""
        self._cards[5].set_value("wtfailr", value)

    @property
    def wtfails(self) -> typing.Optional[float]:
        """Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-s, is not considered in the failure calculation.
        """ # nopep8
        return self._cards[5].get_value("wtfails")

    @wtfails.setter
    def wtfails(self, value: float) -> None:
        """Set the wtfails property."""
        self._cards[5].set_value("wtfails", value)

    @property
    def wtfailt(self) -> typing.Optional[float]:
        """Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-t, is not considered in the failure calculation.
        """ # nopep8
        return self._cards[5].get_value("wtfailt")

    @wtfailt.setter
    def wtfailt(self, value: float) -> None:
        """Set the wtfailt property."""
        self._cards[5].set_value("wtfailt", value)

    @property
    def fcrit(self) -> typing.Optional[float]:
        """Get or set the Failure criterion (see Remark 1):
        EQ.0.0:	two separate criteria, one for negative displacements and rotations, another for positive displacements and rotations
        EQ.1.0:	one criterion that considers both positive and negative displacements and rotations.
        """ # nopep8
        return self._cards[5].get_value("fcrit")

    @fcrit.setter
    def fcrit(self, value: float) -> None:
        """Set the fcrit property."""
        self._cards[5].set_value("fcrit", value)

    @property
    def ucfailr(self) -> typing.Optional[float]:
        """Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Ur, is not considered in the failure calculation.
        Define as a positive number.
        """ # nopep8
        return self._cards[6].get_value("ucfailr")

    @ucfailr.setter
    def ucfailr(self, value: float) -> None:
        """Set the ucfailr property."""
        self._cards[6].set_value("ucfailr", value)

    @property
    def ucfails(self) -> typing.Optional[float]:
        """Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Us, is not considered in the failure calculation.
        Define as a positive number.
        """ # nopep8
        return self._cards[6].get_value("ucfails")

    @ucfails.setter
    def ucfails(self, value: float) -> None:
        """Set the ucfails property."""
        self._cards[6].set_value("ucfails", value)

    @property
    def ucfailt(self) -> typing.Optional[float]:
        """Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Ut, is not considered in the failure calculation.
        Define as a positive number.
        """ # nopep8
        return self._cards[6].get_value("ucfailt")

    @ucfailt.setter
    def ucfailt(self, value: float) -> None:
        """Set the ucfailt property."""
        self._cards[6].set_value("ucfailt", value)

    @property
    def wcfailr(self) -> typing.Optional[float]:
        """Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-r, is not considered in the failure calculation.
        Define as a positive number.
        """ # nopep8
        return self._cards[6].get_value("wcfailr")

    @wcfailr.setter
    def wcfailr(self, value: float) -> None:
        """Set the wcfailr property."""
        self._cards[6].set_value("wcfailr", value)

    @property
    def wcfails(self) -> typing.Optional[float]:
        """Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-s, is not considered in the failure calculation.
        Define as a positive number.
        """ # nopep8
        return self._cards[6].get_value("wcfails")

    @wcfails.setter
    def wcfails(self, value: float) -> None:
        """Set the wcfails property."""
        self._cards[6].set_value("wcfails", value)

    @property
    def wcfailt(self) -> typing.Optional[float]:
        """Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-t, is not considered in the failure calculation.
        Define as a positive number.
        """ # nopep8
        return self._cards[6].get_value("wcfailt")

    @wcfailt.setter
    def wcfailt(self, value: float) -> None:
        """Set the wcfailt property."""
        self._cards[6].set_value("wcfailt", value)

    @property
    def iur(self) -> typing.Optional[float]:
        """Get or set the Initial translational displacement along local r-axis
        """ # nopep8
        return self._cards[7].get_value("iur")

    @iur.setter
    def iur(self, value: float) -> None:
        """Set the iur property."""
        self._cards[7].set_value("iur", value)

    @property
    def ius(self) -> typing.Optional[float]:
        """Get or set the Initial translational displacement along local s-axis
        """ # nopep8
        return self._cards[7].get_value("ius")

    @ius.setter
    def ius(self, value: float) -> None:
        """Set the ius property."""
        self._cards[7].set_value("ius", value)

    @property
    def iut(self) -> typing.Optional[float]:
        """Get or set the Initial translational displacement along local t-axis
        """ # nopep8
        return self._cards[7].get_value("iut")

    @iut.setter
    def iut(self, value: float) -> None:
        """Set the iut property."""
        self._cards[7].set_value("iut", value)

    @property
    def iwr(self) -> typing.Optional[float]:
        """Get or set the Initial rotational displacement along local r-axis
        """ # nopep8
        return self._cards[7].get_value("iwr")

    @iwr.setter
    def iwr(self, value: float) -> None:
        """Set the iwr property."""
        self._cards[7].set_value("iwr", value)

    @property
    def iws(self) -> typing.Optional[float]:
        """Get or set the Initial rotational displacement along local s-axis
        """ # nopep8
        return self._cards[7].get_value("iws")

    @iws.setter
    def iws(self, value: float) -> None:
        """Set the iws property."""
        self._cards[7].set_value("iws", value)

    @property
    def iwt(self) -> typing.Optional[float]:
        """Get or set the Initial rotational displacement along local t-axis
        """ # nopep8
        return self._cards[7].get_value("iwt")

    @iwt.setter
    def iwt(self, value: float) -> None:
        """Set the iwt property."""
        self._cards[7].set_value("iwt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[8].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

