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

class InitialStressBeam(KeywordBase):
    """DYNA INITIAL_STRESS_BEAM keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_BEAM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "rule",
                        int,
                        10,
                        10,
                        kwargs.get("rule", 2)
                    ),
                    Field(
                        "npts",
                        int,
                        20,
                        10,
                        kwargs.get("npts", 0)
                    ),
                    Field(
                        "local",
                        int,
                        30,
                        10,
                        kwargs.get("local", 0)
                    ),
                    Field(
                        "large",
                        int,
                        40,
                        10,
                        kwargs.get("large", 0)
                    ),
                    Field(
                        "nhisv",
                        int,
                        50,
                        10,
                        kwargs.get("nhisv")
                    ),
                    Field(
                        "naxes",
                        int,
                        60,
                        10,
                        kwargs.get("naxes", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "f11",
                        float,
                        0,
                        10,
                        kwargs.get("f11", 0.0)
                    ),
                    Field(
                        "t11",
                        float,
                        10,
                        10,
                        kwargs.get("t11", 0.0)
                    ),
                    Field(
                        "m12",
                        float,
                        20,
                        10,
                        kwargs.get("m12", 0.0)
                    ),
                    Field(
                        "m13",
                        float,
                        30,
                        10,
                        kwargs.get("m13", 0.0)
                    ),
                    Field(
                        "m22",
                        float,
                        40,
                        10,
                        kwargs.get("m22", 0.0)
                    ),
                    Field(
                        "m23",
                        float,
                        50,
                        10,
                        kwargs.get("m23", 0.0)
                    ),
                    Field(
                        "parm",
                        float,
                        60,
                        10,
                        kwargs.get("parm", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "f11",
                        float,
                        0,
                        16,
                        kwargs.get("f11", 0.0)
                    ),
                    Field(
                        "t11",
                        float,
                        16,
                        16,
                        kwargs.get("t11", 0.0)
                    ),
                    Field(
                        "m12",
                        float,
                        32,
                        16,
                        kwargs.get("m12", 0.0)
                    ),
                    Field(
                        "m13",
                        float,
                        48,
                        16,
                        kwargs.get("m13", 0.0)
                    ),
                    Field(
                        "m22",
                        float,
                        64,
                        16,
                        kwargs.get("m22", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "m23",
                        float,
                        0,
                        16,
                        kwargs.get("m23", 0.0)
                    ),
                    Field(
                        "parm",
                        float,
                        16,
                        16,
                        kwargs.get("parm", 0.0)
                    ),
                    Field(
                        "hisv1",
                        float,
                        32,
                        16,
                        kwargs.get("hisv1", 0.0)
                    ),
                    Field(
                        "hisv2",
                        float,
                        48,
                        16,
                        kwargs.get("hisv2", 0.0)
                    ),
                    Field(
                        "hisv3",
                        float,
                        64,
                        16,
                        kwargs.get("hisv3", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sig11",
                        float,
                        0,
                        10,
                        kwargs.get("sig11", 0.0)
                    ),
                    Field(
                        "sig22",
                        float,
                        10,
                        10,
                        kwargs.get("sig22", 0.0)
                    ),
                    Field(
                        "sig33",
                        float,
                        20,
                        10,
                        kwargs.get("sig33", 0.0)
                    ),
                    Field(
                        "sig12",
                        float,
                        30,
                        10,
                        kwargs.get("sig12", 0.0)
                    ),
                    Field(
                        "sig23",
                        float,
                        40,
                        10,
                        kwargs.get("sig23", 0.0)
                    ),
                    Field(
                        "sig31",
                        float,
                        50,
                        10,
                        kwargs.get("sig31", 0.0)
                    ),
                    Field(
                        "eps",
                        float,
                        60,
                        10,
                        kwargs.get("eps", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sig11",
                        float,
                        0,
                        16,
                        kwargs.get("sig11", 0.0)
                    ),
                    Field(
                        "sig22",
                        float,
                        16,
                        16,
                        kwargs.get("sig22", 0.0)
                    ),
                    Field(
                        "sig33",
                        float,
                        32,
                        16,
                        kwargs.get("sig33", 0.0)
                    ),
                    Field(
                        "sig12",
                        float,
                        48,
                        16,
                        kwargs.get("sig12", 0.0)
                    ),
                    Field(
                        "sig23",
                        float,
                        64,
                        16,
                        kwargs.get("sig23", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sig31",
                        float,
                        0,
                        16,
                        kwargs.get("sig31", 0.0)
                    ),
                    Field(
                        "eps",
                        float,
                        16,
                        16,
                        kwargs.get("eps", 0.0)
                    ),
                    Field(
                        "hisv1",
                        float,
                        32,
                        16,
                        kwargs.get("hisv1", 0.0)
                    ),
                    Field(
                        "hisv2",
                        float,
                        48,
                        16,
                        kwargs.get("hisv2", 0.0)
                    ),
                    Field(
                        "hisv3",
                        float,
                        64,
                        16,
                        kwargs.get("hisv3", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hisv4",
                        float,
                        0,
                        16,
                        kwargs.get("hisv4", 0.0)
                    ),
                    Field(
                        "hisv5",
                        float,
                        16,
                        16,
                        kwargs.get("hisv5", 0.0)
                    ),
                    Field(
                        "hisv6",
                        float,
                        32,
                        16,
                        kwargs.get("hisv6", 0.0)
                    ),
                    Field(
                        "hisv7",
                        float,
                        48,
                        16,
                        kwargs.get("hisv7", 0.0)
                    ),
                    Field(
                        "hisv8",
                        float,
                        64,
                        16,
                        kwargs.get("hisv8", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ax1",
                        float,
                        0,
                        16,
                        kwargs.get("ax1", 0.0)
                    ),
                    Field(
                        "ax2",
                        float,
                        16,
                        16,
                        kwargs.get("ax2", 0.0)
                    ),
                    Field(
                        "ax3",
                        float,
                        32,
                        16,
                        kwargs.get("ax3", 0.0)
                    ),
                    Field(
                        "ax4",
                        float,
                        48,
                        16,
                        kwargs.get("ax4", 0.0)
                    ),
                    Field(
                        "ax5",
                        float,
                        64,
                        16,
                        kwargs.get("ax5", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ax6",
                        float,
                        0,
                        16,
                        kwargs.get("ax6", 0.0)
                    ),
                    Field(
                        "ax7",
                        float,
                        16,
                        16,
                        kwargs.get("ax7", 0.0)
                    ),
                    Field(
                        "ax8",
                        float,
                        32,
                        16,
                        kwargs.get("ax8", 0.0)
                    ),
                    Field(
                        "ax9",
                        float,
                        48,
                        16,
                        kwargs.get("ax9", 0.0)
                    ),
                    Field(
                        "ax10",
                        float,
                        64,
                        16,
                        kwargs.get("ax10", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ax11",
                        float,
                        0,
                        16,
                        kwargs.get("ax11", 0.0)
                    ),
                    Field(
                        "ax12",
                        float,
                        16,
                        16,
                        kwargs.get("ax12", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Beam element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def rule(self) -> int:
        """Get or set the Integration rule type number:
        EQ.1.0: 1x1 Gauss quadrature,
        EQ.2.0: 2x2 Gauss quadrature (default),
        EQ.3.0: 3x3 Gauss quadrature,
        EQ.4.0: 3x3 Lobatto quadrature,
        EQ.5.0: 4 x4 Gauss quadrature.
        """ # nopep8
        return self._cards[0].get_value("rule")

    @rule.setter
    def rule(self, value: int) -> None:
        if value not in [2, 1, 3, 4, 5]:
            raise Exception("""rule must be one of {2,1,3,4,5}""")
        self._cards[0].set_value("rule", value)

    @property
    def npts(self) -> int:
        """Get or set the Number of integration points.  For the Belytschko-Schwer resultant beam element, NPTS=1.
        """ # nopep8
        return self._cards[0].get_value("npts")

    @npts.setter
    def npts(self, value: int) -> None:
        self._cards[0].set_value("npts", value)

    @property
    def local(self) -> int:
        """Get or set the Coordinate system for stresses:
        EQ.0: Stress components are defined in the global coordinate system.
        EQ.1: stress components are defined in the local beam system. In the local system components SIG22, SIG33, and SIG23 are set to 0.0.
        """ # nopep8
        return self._cards[0].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""local must be one of {0,1}""")
        self._cards[0].set_value("local", value)

    @property
    def large(self) -> int:
        """Get or set the Format size:
        EQ.0:	off,
        EQ.1:	on.  Each field is twice as long for higher precision.
        """ # nopep8
        return self._cards[0].get_value("large")

    @large.setter
    def large(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""large must be one of {0,1}""")
        self._cards[0].set_value("large", value)

    @property
    def nhisv(self) -> typing.Optional[int]:
        """Get or set the Number of additional history variables.  Only available for LARGE=1
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        self._cards[0].set_value("nhisv", value)

    @property
    def naxes(self) -> int:
        """Get or set the Number of variables giving beam local axes (0 or 12)
        """ # nopep8
        return self._cards[0].get_value("naxes")

    @naxes.setter
    def naxes(self, value: int) -> None:
        if value not in [0, 12]:
            raise Exception("""naxes must be one of {0,12}""")
        self._cards[0].set_value("naxes", value)

    @property
    def f11(self) -> float:
        """Get or set the Axial force resultant along local beam axis 1.
        """ # nopep8
        return self._cards[1].get_value("f11")

    @f11.setter
    def f11(self, value: float) -> None:
        self._cards[1].set_value("f11", value)

    @property
    def t11(self) -> float:
        """Get or set the Torsional moment resultant about local beam axis 1.
        """ # nopep8
        return self._cards[1].get_value("t11")

    @t11.setter
    def t11(self, value: float) -> None:
        self._cards[1].set_value("t11", value)

    @property
    def m12(self) -> float:
        """Get or set the Moment resultant at node 1 about local beam axis 2.
        """ # nopep8
        return self._cards[1].get_value("m12")

    @m12.setter
    def m12(self, value: float) -> None:
        self._cards[1].set_value("m12", value)

    @property
    def m13(self) -> float:
        """Get or set the Moment resultant at node 1 about local beam axis 3.
        """ # nopep8
        return self._cards[1].get_value("m13")

    @m13.setter
    def m13(self, value: float) -> None:
        self._cards[1].set_value("m13", value)

    @property
    def m22(self) -> float:
        """Get or set the Moment resultant at node 2 about local beam axis 2.
        """ # nopep8
        return self._cards[1].get_value("m22")

    @m22.setter
    def m22(self, value: float) -> None:
        self._cards[1].set_value("m22", value)

    @property
    def m23(self) -> float:
        """Get or set the Moment resultant at node 2 about local beam axis 3.
        """ # nopep8
        return self._cards[1].get_value("m23")

    @m23.setter
    def m23(self, value: float) -> None:
        self._cards[1].set_value("m23", value)

    @property
    def parm(self) -> float:
        """Get or set the Generally not used.
        """ # nopep8
        return self._cards[1].get_value("parm")

    @parm.setter
    def parm(self, value: float) -> None:
        self._cards[1].set_value("parm", value)

    @property
    def f11(self) -> float:
        """Get or set the Axial force resultant along local beam axis 1.
        """ # nopep8
        return self._cards[2].get_value("f11")

    @f11.setter
    def f11(self, value: float) -> None:
        self._cards[2].set_value("f11", value)

    @property
    def t11(self) -> float:
        """Get or set the Torsional moment resultant about local beam axis 1.
        """ # nopep8
        return self._cards[2].get_value("t11")

    @t11.setter
    def t11(self, value: float) -> None:
        self._cards[2].set_value("t11", value)

    @property
    def m12(self) -> float:
        """Get or set the Moment resultant at node 1 about local beam axis 2.
        """ # nopep8
        return self._cards[2].get_value("m12")

    @m12.setter
    def m12(self, value: float) -> None:
        self._cards[2].set_value("m12", value)

    @property
    def m13(self) -> float:
        """Get or set the Moment resultant at node 1 about local beam axis 3.
        """ # nopep8
        return self._cards[2].get_value("m13")

    @m13.setter
    def m13(self, value: float) -> None:
        self._cards[2].set_value("m13", value)

    @property
    def m22(self) -> float:
        """Get or set the Moment resultant at node 2 about local beam axis 2.
        """ # nopep8
        return self._cards[2].get_value("m22")

    @m22.setter
    def m22(self, value: float) -> None:
        self._cards[2].set_value("m22", value)

    @property
    def m23(self) -> float:
        """Get or set the Moment resultant at node 2 about local beam axis 3.
        """ # nopep8
        return self._cards[3].get_value("m23")

    @m23.setter
    def m23(self, value: float) -> None:
        self._cards[3].set_value("m23", value)

    @property
    def parm(self) -> float:
        """Get or set the Generally not used
        """ # nopep8
        return self._cards[3].get_value("parm")

    @parm.setter
    def parm(self, value: float) -> None:
        self._cards[3].set_value("parm", value)

    @property
    def hisv1(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[3].get_value("hisv1")

    @hisv1.setter
    def hisv1(self, value: float) -> None:
        self._cards[3].set_value("hisv1", value)

    @property
    def hisv2(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[3].get_value("hisv2")

    @hisv2.setter
    def hisv2(self, value: float) -> None:
        self._cards[3].set_value("hisv2", value)

    @property
    def hisv3(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[3].get_value("hisv3")

    @hisv3.setter
    def hisv3(self, value: float) -> None:
        self._cards[3].set_value("hisv3", value)

    @property
    def sig11(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[4].get_value("sig11")

    @sig11.setter
    def sig11(self, value: float) -> None:
        self._cards[4].set_value("sig11", value)

    @property
    def sig22(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[4].get_value("sig22")

    @sig22.setter
    def sig22(self, value: float) -> None:
        self._cards[4].set_value("sig22", value)

    @property
    def sig33(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[4].get_value("sig33")

    @sig33.setter
    def sig33(self, value: float) -> None:
        self._cards[4].set_value("sig33", value)

    @property
    def sig12(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[4].get_value("sig12")

    @sig12.setter
    def sig12(self, value: float) -> None:
        self._cards[4].set_value("sig12", value)

    @property
    def sig23(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[4].get_value("sig23")

    @sig23.setter
    def sig23(self, value: float) -> None:
        self._cards[4].set_value("sig23", value)

    @property
    def sig31(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[4].get_value("sig31")

    @sig31.setter
    def sig31(self, value: float) -> None:
        self._cards[4].set_value("sig31", value)

    @property
    def eps(self) -> float:
        """Get or set the Effective plastic strain
        """ # nopep8
        return self._cards[4].get_value("eps")

    @eps.setter
    def eps(self, value: float) -> None:
        self._cards[4].set_value("eps", value)

    @property
    def sig11(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[5].get_value("sig11")

    @sig11.setter
    def sig11(self, value: float) -> None:
        self._cards[5].set_value("sig11", value)

    @property
    def sig22(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[5].get_value("sig22")

    @sig22.setter
    def sig22(self, value: float) -> None:
        self._cards[5].set_value("sig22", value)

    @property
    def sig33(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[5].get_value("sig33")

    @sig33.setter
    def sig33(self, value: float) -> None:
        self._cards[5].set_value("sig33", value)

    @property
    def sig12(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[5].get_value("sig12")

    @sig12.setter
    def sig12(self, value: float) -> None:
        self._cards[5].set_value("sig12", value)

    @property
    def sig23(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[5].get_value("sig23")

    @sig23.setter
    def sig23(self, value: float) -> None:
        self._cards[5].set_value("sig23", value)

    @property
    def sig31(self) -> float:
        """Get or set the Define the ij stress component.
        """ # nopep8
        return self._cards[6].get_value("sig31")

    @sig31.setter
    def sig31(self, value: float) -> None:
        self._cards[6].set_value("sig31", value)

    @property
    def eps(self) -> float:
        """Get or set the Effective plastic strain
        """ # nopep8
        return self._cards[6].get_value("eps")

    @eps.setter
    def eps(self, value: float) -> None:
        self._cards[6].set_value("eps", value)

    @property
    def hisv1(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[6].get_value("hisv1")

    @hisv1.setter
    def hisv1(self, value: float) -> None:
        self._cards[6].set_value("hisv1", value)

    @property
    def hisv2(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[6].get_value("hisv2")

    @hisv2.setter
    def hisv2(self, value: float) -> None:
        self._cards[6].set_value("hisv2", value)

    @property
    def hisv3(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[6].get_value("hisv3")

    @hisv3.setter
    def hisv3(self, value: float) -> None:
        self._cards[6].set_value("hisv3", value)

    @property
    def hisv4(self) -> float:
        """Get or set the Define the nth history variable.
        """ # nopep8
        return self._cards[7].get_value("hisv4")

    @hisv4.setter
    def hisv4(self, value: float) -> None:
        self._cards[7].set_value("hisv4", value)

    @property
    def hisv5(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[7].get_value("hisv5")

    @hisv5.setter
    def hisv5(self, value: float) -> None:
        self._cards[7].set_value("hisv5", value)

    @property
    def hisv6(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[7].get_value("hisv6")

    @hisv6.setter
    def hisv6(self, value: float) -> None:
        self._cards[7].set_value("hisv6", value)

    @property
    def hisv7(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[7].get_value("hisv7")

    @hisv7.setter
    def hisv7(self, value: float) -> None:
        self._cards[7].set_value("hisv7", value)

    @property
    def hisv8(self) -> float:
        """Get or set the Define the nth history variable
        """ # nopep8
        return self._cards[7].get_value("hisv8")

    @hisv8.setter
    def hisv8(self, value: float) -> None:
        self._cards[7].set_value("hisv8", value)

    @property
    def ax1(self) -> float:
        """Get or set the The nth local axes value.
        """ # nopep8
        return self._cards[8].get_value("ax1")

    @ax1.setter
    def ax1(self, value: float) -> None:
        self._cards[8].set_value("ax1", value)

    @property
    def ax2(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[8].get_value("ax2")

    @ax2.setter
    def ax2(self, value: float) -> None:
        self._cards[8].set_value("ax2", value)

    @property
    def ax3(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[8].get_value("ax3")

    @ax3.setter
    def ax3(self, value: float) -> None:
        self._cards[8].set_value("ax3", value)

    @property
    def ax4(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[8].get_value("ax4")

    @ax4.setter
    def ax4(self, value: float) -> None:
        self._cards[8].set_value("ax4", value)

    @property
    def ax5(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[8].get_value("ax5")

    @ax5.setter
    def ax5(self, value: float) -> None:
        self._cards[8].set_value("ax5", value)

    @property
    def ax6(self) -> float:
        """Get or set the The nth local axes value.
        """ # nopep8
        return self._cards[9].get_value("ax6")

    @ax6.setter
    def ax6(self, value: float) -> None:
        self._cards[9].set_value("ax6", value)

    @property
    def ax7(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[9].get_value("ax7")

    @ax7.setter
    def ax7(self, value: float) -> None:
        self._cards[9].set_value("ax7", value)

    @property
    def ax8(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[9].get_value("ax8")

    @ax8.setter
    def ax8(self, value: float) -> None:
        self._cards[9].set_value("ax8", value)

    @property
    def ax9(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[9].get_value("ax9")

    @ax9.setter
    def ax9(self, value: float) -> None:
        self._cards[9].set_value("ax9", value)

    @property
    def ax10(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[9].get_value("ax10")

    @ax10.setter
    def ax10(self, value: float) -> None:
        self._cards[9].set_value("ax10", value)

    @property
    def ax11(self) -> float:
        """Get or set the The nth local axes value.
        """ # nopep8
        return self._cards[10].get_value("ax11")

    @ax11.setter
    def ax11(self, value: float) -> None:
        self._cards[10].set_value("ax11", value)

    @property
    def ax12(self) -> float:
        """Get or set the The nth local axes value
        """ # nopep8
        return self._cards[10].get_value("ax12")

    @ax12.setter
    def ax12(self, value: float) -> None:
        self._cards[10].set_value("ax12", value)

