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

class ComponentHybridiiiJointRibcage(KeywordBase):
    """DYNA COMPONENT_HYBRIDIII_JOINT_RIBCAGE keyword"""

    keyword = "COMPONENT"
    subkeyword = "HYBRIDIII_JOINT_RIBCAGE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "did",
                        int,
                        0,
                        10,
                        kwargs.get("did")
                    ),
                    Field(
                        "q1",
                        float,
                        10,
                        10,
                        kwargs.get("q1", 0.0)
                    ),
                    Field(
                        "q2",
                        float,
                        20,
                        10,
                        kwargs.get("q2", 0.0)
                    ),
                    Field(
                        "q3",
                        float,
                        30,
                        10,
                        kwargs.get("q3", 0.0)
                    ),
                    Field(
                        "fric",
                        float,
                        40,
                        10,
                        kwargs.get("fric", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c1",
                        float,
                        0,
                        10,
                        kwargs.get("c1", 0.0)
                    ),
                    Field(
                        "alo1",
                        float,
                        10,
                        10,
                        kwargs.get("alo1", 0.0)
                    ),
                    Field(
                        "blo1",
                        float,
                        20,
                        10,
                        kwargs.get("blo1", 0.0)
                    ),
                    Field(
                        "ahi1",
                        float,
                        30,
                        10,
                        kwargs.get("ahi1", 0.0)
                    ),
                    Field(
                        "bhi1",
                        float,
                        40,
                        10,
                        kwargs.get("bhi1", 0.0)
                    ),
                    Field(
                        "qlo1",
                        float,
                        50,
                        10,
                        kwargs.get("qlo1", 0.0)
                    ),
                    Field(
                        "qhi1",
                        float,
                        60,
                        10,
                        kwargs.get("qhi1", 0.0)
                    ),
                    Field(
                        "sclk1",
                        float,
                        70,
                        10,
                        kwargs.get("sclk1", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c2",
                        float,
                        0,
                        10,
                        kwargs.get("c2", 0.0)
                    ),
                    Field(
                        "alo2",
                        float,
                        10,
                        10,
                        kwargs.get("alo2", 0.0)
                    ),
                    Field(
                        "blo2",
                        float,
                        20,
                        10,
                        kwargs.get("blo2", 0.0)
                    ),
                    Field(
                        "ahi2",
                        float,
                        30,
                        10,
                        kwargs.get("ahi2", 0.0)
                    ),
                    Field(
                        "bhi2",
                        float,
                        40,
                        10,
                        kwargs.get("bhi2", 0.0)
                    ),
                    Field(
                        "qlo2",
                        float,
                        50,
                        10,
                        kwargs.get("qlo2", 0.0)
                    ),
                    Field(
                        "qhi2",
                        float,
                        60,
                        10,
                        kwargs.get("qhi2", 0.0)
                    ),
                    Field(
                        "sclk2",
                        float,
                        70,
                        10,
                        kwargs.get("sclk2", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c3",
                        float,
                        0,
                        10,
                        kwargs.get("c3", 0.0)
                    ),
                    Field(
                        "alo3",
                        float,
                        10,
                        10,
                        kwargs.get("alo3", 0.0)
                    ),
                    Field(
                        "blo3",
                        float,
                        20,
                        10,
                        kwargs.get("blo3", 0.0)
                    ),
                    Field(
                        "ahi3",
                        float,
                        30,
                        10,
                        kwargs.get("ahi3", 0.0)
                    ),
                    Field(
                        "bhi3",
                        float,
                        40,
                        10,
                        kwargs.get("bhi3", 0.0)
                    ),
                    Field(
                        "qlo3",
                        float,
                        50,
                        10,
                        kwargs.get("qlo3", 0.0)
                    ),
                    Field(
                        "qhi3",
                        float,
                        60,
                        10,
                        kwargs.get("qhi3", 0.0)
                    ),
                    Field(
                        "sclk3",
                        float,
                        70,
                        10,
                        kwargs.get("sclk3", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def did(self) -> typing.Optional[int]:
        """Get or set the Dummy ID, see *COMPONENT_HYBRIDIII.
        """ # nopep8
        return self._cards[0].get_value("did")

    @did.setter
    def did(self, value: int) -> None:
        self._cards[0].set_value("did", value)

    @property
    def q1(self) -> float:
        """Get or set the Initial value of the joint's first degree of freedom. Units of degrees are defined for rotational DOF. See Appendix K of the USER'S MANUAL for a listing of the applicable DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[0].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        self._cards[0].set_value("q1", value)

    @property
    def q2(self) -> float:
        """Get or set the Initial value of the joint's second degree of freedom. Units of degrees are defined for rotational DOF. See Appendix K of the USER'S MANUAL for a listing of the applicable DOF
        Default is set to zero.
        """ # nopep8
        return self._cards[0].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        self._cards[0].set_value("q2", value)

    @property
    def q3(self) -> float:
        """Get or set the Initial value of the joint's third degree of freedom. Units of degrees are defined for rotational DOF. See Appendix K of the USER'S MANUAL for a listing of the applicable DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[0].get_value("q3")

    @q3.setter
    def q3(self, value: float) -> None:
        self._cards[0].set_value("q3", value)

    @property
    def fric(self) -> float:
        """Get or set the Friction load on the joint.
        Default is set to zero.
        """ # nopep8
        return self._cards[0].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[0].set_value("fric", value)

    @property
    def c1(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the first DOF of the joint.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def alo1(self) -> float:
        """Get or set the Linear coefficient for the low regime spring of the joint's first DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("alo1")

    @alo1.setter
    def alo1(self, value: float) -> None:
        self._cards[1].set_value("alo1", value)

    @property
    def blo1(self) -> float:
        """Get or set the Cubic coefficient for the low regime spring of the joint's first DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("blo1")

    @blo1.setter
    def blo1(self, value: float) -> None:
        self._cards[1].set_value("blo1", value)

    @property
    def ahi1(self) -> float:
        """Get or set the Linear coefficient for the high regime spring of the joint's first DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("ahi1")

    @ahi1.setter
    def ahi1(self, value: float) -> None:
        self._cards[1].set_value("ahi1", value)

    @property
    def bhi1(self) -> float:
        """Get or set the Cubic coefficient for the high regime spring of the joint's first DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("bhi1")

    @bhi1.setter
    def bhi1(self, value: float) -> None:
        self._cards[1].set_value("bhi1", value)

    @property
    def qlo1(self) -> float:
        """Get or set the Value for which the low regime spring definition becomes active.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("qlo1")

    @qlo1.setter
    def qlo1(self, value: float) -> None:
        self._cards[1].set_value("qlo1", value)

    @property
    def qhi1(self) -> float:
        """Get or set the Value for which the high regime spring definition becomes active.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("qhi1")

    @qhi1.setter
    def qhi1(self, value: float) -> None:
        self._cards[1].set_value("qhi1", value)

    @property
    def sclk1(self) -> float:
        """Get or set the Scale value applied to the stiffness of the joint's first DOF (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("sclk1")

    @sclk1.setter
    def sclk1(self, value: float) -> None:
        self._cards[1].set_value("sclk1", value)

    @property
    def c2(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the second DOF of the joint.
        Default is set to zero.
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[2].set_value("c2", value)

    @property
    def alo2(self) -> float:
        """Get or set the Linear coefficient for the low regime spring of the joint's second DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[2].get_value("alo2")

    @alo2.setter
    def alo2(self, value: float) -> None:
        self._cards[2].set_value("alo2", value)

    @property
    def blo2(self) -> float:
        """Get or set the Cubic coefficient for the low regime spring of the joint's second DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[2].get_value("blo2")

    @blo2.setter
    def blo2(self, value: float) -> None:
        self._cards[2].set_value("blo2", value)

    @property
    def ahi2(self) -> float:
        """Get or set the Linear coefficient for the high regime spring of the joint's second DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[2].get_value("ahi2")

    @ahi2.setter
    def ahi2(self, value: float) -> None:
        self._cards[2].set_value("ahi2", value)

    @property
    def bhi2(self) -> float:
        """Get or set the Cubic coefficient for the high regime spring of the joint's second DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[2].get_value("bhi2")

    @bhi2.setter
    def bhi2(self, value: float) -> None:
        self._cards[2].set_value("bhi2", value)

    @property
    def qlo2(self) -> float:
        """Get or set the Value for which the low regime spring definition becomes active.
        Default is set to zero.
        """ # nopep8
        return self._cards[2].get_value("qlo2")

    @qlo2.setter
    def qlo2(self, value: float) -> None:
        self._cards[2].set_value("qlo2", value)

    @property
    def qhi2(self) -> float:
        """Get or set the Value for which the high regime spring definition becomes active.
        Default is set to zero.
        """ # nopep8
        return self._cards[2].get_value("qhi2")

    @qhi2.setter
    def qhi2(self, value: float) -> None:
        self._cards[2].set_value("qhi2", value)

    @property
    def sclk2(self) -> float:
        """Get or set the Scale value applied to the stiffness of the joint's second DOF (default=1.0).
        """ # nopep8
        return self._cards[2].get_value("sclk2")

    @sclk2.setter
    def sclk2(self, value: float) -> None:
        self._cards[2].set_value("sclk2", value)

    @property
    def c3(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the third DOF of the joint.
        Default is set to zero.
        """ # nopep8
        return self._cards[3].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[3].set_value("c3", value)

    @property
    def alo3(self) -> float:
        """Get or set the Linear coefficient for the low regime spring of the joint's third DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[3].get_value("alo3")

    @alo3.setter
    def alo3(self, value: float) -> None:
        self._cards[3].set_value("alo3", value)

    @property
    def blo3(self) -> float:
        """Get or set the Cubic coefficient for the low regime spring of the joint's third DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[3].get_value("blo3")

    @blo3.setter
    def blo3(self, value: float) -> None:
        self._cards[3].set_value("blo3", value)

    @property
    def ahi3(self) -> float:
        """Get or set the Linear coefficient for the high regime spring of the joint's third DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[3].get_value("ahi3")

    @ahi3.setter
    def ahi3(self, value: float) -> None:
        self._cards[3].set_value("ahi3", value)

    @property
    def bhi3(self) -> float:
        """Get or set the Cubic coefficient for the high regime spring of the joint's third DOF.
        Default is set to zero.
        """ # nopep8
        return self._cards[3].get_value("bhi3")

    @bhi3.setter
    def bhi3(self, value: float) -> None:
        self._cards[3].set_value("bhi3", value)

    @property
    def qlo3(self) -> float:
        """Get or set the Value for which the low regime spring definition becomes active.
        Default is set to zero.
        """ # nopep8
        return self._cards[3].get_value("qlo3")

    @qlo3.setter
    def qlo3(self, value: float) -> None:
        self._cards[3].set_value("qlo3", value)

    @property
    def qhi3(self) -> float:
        """Get or set the Value for which the high regime spring definition becomes active.
        Default is set to zero.
        """ # nopep8
        return self._cards[3].get_value("qhi3")

    @qhi3.setter
    def qhi3(self, value: float) -> None:
        self._cards[3].set_value("qhi3", value)

    @property
    def sclk3(self) -> float:
        """Get or set the Scale value applied to the stiffness of the joint's third DOF (default=1.0).
        """ # nopep8
        return self._cards[3].get_value("sclk3")

    @sclk3.setter
    def sclk3(self, value: float) -> None:
        self._cards[3].set_value("sclk3", value)

