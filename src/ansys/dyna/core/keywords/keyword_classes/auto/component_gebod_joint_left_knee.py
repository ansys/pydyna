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

class ComponentGebodJointLeftKnee(KeywordBase):
    """DYNA COMPONENT_GEBOD_JOINT_LEFT_KNEE keyword"""

    keyword = "COMPONENT"
    subkeyword = "GEBOD_JOINT_LEFT_KNEE"

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
                        "lc1",
                        int,
                        10,
                        10,
                        kwargs.get("lc1", 0)
                    ),
                    Field(
                        "lc2",
                        int,
                        20,
                        10,
                        kwargs.get("lc2", 0)
                    ),
                    Field(
                        "lc3",
                        int,
                        30,
                        10,
                        kwargs.get("lc3", 0)
                    ),
                    Field(
                        "scf1",
                        float,
                        40,
                        10,
                        kwargs.get("scf1", 1.0)
                    ),
                    Field(
                        "scf2",
                        float,
                        50,
                        10,
                        kwargs.get("scf2", 1.0)
                    ),
                    Field(
                        "scf3",
                        float,
                        60,
                        10,
                        kwargs.get("scf3", 1.0)
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
                        "c2",
                        float,
                        10,
                        10,
                        kwargs.get("c2", 0.0)
                    ),
                    Field(
                        "c3",
                        float,
                        20,
                        10,
                        kwargs.get("c3", 0.0)
                    ),
                    Field(
                        "neut1",
                        float,
                        30,
                        10,
                        kwargs.get("neut1", 0.0)
                    ),
                    Field(
                        "neut2",
                        float,
                        40,
                        10,
                        kwargs.get("neut2", 0.0)
                    ),
                    Field(
                        "neut3",
                        float,
                        50,
                        10,
                        kwargs.get("neut3", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "losa1",
                        float,
                        0,
                        10,
                        kwargs.get("losa1", 0.0)
                    ),
                    Field(
                        "hisa1",
                        float,
                        10,
                        10,
                        kwargs.get("hisa1", 0.0)
                    ),
                    Field(
                        "losa2",
                        float,
                        20,
                        10,
                        kwargs.get("losa2", 0.0)
                    ),
                    Field(
                        "hisa2",
                        float,
                        30,
                        10,
                        kwargs.get("hisa2", 0.0)
                    ),
                    Field(
                        "losa3",
                        float,
                        40,
                        10,
                        kwargs.get("losa3", 0.0)
                    ),
                    Field(
                        "hisa3",
                        float,
                        50,
                        10,
                        kwargs.get("hisa3", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unk1",
                        float,
                        0,
                        10,
                        kwargs.get("unk1", 0.0)
                    ),
                    Field(
                        "unk2",
                        float,
                        10,
                        10,
                        kwargs.get("unk2", 0.0)
                    ),
                    Field(
                        "unk3",
                        float,
                        20,
                        10,
                        kwargs.get("unk3", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def did(self) -> typing.Optional[int]:
        """Get or set the Dummy ID, see *COMPONENT_GEBOD_MALE, *COMPONENT_GEBOD_FEMALE, *COMPONENT_GEBOD_CHILD.
        """ # nopep8
        return self._cards[0].get_value("did")

    @did.setter
    def did(self, value: int) -> None:
        self._cards[0].set_value("did", value)

    @property
    def lc1(self) -> int:
        """Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the first degree of freedom of the joint.
        """ # nopep8
        return self._cards[0].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        self._cards[0].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the second degree of freedom of the joint.
        """ # nopep8
        return self._cards[0].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        self._cards[0].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the third degree of freedom of the joint.
        """ # nopep8
        return self._cards[0].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        self._cards[0].set_value("lc3", value)

    @property
    def scf1(self) -> float:
        """Get or set the Scale factor applied to the load curve of the first joint degree of freedom.
        """ # nopep8
        return self._cards[0].get_value("scf1")

    @scf1.setter
    def scf1(self, value: float) -> None:
        self._cards[0].set_value("scf1", value)

    @property
    def scf2(self) -> float:
        """Get or set the Scale factor applied to the load curve of the second joint degree of freedom.
        """ # nopep8
        return self._cards[0].get_value("scf2")

    @scf2.setter
    def scf2(self, value: float) -> None:
        self._cards[0].set_value("scf2", value)

    @property
    def scf3(self) -> float:
        """Get or set the Scale factor applied to the load curve of the third joint degree of freedom.
        """ # nopep8
        return self._cards[0].get_value("scf3")

    @scf3.setter
    def scf3(self, value: float) -> None:
        self._cards[0].set_value("scf3", value)

    @property
    def c1(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the first DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the second DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[1].set_value("c2", value)

    @property
    def c3(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the third DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[1].set_value("c3", value)

    @property
    def neut1(self) -> float:
        """Get or set the Neutral angle (degrees) of joint's first DOF.
        """ # nopep8
        return self._cards[1].get_value("neut1")

    @neut1.setter
    def neut1(self, value: float) -> None:
        self._cards[1].set_value("neut1", value)

    @property
    def neut2(self) -> float:
        """Get or set the Neutral angle (degrees) of joint's second DOF.
        """ # nopep8
        return self._cards[1].get_value("neut2")

    @neut2.setter
    def neut2(self, value: float) -> None:
        self._cards[1].set_value("neut2", value)

    @property
    def neut3(self) -> float:
        """Get or set the Neutral angle (degrees) of joint's third DOF.
        """ # nopep8
        return self._cards[1].get_value("neut3")

    @neut3.setter
    def neut3(self, value: float) -> None:
        self._cards[1].set_value("neut3", value)

    @property
    def losa1(self) -> float:
        """Get or set the Value of the low stop angle (degrees) for the first DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("losa1")

    @losa1.setter
    def losa1(self, value: float) -> None:
        self._cards[2].set_value("losa1", value)

    @property
    def hisa1(self) -> float:
        """Get or set the Value of the high stop angle (degrees) for the first DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("hisa1")

    @hisa1.setter
    def hisa1(self, value: float) -> None:
        self._cards[2].set_value("hisa1", value)

    @property
    def losa2(self) -> float:
        """Get or set the Value of the low stop angle (degrees) for the second DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("losa2")

    @losa2.setter
    def losa2(self, value: float) -> None:
        self._cards[2].set_value("losa2", value)

    @property
    def hisa2(self) -> float:
        """Get or set the Value of the high stop angle (degrees) for the second DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("hisa2")

    @hisa2.setter
    def hisa2(self, value: float) -> None:
        self._cards[2].set_value("hisa2", value)

    @property
    def losa3(self) -> float:
        """Get or set the Value of the low stop angle (degrees) for the third DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("losa3")

    @losa3.setter
    def losa3(self, value: float) -> None:
        self._cards[2].set_value("losa3", value)

    @property
    def hisa3(self) -> float:
        """Get or set the Value of the high stop angle (degrees) for the third DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("hisa3")

    @hisa3.setter
    def hisa3(self, value: float) -> None:
        self._cards[2].set_value("hisa3", value)

    @property
    def unk1(self) -> float:
        """Get or set the Unloading stiffness (torque/radian) for the first degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[3].get_value("unk1")

    @unk1.setter
    def unk1(self, value: float) -> None:
        self._cards[3].set_value("unk1", value)

    @property
    def unk2(self) -> float:
        """Get or set the Unloading stiffness (torque/radian) for the second degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[3].get_value("unk2")

    @unk2.setter
    def unk2(self, value: float) -> None:
        self._cards[3].set_value("unk2", value)

    @property
    def unk3(self) -> float:
        """Get or set the Unloading stiffness (torque/radian) for the third degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[3].get_value("unk3")

    @unk3.setter
    def unk3(self, value: float) -> None:
        self._cards[3].set_value("unk3", value)

