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

"""Module providing the ComponentGebodJointLeftElbow class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ComponentGebodJointLeftElbow(KeywordBase):
    """DYNA COMPONENT_GEBOD_JOINT_LEFT_ELBOW keyword"""

    keyword = "COMPONENT"
    subkeyword = "GEBOD_JOINT_LEFT_ELBOW"

    def __init__(self, **kwargs):
        """Initialize the ComponentGebodJointLeftElbow class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "did",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc1",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lc2",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lc3",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "scf1",
                        float,
                        40,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "scf2",
                        float,
                        50,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "scf3",
                        float,
                        60,
                        10,
                        1.0,
                        **kwargs,
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
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "c2",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "c3",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "neut1",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "neut2",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "neut3",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
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
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "hisa1",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "losa2",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "hisa2",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "losa3",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "hisa3",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
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
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "unk2",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "unk3",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
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
        """Set the did property."""
        self._cards[0].set_value("did", value)

    @property
    def lc1(self) -> int:
        """Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the first degree of freedom of the joint.
        """ # nopep8
        return self._cards[0].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[0].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the second degree of freedom of the joint.
        """ # nopep8
        return self._cards[0].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[0].set_value("lc2", value)

    @property
    def lc3(self) -> int:
        """Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the third degree of freedom of the joint.
        """ # nopep8
        return self._cards[0].get_value("lc3")

    @lc3.setter
    def lc3(self, value: int) -> None:
        """Set the lc3 property."""
        self._cards[0].set_value("lc3", value)

    @property
    def scf1(self) -> float:
        """Get or set the Scale factor applied to the load curve of the first joint degree of freedom.
        """ # nopep8
        return self._cards[0].get_value("scf1")

    @scf1.setter
    def scf1(self, value: float) -> None:
        """Set the scf1 property."""
        self._cards[0].set_value("scf1", value)

    @property
    def scf2(self) -> float:
        """Get or set the Scale factor applied to the load curve of the second joint degree of freedom.
        """ # nopep8
        return self._cards[0].get_value("scf2")

    @scf2.setter
    def scf2(self, value: float) -> None:
        """Set the scf2 property."""
        self._cards[0].set_value("scf2", value)

    @property
    def scf3(self) -> float:
        """Get or set the Scale factor applied to the load curve of the third joint degree of freedom.
        """ # nopep8
        return self._cards[0].get_value("scf3")

    @scf3.setter
    def scf3(self, value: float) -> None:
        """Set the scf3 property."""
        self._cards[0].set_value("scf3", value)

    @property
    def c1(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the first DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the second DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def c3(self) -> float:
        """Get or set the Linear viscous damping coefficient applied to the third DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[1].set_value("c3", value)

    @property
    def neut1(self) -> float:
        """Get or set the Neutral angle (degrees) of joint's first DOF.
        """ # nopep8
        return self._cards[1].get_value("neut1")

    @neut1.setter
    def neut1(self, value: float) -> None:
        """Set the neut1 property."""
        self._cards[1].set_value("neut1", value)

    @property
    def neut2(self) -> float:
        """Get or set the Neutral angle (degrees) of joint's second DOF.
        """ # nopep8
        return self._cards[1].get_value("neut2")

    @neut2.setter
    def neut2(self, value: float) -> None:
        """Set the neut2 property."""
        self._cards[1].set_value("neut2", value)

    @property
    def neut3(self) -> float:
        """Get or set the Neutral angle (degrees) of joint's third DOF.
        """ # nopep8
        return self._cards[1].get_value("neut3")

    @neut3.setter
    def neut3(self, value: float) -> None:
        """Set the neut3 property."""
        self._cards[1].set_value("neut3", value)

    @property
    def losa1(self) -> float:
        """Get or set the Value of the low stop angle (degrees) for the first DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("losa1")

    @losa1.setter
    def losa1(self, value: float) -> None:
        """Set the losa1 property."""
        self._cards[2].set_value("losa1", value)

    @property
    def hisa1(self) -> float:
        """Get or set the Value of the high stop angle (degrees) for the first DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("hisa1")

    @hisa1.setter
    def hisa1(self, value: float) -> None:
        """Set the hisa1 property."""
        self._cards[2].set_value("hisa1", value)

    @property
    def losa2(self) -> float:
        """Get or set the Value of the low stop angle (degrees) for the second DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("losa2")

    @losa2.setter
    def losa2(self, value: float) -> None:
        """Set the losa2 property."""
        self._cards[2].set_value("losa2", value)

    @property
    def hisa2(self) -> float:
        """Get or set the Value of the high stop angle (degrees) for the second DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("hisa2")

    @hisa2.setter
    def hisa2(self, value: float) -> None:
        """Set the hisa2 property."""
        self._cards[2].set_value("hisa2", value)

    @property
    def losa3(self) -> float:
        """Get or set the Value of the low stop angle (degrees) for the third DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("losa3")

    @losa3.setter
    def losa3(self, value: float) -> None:
        """Set the losa3 property."""
        self._cards[2].set_value("losa3", value)

    @property
    def hisa3(self) -> float:
        """Get or set the Value of the high stop angle (degrees) for the third DOF of this joint.
        """ # nopep8
        return self._cards[2].get_value("hisa3")

    @hisa3.setter
    def hisa3(self, value: float) -> None:
        """Set the hisa3 property."""
        self._cards[2].set_value("hisa3", value)

    @property
    def unk1(self) -> float:
        """Get or set the Unloading stiffness (torque/radian) for the first degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[3].get_value("unk1")

    @unk1.setter
    def unk1(self, value: float) -> None:
        """Set the unk1 property."""
        self._cards[3].set_value("unk1", value)

    @property
    def unk2(self) -> float:
        """Get or set the Unloading stiffness (torque/radian) for the second degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[3].get_value("unk2")

    @unk2.setter
    def unk2(self, value: float) -> None:
        """Set the unk2 property."""
        self._cards[3].set_value("unk2", value)

    @property
    def unk3(self) -> float:
        """Get or set the Unloading stiffness (torque/radian) for the third degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[3].get_value("unk3")

    @unk3.setter
    def unk3(self, value: float) -> None:
        """Set the unk3 property."""
        self._cards[3].set_value("unk3", value)

