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

class ConstrainedJointStiffnessFlexionTorsion(KeywordBase):
    """DYNA CONSTRAINED_JOINT_STIFFNESS_FLEXION-TORSION keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_STIFFNESS_FLEXION-TORSION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "jsid",
                        int,
                        0,
                        10,
                        kwargs.get("jsid")
                    ),
                    Field(
                        "pida",
                        int,
                        10,
                        10,
                        kwargs.get("pida")
                    ),
                    Field(
                        "pidb",
                        int,
                        20,
                        10,
                        kwargs.get("pidb")
                    ),
                    Field(
                        "cida",
                        int,
                        30,
                        10,
                        kwargs.get("cida")
                    ),
                    Field(
                        "cidb",
                        int,
                        40,
                        10,
                        kwargs.get("cidb", 0)
                    ),
                    Field(
                        "jid",
                        int,
                        50,
                        10,
                        kwargs.get("jid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidal",
                        int,
                        0,
                        10,
                        kwargs.get("lcidal", 0)
                    ),
                    Field(
                        "lcidg",
                        int,
                        10,
                        10,
                        kwargs.get("lcidg", 0)
                    ),
                    Field(
                        "lcidbt",
                        int,
                        20,
                        10,
                        kwargs.get("lcidbt", 0)
                    ),
                    Field(
                        "dlcidal",
                        int,
                        30,
                        10,
                        kwargs.get("dlcidal", 0)
                    ),
                    Field(
                        "dlcidg",
                        int,
                        40,
                        10,
                        kwargs.get("dlcidg", 0)
                    ),
                    Field(
                        "dlcidbt",
                        int,
                        50,
                        10,
                        kwargs.get("dlcidbt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "esal",
                        float,
                        0,
                        10,
                        kwargs.get("esal", 0.0)
                    ),
                    Field(
                        "fmal",
                        float,
                        10,
                        10,
                        kwargs.get("fmal", 0.0)
                    ),
                    Field(
                        "esbt",
                        float,
                        20,
                        10,
                        kwargs.get("esbt", 0.0)
                    ),
                    Field(
                        "fmbt",
                        float,
                        30,
                        10,
                        kwargs.get("fmbt", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "saal",
                        float,
                        0,
                        10,
                        kwargs.get("saal", 0)
                    ),
                    Field(
                        "nsabt",
                        float,
                        10,
                        10,
                        kwargs.get("nsabt", 0)
                    ),
                    Field(
                        "psabt",
                        float,
                        20,
                        10,
                        kwargs.get("psabt", 0)
                    ),
                ],
            ),
        ]

    @property
    def jsid(self) -> typing.Optional[int]:
        """Get or set the Joint stiffness ID.
        """ # nopep8
        return self._cards[0].get_value("jsid")

    @jsid.setter
    def jsid(self, value: int) -> None:
        self._cards[0].set_value("jsid", value)

    @property
    def pida(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body A, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pida")

    @pida.setter
    def pida(self, value: int) -> None:
        self._cards[0].set_value("pida", value)

    @property
    def pidb(self) -> typing.Optional[int]:
        """Get or set the Part ID for rigid body B, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pidb")

    @pidb.setter
    def pidb(self, value: int) -> None:
        self._cards[0].set_value("pidb", value)

    @property
    def cida(self) -> typing.Optional[int]:
        """Get or set the Coordinate ID for rigid body A, see *DEFINE_COORDINATE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("cida")

    @cida.setter
    def cida(self, value: int) -> None:
        self._cards[0].set_value("cida", value)

    @property
    def cidb(self) -> int:
        """Get or set the Coordinate ID for rigid body B.
        If zero, the coordinate ID for rigid body A is used (default).See *DEFINE_COORDINATE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("cidb")

    @cidb.setter
    def cidb(self, value: int) -> None:
        self._cards[0].set_value("cidb", value)

    @property
    def jid(self) -> typing.Optional[int]:
        """Get or set the Joint ID for the joint reaction forces. If zero, tables can t be used in place of load curves for defining the frictional moments.
        """ # nopep8
        return self._cards[0].get_value("jid")

    @jid.setter
    def jid(self, value: int) -> None:
        self._cards[0].set_value("jid", value)

    @property
    def lcidal(self) -> int:
        """Get or set the Load curve ID for alpha-moment versus rotation in radian, where it should be noted that 0 <= alpha <= pi.
        If zero, the applied moment is set to zero (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidal")

    @lcidal.setter
    def lcidal(self, value: int) -> None:
        self._cards[1].set_value("lcidal", value)

    @property
    def lcidg(self) -> int:
        """Get or set the Load curve ID for gamma versus a scale factor which scales the bending moment due to the alpaha rotation. This load curve should be defined in the interval -pi <= gamma <= pi.
        If zero, the scale factor defaults to 1. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidg")

    @lcidg.setter
    def lcidg(self, value: int) -> None:
        self._cards[1].set_value("lcidg", value)

    @property
    def lcidbt(self) -> int:
        """Get or set the Load curve ID for beta-torsion moment versus twist in radians.
        If zero, the applied twist is set to zero (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidbt")

    @lcidbt.setter
    def lcidbt(self, value: int) -> None:
        self._cards[1].set_value("lcidbt", value)

    @property
    def dlcidal(self) -> int:
        """Get or set the Load curve ID for alpha-damping moment versus rate of rotation in radians per unit time.
        If zero, damping is not considered (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidal")

    @dlcidal.setter
    def dlcidal(self, value: int) -> None:
        self._cards[1].set_value("dlcidal", value)

    @property
    def dlcidg(self) -> int:
        """Get or set the Load curve ID for gamma-damping scale factor versus rate of rotation in radians per unit time. This scale factor scales the alpha-damping moment.
        If zero, the scale factor defaults to 1. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidg")

    @dlcidg.setter
    def dlcidg(self, value: int) -> None:
        self._cards[1].set_value("dlcidg", value)

    @property
    def dlcidbt(self) -> int:
        """Get or set the Load curve ID for beta-damping torque versus rate of twist.
        If zero, damping is not considered (default). See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidbt")

    @dlcidbt.setter
    def dlcidbt(self, value: int) -> None:
        self._cards[1].set_value("dlcidbt", value)

    @property
    def esal(self) -> float:
        """Get or set the Elastic stiffness per unit radian for friction and stop angles for alpha rotation.
        If zero, friction and stop angles are inactive for alpha rotation (default).
        """ # nopep8
        return self._cards[2].get_value("esal")

    @esal.setter
    def esal(self, value: float) -> None:
        self._cards[2].set_value("esal", value)

    @property
    def fmal(self) -> float:
        """Get or set the Frictional moment limiting value for alpha rotation. If zero, friction is inactive for alpha rotation. This option may also be thought of as an elastic-plastic spring.
        """ # nopep8
        return self._cards[2].get_value("fmal")

    @fmal.setter
    def fmal(self, value: float) -> None:
        self._cards[2].set_value("fmal", value)

    @property
    def esbt(self) -> float:
        """Get or set the Elastic stiffness per unit radian for friction and stop angles for beta twist.
        If zero, friction and stop angles are inactive for beta twist (default).
        """ # nopep8
        return self._cards[2].get_value("esbt")

    @esbt.setter
    def esbt(self, value: float) -> None:
        self._cards[2].set_value("esbt", value)

    @property
    def fmbt(self) -> float:
        """Get or set the Frictional moment limiting value for beta twist. If zero, friction is inactive for beta twist. This option may also be thought of as an elastic-plastic spring.
        """ # nopep8
        return self._cards[2].get_value("fmbt")

    @fmbt.setter
    def fmbt(self, value: float) -> None:
        self._cards[2].set_value("fmbt", value)

    @property
    def saal(self) -> float:
        """Get or set the Stop angle in degrees for alpha rotation where 0 <= alpha <= pi.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("saal")

    @saal.setter
    def saal(self, value: float) -> None:
        self._cards[3].set_value("saal", value)

    @property
    def nsabt(self) -> float:
        """Get or set the Stop angle in degrees for negative beta rotation.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("nsabt")

    @nsabt.setter
    def nsabt(self, value: float) -> None:
        self._cards[3].set_value("nsabt", value)

    @property
    def psabt(self) -> float:
        """Get or set the Stop angle in degrees for positive beta rotation.
        If zero, stop angle is ignored (default).
        """ # nopep8
        return self._cards[3].get_value("psabt")

    @psabt.setter
    def psabt(self, value: float) -> None:
        self._cards[3].set_value("psabt", value)

