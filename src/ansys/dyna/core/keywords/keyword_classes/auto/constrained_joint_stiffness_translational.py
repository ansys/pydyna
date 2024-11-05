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

class ConstrainedJointStiffnessTranslational(KeywordBase):
    """DYNA CONSTRAINED_JOINT_STIFFNESS_TRANSLATIONAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_STIFFNESS_TRANSLATIONAL"

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
                        "lcidx",
                        int,
                        0,
                        10,
                        kwargs.get("lcidx")
                    ),
                    Field(
                        "lcidy",
                        int,
                        10,
                        10,
                        kwargs.get("lcidy")
                    ),
                    Field(
                        "lcidz",
                        int,
                        20,
                        10,
                        kwargs.get("lcidz")
                    ),
                    Field(
                        "dlcidx",
                        int,
                        30,
                        10,
                        kwargs.get("dlcidx")
                    ),
                    Field(
                        "dlcidy",
                        int,
                        40,
                        10,
                        kwargs.get("dlcidy")
                    ),
                    Field(
                        "dlcidz",
                        int,
                        50,
                        10,
                        kwargs.get("dlcidz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "esx",
                        float,
                        0,
                        10,
                        kwargs.get("esx", 0.0)
                    ),
                    Field(
                        "ffx",
                        float,
                        10,
                        10,
                        kwargs.get("ffx", 0.0)
                    ),
                    Field(
                        "esy",
                        float,
                        20,
                        10,
                        kwargs.get("esy", 0.0)
                    ),
                    Field(
                        "ffy",
                        float,
                        30,
                        10,
                        kwargs.get("ffy", 0.0)
                    ),
                    Field(
                        "esz",
                        float,
                        40,
                        10,
                        kwargs.get("esz", 0.0)
                    ),
                    Field(
                        "ffz",
                        float,
                        50,
                        10,
                        kwargs.get("ffz", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsdx",
                        float,
                        0,
                        10,
                        kwargs.get("nsdx")
                    ),
                    Field(
                        "psdx",
                        float,
                        10,
                        10,
                        kwargs.get("psdx")
                    ),
                    Field(
                        "nsdy",
                        float,
                        20,
                        10,
                        kwargs.get("nsdy")
                    ),
                    Field(
                        "psdy",
                        float,
                        30,
                        10,
                        kwargs.get("psdy")
                    ),
                    Field(
                        "nsdz",
                        float,
                        40,
                        10,
                        kwargs.get("nsdz")
                    ),
                    Field(
                        "psdz",
                        float,
                        50,
                        10,
                        kwargs.get("psdz")
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
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for x force versus x-translational relative displacement between the origins of CIDA and CIDB based on the x-direction of CIDB. If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        self._cards[1].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for y force versus y-translational relative displacement between the origins of CIDA and CIDB based on the y-direction of CIDB.  If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        self._cards[1].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for z force versus z-translational relative displacement between the origins of CIDA and CIDB based on the z-direction of CIDB.  If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        self._cards[1].set_value("lcidz", value)

    @property
    def dlcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for x damping force versus rate of x-translational displacement per unit time between the origins of CIDA and CIDB based on the x-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidx")

    @dlcidx.setter
    def dlcidx(self, value: int) -> None:
        self._cards[1].set_value("dlcidx", value)

    @property
    def dlcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for y damping force versus rate of y-translational displacement per unit time between the origins of CIDA and CIDB based on the y-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidy")

    @dlcidy.setter
    def dlcidy(self, value: int) -> None:
        self._cards[1].set_value("dlcidy", value)

    @property
    def dlcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for z damping force versus rate of z-translational displacement per unit time between the origins of CIDA and CIDB based on the z-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("dlcidz")

    @dlcidz.setter
    def dlcidz(self, value: int) -> None:
        self._cards[1].set_value("dlcidz", value)

    @property
    def esx(self) -> float:
        """Get or set the Elastic stiffness for friction and stop displacement for x-translation.  If zero, friction and stop angles are inactive for x-translation.
        """ # nopep8
        return self._cards[2].get_value("esx")

    @esx.setter
    def esx(self, value: float) -> None:
        self._cards[2].set_value("esx", value)

    @property
    def ffx(self) -> float:
        """Get or set the Frictional force limiting value for x-translation.  If zero, friction is inactive for x-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus x-translation.
        """ # nopep8
        return self._cards[2].get_value("ffx")

    @ffx.setter
    def ffx(self, value: float) -> None:
        self._cards[2].set_value("ffx", value)

    @property
    def esy(self) -> float:
        """Get or set the Elastic stiffness for friction and stop displacement for y-translation.   If zero, friction and stop angles are inactive for y-translation.
        """ # nopep8
        return self._cards[2].get_value("esy")

    @esy.setter
    def esy(self, value: float) -> None:
        self._cards[2].set_value("esy", value)

    @property
    def ffy(self) -> float:
        """Get or set the Frictional force limiting value for y-translation.  If zero, friction is inactive for y-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus y-translation.
        """ # nopep8
        return self._cards[2].get_value("ffy")

    @ffy.setter
    def ffy(self, value: float) -> None:
        self._cards[2].set_value("ffy", value)

    @property
    def esz(self) -> float:
        """Get or set the Elastic stiffness for friction and stop displacement for z-translation.  If zero, friction and stop angles are inactive for z-translation.
        """ # nopep8
        return self._cards[2].get_value("esz")

    @esz.setter
    def esz(self, value: float) -> None:
        self._cards[2].set_value("esz", value)

    @property
    def ffz(self) -> float:
        """Get or set the Frictional force limiting value for z-translation.  If zero, friction is inactive for z-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus z-translation.
        """ # nopep8
        return self._cards[2].get_value("ffz")

    @ffz.setter
    def ffz(self, value: float) -> None:
        self._cards[2].set_value("ffz", value)

    @property
    def nsdx(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for negative x-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("nsdx")

    @nsdx.setter
    def nsdx(self, value: float) -> None:
        self._cards[3].set_value("nsdx", value)

    @property
    def psdx(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for positive x-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("psdx")

    @psdx.setter
    def psdx(self, value: float) -> None:
        self._cards[3].set_value("psdx", value)

    @property
    def nsdy(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for negative y-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("nsdy")

    @nsdy.setter
    def nsdy(self, value: float) -> None:
        self._cards[3].set_value("nsdy", value)

    @property
    def psdy(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for positive y-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("psdy")

    @psdy.setter
    def psdy(self, value: float) -> None:
        self._cards[3].set_value("psdy", value)

    @property
    def nsdz(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for negative z-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("nsdz")

    @nsdz.setter
    def nsdz(self, value: float) -> None:
        self._cards[3].set_value("nsdz", value)

    @property
    def psdz(self) -> typing.Optional[float]:
        """Get or set the Stop displacement for positive z-translation.  Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("psdz")

    @psdz.setter
    def psdz(self, value: float) -> None:
        self._cards[3].set_value("psdz", value)

