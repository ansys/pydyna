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

class ConstrainedJointStiffnessCylindrical(KeywordBase):
    """DYNA CONSTRAINED_JOINT_STIFFNESS_CYLINDRICAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_STIFFNESS_CYLINDRICAL"

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
                        "lcidr",
                        int,
                        0,
                        10,
                        kwargs.get("lcidr", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "lcidz",
                        int,
                        20,
                        10,
                        kwargs.get("lcidz", 0)
                    ),
                    Field(
                        "dlcidr",
                        int,
                        30,
                        10,
                        kwargs.get("dlcidr", 0)
                    ),
                    Field(
                        "dlcidp",
                        int,
                        40,
                        10,
                        kwargs.get("dlcidp", 0)
                    ),
                    Field(
                        "dlcidz",
                        int,
                        50,
                        10,
                        kwargs.get("dlcidz", 0)
                    ),
                    Field(
                        "lcidt",
                        int,
                        60,
                        10,
                        kwargs.get("lcidt", 0)
                    ),
                    Field(
                        "dlcidt",
                        int,
                        70,
                        10,
                        kwargs.get("dlcidt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "esr",
                        float,
                        0,
                        10,
                        kwargs.get("esr", 0.0)
                    ),
                    Field(
                        "ffr",
                        float,
                        10,
                        10,
                        kwargs.get("ffr", 0.0)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
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
                    Field(
                        "rad1",
                        float,
                        60,
                        10,
                        kwargs.get("rad1", 0.0)
                    ),
                    Field(
                        "rad2",
                        float,
                        70,
                        10,
                        kwargs.get("rad2", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "psdr",
                        float,
                        10,
                        10,
                        kwargs.get("psdr", 0.0)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "nsdz",
                        float,
                        40,
                        10,
                        kwargs.get("nsdz", 0.0)
                    ),
                    Field(
                        "psdz",
                        float,
                        50,
                        10,
                        kwargs.get("psdz", 0.0)
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
    def lcidr(self) -> int:
        """Get or set the Load curve ID for r-force as a function of r-distance between the origins of
        CIDAand CIDB.See * DEFINE_CURVE.
        EQ.0: The applied force is set to 0.0.
        """ # nopep8
        return self._cards[1].get_value("lcidr")

    @lcidr.setter
    def lcidr(self, value: int) -> None:
        self._cards[1].set_value("lcidr", value)

    @property
    def lcidz(self) -> int:
        """Get or set the Load curve ID for z-force as a function of z-distance between the origins of
        CIDAand CIDB.See * DEFINE_CURVE.
        EQ.0: The applied force is set to 0.0.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        self._cards[1].set_value("lcidz", value)

    @property
    def dlcidr(self) -> int:
        """Get or set the Load curve or table ID for r-damping force as a function of rate of
        r-distance per unit timeand optionally r - distance(if table) between the
        origins of CIDAand CIDB.See * DEFINE_CURVE or *DEFINE_TABLE.
        EQ.0: Damping is not considered.
        """ # nopep8
        return self._cards[1].get_value("dlcidr")

    @dlcidr.setter
    def dlcidr(self, value: int) -> None:
        self._cards[1].set_value("dlcidr", value)

    @property
    def dlcidp(self) -> int:
        """Get or set the Load curve or table ID for p-damping force as a function of rate of
        p-distance per unit timeand optionally r - distance(if table) between the
        origins of CIDAand CIDB.See * DEFINE_CURVE or *DEFINE_TABLE.
        EQ.0: Damping is not considered.
        """ # nopep8
        return self._cards[1].get_value("dlcidp")

    @dlcidp.setter
    def dlcidp(self, value: int) -> None:
        self._cards[1].set_value("dlcidp", value)

    @property
    def dlcidz(self) -> int:
        """Get or set the Load curve or table ID for z-damping force as a function of rate of
        z-distance per unit timeand optionally r - distance(if table) between the
        origins of CIDAand CIDB.See * DEFINE_CURVE or *DEFINE_TABLE.
        EQ.0: Damping is not considered.
        """ # nopep8
        return self._cards[1].get_value("dlcidz")

    @dlcidz.setter
    def dlcidz(self, value: int) -> None:
        self._cards[1].set_value("dlcidz", value)

    @property
    def lcidt(self) -> int:
        """Get or set the Load curve ID for theta-moment as a function of angle theta between the
        z-directions of CIDAand CIDB.See * DEFINE_CURVE.
        EQ.0: The applied moment is set to 0.0.
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        self._cards[1].set_value("lcidt", value)

    @property
    def dlcidt(self) -> int:
        """Get or set the Load curve ID for theta-moment as a function of rate of angle theta between the
        z-directions of CIDAand CIDB.See * DEFINE_CURVE.
        EQ.0: The applied moment is set to 0.0.
        """ # nopep8
        return self._cards[1].get_value("dlcidt")

    @dlcidt.setter
    def dlcidt(self, value: int) -> None:
        self._cards[1].set_value("dlcidt", value)

    @property
    def esr(self) -> float:
        """Get or set the Elastic stiffness for friction and stop displacement for r-translation. See Figure 0 - 3.
        EQ.0.0: Friction and stop angles are inactive for r - translation.
        """ # nopep8
        return self._cards[2].get_value("esr")

    @esr.setter
    def esr(self, value: float) -> None:
        self._cards[2].set_value("esr", value)

    @property
    def ffr(self) -> float:
        """Get or set the Frictional force limiting value for r-translation. This option may also be
        thought of as an elastic - plastic spring.See Figure 0 - 3.
        EQ.0.0: Friction is inactive for r - translation.
        LT.0 : -FFR is the load curve ID defining the yield force as a function r - translation.
        """ # nopep8
        return self._cards[2].get_value("ffr")

    @ffr.setter
    def ffr(self, value: float) -> None:
        self._cards[2].set_value("ffr", value)

    @property
    def esz(self) -> float:
        """Get or set the Elastic stiffness for friction and stop displacement for z-translation.
        EQ.0.0: Friction and stop angles are inactive for z - translation.
        """ # nopep8
        return self._cards[2].get_value("esz")

    @esz.setter
    def esz(self, value: float) -> None:
        self._cards[2].set_value("esz", value)

    @property
    def ffz(self) -> float:
        """Get or set the Frictional force limiting value for 𝑧-translation. This option may also be thought of as an elastic - plastic spring.
        EQ.0.0: Friction is inactive for z - translation.
        LT.0 : -FFZ is the load curve ID defining the yield force as a function of z - translation.
        """ # nopep8
        return self._cards[2].get_value("ffz")

    @ffz.setter
    def ffz(self, value: float) -> None:
        self._cards[2].set_value("ffz", value)

    @property
    def rad1(self) -> float:
        """Get or set the Radius of pin, must be strictly positive.
        """ # nopep8
        return self._cards[2].get_value("rad1")

    @rad1.setter
    def rad1(self, value: float) -> None:
        self._cards[2].set_value("rad1", value)

    @property
    def rad2(self) -> float:
        """Get or set the Radius of hole, must be strictly larger than RAD1 to model a play in the connection.
        """ # nopep8
        return self._cards[2].get_value("rad2")

    @rad2.setter
    def rad2(self, value: float) -> None:
        self._cards[2].set_value("rad2", value)

    @property
    def psdr(self) -> float:
        """Get or set the Stop displacement for r-translation. Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("psdr")

    @psdr.setter
    def psdr(self, value: float) -> None:
        self._cards[3].set_value("psdr", value)

    @property
    def nsdz(self) -> float:
        """Get or set the Stop displacement for negative z-translation. Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("nsdz")

    @nsdz.setter
    def nsdz(self, value: float) -> None:
        self._cards[3].set_value("nsdz", value)

    @property
    def psdz(self) -> float:
        """Get or set the Stop displacement for positive z-translation. Ignored if zero.
        """ # nopep8
        return self._cards[3].get_value("psdz")

    @psdz.setter
    def psdz(self, value: float) -> None:
        self._cards[3].set_value("psdz", value)

