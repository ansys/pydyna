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

class ControlExplicitThermalProperties(KeywordBase):
    """DYNA CONTROL_EXPLICIT_THERMAL_PROPERTIES keyword"""

    keyword = "CONTROL"
    subkeyword = "EXPLICIT_THERMAL_PROPERTIES"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "partset",
                        int,
                        0,
                        10,
                        kwargs.get("partset")
                    ),
                    Field(
                        "cp",
                        float,
                        10,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "cptyp",
                        int,
                        20,
                        10,
                        kwargs.get("cptyp", 0)
                    ),
                    Field(
                        "vecid1",
                        int,
                        30,
                        10,
                        kwargs.get("vecid1", 0)
                    ),
                    Field(
                        "vecid2",
                        int,
                        40,
                        10,
                        kwargs.get("vecid2", 0)
                    ),
                    Field(
                        "local",
                        int,
                        50,
                        10,
                        kwargs.get("local", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "kxx",
                        float,
                        0,
                        10,
                        kwargs.get("kxx", 0.0)
                    ),
                    Field(
                        "kxy",
                        float,
                        10,
                        10,
                        kwargs.get("kxy", 0.0)
                    ),
                    Field(
                        "kxz",
                        float,
                        20,
                        10,
                        kwargs.get("kxz", 0.0)
                    ),
                    Field(
                        "kxxtyp",
                        int,
                        30,
                        10,
                        kwargs.get("kxxtyp", 0)
                    ),
                    Field(
                        "kxytyp",
                        int,
                        40,
                        10,
                        kwargs.get("kxytyp", 0)
                    ),
                    Field(
                        "kxztyp",
                        int,
                        50,
                        10,
                        kwargs.get("kxztyp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "kyx",
                        float,
                        0,
                        10,
                        kwargs.get("kyx", 0.0)
                    ),
                    Field(
                        "kyy",
                        float,
                        10,
                        10,
                        kwargs.get("kyy", 0.0)
                    ),
                    Field(
                        "kyz",
                        float,
                        20,
                        10,
                        kwargs.get("kyz", 0.0)
                    ),
                    Field(
                        "kyxtyp",
                        int,
                        30,
                        10,
                        kwargs.get("kyxtyp", 0)
                    ),
                    Field(
                        "kyytyp",
                        int,
                        40,
                        10,
                        kwargs.get("kyytyp", 0)
                    ),
                    Field(
                        "kyztyp",
                        int,
                        50,
                        10,
                        kwargs.get("kyztyp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "kzx",
                        float,
                        0,
                        10,
                        kwargs.get("kzx", 0.0)
                    ),
                    Field(
                        "kzy",
                        float,
                        10,
                        10,
                        kwargs.get("kzy", 0.0)
                    ),
                    Field(
                        "kzz",
                        float,
                        20,
                        10,
                        kwargs.get("kzz", 0.0)
                    ),
                    Field(
                        "kzxtyp",
                        int,
                        30,
                        10,
                        kwargs.get("kzxtyp", 0)
                    ),
                    Field(
                        "kzytyp",
                        int,
                        40,
                        10,
                        kwargs.get("kzytyp", 0)
                    ),
                    Field(
                        "kzztyp",
                        int,
                        50,
                        10,
                        kwargs.get("kzztyp", 0)
                    ),
                ],
            ),
        ]

    @property
    def partset(self) -> typing.Optional[int]:
        """Get or set the Part set ID (See *SET_PART).
        """ # nopep8
        return self._cards[0].get_value("partset")

    @partset.setter
    def partset(self, value: int) -> None:
        self._cards[0].set_value("partset", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Heat capacity.
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[0].set_value("cp", value)

    @property
    def cptyp(self) -> int:
        """Get or set the Type of CP:
        EQ.0:	CP is a constant
        EQ.1 : CP is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat capacity.
        """ # nopep8
        return self._cards[0].get_value("cptyp")

    @cptyp.setter
    def cptyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cptyp must be one of {0,1}""")
        self._cards[0].set_value("cptyp", value)

    @property
    def vecid1(self) -> int:
        """Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system.
        VECID1 and VECID2 give the x- and y-direction, respectively.
        The z-vector is a cross product of VECID1 and VECID2. If VECID2 is not orthogonal to VECID1,
        its direction will be corrected with a cross-product of the z- and x-vectors.
        The conductivity matrix Kij is applied in this coordinate system.
        """ # nopep8
        return self._cards[0].get_value("vecid1")

    @vecid1.setter
    def vecid1(self, value: int) -> None:
        self._cards[0].set_value("vecid1", value)

    @property
    def vecid2(self) -> int:
        """Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system.
        VECID1 and VECID2 give the x- and y-direction, respectively.
        The z-vector is a cross product of VECID1 and VECID2. If VECID2 is not orthogonal to VECID1,
        its direction will be corrected with a cross-product of the z- and x-vectors.
        The conductivity matrix Kij is applied in this coordinate system..
        """ # nopep8
        return self._cards[0].get_value("vecid2")

    @vecid2.setter
    def vecid2(self, value: int) -> None:
        self._cards[0].set_value("vecid2", value)

    @property
    def local(self) -> int:
        """Get or set the Flag to activate an element coordinate system:
        EQ.0:	The vectors VECIDj are considered in a global coordinate system.
        EQ.1 : The vectors VECIDj are considered in a local system attached to the element.
        For shells and solids, the system is the same as DIREC = 1 and CTYPE = 12 in * CONSTRAINED_LAGRANGE_IN_SOLID.
        For shells, the edge centers replace the face centers.For beams, the x - direction is aligned with
        the first 2 nodes in * ELEMENT_BEAM and there should be a 3rd node for the y - direction.
        """ # nopep8
        return self._cards[0].get_value("local")

    @local.setter
    def local(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""local must be one of {0,1}""")
        self._cards[0].set_value("local", value)

    @property
    def kxx(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[1].get_value("kxx")

    @kxx.setter
    def kxx(self, value: float) -> None:
        self._cards[1].set_value("kxx", value)

    @property
    def kxy(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[1].get_value("kxy")

    @kxy.setter
    def kxy(self, value: float) -> None:
        self._cards[1].set_value("kxy", value)

    @property
    def kxz(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[1].get_value("kxz")

    @kxz.setter
    def kxz(self, value: float) -> None:
        self._cards[1].set_value("kxz", value)

    @property
    def kxxtyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[1].get_value("kxxtyp")

    @kxxtyp.setter
    def kxxtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kxxtyp must be one of {0,1}""")
        self._cards[1].set_value("kxxtyp", value)

    @property
    def kxytyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[1].get_value("kxytyp")

    @kxytyp.setter
    def kxytyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kxytyp must be one of {0,1}""")
        self._cards[1].set_value("kxytyp", value)

    @property
    def kxztyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[1].get_value("kxztyp")

    @kxztyp.setter
    def kxztyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kxztyp must be one of {0,1}""")
        self._cards[1].set_value("kxztyp", value)

    @property
    def kyx(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[2].get_value("kyx")

    @kyx.setter
    def kyx(self, value: float) -> None:
        self._cards[2].set_value("kyx", value)

    @property
    def kyy(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[2].get_value("kyy")

    @kyy.setter
    def kyy(self, value: float) -> None:
        self._cards[2].set_value("kyy", value)

    @property
    def kyz(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[2].get_value("kyz")

    @kyz.setter
    def kyz(self, value: float) -> None:
        self._cards[2].set_value("kyz", value)

    @property
    def kyxtyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[2].get_value("kyxtyp")

    @kyxtyp.setter
    def kyxtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kyxtyp must be one of {0,1}""")
        self._cards[2].set_value("kyxtyp", value)

    @property
    def kyytyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[2].get_value("kyytyp")

    @kyytyp.setter
    def kyytyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kyytyp must be one of {0,1}""")
        self._cards[2].set_value("kyytyp", value)

    @property
    def kyztyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[2].get_value("kyztyp")

    @kyztyp.setter
    def kyztyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kyztyp must be one of {0,1}""")
        self._cards[2].set_value("kyztyp", value)

    @property
    def kzx(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[3].get_value("kzx")

    @kzx.setter
    def kzx(self, value: float) -> None:
        self._cards[3].set_value("kzx", value)

    @property
    def kzy(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[3].get_value("kzy")

    @kzy.setter
    def kzy(self, value: float) -> None:
        self._cards[3].set_value("kzy", value)

    @property
    def kzz(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[3].get_value("kzz")

    @kzz.setter
    def kzz(self, value: float) -> None:
        self._cards[3].set_value("kzz", value)

    @property
    def kzxtyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[3].get_value("kzxtyp")

    @kzxtyp.setter
    def kzxtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kzxtyp must be one of {0,1}""")
        self._cards[3].set_value("kzxtyp", value)

    @property
    def kzytyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[3].get_value("kzytyp")

    @kzytyp.setter
    def kzytyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kzytyp must be one of {0,1}""")
        self._cards[3].set_value("kzytyp", value)

    @property
    def kzztyp(self) -> int:
        """Get or set the Type of Kij:
        EQ.0:	Kij is a constant
        EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
        """ # nopep8
        return self._cards[3].get_value("kzztyp")

    @kzztyp.setter
    def kzztyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kzztyp must be one of {0,1}""")
        self._cards[3].set_value("kzztyp", value)

