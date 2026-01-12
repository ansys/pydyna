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

"""Module providing the ControlExplicitThermalProperties class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLEXPLICITTHERMALPROPERTIES_CARD0 = (
    FieldSchema("partset", int, 0, 10, None),
    FieldSchema("cp", float, 10, 10, None),
    FieldSchema("cptyp", int, 20, 10, 0),
    FieldSchema("vecid1", int, 30, 10, 0),
    FieldSchema("vecid2", int, 40, 10, 0),
    FieldSchema("local", int, 50, 10, 0),
)

_CONTROLEXPLICITTHERMALPROPERTIES_CARD1 = (
    FieldSchema("kxx", float, 0, 10, 0.0),
    FieldSchema("kxy", float, 10, 10, 0.0),
    FieldSchema("kxz", float, 20, 10, 0.0),
    FieldSchema("kxxtyp", int, 30, 10, 0),
    FieldSchema("kxytyp", int, 40, 10, 0),
    FieldSchema("kxztyp", int, 50, 10, 0),
)

_CONTROLEXPLICITTHERMALPROPERTIES_CARD2 = (
    FieldSchema("kyx", float, 0, 10, 0.0),
    FieldSchema("kyy", float, 10, 10, 0.0),
    FieldSchema("kyz", float, 20, 10, 0.0),
    FieldSchema("kyxtyp", int, 30, 10, 0),
    FieldSchema("kyytyp", int, 40, 10, 0),
    FieldSchema("kyztyp", int, 50, 10, 0),
)

_CONTROLEXPLICITTHERMALPROPERTIES_CARD3 = (
    FieldSchema("kzx", float, 0, 10, 0.0),
    FieldSchema("kzy", float, 10, 10, 0.0),
    FieldSchema("kzz", float, 20, 10, 0.0),
    FieldSchema("kzxtyp", int, 30, 10, 0),
    FieldSchema("kzytyp", int, 40, 10, 0),
    FieldSchema("kzztyp", int, 50, 10, 0),
)

class ControlExplicitThermalProperties(KeywordBase):
    """DYNA CONTROL_EXPLICIT_THERMAL_PROPERTIES keyword"""

    keyword = "CONTROL"
    subkeyword = "EXPLICIT_THERMAL_PROPERTIES"

    def __init__(self, **kwargs):
        """Initialize the ControlExplicitThermalProperties class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLEXPLICITTHERMALPROPERTIES_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLEXPLICITTHERMALPROPERTIES_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLEXPLICITTHERMALPROPERTIES_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLEXPLICITTHERMALPROPERTIES_CARD3,
                **kwargs,
            ),        ]
    @property
    def partset(self) -> typing.Optional[int]:
        """Get or set the Part set ID (See *SET_PART).
        """ # nopep8
        return self._cards[0].get_value("partset")

    @partset.setter
    def partset(self, value: int) -> None:
        """Set the partset property."""
        self._cards[0].set_value("partset", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Heat capacity.
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
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
        """Set the cptyp property."""
        if value not in [0, 1, None]:
            raise Exception("""cptyp must be `None` or one of {0,1}.""")
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
        """Set the vecid1 property."""
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
        """Set the vecid2 property."""
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
        """Set the local property."""
        if value not in [0, 1, None]:
            raise Exception("""local must be `None` or one of {0,1}.""")
        self._cards[0].set_value("local", value)

    @property
    def kxx(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[1].get_value("kxx")

    @kxx.setter
    def kxx(self, value: float) -> None:
        """Set the kxx property."""
        self._cards[1].set_value("kxx", value)

    @property
    def kxy(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[1].get_value("kxy")

    @kxy.setter
    def kxy(self, value: float) -> None:
        """Set the kxy property."""
        self._cards[1].set_value("kxy", value)

    @property
    def kxz(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[1].get_value("kxz")

    @kxz.setter
    def kxz(self, value: float) -> None:
        """Set the kxz property."""
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
        """Set the kxxtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kxxtyp must be `None` or one of {0,1}.""")
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
        """Set the kxytyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kxytyp must be `None` or one of {0,1}.""")
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
        """Set the kxztyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kxztyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("kxztyp", value)

    @property
    def kyx(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[2].get_value("kyx")

    @kyx.setter
    def kyx(self, value: float) -> None:
        """Set the kyx property."""
        self._cards[2].set_value("kyx", value)

    @property
    def kyy(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[2].get_value("kyy")

    @kyy.setter
    def kyy(self, value: float) -> None:
        """Set the kyy property."""
        self._cards[2].set_value("kyy", value)

    @property
    def kyz(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[2].get_value("kyz")

    @kyz.setter
    def kyz(self, value: float) -> None:
        """Set the kyz property."""
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
        """Set the kyxtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kyxtyp must be `None` or one of {0,1}.""")
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
        """Set the kyytyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kyytyp must be `None` or one of {0,1}.""")
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
        """Set the kyztyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kyztyp must be `None` or one of {0,1}.""")
        self._cards[2].set_value("kyztyp", value)

    @property
    def kzx(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[3].get_value("kzx")

    @kzx.setter
    def kzx(self, value: float) -> None:
        """Set the kzx property."""
        self._cards[3].set_value("kzx", value)

    @property
    def kzy(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[3].get_value("kzy")

    @kzy.setter
    def kzy(self, value: float) -> None:
        """Set the kzy property."""
        self._cards[3].set_value("kzy", value)

    @property
    def kzz(self) -> float:
        """Get or set the Heat conductivity matrix.
        """ # nopep8
        return self._cards[3].get_value("kzz")

    @kzz.setter
    def kzz(self, value: float) -> None:
        """Set the kzz property."""
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
        """Set the kzxtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kzxtyp must be `None` or one of {0,1}.""")
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
        """Set the kzytyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kzytyp must be `None` or one of {0,1}.""")
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
        """Set the kzztyp property."""
        if value not in [0, 1, None]:
            raise Exception("""kzztyp must be `None` or one of {0,1}.""")
        self._cards[3].set_value("kzztyp", value)

