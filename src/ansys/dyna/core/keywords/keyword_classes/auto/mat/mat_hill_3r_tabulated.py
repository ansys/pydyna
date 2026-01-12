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

"""Module providing the MatHill3RTabulated class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATHILL3RTABULATED_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("hr", float, 40, 10, 1.0),
)

_MATHILL3RTABULATED_CARD1 = (
    FieldSchema("r00", float, 0, 10, None),
    FieldSchema("r45", float, 10, 10, None),
    FieldSchema("r90", float, 20, 10, None),
    FieldSchema("lc00", int, 30, 10, None),
    FieldSchema("iconv", int, 40, 10, None),
    FieldSchema("lc90", int, 50, 10, None),
    FieldSchema("lc45", int, 60, 10, None),
    FieldSchema("lcex", int, 70, 10, None),
)

_MATHILL3RTABULATED_CARD2 = (
    FieldSchema("aopt", int, 0, 10, None),
)

_MATHILL3RTABULATED_CARD3 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATHILL3RTABULATED_CARD4 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

class MatHill3RTabulated(KeywordBase):
    """DYNA MAT_HILL_3R_TABULATED keyword"""

    keyword = "MAT"
    subkeyword = "HILL_3R_TABULATED"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatHill3RTabulated class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATHILL3RTABULATED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHILL3RTABULATED_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHILL3RTABULATED_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHILL3RTABULATED_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATHILL3RTABULATED_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatHill3RTabulated.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus, E.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio, v.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def hr(self) -> float:
        """Get or set the Hardening rule:
        EQ.1.0: linear (default),
        EQ.2.0: exponential.
        EQ3.0: load curve.
        """ # nopep8
        return self._cards[0].get_value("hr")

    @hr.setter
    def hr(self, value: float) -> None:
        """Set the hr property."""
        if value not in [1.0, 2.0, 3.0, None]:
            raise Exception("""hr must be `None` or one of {1.0,2.0,3.0}.""")
        self._cards[0].set_value("hr", value)

    @property
    def r00(self) -> typing.Optional[float]:
        """Get or set the R00, Lankford parameter determined from experiments.
        """ # nopep8
        return self._cards[1].get_value("r00")

    @r00.setter
    def r00(self, value: float) -> None:
        """Set the r00 property."""
        self._cards[1].set_value("r00", value)

    @property
    def r45(self) -> typing.Optional[float]:
        """Get or set the R45, Lankford parameter determined from experiments.
        """ # nopep8
        return self._cards[1].get_value("r45")

    @r45.setter
    def r45(self, value: float) -> None:
        """Set the r45 property."""
        self._cards[1].set_value("r45", value)

    @property
    def r90(self) -> typing.Optional[float]:
        """Get or set the R90, Lankford parameter determined from experiments.
        """ # nopep8
        return self._cards[1].get_value("r90")

    @r90.setter
    def r90(self, value: float) -> None:
        """Set the r90 property."""
        self._cards[1].set_value("r90", value)

    @property
    def lc00(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the yield curve in the 0° direction
        """ # nopep8
        return self._cards[1].get_value("lc00")

    @lc00.setter
    def lc00(self, value: int) -> None:
        """Set the lc00 property."""
        self._cards[1].set_value("lc00", value)

    @property
    def iconv(self) -> typing.Optional[int]:
        """Get or set the Convexity option:
        EQ.0.0:	convexity of the yield surface is not enforced.
        EQ.1.0 : convexity of the yield surface is enforced.
        """ # nopep8
        return self._cards[1].get_value("iconv")

    @iconv.setter
    def iconv(self, value: int) -> None:
        """Set the iconv property."""
        self._cards[1].set_value("iconv", value)

    @property
    def lc90(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the yield curve in the 90° direction
        """ # nopep8
        return self._cards[1].get_value("lc90")

    @lc90.setter
    def lc90(self, value: int) -> None:
        """Set the lc90 property."""
        self._cards[1].set_value("lc90", value)

    @property
    def lc45(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the yield curve in the 45° direction
        """ # nopep8
        return self._cards[1].get_value("lc45")

    @lc45.setter
    def lc45(self, value: int) -> None:
        """Set the lc45 property."""
        self._cards[1].set_value("lc45", value)

    @property
    def lcex(self) -> typing.Optional[int]:
        """Get or set the Absolute value is load curve ID for the yield curve in shear or biaxial:
        GT.0.0:	shear yield is provided
        LT.0.0 : biaxial yield is provided
        """ # nopep8
        return self._cards[1].get_value("lcex")

    @lcex.setter
    def lcex(self, value: int) -> None:
        """Set the lcex property."""
        self._cards[1].set_value("lcex", value)

    @property
    def aopt(self) -> typing.Optional[int]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        """Set the aopt property."""
        self._cards[2].set_value("aopt", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

