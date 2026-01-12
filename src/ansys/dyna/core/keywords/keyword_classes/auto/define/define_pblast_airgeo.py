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

"""Module providing the DefinePblastAirgeo class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEPBLASTAIRGEO_CARD0 = (
    FieldSchema("gid", int, 0, 10, 0),
    FieldSchema("gtype1", int, 10, 10, 1),
    FieldSchema("gtype2", int, 20, 10, 1),
)

_DEFINEPBLASTAIRGEO_CARD1 = (
    FieldSchema("xa", float, 0, 10, 0.0),
    FieldSchema("ya", float, 10, 10, 0.0),
    FieldSchema("za", float, 20, 10, 0.0),
    FieldSchema("xb", float, 30, 10, 0.0),
    FieldSchema("yb", float, 40, 10, 0.0),
    FieldSchema("zb", float, 50, 10, 0.0),
)

_DEFINEPBLASTAIRGEO_CARD2 = (
    FieldSchema("x0", float, 0, 10, 0.0),
    FieldSchema("y0", float, 10, 10, 0.0),
    FieldSchema("z0", float, 20, 10, 0.0),
    FieldSchema("g1", float, 30, 10, 0.0),
    FieldSchema("g2", float, 40, 10, 0.0),
    FieldSchema("g3", float, 50, 10, 0.0),
)

_DEFINEPBLASTAIRGEO_CARD3 = (
    FieldSchema("xc", float, 0, 10, 0.0),
    FieldSchema("yc", float, 10, 10, 0.0),
    FieldSchema("zc", float, 20, 10, 0.0),
    FieldSchema("g4", float, 30, 10, 0.0),
    FieldSchema("g5", float, 40, 10, 0.0),
    FieldSchema("g6", float, 50, 10, 0.0),
)

_DEFINEPBLASTAIRGEO_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefinePblastAirgeo(KeywordBase):
    """DYNA DEFINE_PBLAST_AIRGEO keyword"""

    keyword = "DEFINE"
    subkeyword = "PBLAST_AIRGEO"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefinePblastAirgeo class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEPBLASTAIRGEO_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPBLASTAIRGEO_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPBLASTAIRGEO_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEPBLASTAIRGEO_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefinePblastAirgeo.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEPBLASTAIRGEO_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def gid(self) -> int:
        """Get or set the ID of a GEOMETRY defining initial air particle domain.
        """ # nopep8
        return self._cards[0].get_value("gid")

    @gid.setter
    def gid(self, value: int) -> None:
        """Set the gid property."""
        self._cards[0].set_value("gid", value)

    @property
    def gtype1(self) -> int:
        """Get or set the Geometry type
        EQ.1: box
        EQ.2: sphere
        EQ.3: cylinder
        EQ.4: ellipsoid
        EQ.5: hemisphere (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("gtype1")

    @gtype1.setter
    def gtype1(self, value: int) -> None:
        """Set the gtype1 property."""
        if value not in [1, 2, 3, 4, 5, None]:
            raise Exception("""gtype1 must be `None` or one of {1,2,3,4,5}.""")
        self._cards[0].set_value("gtype1", value)

    @property
    def gtype2(self) -> int:
        """Get or set the Geometry type
        EQ.1: box
        EQ.2: sphere
        EQ.3: cylinder
        EQ.4: ellipsoid
        EQ.5: hemisphere (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("gtype2")

    @gtype2.setter
    def gtype2(self, value: int) -> None:
        """Set the gtype2 property."""
        if value not in [1, 2, 3, 4, 5, None]:
            raise Exception("""gtype2 must be `None` or one of {1,2,3,4,5}.""")
        self._cards[0].set_value("gtype2", value)

    @property
    def xa(self) -> float:
        """Get or set the (XA, YA, ZA) defines a vector of the x-axis.
        """ # nopep8
        return self._cards[1].get_value("xa")

    @xa.setter
    def xa(self, value: float) -> None:
        """Set the xa property."""
        self._cards[1].set_value("xa", value)

    @property
    def ya(self) -> float:
        """Get or set the (XA, YA, ZA) defines a vector of the x-axis.
        """ # nopep8
        return self._cards[1].get_value("ya")

    @ya.setter
    def ya(self, value: float) -> None:
        """Set the ya property."""
        self._cards[1].set_value("ya", value)

    @property
    def za(self) -> float:
        """Get or set the (XA, YA, ZA) defines a vector of the x-axis.
        """ # nopep8
        return self._cards[1].get_value("za")

    @za.setter
    def za(self, value: float) -> None:
        """Set the za property."""
        self._cards[1].set_value("za", value)

    @property
    def xb(self) -> float:
        """Get or set the (XB, YB, ZB) defines a vector of the y-axis.
        """ # nopep8
        return self._cards[1].get_value("xb")

    @xb.setter
    def xb(self, value: float) -> None:
        """Set the xb property."""
        self._cards[1].set_value("xb", value)

    @property
    def yb(self) -> float:
        """Get or set the (XB, YB, ZB) defines a vector of the y-axis.
        """ # nopep8
        return self._cards[1].get_value("yb")

    @yb.setter
    def yb(self, value: float) -> None:
        """Set the yb property."""
        self._cards[1].set_value("yb", value)

    @property
    def zb(self) -> float:
        """Get or set the (XB, YB, ZB) defines a vector of the y-axis.
        """ # nopep8
        return self._cards[1].get_value("zb")

    @zb.setter
    def zb(self, value: float) -> None:
        """Set the zb property."""
        self._cards[1].set_value("zb", value)

    @property
    def x0(self) -> float:
        """Get or set the Center coordinates of air domain.
        """ # nopep8
        return self._cards[2].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[2].set_value("x0", value)

    @property
    def y0(self) -> float:
        """Get or set the Center coordinates of air domain.
        """ # nopep8
        return self._cards[2].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[2].set_value("y0", value)

    @property
    def z0(self) -> float:
        """Get or set the Center coordinates of air domain.
        """ # nopep8
        return self._cards[2].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        """Set the z0 property."""
        self._cards[2].set_value("z0", value)

    @property
    def g1(self) -> float:
        """Get or set the Dimension value depending on GTYPE.
        GTYPE.EQ.1: length of x edge
        GTYPE.EQ.2: Radius of sphere
        GTYPE.EQ.3: Radius of cross section
        GTYPE.EQ.4: length of x-axes
        GTYPE.EQ.5: Radius of hemisphere.
        """ # nopep8
        return self._cards[2].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        """Set the g1 property."""
        self._cards[2].set_value("g1", value)

    @property
    def g2(self) -> float:
        """Get or set the Dimension value depending on GTYPE.
        GTYPE.EQ.1: length of y edge
        GTYPE.EQ.3: length of cylinder
        GTYPE.EQ.4: length of y-axes.
        """ # nopep8
        return self._cards[2].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        """Set the g2 property."""
        self._cards[2].set_value("g2", value)

    @property
    def g3(self) -> float:
        """Get or set the Dimension value depending on GTYPE.
        GTYPE.EQ.1: length of z edge
        GTYPE.EQ.4: length of z-axes.
        """ # nopep8
        return self._cards[2].get_value("g3")

    @g3.setter
    def g3(self, value: float) -> None:
        """Set the g3 property."""
        self._cards[2].set_value("g3", value)

    @property
    def xc(self) -> float:
        """Get or set the Center coordinates of domain excluded from the air domain.
        """ # nopep8
        return self._cards[3].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        """Set the xc property."""
        self._cards[3].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Center coordinates of domain excluded from the air domain.
        """ # nopep8
        return self._cards[3].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        """Set the yc property."""
        self._cards[3].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Center coordinates of domain excluded from the air domain.
        """ # nopep8
        return self._cards[3].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        """Set the zc property."""
        self._cards[3].set_value("zc", value)

    @property
    def g4(self) -> float:
        """Get or set the See definition of G1, G2, G3.
        """ # nopep8
        return self._cards[3].get_value("g4")

    @g4.setter
    def g4(self, value: float) -> None:
        """Set the g4 property."""
        self._cards[3].set_value("g4", value)

    @property
    def g5(self) -> float:
        """Get or set the See definition of G1, G2, G3.
        """ # nopep8
        return self._cards[3].get_value("g5")

    @g5.setter
    def g5(self, value: float) -> None:
        """Set the g5 property."""
        self._cards[3].set_value("g5", value)

    @property
    def g6(self) -> float:
        """Get or set the See definition of G1, G2, G3.
        """ # nopep8
        return self._cards[3].get_value("g6")

    @g6.setter
    def g6(self, value: float) -> None:
        """Set the g6 property."""
        self._cards[3].set_value("g6", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

