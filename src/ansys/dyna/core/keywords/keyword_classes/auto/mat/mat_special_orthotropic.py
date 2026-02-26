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

"""Module providing the MatSpecialOrthotropic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATSPECIALORTHOTROPIC_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ys", float, 20, 10, None),
    FieldSchema("ep", float, 30, 10, None),
)

_MATSPECIALORTHOTROPIC_CARD1 = (
    FieldSchema("e11p", float, 0, 10, None),
    FieldSchema("e22p", float, 10, 10, None),
    FieldSchema("v12p", float, 20, 10, None),
    FieldSchema("v21p", float, 30, 10, None),
    FieldSchema("g12p", float, 40, 10, None),
    FieldSchema("g23p", float, 50, 10, None),
    FieldSchema("g31p", float, 60, 10, None),
)

_MATSPECIALORTHOTROPIC_CARD2 = (
    FieldSchema("e11b", float, 0, 10, None),
    FieldSchema("e22b", float, 10, 10, None),
    FieldSchema("v12b", float, 20, 10, None),
    FieldSchema("v21b", float, 30, 10, None),
    FieldSchema("g12b", float, 40, 10, None),
    FieldSchema("aopt", float, 50, 10, None),
)

_MATSPECIALORTHOTROPIC_CARD3 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATSPECIALORTHOTROPIC_CARD4 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MATSPECIALORTHOTROPIC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatSpecialOrthotropic(KeywordBase):
    """DYNA MAT_SPECIAL_ORTHOTROPIC keyword"""

    keyword = "MAT"
    subkeyword = "SPECIAL_ORTHOTROPIC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatSpecialOrthotropic class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSPECIALORTHOTROPIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSPECIALORTHOTROPIC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSPECIALORTHOTROPIC_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSPECIALORTHOTROPIC_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSPECIALORTHOTROPIC_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatSpecialOrthotropic.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATSPECIALORTHOTROPIC_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
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
    def ys(self) -> typing.Optional[float]:
        """Get or set the Yield stress. This parameter is optional and is approximates the yield condition. Set to zero if the behavior is elastic.
        """ # nopep8
        return self._cards[0].get_value("ys")

    @ys.setter
    def ys(self, value: float) -> None:
        """Set the ys property."""
        self._cards[0].set_value("ys", value)

    @property
    def ep(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus.
        """ # nopep8
        return self._cards[0].get_value("ep")

    @ep.setter
    def ep(self, value: float) -> None:
        """Set the ep property."""
        self._cards[0].set_value("ep", value)

    @property
    def e11p(self) -> typing.Optional[float]:
        """Get or set the E11p, for in plane behavior.
        """ # nopep8
        return self._cards[1].get_value("e11p")

    @e11p.setter
    def e11p(self, value: float) -> None:
        """Set the e11p property."""
        self._cards[1].set_value("e11p", value)

    @property
    def e22p(self) -> typing.Optional[float]:
        """Get or set the E22p, for in plane behavior.
        """ # nopep8
        return self._cards[1].get_value("e22p")

    @e22p.setter
    def e22p(self, value: float) -> None:
        """Set the e22p property."""
        self._cards[1].set_value("e22p", value)

    @property
    def v12p(self) -> typing.Optional[float]:
        """Get or set the v12p, for in plane behavior.
        """ # nopep8
        return self._cards[1].get_value("v12p")

    @v12p.setter
    def v12p(self, value: float) -> None:
        """Set the v12p property."""
        self._cards[1].set_value("v12p", value)

    @property
    def v21p(self) -> typing.Optional[float]:
        """Get or set the v21p, for in plane behavior.
        """ # nopep8
        return self._cards[1].get_value("v21p")

    @v21p.setter
    def v21p(self, value: float) -> None:
        """Set the v21p property."""
        self._cards[1].set_value("v21p", value)

    @property
    def g12p(self) -> typing.Optional[float]:
        """Get or set the G12p, for in plane behavior.
        """ # nopep8
        return self._cards[1].get_value("g12p")

    @g12p.setter
    def g12p(self, value: float) -> None:
        """Set the g12p property."""
        self._cards[1].set_value("g12p", value)

    @property
    def g23p(self) -> typing.Optional[float]:
        """Get or set the G23p, for in plane behavior.
        """ # nopep8
        return self._cards[1].get_value("g23p")

    @g23p.setter
    def g23p(self, value: float) -> None:
        """Set the g23p property."""
        self._cards[1].set_value("g23p", value)

    @property
    def g31p(self) -> typing.Optional[float]:
        """Get or set the G31p, for in plane behavior.
        """ # nopep8
        return self._cards[1].get_value("g31p")

    @g31p.setter
    def g31p(self, value: float) -> None:
        """Set the g31p property."""
        self._cards[1].set_value("g31p", value)

    @property
    def e11b(self) -> typing.Optional[float]:
        """Get or set the E11b, for bending behavior.
        """ # nopep8
        return self._cards[2].get_value("e11b")

    @e11b.setter
    def e11b(self, value: float) -> None:
        """Set the e11b property."""
        self._cards[2].set_value("e11b", value)

    @property
    def e22b(self) -> typing.Optional[float]:
        """Get or set the E22b, for bending behavior.
        """ # nopep8
        return self._cards[2].get_value("e22b")

    @e22b.setter
    def e22b(self, value: float) -> None:
        """Set the e22b property."""
        self._cards[2].set_value("e22b", value)

    @property
    def v12b(self) -> typing.Optional[float]:
        """Get or set the v12b, for bending behavior.
        """ # nopep8
        return self._cards[2].get_value("v12b")

    @v12b.setter
    def v12b(self, value: float) -> None:
        """Set the v12b property."""
        self._cards[2].set_value("v12b", value)

    @property
    def v21b(self) -> typing.Optional[float]:
        """Get or set the v21b, for bending behavior.
        """ # nopep8
        return self._cards[2].get_value("v21b")

    @v21b.setter
    def v21b(self, value: float) -> None:
        """Set the v21b property."""
        self._cards[2].set_value("v21b", value)

    @property
    def g12b(self) -> typing.Optional[float]:
        """Get or set the G12b , for bending behavior.
        """ # nopep8
        return self._cards[2].get_value("g12b")

    @g12b.setter
    def g12b(self, value: float) -> None:
        """Set the g12b property."""
        self._cards[2].set_value("g12b", value)

    @property
    def aopt(self) -> typing.Optional[float]:
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
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[2].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
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

