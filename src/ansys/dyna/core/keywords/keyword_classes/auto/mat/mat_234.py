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

"""Module providing the Mat234 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT234_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e1", float, 20, 10, None),
    FieldSchema("e2", float, 30, 10, None),
    FieldSchema("g12", float, 40, 10, None),
    FieldSchema("eu", float, 50, 10, None),
    FieldSchema("thl", float, 60, 10, None),
    FieldSchema("thi", float, 70, 10, None),
)

_MAT234_CARD1 = (
    FieldSchema("ta", float, 0, 10, None),
    FieldSchema("w", float, 10, 10, None),
    FieldSchema("s", float, 20, 10, None),
    FieldSchema("t", float, 30, 10, None),
    FieldSchema("h", float, 40, 10, None),
    FieldSchema("s", float, 50, 10, None),
    FieldSchema("eka", float, 60, 10, None),
    FieldSchema("eua", float, 70, 10, None),
)

_MAT234_CARD2 = (
    FieldSchema("vmb", float, 0, 10, None),
    FieldSchema("c", float, 10, 10, None),
    FieldSchema("g23", float, 20, 10, None),
    FieldSchema("ekb", float, 30, 10, None),
    FieldSchema("aopt", float, 40, 10, None),
)

_MAT234_CARD3 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MAT234_CARD4 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
)

class Mat234(KeywordBase):
    """DYNA MAT_234 keyword"""

    keyword = "MAT"
    subkeyword = "234"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat234 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT234_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT234_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT234_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT234_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT234_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat234.option_specs[0],
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
    def e1(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in the yarn axial-direction.
        """ # nopep8
        return self._cards[0].get_value("e1")

    @e1.setter
    def e1(self, value: float) -> None:
        """Set the e1 property."""
        self._cards[0].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in the yarn transverse-direction.
        """ # nopep8
        return self._cards[0].get_value("e2")

    @e2.setter
    def e2(self, value: float) -> None:
        """Set the e2 property."""
        self._cards[0].set_value("e2", value)

    @property
    def g12(self) -> typing.Optional[float]:
        """Get or set the Shear modulus of the yarns
        """ # nopep8
        return self._cards[0].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        """Set the g12 property."""
        self._cards[0].set_value("g12", value)

    @property
    def eu(self) -> typing.Optional[float]:
        """Get or set the Ultimate strain at failure.
        """ # nopep8
        return self._cards[0].get_value("eu")

    @eu.setter
    def eu(self, value: float) -> None:
        """Set the eu property."""
        self._cards[0].set_value("eu", value)

    @property
    def thl(self) -> typing.Optional[float]:
        """Get or set the Yarn locking angle.
        """ # nopep8
        return self._cards[0].get_value("thl")

    @thl.setter
    def thl(self, value: float) -> None:
        """Set the thl property."""
        self._cards[0].set_value("thl", value)

    @property
    def thi(self) -> typing.Optional[float]:
        """Get or set the Initial brade angle.
        """ # nopep8
        return self._cards[0].get_value("thi")

    @thi.setter
    def thi(self, value: float) -> None:
        """Set the thi property."""
        self._cards[0].set_value("thi", value)

    @property
    def ta(self) -> typing.Optional[float]:
        """Get or set the Transition angle to locking
        """ # nopep8
        return self._cards[1].get_value("ta")

    @ta.setter
    def ta(self, value: float) -> None:
        """Set the ta property."""
        self._cards[1].set_value("ta", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Fiber width.
        """ # nopep8
        return self._cards[1].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[1].set_value("w", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Span between the fibers.
        """ # nopep8
        return self._cards[1].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        """Set the s property."""
        self._cards[1].set_value("s", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Real fiber thickness.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[1].set_value("t", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Effective fiber thickness
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[1].set_value("h", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Fiber cross-sectional area.
        """ # nopep8
        return self._cards[1].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        """Set the s property."""
        self._cards[1].set_value("s", value)

    @property
    def eka(self) -> typing.Optional[float]:
        """Get or set the Elastic constant of element.
        """ # nopep8
        return self._cards[1].get_value("eka")

    @eka.setter
    def eka(self, value: float) -> None:
        """Set the eka property."""
        self._cards[1].set_value("eka", value)

    @property
    def eua(self) -> typing.Optional[float]:
        """Get or set the Ultimate strain of element
        """ # nopep8
        return self._cards[1].get_value("eua")

    @eua.setter
    def eua(self, value: float) -> None:
        """Set the eua property."""
        self._cards[1].set_value("eua", value)

    @property
    def vmb(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient of element
        """ # nopep8
        return self._cards[2].get_value("vmb")

    @vmb.setter
    def vmb(self, value: float) -> None:
        """Set the vmb property."""
        self._cards[2].set_value("vmb", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Coefficient of friction between the fibers.
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[2].set_value("c", value)

    @property
    def g23(self) -> typing.Optional[float]:
        """Get or set the transverse shear modulus.
        """ # nopep8
        return self._cards[2].get_value("g23")

    @g23.setter
    def g23(self, value: float) -> None:
        """Set the g23 property."""
        self._cards[2].set_value("g23", value)

    @property
    def ekb(self) -> typing.Optional[float]:
        """Get or set the Elastic constant of element
        """ # nopep8
        return self._cards[2].get_value("ekb")

    @ekb.setter
    def ekb(self, value: float) -> None:
        """Set the ekb property."""
        self._cards[2].set_value("ekb", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes defined by	the cross product of the vector v with the element normal.
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
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the .
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

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

