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

"""Module providing the MatReinforcedThermoplasticUdfiber class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATREINFORCEDTHERMOPLASTICUDFIBER_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("em", float, 20, 10, None),
    FieldSchema("prm", float, 30, 10, None),
    FieldSchema("g", float, 40, 10, None),
    FieldSchema("ezdef", float, 50, 10, None),
)

_MATREINFORCEDTHERMOPLASTICUDFIBER_CARD1 = (
    FieldSchema("nfib", int, 0, 10, None),
    FieldSchema("aopt", float, 10, 10, None),
    FieldSchema("xp", float, 20, 10, None),
    FieldSchema("yp", float, 30, 10, None),
    FieldSchema("zp", float, 40, 10, None),
    FieldSchema("a1", float, 50, 10, None),
    FieldSchema("a2", float, 60, 10, None),
    FieldSchema("a3", float, 70, 10, None),
)

_MATREINFORCEDTHERMOPLASTICUDFIBER_CARD2 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("mangl", float, 60, 10, None),
)

_MATREINFORCEDTHERMOPLASTICUDFIBER_CARD3 = (
    FieldSchema("idf1", int, 0, 10, None),
    FieldSchema("alph1", float, 10, 10, None),
    FieldSchema("ef1", float, 20, 10, None),
    FieldSchema("kap1", float, 30, 10, None),
)

_MATREINFORCEDTHERMOPLASTICUDFIBER_CARD4 = (
    FieldSchema("idf2", int, 0, 10, None),
    FieldSchema("alph2", float, 10, 10, None),
    FieldSchema("ef2", float, 20, 10, None),
    FieldSchema("kap2", float, 30, 10, None),
)

_MATREINFORCEDTHERMOPLASTICUDFIBER_CARD5 = (
    FieldSchema("idf3", int, 0, 10, None),
    FieldSchema("alph3", float, 10, 10, None),
    FieldSchema("ef3", float, 20, 10, None),
    FieldSchema("kap3", float, 30, 10, None),
)

_MATREINFORCEDTHERMOPLASTICUDFIBER_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatReinforcedThermoplasticUdfiber(KeywordBase):
    """DYNA MAT_REINFORCED_THERMOPLASTIC_UDFIBER keyword"""

    keyword = "MAT"
    subkeyword = "REINFORCED_THERMOPLASTIC_UDFIBER"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatReinforcedThermoplasticUdfiber class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATREINFORCEDTHERMOPLASTICUDFIBER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATREINFORCEDTHERMOPLASTICUDFIBER_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATREINFORCEDTHERMOPLASTICUDFIBER_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATREINFORCEDTHERMOPLASTICUDFIBER_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATREINFORCEDTHERMOPLASTICUDFIBER_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATREINFORCEDTHERMOPLASTICUDFIBER_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatReinforcedThermoplasticUdfiber.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATREINFORCEDTHERMOPLASTICUDFIBER_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def em(self) -> typing.Optional[float]:
        """Get or set the Isotropic young's modulus
        """ # nopep8
        return self._cards[0].get_value("em")

    @em.setter
    def em(self, value: float) -> None:
        """Set the em property."""
        self._cards[0].set_value("em", value)

    @property
    def prm(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for matrix material
        """ # nopep8
        return self._cards[0].get_value("prm")

    @prm.setter
    def prm(self, value: float) -> None:
        """Set the prm property."""
        self._cards[0].set_value("prm", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Linear shear modulus
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def ezdef(self) -> typing.Optional[float]:
        """Get or set the Algorithmic parameter. If set to 1, last row of deformation gradient is not updated during the calculation
        """ # nopep8
        return self._cards[0].get_value("ezdef")

    @ezdef.setter
    def ezdef(self, value: float) -> None:
        """Set the ezdef property."""
        self._cards[0].set_value("ezdef", value)

    @property
    def nfib(self) -> typing.Optional[int]:
        """Get or set the Number of fiber families to be considered
        """ # nopep8
        return self._cards[1].get_value("nfib")

    @nfib.setter
    def nfib(self, value: int) -> None:
        """Set the nfib property."""
        self._cards[1].set_value("nfib", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle MANGL.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[1].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of point p for AOPT = 1
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of point p for AOPT = 1
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of point p for AOPT = 1
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[1].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[1].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[2].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[2].set_value("d3", value)

    @property
    def mangl(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 and 3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA
        """ # nopep8
        return self._cards[2].get_value("mangl")

    @mangl.setter
    def mangl(self, value: float) -> None:
        """Set the mangl property."""
        self._cards[2].set_value("mangl", value)

    @property
    def idf1(self) -> typing.Optional[int]:
        """Get or set the ID for i-th fiber family for post-processing
        """ # nopep8
        return self._cards[3].get_value("idf1")

    @idf1.setter
    def idf1(self, value: int) -> None:
        """Set the idf1 property."""
        self._cards[3].set_value("idf1", value)

    @property
    def alph1(self) -> typing.Optional[float]:
        """Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
        """ # nopep8
        return self._cards[3].get_value("alph1")

    @alph1.setter
    def alph1(self, value: float) -> None:
        """Set the alph1 property."""
        self._cards[3].set_value("alph1", value)

    @property
    def ef1(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for i-th fiber family
        """ # nopep8
        return self._cards[3].get_value("ef1")

    @ef1.setter
    def ef1(self, value: float) -> None:
        """Set the ef1 property."""
        self._cards[3].set_value("ef1", value)

    @property
    def kap1(self) -> typing.Optional[float]:
        """Get or set the Fiber volume ratio for i-th fiber family
        """ # nopep8
        return self._cards[3].get_value("kap1")

    @kap1.setter
    def kap1(self, value: float) -> None:
        """Set the kap1 property."""
        self._cards[3].set_value("kap1", value)

    @property
    def idf2(self) -> typing.Optional[int]:
        """Get or set the ID for i-th fiber family for post-processing
        """ # nopep8
        return self._cards[4].get_value("idf2")

    @idf2.setter
    def idf2(self, value: int) -> None:
        """Set the idf2 property."""
        self._cards[4].set_value("idf2", value)

    @property
    def alph2(self) -> typing.Optional[float]:
        """Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
        """ # nopep8
        return self._cards[4].get_value("alph2")

    @alph2.setter
    def alph2(self, value: float) -> None:
        """Set the alph2 property."""
        self._cards[4].set_value("alph2", value)

    @property
    def ef2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for i-th fiber family
        """ # nopep8
        return self._cards[4].get_value("ef2")

    @ef2.setter
    def ef2(self, value: float) -> None:
        """Set the ef2 property."""
        self._cards[4].set_value("ef2", value)

    @property
    def kap2(self) -> typing.Optional[float]:
        """Get or set the Fiber volume ratio for i-th fiber family
        """ # nopep8
        return self._cards[4].get_value("kap2")

    @kap2.setter
    def kap2(self, value: float) -> None:
        """Set the kap2 property."""
        self._cards[4].set_value("kap2", value)

    @property
    def idf3(self) -> typing.Optional[int]:
        """Get or set the ID for i-th fiber family for post-processing
        """ # nopep8
        return self._cards[5].get_value("idf3")

    @idf3.setter
    def idf3(self, value: int) -> None:
        """Set the idf3 property."""
        self._cards[5].set_value("idf3", value)

    @property
    def alph3(self) -> typing.Optional[float]:
        """Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
        """ # nopep8
        return self._cards[5].get_value("alph3")

    @alph3.setter
    def alph3(self, value: float) -> None:
        """Set the alph3 property."""
        self._cards[5].set_value("alph3", value)

    @property
    def ef3(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for i-th fiber family
        """ # nopep8
        return self._cards[5].get_value("ef3")

    @ef3.setter
    def ef3(self, value: float) -> None:
        """Set the ef3 property."""
        self._cards[5].set_value("ef3", value)

    @property
    def kap3(self) -> typing.Optional[float]:
        """Get or set the Fiber volume ratio for i-th fiber family
        """ # nopep8
        return self._cards[5].get_value("kap3")

    @kap3.setter
    def kap3(self, value: float) -> None:
        """Set the kap3 property."""
        self._cards[5].set_value("kap3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

