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

"""Module providing the MatCorusVegter class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATCORUSVEGTER_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("n", int, 40, 10, None),
    FieldSchema("fbi", float, 50, 10, None),
    FieldSchema("rbi0", float, 60, 10, None),
    FieldSchema("lcid", float, 70, 10, None),
)

_MATCORUSVEGTER_CARD1 = (
    FieldSchema("sys", float, 0, 10, None),
    FieldSchema("sip", float, 10, 10, None),
    FieldSchema("shs", float, 20, 10, None),
    FieldSchema("shl", float, 30, 10, None),
    FieldSchema("esh", float, 40, 10, None),
    FieldSchema("e0", float, 50, 10, None),
    FieldSchema("alpha", float, 60, 10, None),
    FieldSchema("lcid2", float, 70, 10, None),
)

_MATCORUSVEGTER_CARD2 = (
    FieldSchema("aopt", float, 0, 10, None),
)

_MATCORUSVEGTER_CARD3 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATCORUSVEGTER_CARD4 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MATCORUSVEGTER_CARD5 = (
    FieldSchema("fun-i", float, 0, 10, None),
    FieldSchema("run-i", float, 10, 10, None),
    FieldSchema("fps1-i", float, 20, 10, None),
    FieldSchema("fps2-i", float, 30, 10, None),
    FieldSchema("fsh-i", float, 40, 10, None),
)

_MATCORUSVEGTER_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatCorusVegter(KeywordBase):
    """DYNA MAT_CORUS_VEGTER keyword"""

    keyword = "MAT"
    subkeyword = "CORUS_VEGTER"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatCorusVegter class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATCORUSVEGTER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCORUSVEGTER_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCORUSVEGTER_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCORUSVEGTER_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCORUSVEGTER_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCORUSVEGTER_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatCorusVegter.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATCORUSVEGTER_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.  A unique number or label must be specified
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Material density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Elastic Young's modulus
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def n(self) -> typing.Optional[int]:
        """Get or set the Order of Fourier series (i.e., number of test groups minus one).  The minimum number for N is 2, and the maximum is 12
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def fbi(self) -> typing.Optional[float]:
        """Get or set the Normalized yield stress for equibiaxial test
        """ # nopep8
        return self._cards[0].get_value("fbi")

    @fbi.setter
    def fbi(self, value: float) -> None:
        """Set the fbi property."""
        self._cards[0].set_value("fbi", value)

    @property
    def rbi0(self) -> typing.Optional[float]:
        """Get or set the Initial strain ratio for equibiaxial test
        """ # nopep8
        return self._cards[0].get_value("rbi0")

    @rbi0.setter
    def rbi0(self, value: float) -> None:
        """Set the rbi0 property."""
        self._cards[0].set_value("rbi0", value)

    @property
    def lcid(self) -> typing.Optional[float]:
        """Get or set the Stress-strain curve ID.  If defined, SYS, SIP, SHS, and SHL are ignored
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: float) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sys(self) -> typing.Optional[float]:
        """Get or set the Static yield stress
        """ # nopep8
        return self._cards[1].get_value("sys")

    @sys.setter
    def sys(self, value: float) -> None:
        """Set the sys property."""
        self._cards[1].set_value("sys", value)

    @property
    def sip(self) -> typing.Optional[float]:
        """Get or set the Stress increment parameter
        """ # nopep8
        return self._cards[1].get_value("sip")

    @sip.setter
    def sip(self, value: float) -> None:
        """Set the sip property."""
        self._cards[1].set_value("sip", value)

    @property
    def shs(self) -> typing.Optional[float]:
        """Get or set the Strain hardening parameter for small strain
        """ # nopep8
        return self._cards[1].get_value("shs")

    @shs.setter
    def shs(self, value: float) -> None:
        """Set the shs property."""
        self._cards[1].set_value("shs", value)

    @property
    def shl(self) -> typing.Optional[float]:
        """Get or set the Strain hardening parameter for larger strain
        """ # nopep8
        return self._cards[1].get_value("shl")

    @shl.setter
    def shl(self, value: float) -> None:
        """Set the shl property."""
        self._cards[1].set_value("shl", value)

    @property
    def esh(self) -> typing.Optional[float]:
        """Get or set the Exponent for strain hardening
        """ # nopep8
        return self._cards[1].get_value("esh")

    @esh.setter
    def esh(self, value: float) -> None:
        """Set the esh property."""
        self._cards[1].set_value("esh", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial plastic strain
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[1].set_value("e0", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the distribution of hardening used in the curve-fitting.    pure kinematic hardening and   provides pure isotropic hardening
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def lcid2(self) -> typing.Optional[float]:
        """Get or set the Curve ID.  The curve defines Young's modulus change with respect to the plastic strain.  By default it is assumed that Young's modulus remains constant.  Effective value is between 0-1
        """ # nopep8
        return self._cards[1].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: float) -> None:
        """Set the lcid2 property."""
        self._cards[1].set_value("lcid2", value)

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
        """Get or set the Coordinates of point p for AOPT = 1
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 4
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 4
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 4
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def fun_i(self) -> typing.Optional[float]:
        """Get or set the Normalized yield stress for uniaxial test for the ith direction
        """ # nopep8
        return self._cards[5].get_value("fun-i")

    @fun_i.setter
    def fun_i(self, value: float) -> None:
        """Set the fun_i property."""
        self._cards[5].set_value("fun-i", value)

    @property
    def run_i(self) -> typing.Optional[float]:
        """Get or set the Strain ratio for uniaxial test for the ith direction
        """ # nopep8
        return self._cards[5].get_value("run-i")

    @run_i.setter
    def run_i(self, value: float) -> None:
        """Set the run_i property."""
        self._cards[5].set_value("run-i", value)

    @property
    def fps1_i(self) -> typing.Optional[float]:
        """Get or set the First normalized yield stress for plain strain test for the ith direction
        """ # nopep8
        return self._cards[5].get_value("fps1-i")

    @fps1_i.setter
    def fps1_i(self, value: float) -> None:
        """Set the fps1_i property."""
        self._cards[5].set_value("fps1-i", value)

    @property
    def fps2_i(self) -> typing.Optional[float]:
        """Get or set the Second normalized yield stress for plain strain test for the ith direction
        """ # nopep8
        return self._cards[5].get_value("fps2-i")

    @fps2_i.setter
    def fps2_i(self, value: float) -> None:
        """Set the fps2_i property."""
        self._cards[5].set_value("fps2-i", value)

    @property
    def fsh_i(self) -> typing.Optional[float]:
        """Get or set the First normalized yield stress for pure shear test for the ith direction
        """ # nopep8
        return self._cards[5].get_value("fsh-i")

    @fsh_i.setter
    def fsh_i(self, value: float) -> None:
        """Set the fsh_i property."""
        self._cards[5].set_value("fsh-i", value)

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

