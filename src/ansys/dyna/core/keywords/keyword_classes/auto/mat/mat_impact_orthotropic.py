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

"""Module providing the MatImpactOrthotropic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATIMPACTORTHOTROPIC_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ea", float, 20, 10, None),
    FieldSchema("eb", float, 30, 10, None),
    FieldSchema("ec", float, 40, 10, None),
    FieldSchema("prba", float, 50, 10, None),
    FieldSchema("prca", float, 60, 10, None),
    FieldSchema("prcb", float, 70, 10, None),
)

_MATIMPACTORTHOTROPIC_CARD1 = (
    FieldSchema("gab", float, 0, 0, None),
    FieldSchema("gbc", float, 0, 10, None),
    FieldSchema("gca", float, 0, 20, None),
    FieldSchema("unused", int, 0, 30, None),
    FieldSchema("aopt", float, 0, 40, None),
    FieldSchema("_2way", float, 0, 50, None, "2way"),
    FieldSchema("unused", int, 0, 60, None),
    FieldSchema("unused", int, 0, 70, None),
)

_MATIMPACTORTHOTROPIC_CARD2 = (
    FieldSchema("xp", float, 0, 0, None),
    FieldSchema("yp", float, 0, 10, None),
    FieldSchema("zp", float, 0, 20, None),
    FieldSchema("a1", float, 0, 30, None),
    FieldSchema("a2", float, 0, 40, None),
    FieldSchema("a3", float, 0, 50, None),
    FieldSchema("mangle", float, 0, 60, None),
    FieldSchema("unused", int, 0, 70, None),
)

_MATIMPACTORTHOTROPIC_CARD3 = (
    FieldSchema("v1", float, 0, 0, None),
    FieldSchema("v2", float, 0, 10, None),
    FieldSchema("v3", float, 0, 20, None),
    FieldSchema("d1", float, 0, 30, None),
    FieldSchema("d2", float, 0, 40, None),
    FieldSchema("d3", float, 0, 50, None),
    FieldSchema("dfailm", float, 0, 60, None),
    FieldSchema("dfails", float, 0, 70, None),
)

_MATIMPACTORTHOTROPIC_CARD4 = (
    FieldSchema("unused", int, 0, 0, None),
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 0, 20, None),
    FieldSchema("unused", int, 0, 30, None),
    FieldSchema("unused", int, 0, 40, None),
    FieldSchema("dfailt", float, 0, 50, None),
    FieldSchema("unused", int, 0, 60, None),
    FieldSchema("unused", int, 0, 70, None),
)

_MATIMPACTORTHOTROPIC_CARD5 = (
    FieldSchema("unused", int, 0, 0, None),
    FieldSchema("xt", float, 0, 10, None),
    FieldSchema("unused", int, 0, 20, None),
    FieldSchema("yt", float, 0, 30, None),
    FieldSchema("sc", float, 0, 40, None),
    FieldSchema("unused", int, 0, 50, None),
    FieldSchema("unused", int, 0, 60, None),
    FieldSchema("unused", int, 0, 70, None),
)

_MATIMPACTORTHOTROPIC_CARD6 = (
    FieldSchema("s_ca", float, 0, 0, None),
    FieldSchema("s_cb", float, 0, 10, None),
    FieldSchema("dfailsca", float, 0, 20, None),
    FieldSchema("dfailscb", float, 0, 30, None),
    FieldSchema("ftype", float, 0, 40, None),
    FieldSchema("fmaa", float, 0, 50, None),
    FieldSchema("fmbb", float, 0, 60, None),
    FieldSchema("fmcc", float, 0, 70, None),
)

_MATIMPACTORTHOTROPIC_CARD7 = (
    FieldSchema("fmba", float, 0, 0, None),
    FieldSchema("fmca", float, 0, 10, None),
    FieldSchema("fmcb", float, 0, 20, None),
    FieldSchema("fshr", float, 0, 30, None),
    FieldSchema("fsmax", float, 0, 40, None),
    FieldSchema("stype", float, 0, 50, None),
    FieldSchema("yld", float, 0, 60, None),
    FieldSchema("unused", int, 0, 70, None),
)

_MATIMPACTORTHOTROPIC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatImpactOrthotropic(KeywordBase):
    """DYNA MAT_IMPACT_ORTHOTROPIC keyword"""

    keyword = "MAT"
    subkeyword = "IMPACT_ORTHOTROPIC"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatImpactOrthotropic class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATIMPACTORTHOTROPIC_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATIMPACTORTHOTROPIC_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATIMPACTORTHOTROPIC_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATIMPACTORTHOTROPIC_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATIMPACTORTHOTROPIC_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATIMPACTORTHOTROPIC_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATIMPACTORTHOTROPIC_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATIMPACTORTHOTROPIC_CARD7,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatImpactOrthotropic._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATIMPACTORTHOTROPIC_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the Young's modulus along longitudinal direction
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Young's modulus along trnasversal direction
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        """Set the eb property."""
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Young's modulus along normal direction
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        """Set the ec property."""
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ba
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        """Set the prba property."""
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ca
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        """Set the prca property."""
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Posson's ratio cb
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        """Set the prcb property."""
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ab
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        """Set the gab property."""
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Shear modulus bc
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        """Set the gbc property."""
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ca
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        """Set the gca property."""
        self._cards[1].set_value("gca", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see *MAT_OPTIONTROPIC_ELASTIC for a more complete description):
        EQ.0.0 : Locally orthotropic with material axes determined by element nodes 1,2,and 4,as with *DEFINE_COORDINATE_NODES.EQ.1.0 : Locally orthotropic with material axes determined by a point in space and the global location of the element center; this is the a-direction.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR.
        EQ.3.0:	Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element. The plane of a solid element is the midsurface between the inner surface and outer surface, defined by the first four nodes and the last four nodes of the connectivity of the element, respectively. Thus, for solid elements, AOPT = 3 is only available for hexahedra. a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a, and c is the normal vector. Then a and b are rotated about c by an angle. The angle may be set in the keyword input for the element as BETA or in the input for this keyword as MANGLE.
        EQ.4.0:	Locally orthotropic in cylindrical coordinate system with the material axes determined by a vector v, and an originating point, p, which define the centerline axis.
        LT.0.0:	The absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR).
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[1].set_value("aopt", value)

    @property
    def _2way(self) -> typing.Optional[float]:
        """Get or set the Flag to turn on two-way fiber action (see Remark 2):
        EQ.0.0 : Unidirectional behavior, meaning fibers run only in a - direction.
        EQ.1.0 : Two - way fiber behavior, meaning fibers run in both the a - and b - directions.The meaning of the fields DFAILM,DFAILT,XT,and YT are altered if this flag is set.
        """ # nopep8
        return self._cards[1].get_value("_2way")

    @_2way.setter
    def _2way(self, value: float) -> None:
        """Set the _2way property."""
        self._cards[1].set_value("_2way", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[2].set_value("a3", value)

    @property
    def mangle(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3. MANGLE may be overridden on the element card; see BETA on *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[2].get_value("mangle")

    @mangle.setter
    def mangle(self, value: float) -> None:
        """Set the mangle property."""
        self._cards[2].set_value("mangle", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[3].set_value("d3", value)

    @property
    def dfailm(self) -> typing.Optional[float]:
        """Get or set the Maximum strain for the matrix straining in tension. For unidirectional fibers, this is in the b- and c-directions, while for two-way fibers, this is in the c-direciton.
        """ # nopep8
        return self._cards[3].get_value("dfailm")

    @dfailm.setter
    def dfailm(self, value: float) -> None:
        """Set the dfailm property."""
        self._cards[3].set_value("dfailm", value)

    @property
    def dfails(self) -> typing.Optional[float]:
        """Get or set the Maximum shear strain in the ab-plane
        """ # nopep8
        return self._cards[3].get_value("dfails")

    @dfails.setter
    def dfails(self, value: float) -> None:
        """Set the dfails property."""
        self._cards[3].set_value("dfails", value)

    @property
    def dfailt(self) -> typing.Optional[float]:
        """Get or set the Maximum fiber strain under tension in the fiber longitudinal direction. A value of 1 is 100% tensile strain. If the two-way fiber flag is set, then DFAILT is the fiber tensile failure strain in the a- and b-directions; otherwise, it is only in the a-direction.
        """ # nopep8
        return self._cards[4].get_value("dfailt")

    @dfailt.setter
    def dfailt(self, value: float) -> None:
        """Set the dfailt property."""
        self._cards[4].set_value("dfailt", value)

    @property
    def xt(self) -> typing.Optional[float]:
        """Get or set the Tensile strength of the fibers in the fiber longitudinal direction. For unidirectional fibers (TWOWAY = 0.0), this is along the a-direction, while for two-way fiber behavior (TWOWAY = 1.0), this is along the a- and b-directions.
        """ # nopep8
        return self._cards[5].get_value("xt")

    @xt.setter
    def xt(self, value: float) -> None:
        """Set the xt property."""
        self._cards[5].set_value("xt", value)

    @property
    def yt(self) -> typing.Optional[float]:
        """Get or set the Tensile strength of the matrix transverse to the fibers. For unidirectional fibers (TWOWAY = 0.0), this is along the b- and c-directions, while for two-way fiber behavior (TWOWAY = 1.0), this is along the c-direction.
        """ # nopep8
        return self._cards[5].get_value("yt")

    @yt.setter
    def yt(self, value: float) -> None:
        """Set the yt property."""
        self._cards[5].set_value("yt", value)

    @property
    def sc(self) -> typing.Optional[float]:
        """Get or set the Shear strength in the ab-plane
        """ # nopep8
        return self._cards[5].get_value("sc")

    @sc.setter
    def sc(self, value: float) -> None:
        """Set the sc property."""
        self._cards[5].set_value("sc", value)

    @property
    def s_ca(self) -> typing.Optional[float]:
        """Get or set the Shear strength in the ca-plane
        """ # nopep8
        return self._cards[6].get_value("s_ca")

    @s_ca.setter
    def s_ca(self, value: float) -> None:
        """Set the s_ca property."""
        self._cards[6].set_value("s_ca", value)

    @property
    def s_cb(self) -> typing.Optional[float]:
        """Get or set the Shear strength in the cb-plane
        """ # nopep8
        return self._cards[6].get_value("s_cb")

    @s_cb.setter
    def s_cb(self, value: float) -> None:
        """Set the s_cb property."""
        self._cards[6].set_value("s_cb", value)

    @property
    def dfailsca(self) -> typing.Optional[float]:
        """Get or set the Maximum shear strain in the ca-plane
        """ # nopep8
        return self._cards[6].get_value("dfailsca")

    @dfailsca.setter
    def dfailsca(self, value: float) -> None:
        """Set the dfailsca property."""
        self._cards[6].set_value("dfailsca", value)

    @property
    def dfailscb(self) -> typing.Optional[float]:
        """Get or set the Maximum shear strain in the cb-plane
        """ # nopep8
        return self._cards[6].get_value("dfailscb")

    @dfailscb.setter
    def dfailscb(self, value: float) -> None:
        """Set the dfailscb property."""
        self._cards[6].set_value("dfailscb", value)

    @property
    def ftype(self) -> typing.Optional[float]:
        """Get or set the Post-failure option (see Remark 2):
        EQ.1.0 : Isotropic post - failure response.All the directions behave the same after failure; the failed elements can only carry bulk compressive stresses.
        EQ.2.0 : Orthotropic post - failure response.The three spatial directions behave differently after failure, requiring the definition of additional post - failure parameters.These parameters include the post - failure mode in each material direction and the residual shear strength of the failed material.
        """ # nopep8
        return self._cards[6].get_value("ftype")

    @ftype.setter
    def ftype(self, value: float) -> None:
        """Set the ftype property."""
        self._cards[6].set_value("ftype", value)

    @property
    def fmaa(self) -> typing.Optional[float]:
        """Get or set the Failure mode in the failed a-direction (FTYPE = 2 only):
        EQ.1.0 : Bulk
        EQ.2.0 : aa only
        """ # nopep8
        return self._cards[6].get_value("fmaa")

    @fmaa.setter
    def fmaa(self, value: float) -> None:
        """Set the fmaa property."""
        self._cards[6].set_value("fmaa", value)

    @property
    def fmbb(self) -> typing.Optional[float]:
        """Get or set the Failure mode in the failed b-direction (FTYPE = 2 only):
        EQ.1.0 : Bulk
        EQ.2.0 : bb only
        """ # nopep8
        return self._cards[6].get_value("fmbb")

    @fmbb.setter
    def fmbb(self, value: float) -> None:
        """Set the fmbb property."""
        self._cards[6].set_value("fmbb", value)

    @property
    def fmcc(self) -> typing.Optional[float]:
        """Get or set the Failure mode in the failed c-direction (FTYPE = 2 only):
        EQ.1.0 : Bulk
        EQ.2.0 : cc only
        """ # nopep8
        return self._cards[6].get_value("fmcc")

    @fmcc.setter
    def fmcc(self, value: float) -> None:
        """Set the fmcc property."""
        self._cards[6].set_value("fmcc", value)

    @property
    def fmba(self) -> typing.Optional[float]:
        """Get or set the Failure mode in the failed ba plane (FTYPE = 2 only):
        EQ.1.0 : Bulk
        EQ.2.0 : ba and aa only
        EQ.3.0 : ba and bb only
        EQ.4.0 : ba and cc only
        """ # nopep8
        return self._cards[7].get_value("fmba")

    @fmba.setter
    def fmba(self, value: float) -> None:
        """Set the fmba property."""
        self._cards[7].set_value("fmba", value)

    @property
    def fmca(self) -> typing.Optional[float]:
        """Get or set the Failure mode in the failed ca plane (FTYPE = 2 only):
        EQ.1.0 : Bulk
        EQ.2.0 : ca and aa only
        EQ.3.0 : ca and bb only
        EQ.4.0 : ca and cc only
        """ # nopep8
        return self._cards[7].get_value("fmca")

    @fmca.setter
    def fmca(self, value: float) -> None:
        """Set the fmca property."""
        self._cards[7].set_value("fmca", value)

    @property
    def fmcb(self) -> typing.Optional[float]:
        """Get or set the Failure mode in the failed cb plane (FTYPE = 2 only):
        EQ.1.0 : Bulk
        EQ.2.0 : cb and aa only
        EQ.3.0 : cb and bb only
        EQ.4.0 : cb and cc only
        """ # nopep8
        return self._cards[7].get_value("fmcb")

    @fmcb.setter
    def fmcb(self, value: float) -> None:
        """Set the fmcb property."""
        self._cards[7].set_value("fmcb", value)

    @property
    def fshr(self) -> typing.Optional[float]:
        """Get or set the Residual shear stiffness fraction; 0.0 ≤ FSHR ≤ 1.0. For the orthotropic post-failure response, the residual shear modulus is set to the specified value of FHSR times the intact shear modulus.
        """ # nopep8
        return self._cards[7].get_value("fshr")

    @fshr.setter
    def fshr(self, value: float) -> None:
        """Set the fshr property."""
        self._cards[7].set_value("fshr", value)

    @property
    def fsmax(self) -> typing.Optional[float]:
        """Get or set the Maximum residual shear stress, 0.0 ≤ FSMAX ≤ 1.0e20. For the orthotropic post-failure response, the maximum shear stress allowed in a failed element is recommended to be a value less than or equal to the failure shear stress.
        """ # nopep8
        return self._cards[7].get_value("fsmax")

    @fsmax.setter
    def fsmax(self, value: float) -> None:
        """Set the fsmax property."""
        self._cards[7].set_value("fsmax", value)

    @property
    def stype(self) -> typing.Optional[float]:
        """Get or set the Strength model option (see Remark 3):
        EQ.1.0 : Isotropic plasticity(von Mises yield criterion)
        """ # nopep8
        return self._cards[7].get_value("stype")

    @stype.setter
    def stype(self, value: float) -> None:
        """Set the stype property."""
        self._cards[7].set_value("stype", value)

    @property
    def yld(self) -> typing.Optional[float]:
        """Get or set the Yield stress
        """ # nopep8
        return self._cards[7].get_value("yld")

    @yld.setter
    def yld(self, value: float) -> None:
        """Set the yld property."""
        self._cards[7].set_value("yld", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[8].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

