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

"""Module providing the MatTissueDispersed class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATTISSUEDISPERSED_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("f", float, 20, 10, None),
    FieldSchema("sigma", float, 30, 10, None),
    FieldSchema("mu", float, 40, 10, None),
    FieldSchema("kappa", float, 50, 10, None),
    FieldSchema("act", int, 60, 10, None),
    FieldSchema("init", int, 70, 10, None),
)

_MATTISSUEDISPERSED_CARD1 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("orth", int, 10, 10, None),
    FieldSchema("c1", float, 20, 10, None),
    FieldSchema("c2", float, 30, 10, None),
    FieldSchema("c3", float, 40, 10, None),
    FieldSchema("theta", float, 50, 10, None),
)

_MATTISSUEDISPERSED_CARD2 = (
    FieldSchema("act1", float, 0, 10, None),
    FieldSchema("act2", float, 10, 10, None),
    FieldSchema("act3", float, 20, 10, None),
    FieldSchema("act4", float, 30, 10, None),
    FieldSchema("act5", float, 40, 10, None),
    FieldSchema("act6", float, 50, 10, None),
    FieldSchema("act7", float, 60, 10, None),
    FieldSchema("act8", float, 70, 10, None),
)

_MATTISSUEDISPERSED_CARD3 = (
    FieldSchema("act9", float, 0, 10, None),
    FieldSchema("act10", float, 10, 10, None),
)

_MATTISSUEDISPERSED_CARD4 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("beta", float, 10, 10, None),
    FieldSchema("xp", float, 20, 10, None),
    FieldSchema("yp", float, 30, 10, None),
    FieldSchema("zp", float, 40, 10, None),
    FieldSchema("a1", float, 50, 10, None),
    FieldSchema("a2", float, 60, 10, None),
    FieldSchema("a3", float, 70, 10, None),
)

_MATTISSUEDISPERSED_CARD5 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
)

_MATTISSUEDISPERSED_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatTissueDispersed(KeywordBase):
    """DYNA MAT_TISSUE_DISPERSED keyword"""

    keyword = "MAT"
    subkeyword = "TISSUE_DISPERSED"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatTissueDispersed class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATTISSUEDISPERSED_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTISSUEDISPERSED_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTISSUEDISPERSED_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTISSUEDISPERSED_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTISSUEDISPERSED_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTISSUEDISPERSED_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatTissueDispersed.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATTISSUEDISPERSED_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number must be specified
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
    def f(self) -> typing.Optional[float]:
        """Get or set the Fiber dispersion parameter governs the extent to which the fiber dispersion extends to the third dimension. F = 0 and F = 1 apply to 2D splay
        with the normal to the membrane being in the beta and the gama directions,
        respectively . F = 0.5 applies to 3D splay with transverse isotropy.
        Splay will be orthotropic whenever F != 0.5. This parameter is ignored if INIT = 1.
        """ # nopep8
        return self._cards[0].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        """Set the f property."""
        self._cards[0].set_value("f", value)

    @property
    def sigma(self) -> typing.Optional[float]:
        """Get or set the The parameter SIGMA governs the extent of dispersion, such that as
        SIGMA goes to zero, the material symmetry reduces to pure transverse
        isotropy. Conversely, as SIGMA becomes large, the material symmetry
        becomes isotropic in the plane. This parameter is ignored if INIT = 1.
        """ # nopep8
        return self._cards[0].get_value("sigma")

    @sigma.setter
    def sigma(self, value: float) -> None:
        """Set the sigma property."""
        self._cards[0].set_value("sigma", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the MU is the isotropic shear modulus that models elastin
        """ # nopep8
        return self._cards[0].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        """Set the mu property."""
        self._cards[0].set_value("mu", value)

    @property
    def kappa(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus for the hydrostatic pressure
        """ # nopep8
        return self._cards[0].get_value("kappa")

    @kappa.setter
    def kappa(self, value: float) -> None:
        """Set the kappa property."""
        self._cards[0].set_value("kappa", value)

    @property
    def act(self) -> typing.Optional[int]:
        """Get or set the ACT = 1 indicates that an active model will be used that acts in the mean
        fiber-direction. The active model, like the passive model, will be dispersed by SIGMA and F, or if INIT = 1, with the
        *INITIAL_FIELD_SOLID keyword
        """ # nopep8
        return self._cards[0].get_value("act")

    @act.setter
    def act(self, value: int) -> None:
        """Set the act property."""
        self._cards[0].set_value("act", value)

    @property
    def init(self) -> typing.Optional[int]:
        """Get or set the INIT = 1 indicates that the anisotropy eigenvalues will be given by
        *INITIAL_FIELD_SOLID variables in the global coordinate system
        """ # nopep8
        return self._cards[0].get_value("init")

    @init.setter
    def init(self, value: int) -> None:
        """Set the init property."""
        self._cards[0].set_value("init", value)

    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the The passive fiber model number. There are two passive models available: FID = 1 or FID = 2.
        """ # nopep8
        return self._cards[1].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[1].set_value("fid", value)

    @property
    def orth(self) -> typing.Optional[int]:
        """Get or set the ORTH specifies the number (1 or 2) of fibers used. When ORTH = 2
        two fiber families are used and arranges symmetrically THETA degrees
        from the mean fiber direction and lying in the tissue plane
        """ # nopep8
        return self._cards[1].get_value("orth")

    @orth.setter
    def orth(self, value: int) -> None:
        """Set the orth property."""
        self._cards[1].set_value("orth", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Passive fiber model parameters
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Passive fiber model parameters
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Passive fiber model parameters
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[1].set_value("c3", value)

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the The angle between the mean fiber direction and the fiber families. The
        parameter is active only if ORTH = 2 and is particularly important in
        vascular tissues (e.g. arteries)
        """ # nopep8
        return self._cards[1].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        """Set the theta property."""
        self._cards[1].set_value("theta", value)

    @property
    def act1(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[2].get_value("act1")

    @act1.setter
    def act1(self, value: float) -> None:
        """Set the act1 property."""
        self._cards[2].set_value("act1", value)

    @property
    def act2(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[2].get_value("act2")

    @act2.setter
    def act2(self, value: float) -> None:
        """Set the act2 property."""
        self._cards[2].set_value("act2", value)

    @property
    def act3(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[2].get_value("act3")

    @act3.setter
    def act3(self, value: float) -> None:
        """Set the act3 property."""
        self._cards[2].set_value("act3", value)

    @property
    def act4(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[2].get_value("act4")

    @act4.setter
    def act4(self, value: float) -> None:
        """Set the act4 property."""
        self._cards[2].set_value("act4", value)

    @property
    def act5(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[2].get_value("act5")

    @act5.setter
    def act5(self, value: float) -> None:
        """Set the act5 property."""
        self._cards[2].set_value("act5", value)

    @property
    def act6(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[2].get_value("act6")

    @act6.setter
    def act6(self, value: float) -> None:
        """Set the act6 property."""
        self._cards[2].set_value("act6", value)

    @property
    def act7(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[2].get_value("act7")

    @act7.setter
    def act7(self, value: float) -> None:
        """Set the act7 property."""
        self._cards[2].set_value("act7", value)

    @property
    def act8(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[2].get_value("act8")

    @act8.setter
    def act8(self, value: float) -> None:
        """Set the act8 property."""
        self._cards[2].set_value("act8", value)

    @property
    def act9(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[3].get_value("act9")

    @act9.setter
    def act9(self, value: float) -> None:
        """Set the act9 property."""
        self._cards[3].set_value("act9", value)

    @property
    def act10(self) -> typing.Optional[float]:
        """Get or set the Active fiber model parameters. Note that ACT10 is an input for a time
        dependent load curve that overrides some of the ACTx values. See section 2 below
        """ # nopep8
        return self._cards[3].get_value("act10")

    @act10.setter
    def act10(self, value: float) -> None:
        """Set the act10 property."""
        self._cards[3].set_value("act10", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[4].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[4].set_value("aopt", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the ele	ment card *ELEMANT_SOLID_ORTHO
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the XP, YP and ZP define the coordinates of point P for AOPT=1 and AOPT = 4.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the XP, YP and ZP define the coordinates of point P for AOPT=1 and AOPT = 4.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the XP, YP and ZP define the coordinates of point P for AOPT=1 and AOPT = 4.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the A1, A2 and A3 define the components of vector A for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the A1, A2 and A3 define the components of vector A for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the A1, A2 and A3 define the components of vector A for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the V1, V2 and V3 define components of vector V for AOPT = 3 and AOPT = 4
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the V1, V2 and V3 define components of vector V for AOPT = 3 and AOPT = 4
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the V1, V2 and V3 define components of vector V for AOPT = 3 and AOPT = 4
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the D1, D2 and D3 define components of vector D for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the D1, D2 and D3 define components of vector D for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the D1, D2 and D3 define components of vector D for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[5].set_value("d3", value)

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

