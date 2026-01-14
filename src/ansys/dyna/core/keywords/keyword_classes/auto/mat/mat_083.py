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

"""Module providing the Mat083 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT083_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("kcon", float, 30, 10, None),
    FieldSchema("tc", float, 40, 10, 1e+20),
    FieldSchema("fail", float, 50, 10, 0.0),
    FieldSchema("damp", float, 60, 10, None),
    FieldSchema("tbid", int, 70, 10, None),
)

_MAT083_CARD1 = (
    FieldSchema("bvflag", float, 0, 10, 0.0),
    FieldSchema("sflag", float, 10, 10, 0.0),
    FieldSchema("rflag", float, 20, 10, 0.0),
    FieldSchema("tflag", float, 30, 10, 0.0),
    FieldSchema("pvid", int, 40, 10, 0),
    FieldSchema("sraf", float, 50, 10, 0.0),
    FieldSchema("ref", float, 60, 10, 0.0),
    FieldSchema("hu", float, 70, 10, 0.0),
)

_MAT083_CARD2 = (
    FieldSchema("d0", float, 0, 10, None),
    FieldSchema("n0", float, 10, 10, None),
    FieldSchema("n1", float, 20, 10, None),
    FieldSchema("n2", float, 30, 10, None),
    FieldSchema("n3", float, 40, 10, None),
    FieldSchema("c0", float, 50, 10, None),
    FieldSchema("c1", float, 60, 10, None),
    FieldSchema("c2", float, 70, 10, None),
)

_MAT083_CARD3 = (
    FieldSchema("c3", float, 0, 10, None),
    FieldSchema("c4", float, 10, 10, None),
    FieldSchema("c5", float, 20, 10, None),
    FieldSchema("aij", float, 30, 10, None),
    FieldSchema("sij", float, 40, 10, None),
    FieldSchema("minr", float, 50, 10, None),
    FieldSchema("maxr", float, 60, 10, None),
    FieldSchema("shape", float, 70, 10, None),
)

_MAT083_CARD4 = (
    FieldSchema("expon", float, 0, 10, 1.0),
    FieldSchema("riuld", float, 10, 10, 0.0),
)

_MAT083_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat083(KeywordBase):
    """DYNA MAT_083 keyword"""

    keyword = "MAT"
    subkeyword = "083"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "pvid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat083 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT083_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT083_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT083_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT083_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT083_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat083.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT083_OPTION0_CARD0,
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def kcon(self) -> typing.Optional[float]:
        """Get or set the Optional Young's modulus used in the computation of sound speed. This will influence the time step, contact forces, hourglass stabilization forces, and numerical damping(DAMP).
        EQ.0.0: KCON is set equal to the max(E, current tangent to stress-strain curve) if TBID .ne.0. If TBID.eq.0, KCON is set equal to the maximum slope of the stress-strain curve.
        """ # nopep8
        return self._cards[0].get_value("kcon")

    @kcon.setter
    def kcon(self, value: float) -> None:
        """Set the kcon property."""
        self._cards[0].set_value("kcon", value)

    @property
    def tc(self) -> float:
        """Get or set the Tension cut-off stress.
        """ # nopep8
        return self._cards[0].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[0].set_value("tc", value)

    @property
    def fail(self) -> float:
        """Get or set the Failure option after cutoff stress is reached:
        EQ.0.0: tensile stress remains at cut-off value (default),
        EQ.1.0: tensile stress is reset to zero.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        """Set the fail property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""fail must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("fail", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Viscous coefficient (0.05 < recommended value < 0.50) to model damping effects.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[0].set_value("damp", value)

    @property
    def tbid(self) -> typing.Optional[int]:
        """Get or set the Table ID, see *DEFINE_TABLE, for nominal stress strain data as a function of strain rate. If the table ID is provided, cards 3 and 4 may be left blank and the fit will be done internally.
        """ # nopep8
        return self._cards[0].get_value("tbid")

    @tbid.setter
    def tbid(self, value: int) -> None:
        """Set the tbid property."""
        self._cards[0].set_value("tbid", value)

    @property
    def bvflag(self) -> float:
        """Get or set the Toggle to turn bulk viscosity off or on (see Remark 1):
        LT.1.0:	No bulk viscosity(recommended)
        GE.1.0 : Bulk viscosity active.
        """ # nopep8
        return self._cards[1].get_value("bvflag")

    @bvflag.setter
    def bvflag(self, value: float) -> None:
        """Set the bvflag property."""
        self._cards[1].set_value("bvflag", value)

    @property
    def sflag(self) -> float:
        """Get or set the Strain rate flag:
        EQ.0.0: true constant strain rate (default),
        EQ.1.0: engineering strain rate.
        """ # nopep8
        return self._cards[1].get_value("sflag")

    @sflag.setter
    def sflag(self, value: float) -> None:
        """Set the sflag property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""sflag must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("sflag", value)

    @property
    def rflag(self) -> float:
        """Get or set the Strain rate evaluation flag:
        EQ.0.0: first principal direction (default),
        EQ.1.0: principal strain rates for each principal direction,
        EQ.2.0: volumetric strain rate.
        """ # nopep8
        return self._cards[1].get_value("rflag")

    @rflag.setter
    def rflag(self, value: float) -> None:
        """Set the rflag property."""
        if value not in [0.0, 1.0, 2.0, None]:
            raise Exception("""rflag must be `None` or one of {0.0,1.0,2.0}.""")
        self._cards[1].set_value("rflag", value)

    @property
    def tflag(self) -> float:
        """Get or set the Tensile stress evaluation:
        EQ.0.0: linear in tension (default),
        EQ.1.0: input via load curves with the tensile response corresponds to negative values of stress and strain.
        """ # nopep8
        return self._cards[1].get_value("tflag")

    @tflag.setter
    def tflag(self, value: float) -> None:
        """Set the tflag property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""tflag must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("tflag", value)

    @property
    def pvid(self) -> int:
        """Get or set the Optional load curve ID defining pressure versus volumetric strain.
        """ # nopep8
        return self._cards[1].get_value("pvid")

    @pvid.setter
    def pvid(self, value: int) -> None:
        """Set the pvid property."""
        self._cards[1].set_value("pvid", value)

    @property
    def sraf(self) -> float:
        """Get or set the Strain rate averaging flag.
        EQ.0.0: use weighted running average.
        EQ.1.0: average the last twelve values.
        """ # nopep8
        return self._cards[1].get_value("sraf")

    @sraf.setter
    def sraf(self, value: float) -> None:
        """Set the sraf property."""
        self._cards[1].set_value("sraf", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the kyword.*INITIAL_FROM_REFERENCE_GEOMETRY
        EQ. 0.0: off
        EQ.1.0: on.
        """ # nopep8
        return self._cards[1].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ref must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("ref", value)

    @property
    def hu(self) -> float:
        """Get or set the Hysteretic unloading factor between 0 and 1 (default=0).
        """ # nopep8
        return self._cards[1].get_value("hu")

    @hu.setter
    def hu(self, value: float) -> None:
        """Set the hu property."""
        self._cards[1].set_value("hu", value)

    @property
    def d0(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[2].get_value("d0")

    @d0.setter
    def d0(self, value: float) -> None:
        """Set the d0 property."""
        self._cards[2].set_value("d0", value)

    @property
    def n0(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[2].get_value("n0")

    @n0.setter
    def n0(self, value: float) -> None:
        """Set the n0 property."""
        self._cards[2].set_value("n0", value)

    @property
    def n1(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[2].get_value("n1")

    @n1.setter
    def n1(self, value: float) -> None:
        """Set the n1 property."""
        self._cards[2].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[2].get_value("n2")

    @n2.setter
    def n2(self, value: float) -> None:
        """Set the n2 property."""
        self._cards[2].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[2].get_value("n3")

    @n3.setter
    def n3(self, value: float) -> None:
        """Set the n3 property."""
        self._cards[2].set_value("n3", value)

    @property
    def c0(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[2].get_value("c0")

    @c0.setter
    def c0(self, value: float) -> None:
        """Set the c0 property."""
        self._cards[2].set_value("c0", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[2].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[3].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[3].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[3].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        """Set the c4 property."""
        self._cards[3].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[3].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        """Set the c5 property."""
        self._cards[3].set_value("c5", value)

    @property
    def aij(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[3].get_value("aij")

    @aij.setter
    def aij(self, value: float) -> None:
        """Set the aij property."""
        self._cards[3].set_value("aij", value)

    @property
    def sij(self) -> typing.Optional[float]:
        """Get or set the Material constant, see equation in keyword manual page 245 (volume two).
        """ # nopep8
        return self._cards[3].get_value("sij")

    @sij.setter
    def sij(self, value: float) -> None:
        """Set the sij property."""
        self._cards[3].set_value("sij", value)

    @property
    def minr(self) -> typing.Optional[float]:
        """Get or set the Ratemin, minimum strain rate of interest.
        """ # nopep8
        return self._cards[3].get_value("minr")

    @minr.setter
    def minr(self, value: float) -> None:
        """Set the minr property."""
        self._cards[3].set_value("minr", value)

    @property
    def maxr(self) -> typing.Optional[float]:
        """Get or set the Ratemax, maximum strain rate of interest.
        """ # nopep8
        return self._cards[3].get_value("maxr")

    @maxr.setter
    def maxr(self, value: float) -> None:
        """Set the maxr property."""
        self._cards[3].set_value("maxr", value)

    @property
    def shape(self) -> typing.Optional[float]:
        """Get or set the Shape factor for unloading. Active for nonzero values of the hysteretic unloading factor HU. Values less than one reduces the energy dissipation and greater than one increases dissipation.
        """ # nopep8
        return self._cards[3].get_value("shape")

    @shape.setter
    def shape(self, value: float) -> None:
        """Set the shape property."""
        self._cards[3].set_value("shape", value)

    @property
    def expon(self) -> float:
        """Get or set the Exponent for unloading.  Active for nonzero values of the hysteretic unloading factor HU.  Default is 1.0.
        """ # nopep8
        return self._cards[4].get_value("expon")

    @expon.setter
    def expon(self, value: float) -> None:
        """Set the expon property."""
        self._cards[4].set_value("expon", value)

    @property
    def riuld(self) -> float:
        """Get or set the Flag for rate independent unloading, see Remark 6.
        EQ.0.0:	off,
        EQ.1.0:	on.
        """ # nopep8
        return self._cards[4].get_value("riuld")

    @riuld.setter
    def riuld(self, value: float) -> None:
        """Set the riuld property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""riuld must be `None` or one of {0.0,1.0}.""")
        self._cards[4].set_value("riuld", value)

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

    @property
    def pvid_link(self) -> DefineCurve:
        """Get the DefineCurve object for pvid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.pvid:
                return kwd
        return None

    @pvid_link.setter
    def pvid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for pvid."""
        self.pvid = value.lcid

