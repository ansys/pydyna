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

"""Module providing the Mat187 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT187_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("bulk", float, 20, 10, None),
    FieldSchema("gmod", float, 30, 10, None),
    FieldSchema("emod", float, 40, 10, None),
    FieldSchema("nue", float, 50, 10, None),
    FieldSchema("rbcfac", float, 60, 10, None),
    FieldSchema("numint", int, 70, 10, None),
)

_MAT187_CARD1 = (
    FieldSchema("lcid_t", int, 0, 10, None, "lcid-t"),
    FieldSchema("lcid_c", int, 10, 10, None, "lcid-c"),
    FieldSchema("lcid_s", int, 20, 10, None, "lcid-s"),
    FieldSchema("lcid_b", int, 30, 10, None, "lcid-b"),
    FieldSchema("nuep", float, 40, 10, None),
    FieldSchema("lcid_p", int, 50, 10, None, "lcid-p"),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("incdam", int, 70, 10, 0),
)

_MAT187_CARD2 = (
    FieldSchema("lcid_d", int, 0, 10, None),
    FieldSchema("epfail", float, 10, 10, 100000.0),
    FieldSchema("deprpt", float, 20, 10, None),
    FieldSchema("lcid_tri", int, 30, 10, None, "lcid-tri"),
    FieldSchema("lcid_lc", int, 40, 10, None),
)

_MAT187_CARD3 = (
    FieldSchema("miter", int, 0, 10, None),
    FieldSchema("mipds", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("incfail", int, 30, 10, 0),
    FieldSchema("iconv", int, 40, 10, 0),
    FieldSchema("asaf", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("nhsv", int, 70, 10, None),
)

_MAT187_CARD4 = (
    FieldSchema("lcemod", int, 0, 10, None),
    FieldSchema("beta", float, 10, 10, None),
    FieldSchema("filt", float, 20, 10, None),
)

_MAT187_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat187(KeywordBase):
    """DYNA MAT_187 keyword"""

    keyword = "MAT"
    subkeyword = "187"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcid_t": LinkType.DEFINE_CURVE,
        "lcid_c": LinkType.DEFINE_CURVE,
        "lcid_s": LinkType.DEFINE_CURVE,
        "lcid_b": LinkType.DEFINE_CURVE,
        "lcid_p": LinkType.DEFINE_CURVE,
        "lcid_d": LinkType.DEFINE_CURVE,
        "lcid_tri": LinkType.DEFINE_CURVE,
        "lcid_lc": LinkType.DEFINE_CURVE,
        "lcemod": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat187 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT187_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT187_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT187_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT187_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT187_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat187.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT187_OPTION0_CARD0,
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
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus, used by LS-DYNA in the time step calculation
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        """Set the bulk property."""
        self._cards[0].set_value("bulk", value)

    @property
    def gmod(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, used by LS-DYNA in the time step calculation.
        """ # nopep8
        return self._cards[0].get_value("gmod")

    @gmod.setter
    def gmod(self, value: float) -> None:
        """Set the gmod property."""
        self._cards[0].set_value("gmod", value)

    @property
    def emod(self) -> typing.Optional[float]:
        """Get or set the Young's modulus
        """ # nopep8
        return self._cards[0].get_value("emod")

    @emod.setter
    def emod(self, value: float) -> None:
        """Set the emod property."""
        self._cards[0].set_value("emod", value)

    @property
    def nue(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio
        """ # nopep8
        return self._cards[0].get_value("nue")

    @nue.setter
    def nue(self, value: float) -> None:
        """Set the nue property."""
        self._cards[0].set_value("nue", value)

    @property
    def rbcfac(self) -> typing.Optional[float]:
        """Get or set the Ratio of yield in biaxial compression vs. yield in uniaxial compression. If nonzero this will activate the use of a multi-linear yield surface. Default is 0.
        """ # nopep8
        return self._cards[0].get_value("rbcfac")

    @rbcfac.setter
    def rbcfac(self, value: float) -> None:
        """Set the rbcfac property."""
        self._cards[0].set_value("rbcfac", value)

    @property
    def numint(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("numint")

    @numint.setter
    def numint(self, value: int) -> None:
        """Set the numint property."""
        self._cards[0].set_value("numint", value)

    @property
    def lcid_t(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID giving the yield stress as a function of plastic strain, these curves should be obtained from quasi-static and (optionally) dynamic uniaxial tensile tests, this input is mandatory and the material model will not work unless at least one tensile stress-strain curve is given.
        """ # nopep8
        return self._cards[1].get_value("lcid_t")

    @lcid_t.setter
    def lcid_t(self, value: int) -> None:
        """Set the lcid_t property."""
        self._cards[1].set_value("lcid_t", value)

    @property
    def lcid_c(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the yield stress as a function of plastic strain, this curve should be obtained from a quasi-static uniaxial compression test, this input is optional.
        """ # nopep8
        return self._cards[1].get_value("lcid_c")

    @lcid_c.setter
    def lcid_c(self, value: int) -> None:
        """Set the lcid_c property."""
        self._cards[1].set_value("lcid_c", value)

    @property
    def lcid_s(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the yield stress as a function of plastic strain, this curve should be obtained from a quasi-static shear test, this input is optional
        """ # nopep8
        return self._cards[1].get_value("lcid_s")

    @lcid_s.setter
    def lcid_s(self, value: int) -> None:
        """Set the lcid_s property."""
        self._cards[1].set_value("lcid_s", value)

    @property
    def lcid_b(self) -> typing.Optional[int]:
        """Get or set the load curve ID giving the yield stress as a function of plastic strain, this curve should be obtained from a quasi-static biaxial tensile test, this input is optional.
        """ # nopep8
        return self._cards[1].get_value("lcid_b")

    @lcid_b.setter
    def lcid_b(self, value: int) -> None:
        """Set the lcid_b property."""
        self._cards[1].set_value("lcid_b", value)

    @property
    def nuep(self) -> typing.Optional[float]:
        """Get or set the plastic Poisson's ratio : an estimated ratio of transversal to longitudinal plastic rate of deformation should be given, a value <0 will result in associated plasticity to the yield surface (the associated plasticity option is implemented only for IQUAD=1).
        """ # nopep8
        return self._cards[1].get_value("nuep")

    @nuep.setter
    def nuep(self, value: float) -> None:
        """Set the nuep property."""
        self._cards[1].set_value("nuep", value)

    @property
    def lcid_p(self) -> typing.Optional[int]:
        """Get or set the load curve ID giving the plastic Poisson's ratio as a function of equivalent plastic deformation during uniaxial tensile testing, if the (optional) load curve is given, the constant value in the previous field will be ignored.
        """ # nopep8
        return self._cards[1].get_value("lcid_p")

    @lcid_p.setter
    def lcid_p(self, value: int) -> None:
        """Set the lcid_p property."""
        self._cards[1].set_value("lcid_p", value)

    @property
    def incdam(self) -> int:
        """Get or set the Flag to control the damage evolution as a function of triaxiality. If INCDAM=0 damage evolution is independent of the triaxialty. If INCDAM=1 an incremental formulation is used to compute the damage.
        """ # nopep8
        return self._cards[1].get_value("incdam")

    @incdam.setter
    def incdam(self, value: int) -> None:
        """Set the incdam property."""
        if value not in [0, 1, None]:
            raise Exception("""incdam must be `None` or one of {0,1}.""")
        self._cards[1].set_value("incdam", value)

    @property
    def lcid_d(self) -> typing.Optional[int]:
        """Get or set the load curve ID giving the damage parameter as a function of equivalent plastic deformation during uniaxial tensile testing, by default this option assumes that effective yield values are used in the load curves LCID-T, LCID-C, LCID-S and LCID-B, if LCID-D is given a negative value, true yield stress values can be used.
        """ # nopep8
        return self._cards[2].get_value("lcid_d")

    @lcid_d.setter
    def lcid_d(self, value: int) -> None:
        """Set the lcid_d property."""
        self._cards[2].set_value("lcid_d", value)

    @property
    def epfail(self) -> float:
        """Get or set the This parameter is the equivalent plastic strain at failure. If EPFAIL is given as a negative integer, a load curve is expected that defines EPFAIL as a function of the plastic strain rate. Default value is 1.0e+5
        """ # nopep8
        return self._cards[2].get_value("epfail")

    @epfail.setter
    def epfail(self, value: float) -> None:
        """Set the epfail property."""
        self._cards[2].set_value("epfail", value)

    @property
    def deprpt(self) -> typing.Optional[float]:
        """Get or set the Increment of equivalent plastic strain between failure point and rupture point, stresses will fade out to zero between EPFAIL and EPFAIL+DEPRUPT
        """ # nopep8
        return self._cards[2].get_value("deprpt")

    @deprpt.setter
    def deprpt(self, value: float) -> None:
        """Set the deprpt property."""
        self._cards[2].set_value("deprpt", value)

    @property
    def lcid_tri(self) -> typing.Optional[int]:
        """Get or set the Load curve that specifies a factor that works multiplicatively on the value of DC depending on the triaxiality pressue/sigma_vm.. This option is active only if DC is given as a negative value (see above)..
        """ # nopep8
        return self._cards[2].get_value("lcid_tri")

    @lcid_tri.setter
    def lcid_tri(self, value: int) -> None:
        """Set the lcid_tri property."""
        self._cards[2].set_value("lcid_tri", value)

    @property
    def lcid_lc(self) -> typing.Optional[int]:
        """Get or set the Load curve that specifies a factor that works multiplicatively on the value of DC depending on the linear element dimension, this option is active only if DC is given as a negative value (see above).
        """ # nopep8
        return self._cards[2].get_value("lcid_lc")

    @lcid_lc.setter
    def lcid_lc(self, value: int) -> None:
        """Set the lcid_lc property."""
        self._cards[2].set_value("lcid_lc", value)

    @property
    def miter(self) -> typing.Optional[int]:
        """Get or set the Maximum number of iterations in the cutting plane algorithm, default is set to 400.
        """ # nopep8
        return self._cards[3].get_value("miter")

    @miter.setter
    def miter(self, value: int) -> None:
        """Set the miter property."""
        self._cards[3].set_value("miter", value)

    @property
    def mipds(self) -> typing.Optional[int]:
        """Get or set the Maximum number of iterations in the secant iteration performed to enforce plane stress (shell elements only), default set to 10
        """ # nopep8
        return self._cards[3].get_value("mipds")

    @mipds.setter
    def mipds(self, value: int) -> None:
        """Set the mipds property."""
        self._cards[3].set_value("mipds", value)

    @property
    def incfail(self) -> int:
        """Get or set the Flag to control the failure evolution as a function of triaxiality. If INCFAIL=0 failure evolution is independent of the triaxiality. If INCFAIL=1 an incremental formulation is used to compute the failure value. If INCFAIL=-1 the failure model is deactivated.
        """ # nopep8
        return self._cards[3].get_value("incfail")

    @incfail.setter
    def incfail(self, value: int) -> None:
        """Set the incfail property."""
        if value not in [0, 1, -1, None]:
            raise Exception("""incfail must be `None` or one of {0,1,-1}.""")
        self._cards[3].set_value("incfail", value)

    @property
    def iconv(self) -> int:
        """Get or set the Formulation flag :
        ICONV=0 : default
        ICONV=1 : yield surface is internally modified by increasing the shear yield until a convex yield surface is achieved
        ICONV=2 : if the plastic Poisson's ratio is smaller than the elastic Poisson's ratio, both are set equal to the smaller value of the two
        """ # nopep8
        return self._cards[3].get_value("iconv")

    @iconv.setter
    def iconv(self, value: int) -> None:
        """Set the iconv property."""
        if value not in [0, 1, None]:
            raise Exception("""iconv must be `None` or one of {0,1}.""")
        self._cards[3].set_value("iconv", value)

    @property
    def asaf(self) -> typing.Optional[int]:
        """Get or set the Safety factor, used only if ICONV=1, values between 1 and 2 can improve convergence, however the shear yield will be artificially increased if this option is used, default is set to 1.
        """ # nopep8
        return self._cards[3].get_value("asaf")

    @asaf.setter
    def asaf(self, value: int) -> None:
        """Set the asaf property."""
        self._cards[3].set_value("asaf", value)

    @property
    def nhsv(self) -> typing.Optional[int]:
        """Get or set the Number of history variables. Default is 22. Set to 28 if the “instability criterion” should be included in the output (see Remark 5). Note that NEIPS or NEIPH must also be set on *DATABASE_EXTENT_BINARY for the history variable data to be output.
        """ # nopep8
        return self._cards[3].get_value("nhsv")

    @nhsv.setter
    def nhsv(self, value: int) -> None:
        """Set the nhsv property."""
        self._cards[3].set_value("nhsv", value)

    @property
    def lcemod(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining Young's modulus as function of effective strain rate.
        """ # nopep8
        return self._cards[4].get_value("lcemod")

    @lcemod.setter
    def lcemod(self, value: int) -> None:
        """Set the lcemod property."""
        self._cards[4].set_value("lcemod", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Decay constant in viscoelastic law
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def filt(self) -> typing.Optional[float]:
        """Get or set the Factor for strain rate filtering
        """ # nopep8
        return self._cards[4].get_value("filt")

    @filt.setter
    def filt(self, value: float) -> None:
        """Set the filt property."""
        self._cards[4].set_value("filt", value)

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
    def lcid_t_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_t."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_t:
                return kwd
        return None

    @lcid_t_link.setter
    def lcid_t_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_t."""
        self.lcid_t = value.lcid

    @property
    def lcid_c_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_c."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_c:
                return kwd
        return None

    @lcid_c_link.setter
    def lcid_c_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_c."""
        self.lcid_c = value.lcid

    @property
    def lcid_s_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_s."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_s:
                return kwd
        return None

    @lcid_s_link.setter
    def lcid_s_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_s."""
        self.lcid_s = value.lcid

    @property
    def lcid_b_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_b."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_b:
                return kwd
        return None

    @lcid_b_link.setter
    def lcid_b_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_b."""
        self.lcid_b = value.lcid

    @property
    def lcid_p_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_p."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_p:
                return kwd
        return None

    @lcid_p_link.setter
    def lcid_p_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_p."""
        self.lcid_p = value.lcid

    @property
    def lcid_d_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_d."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_d:
                return kwd
        return None

    @lcid_d_link.setter
    def lcid_d_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_d."""
        self.lcid_d = value.lcid

    @property
    def lcid_tri_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_tri."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_tri:
                return kwd
        return None

    @lcid_tri_link.setter
    def lcid_tri_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_tri."""
        self.lcid_tri = value.lcid

    @property
    def lcid_lc_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_lc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_lc:
                return kwd
        return None

    @lcid_lc_link.setter
    def lcid_lc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_lc."""
        self.lcid_lc = value.lcid

    @property
    def lcemod_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcemod."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcemod:
                return kwd
        return None

    @lcemod_link.setter
    def lcemod_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcemod."""
        self.lcemod = value.lcid

