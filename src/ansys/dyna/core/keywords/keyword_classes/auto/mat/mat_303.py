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

"""Module providing the Mat303 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT303_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
)

_MAT303_CARD1 = (
    FieldSchema("fvf", float, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("rf", float, 20, 10, None),
    FieldSchema("rm", float, 30, 10, None),
    FieldSchema("fl", float, 40, 10, None),
    FieldSchema("fd", float, 50, 10, None),
)

_MAT303_CARD2 = (
    FieldSchema("f_e", float, 0, 10, None),
    FieldSchema("f_pr", float, 10, 10, None),
    FieldSchema("iso", int, 20, 10, 0),
    FieldSchema("dam", int, 30, 10, 0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("s_f", int, 60, 10, None),
    FieldSchema("s_m", int, 70, 10, None),
)

_MAT303_CARD3 = (
    FieldSchema("f_el", float, 0, 10, None),
    FieldSchema("f_et", float, 10, 10, None),
    FieldSchema("f_prtl", float, 20, 10, None),
    FieldSchema("f_prtt", float, 30, 10, None),
    FieldSchema("f_glt", float, 40, 10, None),
)

_MAT303_CARD4 = (
    FieldSchema("m_e", float, 0, 10, None),
    FieldSchema("m_pr", float, 10, 10, None),
    FieldSchema("m_s1", float, 20, 10, None),
    FieldSchema("m_s2", float, 30, 10, None),
    FieldSchema("m_s3", float, 40, 10, None),
    FieldSchema("m_s4", float, 50, 10, None),
    FieldSchema("itc", int, 60, 10, 0),
)

_MAT303_CARD5 = (
    FieldSchema("m_ec", float, 0, 10, None),
    FieldSchema("m_prc", float, 10, 10, None),
    FieldSchema("m_s1c", float, 20, 10, None),
    FieldSchema("m_s2c", float, 30, 10, None),
    FieldSchema("m_s3c", float, 40, 10, None),
    FieldSchema("m_s4c", float, 50, 10, None),
    FieldSchema("pt", float, 60, 10, None),
    FieldSchema("pc", float, 70, 10, None),
)

_MAT303_CARD6 = (
    FieldSchema("d_c", float, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("d_ero", float, 40, 10, None),
)

_MAT303_CARD7 = (
    FieldSchema("lcidt", int, 0, 10, None),
    FieldSchema("lcidc", int, 10, 10, None),
    FieldSchema("lcfs", int, 20, 10, None),
    FieldSchema("lcfa", int, 30, 10, None),
    FieldSchema("lcsrs", int, 40, 10, None),
    FieldSchema("lcsra", int, 50, 10, None),
)

_MAT303_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat303(KeywordBase):
    """DYNA MAT_303 keyword"""

    keyword = "MAT"
    subkeyword = "303"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcidt": LinkType.DEFINE_CURVE,
        "lcidc": LinkType.DEFINE_CURVE,
        "lcfs": LinkType.DEFINE_CURVE,
        "lcfa": LinkType.DEFINE_CURVE,
        "lcsrs": LinkType.DEFINE_CURVE,
        "lcsra": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat303 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT303_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT303_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT303_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT303_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT303_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT303_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT303_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT303_CARD7,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat303._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT303_OPTION0_CARD0,
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
    def fvf(self) -> typing.Optional[float]:
        """Get or set the Phi_f, fiber volume fraction.If RFand RM are given, a nonzero FVF must be specified to calculate the mass density of the overall fiber - reinforced composite.This value can be overwritten by *INITIAL_STRESS_SHELL or *INITIAL_STRESS_SOLID.SeeRemark 4
        """ # nopep8
        return self._cards[1].get_value("fvf")

    @fvf.setter
    def fvf(self, value: float) -> None:
        """Set the fvf property."""
        self._cards[1].set_value("fvf", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the rho_c , mass density of the overall fiber - reinforced composite.This value will be neglected if RFand RM are given, respectively)
        """ # nopep8
        return self._cards[1].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[1].set_value("ro", value)

    @property
    def rf(self) -> typing.Optional[float]:
        """Get or set the rho_f , mass density of the fiber phase
        """ # nopep8
        return self._cards[1].get_value("rf")

    @rf.setter
    def rf(self, value: float) -> None:
        """Set the rf property."""
        self._cards[1].set_value("rf", value)

    @property
    def rm(self) -> typing.Optional[float]:
        """Get or set the rho_m, mass density of the matrix phase
        """ # nopep8
        return self._cards[1].get_value("rm")

    @rm.setter
    def rm(self, value: float) -> None:
        """Set the rm property."""
        self._cards[1].set_value("rm", value)

    @property
    def fl(self) -> typing.Optional[float]:
        """Get or set the Fiber length. Alternatively, if you want to specify the fiber aspect ratio, set FL to the fiber aspect ratio and FD to 1.0.
        """ # nopep8
        return self._cards[1].get_value("fl")

    @fl.setter
    def fl(self, value: float) -> None:
        """Set the fl property."""
        self._cards[1].set_value("fl", value)

    @property
    def fd(self) -> typing.Optional[float]:
        """Get or set the Fiber diameter. Alternatively, if you want to directly specify the fiber aspect ratio, set FD to 1.0 and FL to the fiber aspect ratio.
        """ # nopep8
        return self._cards[1].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[1].set_value("fd", value)

    @property
    def f_e(self) -> typing.Optional[float]:
        """Get or set the E_**f, Young's modulus of the fiber phase if the fiber property is isotropic. See Remark 2
        """ # nopep8
        return self._cards[2].get_value("f_e")

    @f_e.setter
    def f_e(self, value: float) -> None:
        """Set the f_e property."""
        self._cards[2].set_value("f_e", value)

    @property
    def f_pr(self) -> typing.Optional[float]:
        """Get or set the v_tl**f  , Poisson's ratio of the fiber phase if the fiber property is isotropic.
        """ # nopep8
        return self._cards[2].get_value("f_pr")

    @f_pr.setter
    def f_pr(self, value: float) -> None:
        """Set the f_pr property."""
        self._cards[2].set_value("f_pr", value)

    @property
    def iso(self) -> int:
        """Get or set the Flag for anisotropy of the fiber phase.
        EQ.0: Isotropic fiber material property
        EQ.1: Transversely isotropic fiber material property
        """ # nopep8
        return self._cards[2].get_value("iso")

    @iso.setter
    def iso(self, value: int) -> None:
        """Set the iso property."""
        if value not in [0, 1, None]:
            raise Exception("""iso must be `None` or one of {0,1}.""")
        self._cards[2].set_value("iso", value)

    @property
    def dam(self) -> int:
        """Get or set the Flag for the composite failure model:
        EQ.0: Do not consider material damage.
        EQ.1: Use the FIBAND model.See Remark 4.
        EQ.2: Material damage occurs when the failure strength of the fiber or failure strain of the matrix is reached. See Remark 5.
        """ # nopep8
        return self._cards[2].get_value("dam")

    @dam.setter
    def dam(self, value: int) -> None:
        """Set the dam property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""dam must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("dam", value)

    @property
    def s_f(self) -> typing.Optional[int]:
        """Get or set the Failure strength of the fiber phase ( ignored unless DAM=2).
        """ # nopep8
        return self._cards[2].get_value("s_f")

    @s_f.setter
    def s_f(self, value: int) -> None:
        """Set the s_f property."""
        self._cards[2].set_value("s_f", value)

    @property
    def s_m(self) -> typing.Optional[int]:
        """Get or set the Failure strain of the matrix phase (ignored unless DAM=2).
        """ # nopep8
        return self._cards[2].get_value("s_m")

    @s_m.setter
    def s_m(self, value: int) -> None:
        """Set the s_m property."""
        self._cards[2].set_value("s_m", value)

    @property
    def f_el(self) -> typing.Optional[float]:
        """Get or set the E_l**f, Young's modulus of the fiber phase along the fiber's longitudinal direction l. See Remark 3
        """ # nopep8
        return self._cards[3].get_value("f_el")

    @f_el.setter
    def f_el(self, value: float) -> None:
        """Set the f_el property."""
        self._cards[3].set_value("f_el", value)

    @property
    def f_et(self) -> typing.Optional[float]:
        """Get or set the E_t**f, Young's modulus of the fiber phase along the fiber's transverse direction t. Note that the transversely isotropic model becomes isotropic by setting E_t**f=E_l**f, v_tt**f=v_tl**f, and G_lt**f=(E_l**f)/[2(1+v_tl**f)] .
        """ # nopep8
        return self._cards[3].get_value("f_et")

    @f_et.setter
    def f_et(self, value: float) -> None:
        """Set the f_et property."""
        self._cards[3].set_value("f_et", value)

    @property
    def f_prtl(self) -> typing.Optional[float]:
        """Get or set the v_tl**f, Poisson's ratio of the fiber phase.
        """ # nopep8
        return self._cards[3].get_value("f_prtl")

    @f_prtl.setter
    def f_prtl(self, value: float) -> None:
        """Set the f_prtl property."""
        self._cards[3].set_value("f_prtl", value)

    @property
    def f_prtt(self) -> typing.Optional[float]:
        """Get or set the v_tt**f, Poisson's ratio of the fiber phase. Note that the transversely isotropic model becomes isotropic by setting E_t**f=E_l**f, v_tt**f=v_tl**f, and G_lt**f=(E_l**f)/[2(1+v_tl**f )] .
        """ # nopep8
        return self._cards[3].get_value("f_prtt")

    @f_prtt.setter
    def f_prtt(self, value: float) -> None:
        """Set the f_prtt property."""
        self._cards[3].set_value("f_prtt", value)

    @property
    def f_glt(self) -> typing.Optional[float]:
        """Get or set the G_lt**f, shear modulus of the fiber phase in the lt direction. Note that the transversely isotropic model becomes isotropic by setting E_t**f=E_l**f, v_tt**f=v_tl**f, and G_lt**f=(E_l**f)/[2(1+v_tl**f )] .
        """ # nopep8
        return self._cards[3].get_value("f_glt")

    @f_glt.setter
    def f_glt(self, value: float) -> None:
        """Set the f_glt property."""
        self._cards[3].set_value("f_glt", value)

    @property
    def m_e(self) -> typing.Optional[float]:
        """Get or set the E_**m, Young's modulus of the matrix phase. See Remark 4
        """ # nopep8
        return self._cards[4].get_value("m_e")

    @m_e.setter
    def m_e(self, value: float) -> None:
        """Set the m_e property."""
        self._cards[4].set_value("m_e", value)

    @property
    def m_pr(self) -> typing.Optional[float]:
        """Get or set the v_**m, Poisson's ratio of the matrix phase
        """ # nopep8
        return self._cards[4].get_value("m_pr")

    @m_pr.setter
    def m_pr(self, value: float) -> None:
        """Set the m_pr property."""
        self._cards[4].set_value("m_pr", value)

    @property
    def m_s1(self) -> typing.Optional[float]:
        """Get or set the s_1**m, plastic yielding parameter of the matrix phase
        """ # nopep8
        return self._cards[4].get_value("m_s1")

    @m_s1.setter
    def m_s1(self, value: float) -> None:
        """Set the m_s1 property."""
        self._cards[4].set_value("m_s1", value)

    @property
    def m_s2(self) -> typing.Optional[float]:
        """Get or set the s_2**m, plastic yielding parameter of the matrix phase
        """ # nopep8
        return self._cards[4].get_value("m_s2")

    @m_s2.setter
    def m_s2(self, value: float) -> None:
        """Set the m_s2 property."""
        self._cards[4].set_value("m_s2", value)

    @property
    def m_s3(self) -> typing.Optional[float]:
        """Get or set the s_3**m, plastic yielding parameter of the matrix phase
        """ # nopep8
        return self._cards[4].get_value("m_s3")

    @m_s3.setter
    def m_s3(self, value: float) -> None:
        """Set the m_s3 property."""
        self._cards[4].set_value("m_s3", value)

    @property
    def m_s4(self) -> typing.Optional[float]:
        """Get or set the h_0**m, plastic yielding parameter of the matrix phase
        """ # nopep8
        return self._cards[4].get_value("m_s4")

    @m_s4.setter
    def m_s4(self, value: float) -> None:
        """Set the m_s4 property."""
        self._cards[4].set_value("m_s4", value)

    @property
    def itc(self) -> int:
        """Get or set the Option for the elastoplastic material law for the matrix phase.
        EQ.0: no tension - compression asymmetry in material properties
        EQ.1: use tension - compression asymmetric material properties
        EQ.2: Use a viscoplastic formulation to account for strain rate effects,
        where a table can define the yield strength as a function of the equivalent plastic strain for various strain rates
        EQ.3: Use tension - compression asymmetric material properties in a
        viscoplastic formulation to account for strain rate effects
        """ # nopep8
        return self._cards[4].get_value("itc")

    @itc.setter
    def itc(self, value: int) -> None:
        """Set the itc property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""itc must be `None` or one of {0,1,2,3}.""")
        self._cards[4].set_value("itc", value)

    @property
    def m_ec(self) -> typing.Optional[float]:
        """Get or set the E_**m, Young's modulus of the matrix phase in compression
        """ # nopep8
        return self._cards[5].get_value("m_ec")

    @m_ec.setter
    def m_ec(self, value: float) -> None:
        """Set the m_ec property."""
        self._cards[5].set_value("m_ec", value)

    @property
    def m_prc(self) -> typing.Optional[float]:
        """Get or set the v_**m, Poisson's ratio of the matrix phase in compression
        """ # nopep8
        return self._cards[5].get_value("m_prc")

    @m_prc.setter
    def m_prc(self, value: float) -> None:
        """Set the m_prc property."""
        self._cards[5].set_value("m_prc", value)

    @property
    def m_s1c(self) -> typing.Optional[float]:
        """Get or set the s_1**m, plastic yielding parameter of the matrix phase in compression
        """ # nopep8
        return self._cards[5].get_value("m_s1c")

    @m_s1c.setter
    def m_s1c(self, value: float) -> None:
        """Set the m_s1c property."""
        self._cards[5].set_value("m_s1c", value)

    @property
    def m_s2c(self) -> typing.Optional[float]:
        """Get or set the s_2**m, plastic yielding parameter of the matrix phase in compression
        """ # nopep8
        return self._cards[5].get_value("m_s2c")

    @m_s2c.setter
    def m_s2c(self, value: float) -> None:
        """Set the m_s2c property."""
        self._cards[5].set_value("m_s2c", value)

    @property
    def m_s3c(self) -> typing.Optional[float]:
        """Get or set the s_3**m, plastic yielding parameter of the matrix phase in compression
        """ # nopep8
        return self._cards[5].get_value("m_s3c")

    @m_s3c.setter
    def m_s3c(self, value: float) -> None:
        """Set the m_s3c property."""
        self._cards[5].set_value("m_s3c", value)

    @property
    def m_s4c(self) -> typing.Optional[float]:
        """Get or set the h_0**m, plastic yielding parameter of the matrix phase in compression
        """ # nopep8
        return self._cards[5].get_value("m_s4c")

    @m_s4c.setter
    def m_s4c(self, value: float) -> None:
        """Set the m_s4c property."""
        self._cards[5].set_value("m_s4c", value)

    @property
    def pt(self) -> typing.Optional[float]:
        """Get or set the Absolute value of the tensile mean stress threshold beyond which the tensile material properties are adopted. If the mean stress ((Sigma_XX+Sigma_YY+Sigma_ZZ ))/3 falls within the range [-PC ,PT ], a weighted average of the tensile and compressive material properties is used for the matrix phase. See Remark 3.
        """ # nopep8
        return self._cards[5].get_value("pt")

    @pt.setter
    def pt(self, value: float) -> None:
        """Set the pt property."""
        self._cards[5].set_value("pt", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Absolute value of the compressive mean stress threshold beyond which compressive material properties are adopted.
        """ # nopep8
        return self._cards[5].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        """Set the pc property."""
        self._cards[5].set_value("pc", value)

    @property
    def d_c(self) -> typing.Optional[float]:
        """Get or set the Critical damage threshold value
        """ # nopep8
        return self._cards[6].get_value("d_c")

    @d_c.setter
    def d_c(self, value: float) -> None:
        """Set the d_c property."""
        self._cards[6].set_value("d_c", value)

    @property
    def d_ero(self) -> typing.Optional[float]:
        """Get or set the Maximum damage value beyond which the element is eroded. Element erosion does not occur if this value is left empty
        """ # nopep8
        return self._cards[6].get_value("d_ero")

    @d_ero.setter
    def d_ero(self, value: float) -> None:
        """Set the d_ero property."""
        self._cards[6].set_value("d_ero", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID. The load curve is available for ITC = 0 and 1 while the table is available for ITC = 2 and 3.
        Load Curve.When LCIDT is a load curve ID, data points representing the accumulated equivalent plastic strainand the corresponding yield strength for the matrix phase are respectively given in the first columnand the second column of the corresponding load curve in *DEFINE_CURVE. If ITC = 1 is specified in Card 4, this load curve describes the matrix material in tension only.See Remark 3.
        Tabular Data.If ITC = 2 or 3 is specified in Card 4, LCIDT is treated as a table ID.Data points representing different strain rates are given in one column of the corresponding table in *DEFINE_TABLE, followed by the definitions of load curves for the yield strength for the matrix phase as a function of effective plastic strain at each given strain rate value. See *DEFINE_TABLE. Linear interpolation of the yield strengths at different given strain rates is used by default. If the strain rate values fall out of range, extrapolation is not used; instead, either the first or last curve determines the yield strength as a function of effective plastic strain, which depends on whether the strain rate falls below the minimum given value or exceeds the maximum given value, respectively.If ITC = 3 is specified in Card 4, this table describes the matrix material in tension only.
        Logarithmically - Defined Tables. If ITC = 2 or 3 is specified in Card 4, LCIDT refers to a table ID.In addition, if the first value in the table is negative, all data points in the table represent the natural logarithm of strain rates,and logarithmic interpolation of the yield strengths at discrete given strain rates is used.Note that this option works only when the lowest strain rate has a value less than 1.0. For values greater than or equal to 1.0, use the LOG_INTERPOLATION option.
        """ # nopep8
        return self._cards[7].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        """Set the lcidt property."""
        self._cards[7].set_value("lcidt", value)

    @property
    def lcidc(self) -> typing.Optional[int]:
        """Get or set the ID for the user-defined load curve or table. Similar to LCIDT, LCIDC is the ID of a load curve or table that contains the accumulated equivalent plastic strain and the yield strength for the matrix phase in compression. This parameter is available for ITC = 1 (load curve) and 3 (table). The description for the load curve and table is the same as for LCIDT but for compression. See Remark 3.
        """ # nopep8
        return self._cards[7].get_value("lcidc")

    @lcidc.setter
    def lcidc(self, value: int) -> None:
        """Set the lcidc property."""
        self._cards[7].set_value("lcidc", value)

    @property
    def lcfs(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve giving the equivalent failure strain (ordinate) as a function of the stress triaxiality state (abscissa).
        This curve should only be defined when DAM = 11.
        """ # nopep8
        return self._cards[7].get_value("lcfs")

    @lcfs.setter
    def lcfs(self, value: int) -> None:
        """Set the lcfs property."""
        self._cards[7].set_value("lcfs", value)

    @property
    def lcfa(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve giving the failure anisotropy factor (ordinate) as a function of the stress triaxiality state (abscissa).
        The failure anisotropy factor falls within the range [0,1]. This curve should only be defined when DAM = 11
        """ # nopep8
        return self._cards[7].get_value("lcfa")

    @lcfa.setter
    def lcfa(self, value: int) -> None:
        """Set the lcfa property."""
        self._cards[7].set_value("lcfa", value)

    @property
    def lcsrs(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve giving scaling factor SRS (ordinate) as a a function of strain rate (abscissa).
        The factor SRS scales the failure strain defined with LCFS. It is optional
        """ # nopep8
        return self._cards[7].get_value("lcsrs")

    @lcsrs.setter
    def lcsrs(self, value: int) -> None:
        """Set the lcsrs property."""
        self._cards[7].set_value("lcsrs", value)

    @property
    def lcsra(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve giving scaling factor SRA (ordinate) as a a function of strain rate (abscissa).
        The factor SRA scales the failure anisotropy factor defined with LCFA. It is optional
        """ # nopep8
        return self._cards[7].get_value("lcsra")

    @lcsra.setter
    def lcsra(self, value: int) -> None:
        """Set the lcsra property."""
        self._cards[7].set_value("lcsra", value)

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

    @property
    def lcidt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidt:
                return kwd
        return None

    @lcidt_link.setter
    def lcidt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidt."""
        self.lcidt = value.lcid

    @property
    def lcidc_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcidc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc:
                return kwd
        return None

    @lcidc_link.setter
    def lcidc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc."""
        self.lcidc = value.lcid

    @property
    def lcfs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfs:
                return kwd
        return None

    @lcfs_link.setter
    def lcfs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfs."""
        self.lcfs = value.lcid

    @property
    def lcfa_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfa."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfa:
                return kwd
        return None

    @lcfa_link.setter
    def lcfa_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfa."""
        self.lcfa = value.lcid

    @property
    def lcsrs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsrs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsrs:
                return kwd
        return None

    @lcsrs_link.setter
    def lcsrs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsrs."""
        self.lcsrs = value.lcid

    @property
    def lcsra_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsra."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsra:
                return kwd
        return None

    @lcsra_link.setter
    def lcsra_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsra."""
        self.lcsra = value.lcid

