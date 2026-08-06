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

"""Module providing the Mat077H class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT077H_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("pr", float, 20, 10, None),
    FieldSchema("n", int, 30, 10, 0),
    FieldSchema("nv", int, 40, 10, None),
    FieldSchema("g", float, 50, 10, None),
    FieldSchema("sigf", float, 60, 10, None),
    FieldSchema("ref", float, 70, 10, 0.0),
)

_MAT077H_CARD1 = (
    FieldSchema("tbhys", float, 0, 10, None),
    FieldSchema("lcbi", float, 10, 10, None),
    FieldSchema("lcpl", float, 20, 10, None),
    FieldSchema("wbi", float, 30, 10, None),
    FieldSchema("wpl", float, 40, 10, None),
    FieldSchema("d1", float, 50, 10, None),
    FieldSchema("d2", float, 60, 10, None),
    FieldSchema("d3", float, 70, 10, None),
)

_MAT077H_CARD2 = (
    FieldSchema("sgl", float, 0, 10, None),
    FieldSchema("sw", float, 10, 10, None),
    FieldSchema("st", float, 20, 10, None),
    FieldSchema("lcid1", int, 30, 10, None),
    FieldSchema("data", float, 40, 10, None),
    FieldSchema("lcid2", int, 50, 10, None),
    FieldSchema("bstart", float, 60, 10, None),
    FieldSchema("tramp", float, 70, 10, None),
)

_MAT077H_CARD3 = (
    FieldSchema("c10", float, 0, 10, None),
    FieldSchema("c01", float, 10, 10, None),
    FieldSchema("c11", float, 20, 10, None),
    FieldSchema("c20", float, 30, 10, None),
    FieldSchema("c02", float, 40, 10, None),
    FieldSchema("c30", float, 50, 10, None),
    FieldSchema("therml", float, 60, 10, None),
)

_MAT077H_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat077H(KeywordBase):
    """DYNA MAT_077_H keyword"""

    keyword = "MAT"
    subkeyword = "077_H"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcid1": LinkType.DEFINE_CURVE,
        "lcid2": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat077H class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT077H_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT077H_CARD1,
                active_func=lambda: self.pr and self.pr < 0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT077H_CARD2,
                active_func=lambda: self.n > 0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT077H_CARD3,
                active_func=lambda: self.n == 0,
                **kwargs,
            ),
            TableCard(
                [
                    Field("gi", float, 0, 10, None),
                    Field("betai", float, 10, 10, None),
                    Field("gj", float, 20, 10, None),
                    Field("sigfj", float, 30, 10, None),
                    Field("vflag", int, 40, 10, None),
                ],
                None,
                name="constants",
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat077H._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT077H_OPTION0_CARD0,
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
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio. If set to a negative number, the Poisson's ratio is the absolute value, and Card 2 is included for extra parameters. Setting to 0.5 activates a U-P formulation for implicit analysis; see Remark Error! Reference source not found. of *MAT_027 (the Mooney-Rivlin rubber model).
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def n(self) -> int:
        """Get or set the Number of hyperelastic constants to solve for from LCID1 or combinations of LCID1/LCBI/LCPL:
        EQ.0: set hyperelastic constants directly
        EQ.1: Solve for C10 and C01,
        EQ.2: Solve for C10, C01, C11, C20, and C02,
        EQ.3: Solve for C10, C01, C11, C20, C02, and C30
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        """Set the n property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""n must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("n", value)

    @property
    def nv(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("nv")

    @nv.setter
    def nv(self, value: int) -> None:
        """Set the nv property."""
        self._cards[0].set_value("nv", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus for frequency independent damping. Frequency independent damping is based of a spring and slider in series. The critical stress for the slider mechanism is SIGF defined below. For the best results, the value of G should be 250-1000 times greater than SIGF.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def sigf(self) -> typing.Optional[float]:
        """Get or set the Limit stress for frequency independent, frictional, damping.
        """ # nopep8
        return self._cards[0].get_value("sigf")

    @sigf.setter
    def sigf(self, value: float) -> None:
        """Set the sigf property."""
        self._cards[0].set_value("sigf", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference
        geometry is defined by the keyword:*INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
        EQ.0.0: Off,
        EQ.1.0: On..
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ref must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("ref", value)

    @property
    def tbhys(self) -> typing.Optional[float]:
        """Get or set the Table ID for hysteresis, which can be positive or negative; see Remarks 1 and 2. This field only applies to solid elements.
        """ # nopep8
        return self._cards[1].get_value("tbhys")

    @tbhys.setter
    def tbhys(self, value: float) -> None:
        """Set the tbhys property."""
        self._cards[1].set_value("tbhys", value)

    @property
    def lcbi(self) -> typing.Optional[float]:
        """Get or set the Load curve ID giving force as a function of displacement for the biaxial test used for parameter fitting. Make sure N > 0 on Card 1 if setting this parameter. See Remark 3
        """ # nopep8
        return self._cards[1].get_value("lcbi")

    @lcbi.setter
    def lcbi(self, value: float) -> None:
        """Set the lcbi property."""
        self._cards[1].set_value("lcbi", value)

    @property
    def lcpl(self) -> typing.Optional[float]:
        """Get or set the Load curve ID giving force as a function of displacement for the planar test used for parameter fitting. Make sure N > 0 on Card 1 if setting this parameter. See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("lcpl")

    @lcpl.setter
    def lcpl(self, value: float) -> None:
        """Set the lcpl property."""
        self._cards[1].set_value("lcpl", value)

    @property
    def wbi(self) -> typing.Optional[float]:
        """Get or set the Weight factor giving the relative influence of the biaxial test data in the fitting of material parameters. A value of 1.0 means that the biaxial test data is of equal importance as the uniaxial test data. Make sure N > 0 on Card 1 if setting this parameter. See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("wbi")

    @wbi.setter
    def wbi(self, value: float) -> None:
        """Set the wbi property."""
        self._cards[1].set_value("wbi", value)

    @property
    def wpl(self) -> typing.Optional[float]:
        """Get or set the Weight factor giving the relative influence of planar test data in the fitting of material parameters. A value of 1.0 means that the planar test data is of equal importance as the uniaxial test data. Make sure N > 0 on Card 1 if setting this parameter. See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("wpl")

    @wpl.setter
    def wpl(self, value: float) -> None:
        """Set the wpl property."""
        self._cards[1].set_value("wpl", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Compression compliance constant. If this parameter is greater than zero, then PR is ignored.
        """ # nopep8
        return self._cards[1].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[1].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Compression compliance constant.
        """ # nopep8
        return self._cards[1].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[1].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Compression compliance constant.
        """ # nopep8
        return self._cards[1].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[1].set_value("d3", value)

    @property
    def sgl(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length.
        """ # nopep8
        return self._cards[2].get_value("sgl")

    @sgl.setter
    def sgl(self, value: float) -> None:
        """Set the sgl property."""
        self._cards[2].set_value("sgl", value)

    @property
    def sw(self) -> typing.Optional[float]:
        """Get or set the Specimen width.
        """ # nopep8
        return self._cards[2].get_value("sw")

    @sw.setter
    def sw(self, value: float) -> None:
        """Set the sw property."""
        self._cards[2].set_value("sw", value)

    @property
    def st(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness.
        """ # nopep8
        return self._cards[2].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        """Set the st property."""
        self._cards[2].set_value("st", value)

    @property
    def lcid1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the force as a function of actual change in the gauge length. If SGL, SW, and ST are set to unity (1.0), curve LCID1 is also engineering stress as a function of engineering strain. Curve should have both negative (compressive) and positive (tensile) values.
        """ # nopep8
        return self._cards[2].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        """Set the lcid1 property."""
        self._cards[2].set_value("lcid1", value)

    @property
    def data(self) -> typing.Optional[float]:
        """Get or set the Type of experimental data (only active if LCBI, LCPL, WBI, and WPL are all zero on Card 2 or Card 2 is not activated):
        """ # nopep8
        return self._cards[2].get_value("data")

    @data.setter
    def data(self, value: float) -> None:
        """Set the data property."""
        self._cards[2].set_value("data", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of the deviatoric stress relaxation curve, neglecting the long term deviatoric stress. If LCID2 is specified, constants G_i and beta, are determined internally using a least squares fit. The ordinate of the curve is the viscoelastic deviatoric stress divided by 2 times the constant value of deviatoric strain where the stress and strain are in the direction of the prescribed strain, or in non-directional terms, the effective stress divided by 3 times the effective strain.
        """ # nopep8
        return self._cards[2].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        """Set the lcid2 property."""
        self._cards[2].set_value("lcid2", value)

    @property
    def bstart(self) -> typing.Optional[float]:
        """Get or set the In the fit, beta_1 is set to zero, beta_2 is set to BSTART, beta_3 is 10 times beta_2, beta_4 is 100 times grater than beta_3, and so on. If zero, BSTART is determined by an iterative trial and error scheme.
        """ # nopep8
        return self._cards[2].get_value("bstart")

    @bstart.setter
    def bstart(self, value: float) -> None:
        """Set the bstart property."""
        self._cards[2].set_value("bstart", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for loading.
        """ # nopep8
        return self._cards[2].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        """Set the tramp property."""
        self._cards[2].set_value("tramp", value)

    @property
    def c10(self) -> typing.Optional[float]:
        """Get or set the C10
        """ # nopep8
        return self._cards[3].get_value("c10")

    @c10.setter
    def c10(self, value: float) -> None:
        """Set the c10 property."""
        self._cards[3].set_value("c10", value)

    @property
    def c01(self) -> typing.Optional[float]:
        """Get or set the C01
        """ # nopep8
        return self._cards[3].get_value("c01")

    @c01.setter
    def c01(self, value: float) -> None:
        """Set the c01 property."""
        self._cards[3].set_value("c01", value)

    @property
    def c11(self) -> typing.Optional[float]:
        """Get or set the C11
        """ # nopep8
        return self._cards[3].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        """Set the c11 property."""
        self._cards[3].set_value("c11", value)

    @property
    def c20(self) -> typing.Optional[float]:
        """Get or set the C20
        """ # nopep8
        return self._cards[3].get_value("c20")

    @c20.setter
    def c20(self, value: float) -> None:
        """Set the c20 property."""
        self._cards[3].set_value("c20", value)

    @property
    def c02(self) -> typing.Optional[float]:
        """Get or set the C02
        """ # nopep8
        return self._cards[3].get_value("c02")

    @c02.setter
    def c02(self, value: float) -> None:
        """Set the c02 property."""
        self._cards[3].set_value("c02", value)

    @property
    def c30(self) -> typing.Optional[float]:
        """Get or set the C30
        """ # nopep8
        return self._cards[3].get_value("c30")

    @c30.setter
    def c30(self, value: float) -> None:
        """Set the c30 property."""
        self._cards[3].set_value("c30", value)

    @property
    def therml(self) -> typing.Optional[float]:
        """Get or set the Flag for the thermal option. If THERML>0.0, then G, SIGF, C10 and C01 specify curve IDs giving the values as functions of temperature, otherwise they specify the constants.
        """ # nopep8
        return self._cards[3].get_value("therml")

    @therml.setter
    def therml(self, value: float) -> None:
        """Set the therml property."""
        self._cards[3].set_value("therml", value)

    @property
    def constants(self) -> pd.DataFrame:
        """Get the table of constants."""
        return self._cards[4].table

    @constants.setter
    def constants(self, df: pd.DataFrame):
        """Set constants from the dataframe df"""
        self._cards[4].table = df

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
    def lcid1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid1:
                return kwd
        return None

    @lcid1_link.setter
    def lcid1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid1."""
        self.lcid1 = value.lcid

    @property
    def lcid2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid2:
                return kwd
        return None

    @lcid2_link.setter
    def lcid2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid2."""
        self.lcid2 = value.lcid


class MatHyperelasticRubber(Mat077H):
    """Alias for MAT keyword."""
    subkeyword = "HYPERELASTIC_RUBBER"
