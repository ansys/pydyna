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

"""Module providing the Mat077O class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT077O_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("pr", float, 20, 10, None),
    FieldSchema("n", int, 30, 10, 0),
    FieldSchema("nv", int, 40, 10, 6),
    FieldSchema("g", float, 50, 10, None),
    FieldSchema("sigf", float, 60, 10, None),
    FieldSchema("ref", float, 70, 10, 0.0),
)

_MAT077O_CARD1 = (
    FieldSchema("tbhys", float, 0, 10, None),
    FieldSchema("lcbi", float, 10, 10, None),
    FieldSchema("lcpl", float, 20, 10, None),
    FieldSchema("wbi", float, 30, 10, None),
    FieldSchema("wpl", float, 40, 10, None),
    FieldSchema("d1", float, 50, 10, None),
    FieldSchema("d2", float, 60, 10, None),
    FieldSchema("d3", float, 70, 10, None),
)

_MAT077O_CARD2 = (
    FieldSchema("sgl", float, 0, 10, None),
    FieldSchema("sw", float, 10, 10, None),
    FieldSchema("st", float, 20, 10, None),
    FieldSchema("lcid1", int, 30, 10, None),
    FieldSchema("data", float, 40, 10, 1.0),
    FieldSchema("lcid2", int, 50, 10, None),
    FieldSchema("bstart", float, 60, 10, None),
    FieldSchema("tramp", float, 70, 10, None),
)

_MAT077O_CARD3 = (
    FieldSchema("mu1", float, 0, 10, None),
    FieldSchema("mu2", float, 10, 10, None),
    FieldSchema("mu3", float, 20, 10, None),
    FieldSchema("mu4", float, 30, 10, None),
    FieldSchema("mu5", float, 40, 10, None),
    FieldSchema("mu6", float, 50, 10, None),
    FieldSchema("mu7", float, 60, 10, None),
    FieldSchema("mu8", float, 70, 10, None),
)

_MAT077O_CARD4 = (
    FieldSchema("alpha1", float, 0, 10, None),
    FieldSchema("alpha2", float, 10, 10, None),
    FieldSchema("alpha3", float, 20, 10, None),
    FieldSchema("alpha4", float, 30, 10, None),
    FieldSchema("alpha5", float, 40, 10, None),
    FieldSchema("alpha6", float, 50, 10, None),
    FieldSchema("alpha7", float, 60, 10, None),
    FieldSchema("alpha8", float, 70, 10, None),
)

_MAT077O_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat077O(KeywordBase):
    """DYNA MAT_077_O keyword"""

    keyword = "MAT"
    subkeyword = "077_O"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcid1": LinkType.DEFINE_CURVE,
        "lcid2": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat077O class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT077O_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT077O_CARD1,
                active_func=lambda: self.pr and self.pr < 0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT077O_CARD2,
                active_func=lambda: self.n > 0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT077O_CARD3,
                active_func=lambda: self.n == 0 or self.n == -1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT077O_CARD4,
                active_func=lambda: self.n == 0 or self.n == -1,
                **kwargs,
            ),
            TableCard(
                [
                    Field("gi", float, 0, 10, None),
                    Field("betai", float, 10, 10, None),
                    Field("vflag", int, 20, 10, None),
                ],
                None,
                name="constants",
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat077O._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT077O_OPTION0_CARD0,
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
        """Get or set the Poissons ratio. If set to a negative number, the Poissons ratio is the absolute value, and Card 2 is included for extra parameters.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def n(self) -> int:
        """Get or set the Order of fit to curve LCID1 or combinations of LCID1, LCBI, and LCPL for the Ogden model (currently < 9, 2 generally works okay). LS-DYNA prints the constants generated during the fit to d3hsp. To save the cost of performing the nonlinear fit in future runs, directly input the constants from this fit. You can visually evaluate the goodness of the fit by plotting data in the output file curveplot*. To do this with LS-PrePost, click  XYplot  Add to read the curveplot* file.
        EQ.0: Allows you to specify the material parameters directly with Cards 3b.1 and 3b.2
        EQ. - 1: Same as N = 0 but invokes a thermal option: parameters MUi and ALPHAi are read as load curves IDs and thereby define these parameters as functions of temperature.It is available only for solid elements.VFLAG must be 0.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        """Set the n property."""
        if value not in [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""n must be `None` or one of {-1,0,1,2,3,4,5,6,7,8}.""")
        self._cards[0].set_value("n", value)

    @property
    def nv(self) -> int:
        """Get or set the Number of Prony series terms for fitting curve LCID2. If zero, the default is 6. Currently, 12 is the maximum number. We recommend values less than 12, possibly 3  5, since each term used adds significantly to the cost. Exercise caution when taking the results from the fit. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once you have achieved a satisfactory fit, we recommend inputting the coefficients written into the output file for future runs.
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
        """Get or set the Use reference geometry to initialize the stress tensor. *INITIAL_FOAM_REFERENCE_GEOMETRY  defines the reference geometry.
        EQ.0.0: Off
        EQ.1.0: On
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
        """Get or set the Table ID for hysteresis, could be positive or negative; see Remarks in the manual page for *MAT_HYPERELASTIC_RUBBER. This filed only applies to solid elements.
        """ # nopep8
        return self._cards[1].get_value("tbhys")

    @tbhys.setter
    def tbhys(self, value: float) -> None:
        """Set the tbhys property."""
        self._cards[1].set_value("tbhys", value)

    @property
    def lcbi(self) -> typing.Optional[float]:
        """Get or set the Load curve ID giving force as a function of displacement for the biaxial test used in parameter fitting. Make sure N > 0 on Card 1 if setting this parameter. See Remark in the manual page for *MAT_HYPERELASTIC_RUBBER.
        """ # nopep8
        return self._cards[1].get_value("lcbi")

    @lcbi.setter
    def lcbi(self, value: float) -> None:
        """Set the lcbi property."""
        self._cards[1].set_value("lcbi", value)

    @property
    def lcpl(self) -> typing.Optional[float]:
        """Get or set the Load curve ID giving force as a function of displacement for the planar test used in parameter fitting. Make sure N > 0 on Card 1 if setting this parameter. See Remark in the manual page for *MAT_HYPERELASTIC_RUBBER.
        """ # nopep8
        return self._cards[1].get_value("lcpl")

    @lcpl.setter
    def lcpl(self, value: float) -> None:
        """Set the lcpl property."""
        self._cards[1].set_value("lcpl", value)

    @property
    def wbi(self) -> typing.Optional[float]:
        """Get or set the Weight factor giving the relative influence of the biaxial test data in the fitting of material parameters, a value of 1.0 means that it is of equal importance as the uniaxial test data. Make sure N > 0 on Card 1 if setting this parameter. See in the manual page for *MAT_HYPERELASTIC_RUBBER.
        """ # nopep8
        return self._cards[1].get_value("wbi")

    @wbi.setter
    def wbi(self, value: float) -> None:
        """Set the wbi property."""
        self._cards[1].set_value("wbi", value)

    @property
    def wpl(self) -> typing.Optional[float]:
        """Get or set the Weight factor giving the relative influence of the planar test data in the fitting of material parameters, a value of 1.0 means that it is of equal importance as the uniaxial test data. Make sure N > 0 on Card 1 if setting this parameter. See in the manual page for *MAT_HYPERELASTIC_RUBBER.
        """ # nopep8
        return self._cards[1].get_value("wpl")

    @wpl.setter
    def wpl(self, value: float) -> None:
        """Set the wpl property."""
        self._cards[1].set_value("wpl", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Compression compliance constant. If this parameter is greater than zero, then LS-DYNA does not use the value of PR set on Card 1 for Poissons ratio.
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
        """Get or set the Load curve ID giving the force versus actual change in the gauge length.
        """ # nopep8
        return self._cards[2].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        """Set the lcid1 property."""
        self._cards[2].set_value("lcid1", value)

    @property
    def data(self) -> float:
        """Get or set the Type of experimental data (only active if LCBI, LCPL, WBI, and WPL are all zero on Card 2 or Card 2 is not activated):
        EQ.1.0: uniaxial data (default),
        EQ.2.0: biaxial data.
        EQ.3.0: pure shear data
        """ # nopep8
        return self._cards[2].get_value("data")

    @data.setter
    def data(self, value: float) -> None:
        """Set the data property."""
        if value not in [1.0, 2.0, 3.0, None]:
            raise Exception("""data must be `None` or one of {1.0,2.0,3.0}.""")
        self._cards[2].set_value("data", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of relaxation curve If constants beta-i are determined via a least squares fit.
        This model ignores the constant stress.
        """ # nopep8
        return self._cards[2].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        """Set the lcid2 property."""
        self._cards[2].set_value("lcid2", value)

    @property
    def bstart(self) -> typing.Optional[float]:
        """Get or set the In the fit, beta-1 is set to zero, beta-2 is set to BSTART, beta-3 is 10 times beta-2, beta-4 is 100 times greater than beta-3, and so on. If zero, BSTART is determined by an iterative trial and error scheme.
        """ # nopep8
        return self._cards[2].get_value("bstart")

    @bstart.setter
    def bstart(self, value: float) -> None:
        """Set the bstart property."""
        self._cards[2].set_value("bstart", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for loading. If N=0, the constants MUi and ALPHAi have to be defined:
        """ # nopep8
        return self._cards[2].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        """Set the tramp property."""
        self._cards[2].set_value("tramp", value)

    @property
    def mu1(self) -> typing.Optional[float]:
        """Get or set the mu-1, first shear modulus(N=0).
        For N = -1, load curve ids can be defined to specify shear moduli as functions of temperature, i.e., nu_i (T). If individual curve ids are zero, then the corresponding shear modulus is constantly zero.
        """ # nopep8
        return self._cards[3].get_value("mu1")

    @mu1.setter
    def mu1(self, value: float) -> None:
        """Set the mu1 property."""
        self._cards[3].set_value("mu1", value)

    @property
    def mu2(self) -> typing.Optional[float]:
        """Get or set the mu-2, second shear modulus(N=0).
        For N = -1, load curve ids can be defined to specify shear moduli as functions of temperature, i.e., nu_i (T). If individual curve ids are zero, then the corresponding shear modulus is constantly zero.
        """ # nopep8
        return self._cards[3].get_value("mu2")

    @mu2.setter
    def mu2(self, value: float) -> None:
        """Set the mu2 property."""
        self._cards[3].set_value("mu2", value)

    @property
    def mu3(self) -> typing.Optional[float]:
        """Get or set the mu-3, third shear modulus(N=0).
        For N = -1, load curve ids can be defined to specify shear moduli as functions of temperature, i.e., nu_i (T). If individual curve ids are zero, then the corresponding shear modulus is constantly zero.
        """ # nopep8
        return self._cards[3].get_value("mu3")

    @mu3.setter
    def mu3(self, value: float) -> None:
        """Set the mu3 property."""
        self._cards[3].set_value("mu3", value)

    @property
    def mu4(self) -> typing.Optional[float]:
        """Get or set the mu-4, fourth shear modulus(N=0).
        For N = -1, load curve ids can be defined to specify shear moduli as functions of temperature, i.e., nu_i (T). If individual curve ids are zero, then the corresponding shear modulus is constantly zero.
        """ # nopep8
        return self._cards[3].get_value("mu4")

    @mu4.setter
    def mu4(self, value: float) -> None:
        """Set the mu4 property."""
        self._cards[3].set_value("mu4", value)

    @property
    def mu5(self) -> typing.Optional[float]:
        """Get or set the mu-5, fifth shear modulus(N=0).
        For N = -1, load curve ids can be defined to specify shear moduli as functions of temperature, i.e., nu_i (T). If individual curve ids are zero, then the corresponding shear modulus is constantly zero.
        """ # nopep8
        return self._cards[3].get_value("mu5")

    @mu5.setter
    def mu5(self, value: float) -> None:
        """Set the mu5 property."""
        self._cards[3].set_value("mu5", value)

    @property
    def mu6(self) -> typing.Optional[float]:
        """Get or set the mu-6, sixth shear modulus(N=0).
        For N = -1, load curve ids can be defined to specify shear moduli as functions of temperature, i.e., nu_i (T). If individual curve ids are zero, then the corresponding shear modulus is constantly zero.
        """ # nopep8
        return self._cards[3].get_value("mu6")

    @mu6.setter
    def mu6(self, value: float) -> None:
        """Set the mu6 property."""
        self._cards[3].set_value("mu6", value)

    @property
    def mu7(self) -> typing.Optional[float]:
        """Get or set the mu-7, seventh shear modulus(N=0).
        For N = -1, load curve ids can be defined to specify shear moduli as functions of temperature, i.e., nu_i (T). If individual curve ids are zero, then the corresponding shear modulus is constantly zero.
        """ # nopep8
        return self._cards[3].get_value("mu7")

    @mu7.setter
    def mu7(self, value: float) -> None:
        """Set the mu7 property."""
        self._cards[3].set_value("mu7", value)

    @property
    def mu8(self) -> typing.Optional[float]:
        """Get or set the mu-8, eighth shear modulus(N=0).
        For N = -1, load curve ids can be defined to specify shear moduli as functions of temperature, i.e., nu_i (T). If individual curve ids are zero, then the corresponding shear modulus is constantly zero.
        """ # nopep8
        return self._cards[3].get_value("mu8")

    @mu8.setter
    def mu8(self, value: float) -> None:
        """Set the mu8 property."""
        self._cards[3].set_value("mu8", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the alpha-1, first exponent(N=0)..
        For N = -1, load curve ids can be defined to specify exponents as functions of temperature, i.e., alpha_i (T). If individual curve ids are zero, then the corresponding exponent is constantly zero.
        """ # nopep8
        return self._cards[4].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[4].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the alpha-2, second exponent(N=0)..
        For N = -1, load curve ids can be defined to specify exponents as functions of temperature, i.e., alpha_i (T). If individual curve ids are zero, then the corresponding exponent is constantly zero.
        """ # nopep8
        return self._cards[4].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[4].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the alpha-3, third exponent(N=0)..
        For N = -1, load curve ids can be defined to specify exponents as functions of temperature, i.e., alpha_i (T). If individual curve ids are zero, then the corresponding exponent is constantly zero.
        """ # nopep8
        return self._cards[4].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[4].set_value("alpha3", value)

    @property
    def alpha4(self) -> typing.Optional[float]:
        """Get or set the alpha-4, fourth exponent(N=0)..
        For N = -1, load curve ids can be defined to specify exponents as functions of temperature, i.e., alpha_i (T). If individual curve ids are zero, then the corresponding exponent is constantly zero.
        """ # nopep8
        return self._cards[4].get_value("alpha4")

    @alpha4.setter
    def alpha4(self, value: float) -> None:
        """Set the alpha4 property."""
        self._cards[4].set_value("alpha4", value)

    @property
    def alpha5(self) -> typing.Optional[float]:
        """Get or set the alpha-5, fifth exponent(N=0)..
        For N = -1, load curve ids can be defined to specify exponents as functions of temperature, i.e., alpha_i (T). If individual curve ids are zero, then the corresponding exponent is constantly zero.
        """ # nopep8
        return self._cards[4].get_value("alpha5")

    @alpha5.setter
    def alpha5(self, value: float) -> None:
        """Set the alpha5 property."""
        self._cards[4].set_value("alpha5", value)

    @property
    def alpha6(self) -> typing.Optional[float]:
        """Get or set the alpha-6, sixth exponent(N=0)..
        For N = -1, load curve ids can be defined to specify exponents as functions of temperature, i.e., alpha_i (T). If individual curve ids are zero, then the corresponding exponent is constantly zero.
        """ # nopep8
        return self._cards[4].get_value("alpha6")

    @alpha6.setter
    def alpha6(self, value: float) -> None:
        """Set the alpha6 property."""
        self._cards[4].set_value("alpha6", value)

    @property
    def alpha7(self) -> typing.Optional[float]:
        """Get or set the alpha-7, seventh exponent(N=0)..
        For N = -1, load curve ids can be defined to specify exponents as functions of temperature, i.e., alpha_i (T). If individual curve ids are zero, then the corresponding exponent is constantly zero.
        """ # nopep8
        return self._cards[4].get_value("alpha7")

    @alpha7.setter
    def alpha7(self, value: float) -> None:
        """Set the alpha7 property."""
        self._cards[4].set_value("alpha7", value)

    @property
    def alpha8(self) -> typing.Optional[float]:
        """Get or set the alpha-8, eighth exponent(N=0)..
        For N = -1, load curve ids can be defined to specify exponents as functions of temperature, i.e., alpha_i (T). If individual curve ids are zero, then the corresponding exponent is constantly zero.
        """ # nopep8
        return self._cards[4].get_value("alpha8")

    @alpha8.setter
    def alpha8(self, value: float) -> None:
        """Set the alpha8 property."""
        self._cards[4].set_value("alpha8", value)

    @property
    def constants(self) -> pd.DataFrame:
        """Get the table of constants."""
        return self._cards[5].table

    @constants.setter
    def constants(self, df: pd.DataFrame):
        """Set constants from the dataframe df"""
        self._cards[5].table = df

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


class MatOgdenRubber(Mat077O):
    """Alias for MAT keyword."""
    subkeyword = "OGDEN_RUBBER"
