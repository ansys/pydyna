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

"""Module providing the MatQuasilinearViscoelastic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATQUASILINEARVISCOELASTIC_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("k", float, 20, 10, None),
    FieldSchema("lc1", int, 30, 10, 0),
    FieldSchema("lc2", int, 40, 10, 0),
    FieldSchema("n", float, 50, 10, 6.0),
    FieldSchema("gstart", float, 60, 10, None),
    FieldSchema("m", float, 70, 10, 6.0),
)

_MATQUASILINEARVISCOELASTIC_CARD1 = (
    FieldSchema("so", float, 0, 10, 0.0),
    FieldSchema("e_min", float, 10, 10, -0.9),
    FieldSchema("e_max", float, 20, 10, 5.1),
    FieldSchema("gama1", float, 30, 10, None),
    FieldSchema("gama2", float, 40, 10, None),
    FieldSchema("k", float, 50, 10, None),
    FieldSchema("eh", float, 60, 10, None),
    FieldSchema("form", int, 70, 10, 0),
)

_MATQUASILINEARVISCOELASTIC_CARD2 = (
    FieldSchema("g1", float, 0, 10, None),
    FieldSchema("beta1", float, 10, 10, None),
    FieldSchema("g2", float, 20, 10, None),
    FieldSchema("beta2", float, 30, 10, None),
    FieldSchema("g3", float, 40, 10, None),
    FieldSchema("beta3", float, 50, 10, None),
    FieldSchema("g4", float, 60, 10, None),
    FieldSchema("beta4", float, 70, 10, None),
)

_MATQUASILINEARVISCOELASTIC_CARD3 = (
    FieldSchema("g5", float, 0, 10, None),
    FieldSchema("beta5", float, 10, 10, None),
    FieldSchema("g6", float, 20, 10, None),
    FieldSchema("beta6", float, 30, 10, None),
    FieldSchema("g7", float, 40, 10, None),
    FieldSchema("beta7", float, 50, 10, None),
    FieldSchema("g8", float, 60, 10, None),
    FieldSchema("beta8", float, 70, 10, None),
)

_MATQUASILINEARVISCOELASTIC_CARD4 = (
    FieldSchema("g9", float, 0, 10, None),
    FieldSchema("beta9", float, 10, 10, None),
    FieldSchema("g10", float, 20, 10, None),
    FieldSchema("beta10", float, 30, 10, None),
    FieldSchema("g11", float, 40, 10, None),
    FieldSchema("beta11", float, 50, 10, None),
    FieldSchema("g12", float, 60, 10, None),
    FieldSchema("beta12", float, 70, 10, None),
)

_MATQUASILINEARVISCOELASTIC_CARD5 = (
    FieldSchema("c1", float, 0, 10, None),
    FieldSchema("c2", float, 10, 10, None),
    FieldSchema("c3", float, 20, 10, None),
    FieldSchema("c4", float, 30, 10, None),
    FieldSchema("c5", float, 40, 10, None),
    FieldSchema("c6", float, 50, 10, None),
)

_MATQUASILINEARVISCOELASTIC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatQuasilinearViscoelastic(KeywordBase):
    """DYNA MAT_QUASILINEAR_VISCOELASTIC keyword"""

    keyword = "MAT"
    subkeyword = "QUASILINEAR_VISCOELASTIC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatQuasilinearViscoelastic class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATQUASILINEARVISCOELASTIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATQUASILINEARVISCOELASTIC_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATQUASILINEARVISCOELASTIC_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATQUASILINEARVISCOELASTIC_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATQUASILINEARVISCOELASTIC_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATQUASILINEARVISCOELASTIC_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatQuasilinearViscoelastic.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATQUASILINEARVISCOELASTIC_OPTION0_CARD0,
                        **kwargs,
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
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def lc1(self) -> int:
        """Get or set the Load curve ID that defines the relaxation function in shear. This curve is used to fit the coefficients Gi and BETAi. If zero, define the coefficients directly. The latter is recommended.
        """ # nopep8
        return self._cards[0].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[0].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID that defines the instantaneous elastic response in shear. This curve is used to fit the coefficients Ci. If zero, define the coefficients directly. The latter is recommended.
        """ # nopep8
        return self._cards[0].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[0].set_value("lc2", value)

    @property
    def n(self) -> float:
        """Get or set the Number of terms used in the Prony series, a number less than or equal to 12. This number should be equal to the number of decades of time covered by the experimental data. Define this number is LC1 is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def gstart(self) -> typing.Optional[float]:
        """Get or set the Starting value for the least square fit. If zero, a default value is set equal to the inverse of the largest time in the experiment. Define this number if LC1 is nonzero.
        """ # nopep8
        return self._cards[0].get_value("gstart")

    @gstart.setter
    def gstart(self, value: float) -> None:
        """Set the gstart property."""
        self._cards[0].set_value("gstart", value)

    @property
    def m(self) -> float:
        """Get or set the Number of terms used to determine the instantaneous elastic response. Define this number if LC2 is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[0].set_value("m", value)

    @property
    def so(self) -> float:
        """Get or set the Strain (logarithmic) output option to be plotted as component 7 in LS-TAURUS (D3PLOT file) which is the effective plastic strain component. The maximum values are updated for each element each time step:
        EQ.0.0: maximum principal strain that occurs during the calculation,
        EQ.1.0: maximum magnitude of the principal strain values that occurs during the calculation,
        EQ.2.0: maximum effective strain that occurs during the calculation.
        """ # nopep8
        return self._cards[1].get_value("so")

    @so.setter
    def so(self, value: float) -> None:
        """Set the so property."""
        if value not in [0.0, 1.0, 2.0, None]:
            raise Exception("""so must be `None` or one of {0.0,1.0,2.0}.""")
        self._cards[1].set_value("so", value)

    @property
    def e_min(self) -> float:
        """Get or set the Minimum strain used to generate the load curve from Ci. The default range is -0.9 to 5.1. The computed solution will be more accurate if the user specifies the range used to fit the Ci. Linear extrapolation is used outside the specified range
        """ # nopep8
        return self._cards[1].get_value("e_min")

    @e_min.setter
    def e_min(self, value: float) -> None:
        """Set the e_min property."""
        self._cards[1].set_value("e_min", value)

    @property
    def e_max(self) -> float:
        """Get or set the Maximum strain used to generate the load curve from Ci.
        """ # nopep8
        return self._cards[1].get_value("e_max")

    @e_max.setter
    def e_max(self, value: float) -> None:
        """Set the e_max property."""
        self._cards[1].set_value("e_max", value)

    @property
    def gama1(self) -> typing.Optional[float]:
        """Get or set the Material failure parameter, see *MAT_SIMPLIFIED_RUBBER and Figure 23.181.1.
        """ # nopep8
        return self._cards[1].get_value("gama1")

    @gama1.setter
    def gama1(self, value: float) -> None:
        """Set the gama1 property."""
        self._cards[1].set_value("gama1", value)

    @property
    def gama2(self) -> typing.Optional[float]:
        """Get or set the Material failure parameter, see *MAT_SIMPLIFIED_RUBBER
        """ # nopep8
        return self._cards[1].get_value("gama2")

    @gama2.setter
    def gama2(self, value: float) -> None:
        """Set the gama2 property."""
        self._cards[1].set_value("gama2", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Material failure parameter that controls the volume enclosed by the failure surface, see *MAT_SIMPLIFIED_RUBBER.
        LE.0.0: ignore failure criterion;
        GT.0.0: use actual K value for failure criterions.
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def eh(self) -> typing.Optional[float]:
        """Get or set the Damage parameter, see *MAT_SIMPLIFIED_RUBBER
        """ # nopep8
        return self._cards[1].get_value("eh")

    @eh.setter
    def eh(self, value: float) -> None:
        """Set the eh property."""
        self._cards[1].set_value("eh", value)

    @property
    def form(self) -> int:
        """Get or set the Formulation of model. FORM=0 gives the original model developed by Fung, which always relaxes to a zero stress state as time approaches infinity, and FORM=1 gives the alternative model, which relaxes to the quasi-static elastic response. In general, the two formulations won't give the same responses.  Formulation, FORM=-1, is an improvement on FORM=0 where the instantaneous elastic response is used in the viscoelastic stress update, not just in the relaxation, as in FORM=0.  Consequently, the constants for the elastic response do not need to be scaled
        """ # nopep8
        return self._cards[1].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        """Set the form property."""
        self._cards[1].set_value("form", value)

    @property
    def g1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[2].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        """Set the g1 property."""
        self._cards[2].set_value("g1", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[2].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        """Set the beta1 property."""
        self._cards[2].set_value("beta1", value)

    @property
    def g2(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[2].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        """Set the g2 property."""
        self._cards[2].set_value("g2", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[2].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        """Set the beta2 property."""
        self._cards[2].set_value("beta2", value)

    @property
    def g3(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[2].get_value("g3")

    @g3.setter
    def g3(self, value: float) -> None:
        """Set the g3 property."""
        self._cards[2].set_value("g3", value)

    @property
    def beta3(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[2].get_value("beta3")

    @beta3.setter
    def beta3(self, value: float) -> None:
        """Set the beta3 property."""
        self._cards[2].set_value("beta3", value)

    @property
    def g4(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[2].get_value("g4")

    @g4.setter
    def g4(self, value: float) -> None:
        """Set the g4 property."""
        self._cards[2].set_value("g4", value)

    @property
    def beta4(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[2].get_value("beta4")

    @beta4.setter
    def beta4(self, value: float) -> None:
        """Set the beta4 property."""
        self._cards[2].set_value("beta4", value)

    @property
    def g5(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[3].get_value("g5")

    @g5.setter
    def g5(self, value: float) -> None:
        """Set the g5 property."""
        self._cards[3].set_value("g5", value)

    @property
    def beta5(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[3].get_value("beta5")

    @beta5.setter
    def beta5(self, value: float) -> None:
        """Set the beta5 property."""
        self._cards[3].set_value("beta5", value)

    @property
    def g6(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[3].get_value("g6")

    @g6.setter
    def g6(self, value: float) -> None:
        """Set the g6 property."""
        self._cards[3].set_value("g6", value)

    @property
    def beta6(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[3].get_value("beta6")

    @beta6.setter
    def beta6(self, value: float) -> None:
        """Set the beta6 property."""
        self._cards[3].set_value("beta6", value)

    @property
    def g7(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[3].get_value("g7")

    @g7.setter
    def g7(self, value: float) -> None:
        """Set the g7 property."""
        self._cards[3].set_value("g7", value)

    @property
    def beta7(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[3].get_value("beta7")

    @beta7.setter
    def beta7(self, value: float) -> None:
        """Set the beta7 property."""
        self._cards[3].set_value("beta7", value)

    @property
    def g8(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[3].get_value("g8")

    @g8.setter
    def g8(self, value: float) -> None:
        """Set the g8 property."""
        self._cards[3].set_value("g8", value)

    @property
    def beta8(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[3].get_value("beta8")

    @beta8.setter
    def beta8(self, value: float) -> None:
        """Set the beta8 property."""
        self._cards[3].set_value("beta8", value)

    @property
    def g9(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[4].get_value("g9")

    @g9.setter
    def g9(self, value: float) -> None:
        """Set the g9 property."""
        self._cards[4].set_value("g9", value)

    @property
    def beta9(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[4].get_value("beta9")

    @beta9.setter
    def beta9(self, value: float) -> None:
        """Set the beta9 property."""
        self._cards[4].set_value("beta9", value)

    @property
    def g10(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[4].get_value("g10")

    @g10.setter
    def g10(self, value: float) -> None:
        """Set the g10 property."""
        self._cards[4].set_value("g10", value)

    @property
    def beta10(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[4].get_value("beta10")

    @beta10.setter
    def beta10(self, value: float) -> None:
        """Set the beta10 property."""
        self._cards[4].set_value("beta10", value)

    @property
    def g11(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[4].get_value("g11")

    @g11.setter
    def g11(self, value: float) -> None:
        """Set the g11 property."""
        self._cards[4].set_value("g11", value)

    @property
    def beta11(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[4].get_value("beta11")

    @beta11.setter
    def beta11(self, value: float) -> None:
        """Set the beta11 property."""
        self._cards[4].set_value("beta11", value)

    @property
    def g12(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[4].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        """Set the g12 property."""
        self._cards[4].set_value("g12", value)

    @property
    def beta12(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[4].get_value("beta12")

    @beta12.setter
    def beta12(self, value: float) -> None:
        """Set the beta12 property."""
        self._cards[4].set_value("beta12", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[5].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[5].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[5].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        """Set the c4 property."""
        self._cards[5].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        """Set the c5 property."""
        self._cards[5].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        """Set the c6 property."""
        self._cards[5].set_value("c6", value)

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

