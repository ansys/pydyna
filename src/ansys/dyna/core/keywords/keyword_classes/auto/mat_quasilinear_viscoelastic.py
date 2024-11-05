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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatQuasilinearViscoelastic(KeywordBase):
    """DYNA MAT_QUASILINEAR_VISCOELASTIC keyword"""

    keyword = "MAT"
    subkeyword = "QUASILINEAR_VISCOELASTIC"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "k",
                        float,
                        20,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "lc1",
                        int,
                        30,
                        10,
                        kwargs.get("lc1", 0)
                    ),
                    Field(
                        "lc2",
                        int,
                        40,
                        10,
                        kwargs.get("lc2", 0)
                    ),
                    Field(
                        "n",
                        float,
                        50,
                        10,
                        kwargs.get("n", 6)
                    ),
                    Field(
                        "gstart",
                        float,
                        60,
                        10,
                        kwargs.get("gstart")
                    ),
                    Field(
                        "m",
                        float,
                        70,
                        10,
                        kwargs.get("m", 6)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "so",
                        float,
                        0,
                        10,
                        kwargs.get("so", 0.0)
                    ),
                    Field(
                        "e_min",
                        float,
                        10,
                        10,
                        kwargs.get("e_min", -0.9)
                    ),
                    Field(
                        "e_max",
                        float,
                        20,
                        10,
                        kwargs.get("e_max", 5.1)
                    ),
                    Field(
                        "gama1",
                        float,
                        30,
                        10,
                        kwargs.get("gama1")
                    ),
                    Field(
                        "gama2",
                        float,
                        40,
                        10,
                        kwargs.get("gama2")
                    ),
                    Field(
                        "k",
                        float,
                        50,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "eh",
                        float,
                        60,
                        10,
                        kwargs.get("eh")
                    ),
                    Field(
                        "form",
                        int,
                        70,
                        10,
                        kwargs.get("form", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g1",
                        float,
                        0,
                        10,
                        kwargs.get("g1")
                    ),
                    Field(
                        "beta1",
                        float,
                        10,
                        10,
                        kwargs.get("beta1")
                    ),
                    Field(
                        "g2",
                        float,
                        20,
                        10,
                        kwargs.get("g2")
                    ),
                    Field(
                        "beta2",
                        float,
                        30,
                        10,
                        kwargs.get("beta2")
                    ),
                    Field(
                        "g3",
                        float,
                        40,
                        10,
                        kwargs.get("g3")
                    ),
                    Field(
                        "beta3",
                        float,
                        50,
                        10,
                        kwargs.get("beta3")
                    ),
                    Field(
                        "g4",
                        float,
                        60,
                        10,
                        kwargs.get("g4")
                    ),
                    Field(
                        "beta4",
                        float,
                        70,
                        10,
                        kwargs.get("beta4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g5",
                        float,
                        0,
                        10,
                        kwargs.get("g5")
                    ),
                    Field(
                        "beta5",
                        float,
                        10,
                        10,
                        kwargs.get("beta5")
                    ),
                    Field(
                        "g6",
                        float,
                        20,
                        10,
                        kwargs.get("g6")
                    ),
                    Field(
                        "beta6",
                        float,
                        30,
                        10,
                        kwargs.get("beta6")
                    ),
                    Field(
                        "g7",
                        float,
                        40,
                        10,
                        kwargs.get("g7")
                    ),
                    Field(
                        "beta7",
                        float,
                        50,
                        10,
                        kwargs.get("beta7")
                    ),
                    Field(
                        "g8",
                        float,
                        60,
                        10,
                        kwargs.get("g8")
                    ),
                    Field(
                        "beta8",
                        float,
                        70,
                        10,
                        kwargs.get("beta8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g9",
                        float,
                        0,
                        10,
                        kwargs.get("g9")
                    ),
                    Field(
                        "beta9",
                        float,
                        10,
                        10,
                        kwargs.get("beta9")
                    ),
                    Field(
                        "g10",
                        float,
                        20,
                        10,
                        kwargs.get("g10")
                    ),
                    Field(
                        "beta10",
                        float,
                        30,
                        10,
                        kwargs.get("beta10")
                    ),
                    Field(
                        "g11",
                        float,
                        40,
                        10,
                        kwargs.get("g11")
                    ),
                    Field(
                        "beta11",
                        float,
                        50,
                        10,
                        kwargs.get("beta11")
                    ),
                    Field(
                        "g12",
                        float,
                        60,
                        10,
                        kwargs.get("g12")
                    ),
                    Field(
                        "beta12",
                        float,
                        70,
                        10,
                        kwargs.get("beta12")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c1",
                        float,
                        0,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        10,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        20,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        float,
                        30,
                        10,
                        kwargs.get("c4")
                    ),
                    Field(
                        "c5",
                        float,
                        40,
                        10,
                        kwargs.get("c5")
                    ),
                    Field(
                        "c6",
                        float,
                        50,
                        10,
                        kwargs.get("c6")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatQuasilinearViscoelastic.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
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
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def lc1(self) -> int:
        """Get or set the Load curve ID that defines the relaxation function in shear. This curve is used to fit the coefficients Gi and BETAi. If zero, define the coefficients directly. The latter is recommended.
        """ # nopep8
        return self._cards[0].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        self._cards[0].set_value("lc1", value)

    @property
    def lc2(self) -> int:
        """Get or set the Load curve ID that defines the instantaneous elastic response in shear. This curve is used to fit the coefficients Ci. If zero, define the coefficients directly. The latter is recommended.
        """ # nopep8
        return self._cards[0].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        self._cards[0].set_value("lc2", value)

    @property
    def n(self) -> float:
        """Get or set the Number of terms used in the Prony series, a number less than or equal to 12. This number should be equal to the number of decades of time covered by the experimental data. Define this number is LC1 is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def gstart(self) -> typing.Optional[float]:
        """Get or set the Starting value for the least square fit. If zero, a default value is set equal to the inverse of the largest time in the experiment. Define this number if LC1 is nonzero.
        """ # nopep8
        return self._cards[0].get_value("gstart")

    @gstart.setter
    def gstart(self, value: float) -> None:
        self._cards[0].set_value("gstart", value)

    @property
    def m(self) -> float:
        """Get or set the Number of terms used to determine the instantaneous elastic response. Define this number if LC2 is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
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
        if value not in [0.0, 1.0, 2.0]:
            raise Exception("""so must be one of {0.0,1.0,2.0}""")
        self._cards[1].set_value("so", value)

    @property
    def e_min(self) -> float:
        """Get or set the Minimum strain used to generate the load curve from Ci. The default range is -0.9 to 5.1. The computed solution will be more accurate if the user specifies the range used to fit the Ci. Linear extrapolation is used outside the specified range
        """ # nopep8
        return self._cards[1].get_value("e_min")

    @e_min.setter
    def e_min(self, value: float) -> None:
        self._cards[1].set_value("e_min", value)

    @property
    def e_max(self) -> float:
        """Get or set the Maximum strain used to generate the load curve from Ci.
        """ # nopep8
        return self._cards[1].get_value("e_max")

    @e_max.setter
    def e_max(self, value: float) -> None:
        self._cards[1].set_value("e_max", value)

    @property
    def gama1(self) -> typing.Optional[float]:
        """Get or set the Material failure parameter, see *MAT_SIMPLIFIED_RUBBER and Figure 23.181.1.
        """ # nopep8
        return self._cards[1].get_value("gama1")

    @gama1.setter
    def gama1(self, value: float) -> None:
        self._cards[1].set_value("gama1", value)

    @property
    def gama2(self) -> typing.Optional[float]:
        """Get or set the Material failure parameter, see *MAT_SIMPLIFIED_RUBBER
        """ # nopep8
        return self._cards[1].get_value("gama2")

    @gama2.setter
    def gama2(self, value: float) -> None:
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
        self._cards[1].set_value("k", value)

    @property
    def eh(self) -> typing.Optional[float]:
        """Get or set the Damage parameter, see *MAT_SIMPLIFIED_RUBBER
        """ # nopep8
        return self._cards[1].get_value("eh")

    @eh.setter
    def eh(self, value: float) -> None:
        self._cards[1].set_value("eh", value)

    @property
    def form(self) -> int:
        """Get or set the Formulation of model. FORM=0 gives the original model developed by Fung, which always relaxes to a zero stress state as time approaches infinity, and FORM=1 gives the alternative model, which relaxes to the quasi-static elastic response. In general, the two formulations won't give the same responses.  Formulation, FORM=-1, is an improvement on FORM=0 where the instantaneous elastic response is used in the viscoelastic stress update, not just in the relaxation, as in FORM=0.  Consequently, the constants for the elastic response do not need to be scaled
        """ # nopep8
        return self._cards[1].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        self._cards[1].set_value("form", value)

    @property
    def g1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[2].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        self._cards[2].set_value("g1", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[2].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        self._cards[2].set_value("beta1", value)

    @property
    def g2(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[2].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        self._cards[2].set_value("g2", value)

    @property
    def beta2(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[2].get_value("beta2")

    @beta2.setter
    def beta2(self, value: float) -> None:
        self._cards[2].set_value("beta2", value)

    @property
    def g3(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[2].get_value("g3")

    @g3.setter
    def g3(self, value: float) -> None:
        self._cards[2].set_value("g3", value)

    @property
    def beta3(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[2].get_value("beta3")

    @beta3.setter
    def beta3(self, value: float) -> None:
        self._cards[2].set_value("beta3", value)

    @property
    def g4(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[2].get_value("g4")

    @g4.setter
    def g4(self, value: float) -> None:
        self._cards[2].set_value("g4", value)

    @property
    def beta4(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[2].get_value("beta4")

    @beta4.setter
    def beta4(self, value: float) -> None:
        self._cards[2].set_value("beta4", value)

    @property
    def g5(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[3].get_value("g5")

    @g5.setter
    def g5(self, value: float) -> None:
        self._cards[3].set_value("g5", value)

    @property
    def beta5(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[3].get_value("beta5")

    @beta5.setter
    def beta5(self, value: float) -> None:
        self._cards[3].set_value("beta5", value)

    @property
    def g6(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[3].get_value("g6")

    @g6.setter
    def g6(self, value: float) -> None:
        self._cards[3].set_value("g6", value)

    @property
    def beta6(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[3].get_value("beta6")

    @beta6.setter
    def beta6(self, value: float) -> None:
        self._cards[3].set_value("beta6", value)

    @property
    def g7(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[3].get_value("g7")

    @g7.setter
    def g7(self, value: float) -> None:
        self._cards[3].set_value("g7", value)

    @property
    def beta7(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[3].get_value("beta7")

    @beta7.setter
    def beta7(self, value: float) -> None:
        self._cards[3].set_value("beta7", value)

    @property
    def g8(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[3].get_value("g8")

    @g8.setter
    def g8(self, value: float) -> None:
        self._cards[3].set_value("g8", value)

    @property
    def beta8(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[3].get_value("beta8")

    @beta8.setter
    def beta8(self, value: float) -> None:
        self._cards[3].set_value("beta8", value)

    @property
    def g9(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[4].get_value("g9")

    @g9.setter
    def g9(self, value: float) -> None:
        self._cards[4].set_value("g9", value)

    @property
    def beta9(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[4].get_value("beta9")

    @beta9.setter
    def beta9(self, value: float) -> None:
        self._cards[4].set_value("beta9", value)

    @property
    def g10(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[4].get_value("g10")

    @g10.setter
    def g10(self, value: float) -> None:
        self._cards[4].set_value("g10", value)

    @property
    def beta10(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[4].get_value("beta10")

    @beta10.setter
    def beta10(self, value: float) -> None:
        self._cards[4].set_value("beta10", value)

    @property
    def g11(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[4].get_value("g11")

    @g11.setter
    def g11(self, value: float) -> None:
        self._cards[4].set_value("g11", value)

    @property
    def beta11(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[4].get_value("beta11")

    @beta11.setter
    def beta11(self, value: float) -> None:
        self._cards[4].set_value("beta11", value)

    @property
    def g12(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the relaxation function. Define these coefficients if LC1 is set to zero. At least 2 coefficients must be nonzero.
        """ # nopep8
        return self._cards[4].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        self._cards[4].set_value("g12", value)

    @property
    def beta12(self) -> typing.Optional[float]:
        """Get or set the Decay constants of the relaxation function. Define these coefficients if LC1 is set to zero.
        """ # nopep8
        return self._cards[4].get_value("beta12")

    @beta12.setter
    def beta12(self, value: float) -> None:
        self._cards[4].set_value("beta12", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[5].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[5].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[5].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[5].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[5].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the Coefficients of the instantaneous elastic response. Define these coefficients only if LC2 is set to zero.
        """ # nopep8
        return self._cards[5].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[5].set_value("c6", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

