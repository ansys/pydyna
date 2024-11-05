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

class Mat254(KeywordBase):
    """DYNA MAT_254 keyword"""

    keyword = "MAT"
    subkeyword = "254"
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
                        "n",
                        int,
                        20,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "e",
                        float,
                        30,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        40,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "mix",
                        int,
                        50,
                        10,
                        kwargs.get("mix")
                    ),
                    Field(
                        "mixr",
                        int,
                        60,
                        10,
                        kwargs.get("mixr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tastrt",
                        float,
                        0,
                        10,
                        kwargs.get("tastrt")
                    ),
                    Field(
                        "tend",
                        float,
                        10,
                        10,
                        kwargs.get("tend")
                    ),
                    Field(
                        "cte",
                        float,
                        20,
                        10,
                        kwargs.get("cte")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "epsini",
                        int,
                        50,
                        10,
                        kwargs.get("epsini")
                    ),
                    Field(
                        "dtemp",
                        float,
                        60,
                        10,
                        kwargs.get("dtemp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xastr",
                        float,
                        0,
                        10,
                        kwargs.get("xastr")
                    ),
                    Field(
                        "xaend",
                        float,
                        10,
                        10,
                        kwargs.get("xaend")
                    ),
                    Field(
                        "xa1pa1",
                        int,
                        20,
                        10,
                        kwargs.get("xa1pa1")
                    ),
                    Field(
                        "xa1pa2",
                        int,
                        30,
                        10,
                        kwargs.get("xa1pa2")
                    ),
                    Field(
                        "xa1pa3",
                        int,
                        40,
                        10,
                        kwargs.get("xa1pa3")
                    ),
                    Field(
                        "xafpa",
                        float,
                        50,
                        10,
                        kwargs.get("xafpa")
                    ),
                    Field(
                        "cteann",
                        float,
                        60,
                        10,
                        kwargs.get("cteann")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ptlaw",
                        int,
                        0,
                        10,
                        kwargs.get("ptlaw")
                    ),
                    Field(
                        "ptstr",
                        int,
                        10,
                        10,
                        kwargs.get("ptstr")
                    ),
                    Field(
                        "ptend",
                        int,
                        20,
                        10,
                        kwargs.get("ptend")
                    ),
                    Field(
                        "ptx1",
                        int,
                        30,
                        10,
                        kwargs.get("ptx1")
                    ),
                    Field(
                        "ptx2",
                        int,
                        40,
                        10,
                        kwargs.get("ptx2")
                    ),
                    Field(
                        "ptx3",
                        int,
                        50,
                        10,
                        kwargs.get("ptx3")
                    ),
                    Field(
                        "ptx4",
                        int,
                        60,
                        10,
                        kwargs.get("ptx4")
                    ),
                    Field(
                        "ptx5",
                        int,
                        70,
                        10,
                        kwargs.get("ptx5")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pttab1",
                        int,
                        0,
                        10,
                        kwargs.get("pttab1")
                    ),
                    Field(
                        "pttab2",
                        int,
                        10,
                        10,
                        kwargs.get("pttab2")
                    ),
                    Field(
                        "pttab3",
                        int,
                        20,
                        10,
                        kwargs.get("pttab3")
                    ),
                    Field(
                        "pttab4",
                        int,
                        30,
                        10,
                        kwargs.get("pttab4")
                    ),
                    Field(
                        "pttab5",
                        int,
                        40,
                        10,
                        kwargs.get("pttab5")
                    ),
                    Field(
                        "pttab6",
                        int,
                        50,
                        10,
                        kwargs.get("pttab6")
                    ),
                    Field(
                        "pttab7",
                        int,
                        60,
                        10,
                        kwargs.get("pttab7")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pteps",
                        int,
                        0,
                        10,
                        kwargs.get("pteps")
                    ),
                    Field(
                        "ptrip",
                        int,
                        10,
                        10,
                        kwargs.get("ptrip")
                    ),
                    Field(
                        "ptlat",
                        int,
                        20,
                        10,
                        kwargs.get("ptlat")
                    ),
                    Field(
                        "postv",
                        int,
                        30,
                        10,
                        kwargs.get("postv")
                    ),
                    Field(
                        "nushis",
                        int,
                        40,
                        10,
                        kwargs.get("nushis")
                    ),
                    Field(
                        "grai",
                        float,
                        50,
                        10,
                        kwargs.get("grai")
                    ),
                    Field(
                        "t1phas",
                        int,
                        60,
                        10,
                        kwargs.get("t1phas")
                    ),
                    Field(
                        "t2phas",
                        int,
                        70,
                        10,
                        kwargs.get("t2phas")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fushi1",
                        int,
                        0,
                        10,
                        kwargs.get("fushi1")
                    ),
                    Field(
                        "fushi2",
                        int,
                        10,
                        10,
                        kwargs.get("fushi2")
                    ),
                    Field(
                        "fushi3",
                        int,
                        20,
                        10,
                        kwargs.get("fushi3")
                    ),
                    Field(
                        "fushi4",
                        int,
                        30,
                        10,
                        kwargs.get("fushi4")
                    ),
                    Field(
                        "fushi5",
                        int,
                        40,
                        10,
                        kwargs.get("fushi5")
                    ),
                    Field(
                        "fushi6",
                        int,
                        50,
                        10,
                        kwargs.get("fushi6")
                    ),
                    Field(
                        "fushi7",
                        int,
                        60,
                        10,
                        kwargs.get("fushi7")
                    ),
                    Field(
                        "fushi8",
                        int,
                        70,
                        10,
                        kwargs.get("fushi8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigy1",
                        int,
                        0,
                        10,
                        kwargs.get("sigy1")
                    ),
                    Field(
                        "sigy2",
                        int,
                        10,
                        10,
                        kwargs.get("sigy2")
                    ),
                    Field(
                        "sigy3",
                        int,
                        20,
                        10,
                        kwargs.get("sigy3")
                    ),
                    Field(
                        "sigy4",
                        int,
                        30,
                        10,
                        kwargs.get("sigy4")
                    ),
                    Field(
                        "sigy5",
                        int,
                        40,
                        10,
                        kwargs.get("sigy5")
                    ),
                    Field(
                        "sigy6",
                        int,
                        50,
                        10,
                        kwargs.get("sigy6")
                    ),
                    Field(
                        "sigy7",
                        int,
                        60,
                        10,
                        kwargs.get("sigy7")
                    ),
                    Field(
                        "sigy8",
                        int,
                        70,
                        10,
                        kwargs.get("sigy8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat254.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density 𝜌.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def n(self) -> typing.Optional[int]:
        """Get or set the Number of phases.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        self._cards[0].set_value("n", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Youngs' modulus:
        GT.0.0: constant value is used
        LT.0.0: LCID or TABID. Temperature dependent Youngs' modulus given by load curve ID = -E or a Table
        ID = -E. Use TABID to describe temperature dependent modulus for each phase individually.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's modulus:
        GT.0.0: constant value is used
        LT.0.0: LCID or TABID. Temperature dependent Posson's ratio given by load curve ID = -E or a Table ID = -E.
        Use TABID to describe temperature dependent parameter for each phase individually..
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def mix(self) -> typing.Optional[int]:
        """Get or set the Load curve ID with initial phase concentrations.
        """ # nopep8
        return self._cards[0].get_value("mix")

    @mix.setter
    def mix(self, value: int) -> None:
        self._cards[0].set_value("mix", value)

    @property
    def mixr(self) -> typing.Optional[int]:
        """Get or set the LCID or TABID for mixture rule. Use a TABID to define a temperature dependency.
        """ # nopep8
        return self._cards[0].get_value("mixr")

    @mixr.setter
    def mixr(self, value: int) -> None:
        self._cards[0].set_value("mixr", value)

    @property
    def tastrt(self) -> typing.Optional[float]:
        """Get or set the Temperature start for simple linear annealing.  See Remark 4.
        IF TASTAR.gt.0 and TAEND.eq.0, an enhanced annealing algorithm is used.In that case, TASTAR is interpreted as anneal optionand an additional card is required.Possible values for the extended anneal option are
        EQ.1:	linear annealing
        EQ.2 : JMAK
        """ # nopep8
        return self._cards[1].get_value("tastrt")

    @tastrt.setter
    def tastrt(self, value: float) -> None:
        self._cards[1].set_value("tastrt", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the Temperature end for simple linear annealing.  See Remark 4.IF TASTAR.gt.0 and TAEND.eq.0, an enhanced annealing algorithm is used..
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[1].set_value("tend", value)

    @property
    def cte(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion:
        GT.0.0: constant value is used
        LT.0.0: LCID or TABID. Temperature dependent CTE given	by load curve ID = -CTE or a Table ID = -CTE. Use
        Table ID to describe temperature dependent CTE for	each phase individually.
        """ # nopep8
        return self._cards[1].get_value("cte")

    @cte.setter
    def cte(self, value: float) -> None:
        self._cards[1].set_value("cte", value)

    @property
    def epsini(self) -> typing.Optional[int]:
        """Get or set the Initial plastic strains, uniformly distributed within the whole part
        """ # nopep8
        return self._cards[1].get_value("epsini")

    @epsini.setter
    def epsini(self, value: int) -> None:
        self._cards[1].set_value("epsini", value)

    @property
    def dtemp(self) -> typing.Optional[float]:
        """Get or set the Maximum temperature variation within a time step. If exceeded during the analysis, a local sub-cycling is used for the calculation of phase transformations
        """ # nopep8
        return self._cards[1].get_value("dtemp")

    @dtemp.setter
    def dtemp(self, value: float) -> None:
        self._cards[1].set_value("dtemp", value)

    @property
    def xastr(self) -> typing.Optional[float]:
        """Get or set the Annealing start temperature.
        """ # nopep8
        return self._cards[2].get_value("xastr")

    @xastr.setter
    def xastr(self, value: float) -> None:
        self._cards[2].set_value("xastr", value)

    @property
    def xaend(self) -> typing.Optional[float]:
        """Get or set the Annealing end temperature.
        """ # nopep8
        return self._cards[2].get_value("xaend")

    @xaend.setter
    def xaend(self, value: float) -> None:
        self._cards[2].set_value("xaend", value)

    @property
    def xa1pa1(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID defining the i-th parameter of the enhanced annealing option.  Interpretation of the parameter depends on TASTAR..
        """ # nopep8
        return self._cards[2].get_value("xa1pa1")

    @xa1pa1.setter
    def xa1pa1(self, value: int) -> None:
        self._cards[2].set_value("xa1pa1", value)

    @property
    def xa1pa2(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID defining the i-th parameter of the enhanced annealing option.  Interpretation of the parameter depends on TASTAR..
        """ # nopep8
        return self._cards[2].get_value("xa1pa2")

    @xa1pa2.setter
    def xa1pa2(self, value: int) -> None:
        self._cards[2].set_value("xa1pa2", value)

    @property
    def xa1pa3(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID defining the i-th parameter of the enhanced annealing option.  Interpretation of the parameter depends on TASTAR..
        """ # nopep8
        return self._cards[2].get_value("xa1pa3")

    @xa1pa3.setter
    def xa1pa3(self, value: int) -> None:
        self._cards[2].set_value("xa1pa3", value)

    @property
    def xafpa(self) -> typing.Optional[float]:
        """Get or set the Scalar parameter of the enhanced annealing option if applicable.  Interpretation of the parameter depends on TASTA.
        """ # nopep8
        return self._cards[2].get_value("xafpa")

    @xafpa.setter
    def xafpa(self, value: float) -> None:
        self._cards[2].set_value("xafpa", value)

    @property
    def cteann(self) -> typing.Optional[float]:
        """Get or set the Annealing option for thermal expansion.
        LT.0: | CTEAN | defines the upper temperature limit(cut - off temperature) for evaluation of thermal strains.
        EQ.0 : no modification of thermal strains
        EQ.1 : XAEND defines the upper temperature limit(cut - off temperature) for evaluation of thermal strains
        """ # nopep8
        return self._cards[2].get_value("cteann")

    @cteann.setter
    def cteann(self, value: float) -> None:
        self._cards[2].set_value("cteann", value)

    @property
    def ptlaw(self) -> typing.Optional[int]:
        """Get or set the Table ID to define the phase transformation model as a function of source phase and target phase.  The values in *DEFINE_TABLE are the phase numbers before transformation.  The curves referenced by the table specify transformation model (ordinate) as a function of phase number after transformation.
        LT.0:	transformation model used in heating
        EQ.0 : no transformation
        GT.0 : transformation model is used in cooling
        There is a variety of possible transformation models which can be specified as ordinate values of the curves :
        EQ.1 : Koinstinen - Marburger
        EQ.2 : Johnson - Mehl - Avrami - Kolmogorov(JMAK)
        EQ.3 : Akerstrom(only for cooling)
        EQ.4 : Oddy(only for heating)
        EQ.5 : Phase Recovery I(only for heating)
        EQ.6 : Phase Recovery II(only for heating)
        EQ.7 : Parabolic Dissolution I(only for heating)
        EQ.8 : Parabolic Dissolution II(only for heating)
        EQ.9 : extended Koinstinen - Marburger(only for cooling)
        EQ.12 : JMAK for both coolingand heating
        """ # nopep8
        return self._cards[3].get_value("ptlaw")

    @ptlaw.setter
    def ptlaw(self, value: int) -> None:
        self._cards[3].set_value("ptlaw", value)

    @property
    def ptstr(self) -> typing.Optional[int]:
        """Get or set the Table ID to define start temperatures for the transformations as
        function of source phase and target phase. The values in
        *DEFINE_TABLE are the phase numbers before transformation
        (source phase). The curves referenced by the table specify start
        temperature (ordinate) versus phase number after transformation	(abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptstr")

    @ptstr.setter
    def ptstr(self, value: int) -> None:
        self._cards[3].set_value("ptstr", value)

    @property
    def ptend(self) -> typing.Optional[int]:
        """Get or set the Table ID to define end temperatures for the transformations as
        function of source phase and target phase. The values in
        *DEFINE_TABLE are the phase numbers before transformation
        (source phase). The curves referenced by the table specify end
        temperature (ordinate) versus phase number after transformation	(abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptend")

    @ptend.setter
    def ptend(self, value: int) -> None:
        self._cards[3].set_value("ptend", value)

    @property
    def ptx1(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx1")

    @ptx1.setter
    def ptx1(self, value: int) -> None:
        self._cards[3].set_value("ptx1", value)

    @property
    def ptx2(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx2")

    @ptx2.setter
    def ptx2(self, value: int) -> None:
        self._cards[3].set_value("ptx2", value)

    @property
    def ptx3(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx3")

    @ptx3.setter
    def ptx3(self, value: int) -> None:
        self._cards[3].set_value("ptx3", value)

    @property
    def ptx4(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx4")

    @ptx4.setter
    def ptx4(self, value: int) -> None:
        self._cards[3].set_value("ptx4", value)

    @property
    def ptx5(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx5")

    @ptx5.setter
    def ptx5(self, value: int) -> None:
        self._cards[3].set_value("ptx5", value)

    @property
    def pttab1(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab1")

    @pttab1.setter
    def pttab1(self, value: int) -> None:
        self._cards[4].set_value("pttab1", value)

    @property
    def pttab2(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab2")

    @pttab2.setter
    def pttab2(self, value: int) -> None:
        self._cards[4].set_value("pttab2", value)

    @property
    def pttab3(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab3")

    @pttab3.setter
    def pttab3(self, value: int) -> None:
        self._cards[4].set_value("pttab3", value)

    @property
    def pttab4(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab4")

    @pttab4.setter
    def pttab4(self, value: int) -> None:
        self._cards[4].set_value("pttab4", value)

    @property
    def pttab5(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab5")

    @pttab5.setter
    def pttab5(self, value: int) -> None:
        self._cards[4].set_value("pttab5", value)

    @property
    def pttab6(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab6")

    @pttab6.setter
    def pttab6(self, value: int) -> None:
        self._cards[4].set_value("pttab6", value)

    @property
    def pttab7(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab7")

    @pttab7.setter
    def pttab7(self, value: int) -> None:
        self._cards[4].set_value("pttab7", value)

    @property
    def pteps(self) -> typing.Optional[int]:
        """Get or set the Table ID containing transformation induced strains as function of
        source phase and target phase.
        """ # nopep8
        return self._cards[5].get_value("pteps")

    @pteps.setter
    def pteps(self, value: int) -> None:
        self._cards[5].set_value("pteps", value)

    @property
    def ptrip(self) -> typing.Optional[int]:
        """Get or set the Flag for transformation induced plasticity (TRIP). Algorithm active
        for positive value of PTRIP.
        """ # nopep8
        return self._cards[5].get_value("ptrip")

    @ptrip.setter
    def ptrip(self, value: int) -> None:
        self._cards[5].set_value("ptrip", value)

    @property
    def ptlat(self) -> typing.Optional[int]:
        """Get or set the Table ID defining transformation induced heat generation .
        """ # nopep8
        return self._cards[5].get_value("ptlat")

    @ptlat.setter
    def ptlat(self, value: int) -> None:
        self._cards[5].set_value("ptlat", value)

    @property
    def postv(self) -> typing.Optional[int]:
        """Get or set the Define additional pre-defined history variables that might be useful for post-processing.
        """ # nopep8
        return self._cards[5].get_value("postv")

    @postv.setter
    def postv(self, value: int) -> None:
        self._cards[5].set_value("postv", value)

    @property
    def nushis(self) -> typing.Optional[int]:
        """Get or set the Number of additional user defined history variables.  .
        """ # nopep8
        return self._cards[5].get_value("nushis")

    @nushis.setter
    def nushis(self, value: int) -> None:
        self._cards[5].set_value("nushis", value)

    @property
    def grai(self) -> typing.Optional[float]:
        """Get or set the Initial grain size.
        """ # nopep8
        return self._cards[5].get_value("grai")

    @grai.setter
    def grai(self, value: float) -> None:
        self._cards[5].set_value("grai", value)

    @property
    def t1phas(self) -> typing.Optional[int]:
        """Get or set the Lower temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables.
        """ # nopep8
        return self._cards[5].get_value("t1phas")

    @t1phas.setter
    def t1phas(self, value: int) -> None:
        self._cards[5].set_value("t1phas", value)

    @property
    def t2phas(self) -> typing.Optional[int]:
        """Get or set the Upper temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables.
        """ # nopep8
        return self._cards[5].get_value("t2phas")

    @t2phas.setter
    def t2phas(self, value: int) -> None:
        self._cards[5].set_value("t2phas", value)

    @property
    def fushi1(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi1")

    @fushi1.setter
    def fushi1(self, value: int) -> None:
        self._cards[6].set_value("fushi1", value)

    @property
    def fushi2(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi2")

    @fushi2.setter
    def fushi2(self, value: int) -> None:
        self._cards[6].set_value("fushi2", value)

    @property
    def fushi3(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi3")

    @fushi3.setter
    def fushi3(self, value: int) -> None:
        self._cards[6].set_value("fushi3", value)

    @property
    def fushi4(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi4")

    @fushi4.setter
    def fushi4(self, value: int) -> None:
        self._cards[6].set_value("fushi4", value)

    @property
    def fushi5(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi5")

    @fushi5.setter
    def fushi5(self, value: int) -> None:
        self._cards[6].set_value("fushi5", value)

    @property
    def fushi6(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi6")

    @fushi6.setter
    def fushi6(self, value: int) -> None:
        self._cards[6].set_value("fushi6", value)

    @property
    def fushi7(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi7")

    @fushi7.setter
    def fushi7(self, value: int) -> None:
        self._cards[6].set_value("fushi7", value)

    @property
    def fushi8(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi8")

    @fushi8.setter
    def fushi8(self, value: int) -> None:
        self._cards[6].set_value("fushi8", value)

    @property
    def sigy1(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy1")

    @sigy1.setter
    def sigy1(self, value: int) -> None:
        self._cards[7].set_value("sigy1", value)

    @property
    def sigy2(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy2")

    @sigy2.setter
    def sigy2(self, value: int) -> None:
        self._cards[7].set_value("sigy2", value)

    @property
    def sigy3(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy3")

    @sigy3.setter
    def sigy3(self, value: int) -> None:
        self._cards[7].set_value("sigy3", value)

    @property
    def sigy4(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy4")

    @sigy4.setter
    def sigy4(self, value: int) -> None:
        self._cards[7].set_value("sigy4", value)

    @property
    def sigy5(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy5")

    @sigy5.setter
    def sigy5(self, value: int) -> None:
        self._cards[7].set_value("sigy5", value)

    @property
    def sigy6(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy6")

    @sigy6.setter
    def sigy6(self, value: int) -> None:
        self._cards[7].set_value("sigy6", value)

    @property
    def sigy7(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy7")

    @sigy7.setter
    def sigy7(self, value: int) -> None:
        self._cards[7].set_value("sigy7", value)

    @property
    def sigy8(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy8")

    @sigy8.setter
    def sigy8(self, value: int) -> None:
        self._cards[7].set_value("sigy8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[8].cards[0].set_value("title", value)

