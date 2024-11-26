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

class MatViscoelasticHillFoam(KeywordBase):
    """DYNA MAT_VISCOELASTIC_HILL_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "VISCOELASTIC_HILL_FOAM"
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
                        "n",
                        float,
                        30,
                        10,
                        kwargs.get("n", 0)
                    ),
                    Field(
                        "nu",
                        float,
                        40,
                        10,
                        kwargs.get("nu", 0)
                    ),
                    Field(
                        "lcid",
                        int,
                        50,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "fittype",
                        int,
                        60,
                        10,
                        kwargs.get("fittype", 1)
                    ),
                    Field(
                        "lcsr",
                        int,
                        70,
                        10,
                        kwargs.get("lcsr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcve",
                        int,
                        0,
                        10,
                        kwargs.get("lcve", 0)
                    ),
                    Field(
                        "nt",
                        float,
                        10,
                        10,
                        kwargs.get("nt", 6)
                    ),
                    Field(
                        "gstart",
                        float,
                        20,
                        10,
                        kwargs.get("gstart", 1)
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
                    Field(
                        "c7",
                        float,
                        60,
                        10,
                        kwargs.get("c7")
                    ),
                    Field(
                        "c8",
                        float,
                        70,
                        10,
                        kwargs.get("c8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "b1",
                        float,
                        0,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "b2",
                        float,
                        10,
                        10,
                        kwargs.get("b2")
                    ),
                    Field(
                        "b3",
                        float,
                        20,
                        10,
                        kwargs.get("b3")
                    ),
                    Field(
                        "b4",
                        float,
                        30,
                        10,
                        kwargs.get("b4")
                    ),
                    Field(
                        "b5",
                        float,
                        40,
                        10,
                        kwargs.get("b5")
                    ),
                    Field(
                        "b6",
                        float,
                        50,
                        10,
                        kwargs.get("b6")
                    ),
                    Field(
                        "b7",
                        float,
                        60,
                        10,
                        kwargs.get("b7")
                    ),
                    Field(
                        "b8",
                        float,
                        70,
                        10,
                        kwargs.get("b8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gi",
                        float,
                        0,
                        10,
                        kwargs.get("gi")
                    ),
                    Field(
                        "betai",
                        float,
                        10,
                        10,
                        kwargs.get("betai")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatViscoelasticHillFoam.option_specs[0],
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
        """Get or set the Bulk modulus. This modulus is used for determining the contact interface stiffness.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def n(self) -> float:
        """Get or set the Material constant. Define if LCID=0 below; otherwise, N is fit from the load curve data.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def nu(self) -> float:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("nu")

    @nu.setter
    def nu(self, value: float) -> None:
        self._cards[0].set_value("nu", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID that defines the force per unit area versus the stretch ratio. This curve can be given for either uniaxial or biaxial data depending on FITTYPE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def fittype(self) -> int:
        """Get or set the Type of fit:
        EQ.1:uniaxial data,
        EQ.2:biaxial data.
        """ # nopep8
        return self._cards[0].get_value("fittype")

    @fittype.setter
    def fittype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""fittype must be one of {1,2}""")
        self._cards[0].set_value("fittype", value)

    @property
    def lcsr(self) -> int:
        """Get or set the Load curve ID that defines the uniaxial or biaxial stress ratio (see FITTYPE) versus the transverse stretch ratio.
        """ # nopep8
        return self._cards[0].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        self._cards[0].set_value("lcsr", value)

    @property
    def lcve(self) -> int:
        """Get or set the Optional load curve ID that defines the relaxation function in shear. This curve is used to fit the coefficients Gi and BETAi. If zero, define the coefficients directly. The latter is recommended.
        """ # nopep8
        return self._cards[1].get_value("lcve")

    @lcve.setter
    def lcve(self, value: int) -> None:
        self._cards[1].set_value("lcve", value)

    @property
    def nt(self) -> float:
        """Get or set the Number of terms used to fit the Prony serives, which is a number less than or equal to 12. This number should be equal to the number of decades of time covered by the experimental data. Define this number if LCVE is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
        """ # nopep8
        return self._cards[1].get_value("nt")

    @nt.setter
    def nt(self, value: float) -> None:
        self._cards[1].set_value("nt", value)

    @property
    def gstart(self) -> float:
        """Get or set the Starting value for least square fit. If zero, a default value is set equal to the inverse of the largest time in the experiment. Define this number if LC1 is nonzero, Ci, Material constants. Define up to 8 coefficients.
        """ # nopep8
        return self._cards[1].get_value("gstart")

    @gstart.setter
    def gstart(self, value: float) -> None:
        if value not in [1, TMAX]:
            raise Exception("""gstart must be one of {1,TMAX}""")
        self._cards[1].set_value("gstart", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[2].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[2].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[2].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[2].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[2].set_value("c6", value)

    @property
    def c7(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("c7")

    @c7.setter
    def c7(self, value: float) -> None:
        self._cards[2].set_value("c7", value)

    @property
    def c8(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[2].get_value("c8")

    @c8.setter
    def c8(self, value: float) -> None:
        self._cards[2].set_value("c8", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[3].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[3].set_value("b1", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[3].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[3].set_value("b2", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[3].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        self._cards[3].set_value("b3", value)

    @property
    def b4(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[3].get_value("b4")

    @b4.setter
    def b4(self, value: float) -> None:
        self._cards[3].set_value("b4", value)

    @property
    def b5(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[3].get_value("b5")

    @b5.setter
    def b5(self, value: float) -> None:
        self._cards[3].set_value("b5", value)

    @property
    def b6(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[3].get_value("b6")

    @b6.setter
    def b6(self, value: float) -> None:
        self._cards[3].set_value("b6", value)

    @property
    def b7(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[3].get_value("b7")

    @b7.setter
    def b7(self, value: float) -> None:
        self._cards[3].set_value("b7", value)

    @property
    def b8(self) -> typing.Optional[float]:
        """Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
        """ # nopep8
        return self._cards[3].get_value("b8")

    @b8.setter
    def b8(self, value: float) -> None:
        self._cards[3].set_value("b8", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Optional shear relaxation modulus for the ith term.
        """ # nopep8
        return self._cards[4].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        self._cards[4].set_value("gi", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the Optional decay constant for the ith term
        """ # nopep8
        return self._cards[4].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        self._cards[4].set_value("betai", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

