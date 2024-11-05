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

class MatConcreteDamageRel3(KeywordBase):
    """DYNA MAT_CONCRETE_DAMAGE_REL3 keyword"""

    keyword = "MAT"
    subkeyword = "CONCRETE_DAMAGE_REL3"
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
                        "pr",
                        float,
                        20,
                        10,
                        kwargs.get("pr")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ft",
                        float,
                        0,
                        10,
                        kwargs.get("ft")
                    ),
                    Field(
                        "a0",
                        float,
                        10,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        30,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "b1",
                        float,
                        40,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "omega",
                        float,
                        50,
                        10,
                        kwargs.get("omega")
                    ),
                    Field(
                        "a1f",
                        float,
                        60,
                        10,
                        kwargs.get("a1f")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "slambda",
                        float,
                        0,
                        10,
                        kwargs.get("slambda")
                    ),
                    Field(
                        "nout",
                        float,
                        10,
                        10,
                        kwargs.get("nout")
                    ),
                    Field(
                        "edrop",
                        float,
                        20,
                        10,
                        kwargs.get("edrop")
                    ),
                    Field(
                        "rsize",
                        float,
                        30,
                        10,
                        kwargs.get("rsize")
                    ),
                    Field(
                        "ucf",
                        float,
                        40,
                        10,
                        kwargs.get("ucf")
                    ),
                    Field(
                        "lcrate",
                        int,
                        50,
                        10,
                        kwargs.get("lcrate")
                    ),
                    Field(
                        "locwidth",
                        float,
                        60,
                        10,
                        kwargs.get("locwidth")
                    ),
                    Field(
                        "npts",
                        float,
                        70,
                        10,
                        kwargs.get("npts")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lambda1",
                        float,
                        0,
                        10,
                        kwargs.get("lambda1")
                    ),
                    Field(
                        "lambda2",
                        float,
                        10,
                        10,
                        kwargs.get("lambda2")
                    ),
                    Field(
                        "lambda3",
                        float,
                        20,
                        10,
                        kwargs.get("lambda3")
                    ),
                    Field(
                        "lambda4",
                        float,
                        30,
                        10,
                        kwargs.get("lambda4")
                    ),
                    Field(
                        "lambda5",
                        float,
                        40,
                        10,
                        kwargs.get("lambda5")
                    ),
                    Field(
                        "lambda6",
                        float,
                        50,
                        10,
                        kwargs.get("lambda6")
                    ),
                    Field(
                        "lambda7",
                        float,
                        60,
                        10,
                        kwargs.get("lambda7")
                    ),
                    Field(
                        "lambda8",
                        float,
                        70,
                        10,
                        kwargs.get("lambda8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lambda09",
                        float,
                        0,
                        10,
                        kwargs.get("lambda09")
                    ),
                    Field(
                        "lambda10",
                        float,
                        10,
                        10,
                        kwargs.get("lambda10")
                    ),
                    Field(
                        "lambda11",
                        float,
                        20,
                        10,
                        kwargs.get("lambda11")
                    ),
                    Field(
                        "lambda12",
                        float,
                        30,
                        10,
                        kwargs.get("lambda12")
                    ),
                    Field(
                        "lambda13",
                        float,
                        40,
                        10,
                        kwargs.get("lambda13")
                    ),
                    Field(
                        "b3",
                        float,
                        50,
                        10,
                        kwargs.get("b3")
                    ),
                    Field(
                        "a0y",
                        float,
                        60,
                        10,
                        kwargs.get("a0y")
                    ),
                    Field(
                        "a1y",
                        float,
                        70,
                        10,
                        kwargs.get("a1y")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eta1",
                        float,
                        0,
                        10,
                        kwargs.get("eta1")
                    ),
                    Field(
                        "eta2",
                        float,
                        10,
                        10,
                        kwargs.get("eta2")
                    ),
                    Field(
                        "eta3",
                        float,
                        20,
                        10,
                        kwargs.get("eta3")
                    ),
                    Field(
                        "eta4",
                        float,
                        30,
                        10,
                        kwargs.get("eta4")
                    ),
                    Field(
                        "eta5",
                        float,
                        40,
                        10,
                        kwargs.get("eta5")
                    ),
                    Field(
                        "eta6",
                        float,
                        50,
                        10,
                        kwargs.get("eta6")
                    ),
                    Field(
                        "eta7",
                        float,
                        60,
                        10,
                        kwargs.get("eta7")
                    ),
                    Field(
                        "eta8",
                        float,
                        70,
                        10,
                        kwargs.get("eta8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eta09",
                        float,
                        0,
                        10,
                        kwargs.get("eta09")
                    ),
                    Field(
                        "eta10",
                        float,
                        10,
                        10,
                        kwargs.get("eta10")
                    ),
                    Field(
                        "eta11",
                        float,
                        20,
                        10,
                        kwargs.get("eta11")
                    ),
                    Field(
                        "eta12",
                        float,
                        30,
                        10,
                        kwargs.get("eta12")
                    ),
                    Field(
                        "eta13",
                        float,
                        40,
                        10,
                        kwargs.get("eta13")
                    ),
                    Field(
                        "b2",
                        float,
                        50,
                        10,
                        kwargs.get("b2")
                    ),
                    Field(
                        "a2f",
                        float,
                        60,
                        10,
                        kwargs.get("a2f")
                    ),
                    Field(
                        "a2y",
                        float,
                        70,
                        10,
                        kwargs.get("a2y")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatConcreteDamageRel3.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be used.
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
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Uniaxial tensile strength
        """ # nopep8
        return self._cards[1].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        self._cards[1].set_value("ft", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Maximum shear failure surface parameter,   or   for parameter generation
        """ # nopep8
        return self._cards[1].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[1].set_value("a0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Maximum shear failure surface parameter  .
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Maximum shear failure surface parameter  .
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Compressive damage scaling parameter,
        """ # nopep8
        return self._cards[1].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[1].set_value("b1", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Fractional dilatancy,
        """ # nopep8
        return self._cards[1].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        self._cards[1].set_value("omega", value)

    @property
    def a1f(self) -> typing.Optional[float]:
        """Get or set the Residual failure surface coefficient,  .
        """ # nopep8
        return self._cards[1].get_value("a1f")

    @a1f.setter
    def a1f(self, value: float) -> None:
        self._cards[1].set_value("a1f", value)

    @property
    def slambda(self) -> typing.Optional[float]:
        """Get or set the stretch factor, s.
        """ # nopep8
        return self._cards[2].get_value("slambda")

    @slambda.setter
    def slambda(self, value: float) -> None:
        self._cards[2].set_value("slambda", value)

    @property
    def nout(self) -> typing.Optional[float]:
        """Get or set the Output selector for effective plastic strain (see table).
        """ # nopep8
        return self._cards[2].get_value("nout")

    @nout.setter
    def nout(self, value: float) -> None:
        self._cards[2].set_value("nout", value)

    @property
    def edrop(self) -> typing.Optional[float]:
        """Get or set the Post peak dilatancy decay,
        """ # nopep8
        return self._cards[2].get_value("edrop")

    @edrop.setter
    def edrop(self, value: float) -> None:
        self._cards[2].set_value("edrop", value)

    @property
    def rsize(self) -> typing.Optional[float]:
        """Get or set the Unit conversion factor for length (inches/user-unit), e.g. 39.37 if user length unit in meters.
        """ # nopep8
        return self._cards[2].get_value("rsize")

    @rsize.setter
    def rsize(self, value: float) -> None:
        self._cards[2].set_value("rsize", value)

    @property
    def ucf(self) -> typing.Optional[float]:
        """Get or set the Unit conversion factor for stress (psi/user-unit), e.g. 145 if   in MPa.
        """ # nopep8
        return self._cards[2].get_value("ucf")

    @ucf.setter
    def ucf(self, value: float) -> None:
        self._cards[2].set_value("ucf", value)

    @property
    def lcrate(self) -> typing.Optional[int]:
        """Get or set the Define (load) curve number for strain-rate effects; effective strain rate on abscissa (negative = tension) and strength enhancement on ordinate. If LCRATE is set to= -1, strain rate effects areis automatically included, based on equations provided in Wu, Crawford, Lan, and Magallanes [2014].
        """ # nopep8
        return self._cards[2].get_value("lcrate")

    @lcrate.setter
    def lcrate(self, value: int) -> None:
        self._cards[2].set_value("lcrate", value)

    @property
    def locwidth(self) -> typing.Optional[float]:
        """Get or set the Three times the maximum aggregate diameter (input in user length units).
        """ # nopep8
        return self._cards[2].get_value("locwidth")

    @locwidth.setter
    def locwidth(self, value: float) -> None:
        self._cards[2].set_value("locwidth", value)

    @property
    def npts(self) -> typing.Optional[float]:
        """Get or set the Number of points in   versus  damage relation; must be 13 points.
        """ # nopep8
        return self._cards[2].get_value("npts")

    @npts.setter
    def npts(self, value: float) -> None:
        self._cards[2].set_value("npts", value)

    @property
    def lambda1(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[3].get_value("lambda1")

    @lambda1.setter
    def lambda1(self, value: float) -> None:
        self._cards[3].set_value("lambda1", value)

    @property
    def lambda2(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[3].get_value("lambda2")

    @lambda2.setter
    def lambda2(self, value: float) -> None:
        self._cards[3].set_value("lambda2", value)

    @property
    def lambda3(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[3].get_value("lambda3")

    @lambda3.setter
    def lambda3(self, value: float) -> None:
        self._cards[3].set_value("lambda3", value)

    @property
    def lambda4(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[3].get_value("lambda4")

    @lambda4.setter
    def lambda4(self, value: float) -> None:
        self._cards[3].set_value("lambda4", value)

    @property
    def lambda5(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[3].get_value("lambda5")

    @lambda5.setter
    def lambda5(self, value: float) -> None:
        self._cards[3].set_value("lambda5", value)

    @property
    def lambda6(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[3].get_value("lambda6")

    @lambda6.setter
    def lambda6(self, value: float) -> None:
        self._cards[3].set_value("lambda6", value)

    @property
    def lambda7(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[3].get_value("lambda7")

    @lambda7.setter
    def lambda7(self, value: float) -> None:
        self._cards[3].set_value("lambda7", value)

    @property
    def lambda8(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[3].get_value("lambda8")

    @lambda8.setter
    def lambda8(self, value: float) -> None:
        self._cards[3].set_value("lambda8", value)

    @property
    def lambda09(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[4].get_value("lambda09")

    @lambda09.setter
    def lambda09(self, value: float) -> None:
        self._cards[4].set_value("lambda09", value)

    @property
    def lambda10(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[4].get_value("lambda10")

    @lambda10.setter
    def lambda10(self, value: float) -> None:
        self._cards[4].set_value("lambda10", value)

    @property
    def lambda11(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[4].get_value("lambda11")

    @lambda11.setter
    def lambda11(self, value: float) -> None:
        self._cards[4].set_value("lambda11", value)

    @property
    def lambda12(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[4].get_value("lambda12")

    @lambda12.setter
    def lambda12(self, value: float) -> None:
        self._cards[4].set_value("lambda12", value)

    @property
    def lambda13(self) -> typing.Optional[float]:
        """Get or set the value of damage function
        """ # nopep8
        return self._cards[4].get_value("lambda13")

    @lambda13.setter
    def lambda13(self, value: float) -> None:
        self._cards[4].set_value("lambda13", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the Damage scaling coefficient for triaxial tension
        """ # nopep8
        return self._cards[4].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        self._cards[4].set_value("b3", value)

    @property
    def a0y(self) -> typing.Optional[float]:
        """Get or set the Initial yield surface cohesion,
        """ # nopep8
        return self._cards[4].get_value("a0y")

    @a0y.setter
    def a0y(self, value: float) -> None:
        self._cards[4].set_value("a0y", value)

    @property
    def a1y(self) -> typing.Optional[float]:
        """Get or set the Initial yield surface coefficient,
        """ # nopep8
        return self._cards[4].get_value("a1y")

    @a1y.setter
    def a1y(self, value: float) -> None:
        self._cards[4].set_value("a1y", value)

    @property
    def eta1(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[5].get_value("eta1")

    @eta1.setter
    def eta1(self, value: float) -> None:
        self._cards[5].set_value("eta1", value)

    @property
    def eta2(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[5].get_value("eta2")

    @eta2.setter
    def eta2(self, value: float) -> None:
        self._cards[5].set_value("eta2", value)

    @property
    def eta3(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[5].get_value("eta3")

    @eta3.setter
    def eta3(self, value: float) -> None:
        self._cards[5].set_value("eta3", value)

    @property
    def eta4(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[5].get_value("eta4")

    @eta4.setter
    def eta4(self, value: float) -> None:
        self._cards[5].set_value("eta4", value)

    @property
    def eta5(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[5].get_value("eta5")

    @eta5.setter
    def eta5(self, value: float) -> None:
        self._cards[5].set_value("eta5", value)

    @property
    def eta6(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[5].get_value("eta6")

    @eta6.setter
    def eta6(self, value: float) -> None:
        self._cards[5].set_value("eta6", value)

    @property
    def eta7(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[5].get_value("eta7")

    @eta7.setter
    def eta7(self, value: float) -> None:
        self._cards[5].set_value("eta7", value)

    @property
    def eta8(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[5].get_value("eta8")

    @eta8.setter
    def eta8(self, value: float) -> None:
        self._cards[5].set_value("eta8", value)

    @property
    def eta09(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[6].get_value("eta09")

    @eta09.setter
    def eta09(self, value: float) -> None:
        self._cards[6].set_value("eta09", value)

    @property
    def eta10(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[6].get_value("eta10")

    @eta10.setter
    def eta10(self, value: float) -> None:
        self._cards[6].set_value("eta10", value)

    @property
    def eta11(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[6].get_value("eta11")

    @eta11.setter
    def eta11(self, value: float) -> None:
        self._cards[6].set_value("eta11", value)

    @property
    def eta12(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[6].get_value("eta12")

    @eta12.setter
    def eta12(self, value: float) -> None:
        self._cards[6].set_value("eta12", value)

    @property
    def eta13(self) -> typing.Optional[float]:
        """Get or set the 1st value of scale factor
        """ # nopep8
        return self._cards[6].get_value("eta13")

    @eta13.setter
    def eta13(self, value: float) -> None:
        self._cards[6].set_value("eta13", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Tensile damage scaling exponent
        """ # nopep8
        return self._cards[6].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[6].set_value("b2", value)

    @property
    def a2f(self) -> typing.Optional[float]:
        """Get or set the Residual failure surface coefficient
        """ # nopep8
        return self._cards[6].get_value("a2f")

    @a2f.setter
    def a2f(self, value: float) -> None:
        self._cards[6].set_value("a2f", value)

    @property
    def a2y(self) -> typing.Optional[float]:
        """Get or set the Initial yield surface coefficient
        """ # nopep8
        return self._cards[6].get_value("a2y")

    @a2y.setter
    def a2y(self, value: float) -> None:
        self._cards[6].set_value("a2y", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

