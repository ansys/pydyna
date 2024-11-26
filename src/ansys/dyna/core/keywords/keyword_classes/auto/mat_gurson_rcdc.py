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

class MatGursonRcdc(KeywordBase):
    """DYNA MAT_GURSON_RCDC keyword"""

    keyword = "MAT"
    subkeyword = "GURSON_RCDC"
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
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "sigy",
                        float,
                        40,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "n",
                        float,
                        50,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "q1",
                        float,
                        60,
                        10,
                        kwargs.get("q1")
                    ),
                    Field(
                        "q2",
                        float,
                        70,
                        10,
                        kwargs.get("q2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fc",
                        float,
                        0,
                        10,
                        kwargs.get("fc")
                    ),
                    Field(
                        "f0",
                        float,
                        10,
                        10,
                        kwargs.get("f0")
                    ),
                    Field(
                        "en",
                        float,
                        20,
                        10,
                        kwargs.get("en")
                    ),
                    Field(
                        "sn",
                        float,
                        30,
                        10,
                        kwargs.get("sn")
                    ),
                    Field(
                        "fn",
                        float,
                        40,
                        10,
                        kwargs.get("fn")
                    ),
                    Field(
                        "etan",
                        float,
                        50,
                        10,
                        kwargs.get("etan")
                    ),
                    Field(
                        "atyp",
                        float,
                        60,
                        10,
                        kwargs.get("atyp", 1)
                    ),
                    Field(
                        "ff0",
                        float,
                        70,
                        10,
                        kwargs.get("ff0")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eps1",
                        float,
                        0,
                        10,
                        kwargs.get("eps1")
                    ),
                    Field(
                        "eps2",
                        float,
                        10,
                        10,
                        kwargs.get("eps2")
                    ),
                    Field(
                        "eps3",
                        float,
                        20,
                        10,
                        kwargs.get("eps3")
                    ),
                    Field(
                        "eps4",
                        float,
                        30,
                        10,
                        kwargs.get("eps4")
                    ),
                    Field(
                        "eps5",
                        float,
                        40,
                        10,
                        kwargs.get("eps5")
                    ),
                    Field(
                        "eps6",
                        float,
                        50,
                        10,
                        kwargs.get("eps6")
                    ),
                    Field(
                        "eps7",
                        float,
                        60,
                        10,
                        kwargs.get("eps7")
                    ),
                    Field(
                        "eps8",
                        float,
                        70,
                        10,
                        kwargs.get("eps8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "es1",
                        float,
                        0,
                        10,
                        kwargs.get("es1")
                    ),
                    Field(
                        "es2",
                        float,
                        10,
                        10,
                        kwargs.get("es2")
                    ),
                    Field(
                        "es3",
                        float,
                        20,
                        10,
                        kwargs.get("es3")
                    ),
                    Field(
                        "es4",
                        float,
                        30,
                        10,
                        kwargs.get("es4")
                    ),
                    Field(
                        "es5",
                        float,
                        40,
                        10,
                        kwargs.get("es5")
                    ),
                    Field(
                        "es6",
                        float,
                        50,
                        10,
                        kwargs.get("es6")
                    ),
                    Field(
                        "es7",
                        float,
                        60,
                        10,
                        kwargs.get("es7")
                    ),
                    Field(
                        "es8",
                        float,
                        70,
                        10,
                        kwargs.get("es8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "l1",
                        float,
                        0,
                        10,
                        kwargs.get("l1")
                    ),
                    Field(
                        "l2",
                        float,
                        10,
                        10,
                        kwargs.get("l2")
                    ),
                    Field(
                        "l3",
                        float,
                        20,
                        10,
                        kwargs.get("l3")
                    ),
                    Field(
                        "l4",
                        float,
                        30,
                        10,
                        kwargs.get("l4")
                    ),
                    Field(
                        "ff1",
                        float,
                        40,
                        10,
                        kwargs.get("ff1")
                    ),
                    Field(
                        "ff2",
                        float,
                        50,
                        10,
                        kwargs.get("ff2")
                    ),
                    Field(
                        "ff3",
                        float,
                        60,
                        10,
                        kwargs.get("ff3")
                    ),
                    Field(
                        "ff4",
                        float,
                        70,
                        10,
                        kwargs.get("ff4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcss",
                        int,
                        0,
                        10,
                        kwargs.get("lcss", 0)
                    ),
                    Field(
                        "lclf",
                        int,
                        10,
                        10,
                        kwargs.get("lclf", 0)
                    ),
                    Field(
                        "numint",
                        float,
                        20,
                        10,
                        kwargs.get("numint", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha",
                        float,
                        0,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "beta",
                        float,
                        10,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "gamma",
                        float,
                        20,
                        10,
                        kwargs.get("gamma")
                    ),
                    Field(
                        "d0",
                        float,
                        30,
                        10,
                        kwargs.get("d0")
                    ),
                    Field(
                        "b",
                        float,
                        40,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "lambda",
                        float,
                        50,
                        10,
                        kwargs.get("lambda")
                    ),
                    Field(
                        "ds",
                        float,
                        60,
                        10,
                        kwargs.get("ds")
                    ),
                    Field(
                        "l",
                        float,
                        70,
                        10,
                        kwargs.get("l")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatGursonRcdc.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent for Power law.This value is only used if ATYP=1 and LCSS=0.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[0].set_value("n", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Parameter q1.
        """ # nopep8
        return self._cards[0].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        self._cards[0].set_value("q1", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Parameter q2.
        """ # nopep8
        return self._cards[0].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        self._cards[0].set_value("q2", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Critical void volume fraction fc.
        """ # nopep8
        return self._cards[1].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        self._cards[1].set_value("fc", value)

    @property
    def f0(self) -> typing.Optional[float]:
        """Get or set the Initial void volume fraction f0.
        """ # nopep8
        return self._cards[1].get_value("f0")

    @f0.setter
    def f0(self, value: float) -> None:
        self._cards[1].set_value("f0", value)

    @property
    def en(self) -> typing.Optional[float]:
        """Get or set the Mean nucleation strain En .
        GT.0.0:	Constant value,
        LT.0.0:	Load curve ID = (-EN) which defines mean nucleation strain ε_N  as a function of element length.
        """ # nopep8
        return self._cards[1].get_value("en")

    @en.setter
    def en(self, value: float) -> None:
        self._cards[1].set_value("en", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Standard deviation Sn of the normal distribution of En.
        GT.0.0:	Constant value,
        LT.0.0:	Load curve ID = (-SN) which defines standard deviation s_N of the normal distribution of ε_N as a function of element length.
        """ # nopep8
        return self._cards[1].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        self._cards[1].set_value("sn", value)

    @property
    def fn(self) -> typing.Optional[float]:
        """Get or set the Void volume fraction of nucleating particles.
        """ # nopep8
        return self._cards[1].get_value("fn")

    @fn.setter
    def fn(self, value: float) -> None:
        self._cards[1].set_value("fn", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Hardening modulus. This value is only used if ATYP=2 and LCSS=0.
        """ # nopep8
        return self._cards[1].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        self._cards[1].set_value("etan", value)

    @property
    def atyp(self) -> float:
        """Get or set the Type of hardening.
        EQ.1.0 Power law.
        EQ.2.0: Linear hardening.
        EQ.3.0: 8 points curve.
        """ # nopep8
        return self._cards[1].get_value("atyp")

    @atyp.setter
    def atyp(self, value: float) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""atyp must be one of {1,2,3}""")
        self._cards[1].set_value("atyp", value)

    @property
    def ff0(self) -> typing.Optional[float]:
        """Get or set the Failure void volume fraction. This value is used if no curve is given by the points L1,FF1 - L4,FF4 and LCLF=0.
        """ # nopep8
        return self._cards[1].get_value("ff0")

    @ff0.setter
    def ff0(self, value: float) -> None:
        self._cards[1].set_value("ff0", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values.The first point must be zero corresponding to the initial yield stress. This option is only used if ATYP equal to 3. At least 2 points should be defined.These values are used if ATYP=3 and LCSS=0.
        """ # nopep8
        return self._cards[2].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        self._cards[2].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 2
        """ # nopep8
        return self._cards[2].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        self._cards[2].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 3
        """ # nopep8
        return self._cards[2].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        self._cards[2].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 4
        """ # nopep8
        return self._cards[2].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        self._cards[2].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 5
        """ # nopep8
        return self._cards[2].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        self._cards[2].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 6
        """ # nopep8
        return self._cards[2].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        self._cards[2].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 7
        """ # nopep8
        return self._cards[2].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        self._cards[2].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain values at point 8
        """ # nopep8
        return self._cards[2].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        self._cards[2].set_value("eps8", value)

    @property
    def es1(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS1 - EPS8. These values are used if ATYP=3 and LCSS=0.
        """ # nopep8
        return self._cards[3].get_value("es1")

    @es1.setter
    def es1(self, value: float) -> None:
        self._cards[3].set_value("es1", value)

    @property
    def es2(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS2
        """ # nopep8
        return self._cards[3].get_value("es2")

    @es2.setter
    def es2(self, value: float) -> None:
        self._cards[3].set_value("es2", value)

    @property
    def es3(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS3
        """ # nopep8
        return self._cards[3].get_value("es3")

    @es3.setter
    def es3(self, value: float) -> None:
        self._cards[3].set_value("es3", value)

    @property
    def es4(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS4
        """ # nopep8
        return self._cards[3].get_value("es4")

    @es4.setter
    def es4(self, value: float) -> None:
        self._cards[3].set_value("es4", value)

    @property
    def es5(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS5
        """ # nopep8
        return self._cards[3].get_value("es5")

    @es5.setter
    def es5(self, value: float) -> None:
        self._cards[3].set_value("es5", value)

    @property
    def es6(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS6
        """ # nopep8
        return self._cards[3].get_value("es6")

    @es6.setter
    def es6(self, value: float) -> None:
        self._cards[3].set_value("es6", value)

    @property
    def es7(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS7
        """ # nopep8
        return self._cards[3].get_value("es7")

    @es7.setter
    def es7(self, value: float) -> None:
        self._cards[3].set_value("es7", value)

    @property
    def es8(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress values to EPS8
        """ # nopep8
        return self._cards[3].get_value("es8")

    @es8.setter
    def es8(self, value: float) -> None:
        self._cards[3].set_value("es8", value)

    @property
    def l1(self) -> typing.Optional[float]:
        """Get or set the Element length values.These values are only used if LCLF=0.
        """ # nopep8
        return self._cards[4].get_value("l1")

    @l1.setter
    def l1(self, value: float) -> None:
        self._cards[4].set_value("l1", value)

    @property
    def l2(self) -> typing.Optional[float]:
        """Get or set the Element length values.
        """ # nopep8
        return self._cards[4].get_value("l2")

    @l2.setter
    def l2(self, value: float) -> None:
        self._cards[4].set_value("l2", value)

    @property
    def l3(self) -> typing.Optional[float]:
        """Get or set the Element length values.
        """ # nopep8
        return self._cards[4].get_value("l3")

    @l3.setter
    def l3(self, value: float) -> None:
        self._cards[4].set_value("l3", value)

    @property
    def l4(self) -> typing.Optional[float]:
        """Get or set the Element length values.
        """ # nopep8
        return self._cards[4].get_value("l4")

    @l4.setter
    def l4(self, value: float) -> None:
        self._cards[4].set_value("l4", value)

    @property
    def ff1(self) -> typing.Optional[float]:
        """Get or set the Corresponding failure void volume fraction. These values are only used if LCLF=0.
        """ # nopep8
        return self._cards[4].get_value("ff1")

    @ff1.setter
    def ff1(self, value: float) -> None:
        self._cards[4].set_value("ff1", value)

    @property
    def ff2(self) -> typing.Optional[float]:
        """Get or set the Corresponding failure void volume fraction.
        """ # nopep8
        return self._cards[4].get_value("ff2")

    @ff2.setter
    def ff2(self, value: float) -> None:
        self._cards[4].set_value("ff2", value)

    @property
    def ff3(self) -> typing.Optional[float]:
        """Get or set the Corresponding failure void volume fraction.
        """ # nopep8
        return self._cards[4].get_value("ff3")

    @ff3.setter
    def ff3(self, value: float) -> None:
        self._cards[4].set_value("ff3", value)

    @property
    def ff4(self) -> typing.Optional[float]:
        """Get or set the Corresponding failure void volume fraction.
        """ # nopep8
        return self._cards[4].get_value("ff4")

    @ff4.setter
    def ff4(self, value: float) -> None:
        self._cards[4].set_value("ff4", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID defining effective stress versus effective plastic strain. ATYP is ignored with this option.
        """ # nopep8
        return self._cards[5].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[5].set_value("lcss", value)

    @property
    def lclf(self) -> int:
        """Get or set the Load curve ID defining failure void volume fraction versus element length. The values L1-L4 and FF1-FF4 are ignored with this option.
        """ # nopep8
        return self._cards[5].get_value("lclf")

    @lclf.setter
    def lclf(self, value: int) -> None:
        self._cards[5].set_value("lclf", value)

    @property
    def numint(self) -> float:
        """Get or set the Number of through thickness integration points which must fail before the element is deleted.
        """ # nopep8
        return self._cards[5].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        self._cards[5].set_value("numint", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Parameter alpha. for the Rc-Dc model
        """ # nopep8
        return self._cards[6].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[6].set_value("alpha", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Parameter beta. for the Rc-Dc model
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[6].set_value("beta", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Parameter gamma. for the Rc-Dc model
        """ # nopep8
        return self._cards[6].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[6].set_value("gamma", value)

    @property
    def d0(self) -> typing.Optional[float]:
        """Get or set the Parameter D0. for the Rc-Dc model
        """ # nopep8
        return self._cards[6].get_value("d0")

    @d0.setter
    def d0(self, value: float) -> None:
        self._cards[6].set_value("d0", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Parameter b. for the Rc-Dc model
        """ # nopep8
        return self._cards[6].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[6].set_value("b", value)

    @property
    def lambda_(self) -> typing.Optional[float]:
        """Get or set the Parameter lambda. for the Rc-Dc model
        """ # nopep8
        return self._cards[6].get_value("lambda")

    @lambda_.setter
    def lambda_(self, value: float) -> None:
        self._cards[6].set_value("lambda", value)

    @property
    def ds(self) -> typing.Optional[float]:
        """Get or set the Parameter Ds. for the Rc-Dc model
        """ # nopep8
        return self._cards[6].get_value("ds")

    @ds.setter
    def ds(self, value: float) -> None:
        self._cards[6].set_value("ds", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Characteristic element length for this material
        """ # nopep8
        return self._cards[6].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        self._cards[6].set_value("l", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

