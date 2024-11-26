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

class MatSpotweldDamageFailureUniaxial(KeywordBase):
    """DYNA MAT_SPOTWELD_DAMAGE-FAILURE_UNIAXIAL keyword"""

    keyword = "MAT"
    subkeyword = "SPOTWELD_DAMAGE-FAILURE_UNIAXIAL"
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
                        "eh",
                        float,
                        50,
                        10,
                        kwargs.get("eh")
                    ),
                    Field(
                        "dt",
                        float,
                        60,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "tfail",
                        float,
                        70,
                        10,
                        kwargs.get("tfail")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "efail",
                        float,
                        0,
                        10,
                        kwargs.get("efail")
                    ),
                    Field(
                        "nrr",
                        float,
                        10,
                        10,
                        kwargs.get("nrr")
                    ),
                    Field(
                        "nrs",
                        float,
                        20,
                        10,
                        kwargs.get("nrs")
                    ),
                    Field(
                        "nrt",
                        float,
                        30,
                        10,
                        kwargs.get("nrt")
                    ),
                    Field(
                        "mrr",
                        float,
                        40,
                        10,
                        kwargs.get("mrr")
                    ),
                    Field(
                        "mss",
                        float,
                        50,
                        10,
                        kwargs.get("mss")
                    ),
                    Field(
                        "mtt",
                        float,
                        60,
                        10,
                        kwargs.get("mtt")
                    ),
                    Field(
                        "nf",
                        float,
                        70,
                        10,
                        kwargs.get("nf")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "efail",
                        float,
                        0,
                        10,
                        kwargs.get("efail")
                    ),
                    Field(
                        "sigax",
                        float,
                        10,
                        10,
                        kwargs.get("sigax")
                    ),
                    Field(
                        "sigtu",
                        float,
                        20,
                        10,
                        kwargs.get("sigtu")
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
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "nf",
                        float,
                        70,
                        10,
                        kwargs.get("nf")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSpotweldDamageFailureUniaxial.option_specs[0],
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
        """Get or set the GT.0:Initial yield stress.
        LT.0: A yield curve or table is assigned by |SIGY|.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[0].set_value("sigy", value)

    @property
    def eh(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus, Eh.
        """ # nopep8
        return self._cards[0].get_value("eh")

    @eh.setter
    def eh(self, value: float) -> None:
        self._cards[0].set_value("eh", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time step size for mass scaling, Delta t.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def tfail(self) -> typing.Optional[float]:
        """Get or set the Failure time if nonzero. If zero this option is ignored.
        """ # nopep8
        return self._cards[0].get_value("tfail")

    @tfail.setter
    def tfail(self, value: float) -> None:
        self._cards[0].set_value("tfail", value)

    @property
    def efail(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain in weld material at failure.  The plastic strain must exceed the rupture strain (RS) at each integration point before deletion occurs.  See Card 3.
        """ # nopep8
        return self._cards[1].get_value("efail")

    @efail.setter
    def efail(self, value: float) -> None:
        self._cards[1].set_value("efail", value)

    @property
    def nrr(self) -> typing.Optional[float]:
        """Get or set the Axial force resultant Nrrf or maximum axial stress at failure depending on the value of OPT (see below).
        If zero, failure due to this component is not considered.
        """ # nopep8
        return self._cards[1].get_value("nrr")

    @nrr.setter
    def nrr(self, value: float) -> None:
        self._cards[1].set_value("nrr", value)

    @property
    def nrs(self) -> typing.Optional[float]:
        """Get or set the Force resultant Nrsf or maximum shear stress Tf at failure depending on the value of OPT (see below).
        If zero, failure due to this component is not considered.
        """ # nopep8
        return self._cards[1].get_value("nrs")

    @nrs.setter
    def nrs(self, value: float) -> None:
        self._cards[1].set_value("nrs", value)

    @property
    def nrt(self) -> typing.Optional[float]:
        """Get or set the Force resultant NrtF at failure.
        If zero, failure due to this component is not considered.
        """ # nopep8
        return self._cards[1].get_value("nrt")

    @nrt.setter
    def nrt(self, value: float) -> None:
        self._cards[1].set_value("nrt", value)

    @property
    def mrr(self) -> typing.Optional[float]:
        """Get or set the Torsional moment resultant Mrrf at failure.
        If zero, failure due to this component is not considered.
        """ # nopep8
        return self._cards[1].get_value("mrr")

    @mrr.setter
    def mrr(self, value: float) -> None:
        self._cards[1].set_value("mrr", value)

    @property
    def mss(self) -> typing.Optional[float]:
        """Get or set the Moment resultant MssF at failure.
        If zero, failure due to this component is not considered.
        """ # nopep8
        return self._cards[1].get_value("mss")

    @mss.setter
    def mss(self, value: float) -> None:
        self._cards[1].set_value("mss", value)

    @property
    def mtt(self) -> typing.Optional[float]:
        """Get or set the Moment resultant MttF at failure.
        If zero, failure due to this component is not considered.
        """ # nopep8
        return self._cards[1].get_value("mtt")

    @mtt.setter
    def mtt(self, value: float) -> None:
        self._cards[1].set_value("mtt", value)

    @property
    def nf(self) -> typing.Optional[float]:
        """Get or set the Number of force vectors stored for filtering (default = 0). Default is recommended unless oscillatory resultant forces are observed in the time history databases. Even though these welds should not oscillate significantly, this option was added for consistency with the other spot weld options. NF affects the storage since it is necessary to store the resultant forces as history variables. When NF is nonzero, the resultants in the output databases are filtered.
        """ # nopep8
        return self._cards[1].get_value("nf")

    @nf.setter
    def nf(self, value: float) -> None:
        self._cards[1].set_value("nf", value)

    @property
    def efail(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain in weld material at failure.  The plastic strain must exceed the rupture strain (RS) at each integration point before deletion occurs.  See Card 3
        """ # nopep8
        return self._cards[2].get_value("efail")

    @efail.setter
    def efail(self, value: float) -> None:
        self._cards[2].set_value("efail", value)

    @property
    def sigax(self) -> typing.Optional[float]:
        """Get or set the Maximum axial stress  rr F at failure. If zero, failure due to this component is not considered.
        """ # nopep8
        return self._cards[2].get_value("sigax")

    @sigax.setter
    def sigax(self, value: float) -> None:
        self._cards[2].set_value("sigax", value)

    @property
    def sigtu(self) -> typing.Optional[float]:
        """Get or set the Maximum shear stress  F at failure. If zero, failure due to this component is not considered.
        """ # nopep8
        return self._cards[2].get_value("sigtu")

    @sigtu.setter
    def sigtu(self, value: float) -> None:
        self._cards[2].set_value("sigtu", value)

    @property
    def nf(self) -> typing.Optional[float]:
        """Get or set the Number of force vectors stored for filtering (default = 0). Default is recommended unless oscillatory resultant forces are observed in the time history databases. Even though these welds should not oscillate significantly, this option was added for consistency with the other spot weld options. NF affects the storage since it is necessary to store the resultant forces as history variables. When NF is nonzero, the resultants in the output databases are filtered.
        """ # nopep8
        return self._cards[2].get_value("nf")

    @nf.setter
    def nf(self, value: float) -> None:
        self._cards[2].set_value("nf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

