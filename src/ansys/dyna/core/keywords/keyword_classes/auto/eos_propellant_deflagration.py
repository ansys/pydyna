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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class EosPropellantDeflagration(KeywordBase):
    """DYNA EOS_PROPELLANT_DEFLAGRATION keyword"""

    keyword = "EOS"
    subkeyword = "PROPELLANT_DEFLAGRATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                    Field(
                        "a",
                        float,
                        10,
                        10,
                        kwargs.get("a", 0.0)
                    ),
                    Field(
                        "b",
                        float,
                        20,
                        10,
                        kwargs.get("b", 0.0)
                    ),
                    Field(
                        "xp1",
                        float,
                        30,
                        10,
                        kwargs.get("xp1", 0.0)
                    ),
                    Field(
                        "xp2",
                        float,
                        40,
                        10,
                        kwargs.get("xp2", 0.0)
                    ),
                    Field(
                        "frer",
                        float,
                        50,
                        10,
                        kwargs.get("frer", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "g",
                        float,
                        0,
                        10,
                        kwargs.get("g", 0.0)
                    ),
                    Field(
                        "r1",
                        float,
                        10,
                        10,
                        kwargs.get("r1", 0.0)
                    ),
                    Field(
                        "r2",
                        float,
                        20,
                        10,
                        kwargs.get("r2", 0.0)
                    ),
                    Field(
                        "r3",
                        float,
                        30,
                        10,
                        kwargs.get("r3", 0.0)
                    ),
                    Field(
                        "r5",
                        float,
                        40,
                        10,
                        kwargs.get("r5", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r6",
                        float,
                        0,
                        10,
                        kwargs.get("r6", 0.0)
                    ),
                    Field(
                        "fmxig",
                        float,
                        10,
                        10,
                        kwargs.get("fmxig", 0.0)
                    ),
                    Field(
                        "freq",
                        float,
                        20,
                        10,
                        kwargs.get("freq", 0.0)
                    ),
                    Field(
                        "grow1",
                        float,
                        30,
                        10,
                        kwargs.get("grow1", 0.0)
                    ),
                    Field(
                        "em",
                        float,
                        40,
                        10,
                        kwargs.get("em", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ar1",
                        float,
                        0,
                        10,
                        kwargs.get("ar1", 0.0)
                    ),
                    Field(
                        "es1",
                        float,
                        10,
                        10,
                        kwargs.get("es1", 0.0)
                    ),
                    Field(
                        "cvp",
                        float,
                        20,
                        10,
                        kwargs.get("cvp", 0.0)
                    ),
                    Field(
                        "cvr",
                        float,
                        30,
                        10,
                        kwargs.get("cvr", 0.0)
                    ),
                    Field(
                        "eetal",
                        float,
                        40,
                        10,
                        kwargs.get("eetal", 0.0)
                    ),
                    Field(
                        "ccrit",
                        float,
                        50,
                        10,
                        kwargs.get("ccrit", 0.0)
                    ),
                    Field(
                        "enq",
                        float,
                        60,
                        10,
                        kwargs.get("enq", 0.0)
                    ),
                    Field(
                        "tmp0",
                        float,
                        70,
                        10,
                        kwargs.get("tmp0", 298.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "grow2",
                        float,
                        0,
                        10,
                        kwargs.get("grow2", 0.0)
                    ),
                    Field(
                        "ar2",
                        float,
                        10,
                        10,
                        kwargs.get("ar2", 0.0)
                    ),
                    Field(
                        "es2",
                        float,
                        20,
                        10,
                        kwargs.get("es2", 0.0)
                    ),
                    Field(
                        "en",
                        float,
                        30,
                        10,
                        kwargs.get("en", 0.0)
                    ),
                    Field(
                        "fmxgr",
                        float,
                        40,
                        10,
                        kwargs.get("fmxgr", 0.0)
                    ),
                    Field(
                        "fmngr",
                        float,
                        50,
                        10,
                        kwargs.get("fmngr", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state label.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def a(self) -> float:
        """Get or set the Product JWL coefficient.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> float:
        """Get or set the Product JWL coefficient.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[0].set_value("b", value)

    @property
    def xp1(self) -> float:
        """Get or set the Product JWL coefficient.
        """ # nopep8
        return self._cards[0].get_value("xp1")

    @xp1.setter
    def xp1(self, value: float) -> None:
        self._cards[0].set_value("xp1", value)

    @property
    def xp2(self) -> float:
        """Get or set the Product JWL coefficient.
        """ # nopep8
        return self._cards[0].get_value("xp2")

    @xp2.setter
    def xp2(self, value: float) -> None:
        self._cards[0].set_value("xp2", value)

    @property
    def frer(self) -> float:
        """Get or set the Unreacted Co-volume.
        """ # nopep8
        return self._cards[0].get_value("frer")

    @frer.setter
    def frer(self, value: float) -> None:
        self._cards[0].set_value("frer", value)

    @property
    def g(self) -> float:
        """Get or set the Product wCv.
        """ # nopep8
        return self._cards[1].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[1].set_value("g", value)

    @property
    def r1(self) -> float:
        """Get or set the Unreacted JWL coefficient.
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        self._cards[1].set_value("r1", value)

    @property
    def r2(self) -> float:
        """Get or set the Unreacted JWL coefficient.
        """ # nopep8
        return self._cards[1].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        self._cards[1].set_value("r2", value)

    @property
    def r3(self) -> float:
        """Get or set the Unreacted wCv.
        """ # nopep8
        return self._cards[1].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        self._cards[1].set_value("r3", value)

    @property
    def r5(self) -> float:
        """Get or set the Unreacted JWL coefficient.
        """ # nopep8
        return self._cards[1].get_value("r5")

    @r5.setter
    def r5(self, value: float) -> None:
        self._cards[1].set_value("r5", value)

    @property
    def r6(self) -> float:
        """Get or set the Unreacted JWL coefficient.
        """ # nopep8
        return self._cards[2].get_value("r6")

    @r6.setter
    def r6(self, value: float) -> None:
        self._cards[2].set_value("r6", value)

    @property
    def fmxig(self) -> float:
        """Get or set the Initial Fraction Reacted Fo.
        """ # nopep8
        return self._cards[2].get_value("fmxig")

    @fmxig.setter
    def fmxig(self, value: float) -> None:
        self._cards[2].set_value("fmxig", value)

    @property
    def freq(self) -> float:
        """Get or set the Initial Pressure Po.
        """ # nopep8
        return self._cards[2].get_value("freq")

    @freq.setter
    def freq(self, value: float) -> None:
        self._cards[2].set_value("freq", value)

    @property
    def grow1(self) -> float:
        """Get or set the First burn rate coefficient.
        """ # nopep8
        return self._cards[2].get_value("grow1")

    @grow1.setter
    def grow1(self, value: float) -> None:
        self._cards[2].set_value("grow1", value)

    @property
    def em(self) -> float:
        """Get or set the Pressure Exponent (1st term).
        """ # nopep8
        return self._cards[2].get_value("em")

    @em.setter
    def em(self, value: float) -> None:
        self._cards[2].set_value("em", value)

    @property
    def ar1(self) -> float:
        """Get or set the Exponent on F (1st term).
        """ # nopep8
        return self._cards[3].get_value("ar1")

    @ar1.setter
    def ar1(self, value: float) -> None:
        self._cards[3].set_value("ar1", value)

    @property
    def es1(self) -> float:
        """Get or set the Exponent on (1-F) (1st term).
        """ # nopep8
        return self._cards[3].get_value("es1")

    @es1.setter
    def es1(self, value: float) -> None:
        self._cards[3].set_value("es1", value)

    @property
    def cvp(self) -> float:
        """Get or set the Heat capacity products.
        """ # nopep8
        return self._cards[3].get_value("cvp")

    @cvp.setter
    def cvp(self, value: float) -> None:
        self._cards[3].set_value("cvp", value)

    @property
    def cvr(self) -> float:
        """Get or set the Heat capacity unreacted.
        """ # nopep8
        return self._cards[3].get_value("cvr")

    @cvr.setter
    def cvr(self, value: float) -> None:
        self._cards[3].set_value("cvr", value)

    @property
    def eetal(self) -> float:
        """Get or set the Extra, not presently used.
        """ # nopep8
        return self._cards[3].get_value("eetal")

    @eetal.setter
    def eetal(self, value: float) -> None:
        self._cards[3].set_value("eetal", value)

    @property
    def ccrit(self) -> float:
        """Get or set the Product co-volume.
        """ # nopep8
        return self._cards[3].get_value("ccrit")

    @ccrit.setter
    def ccrit(self, value: float) -> None:
        self._cards[3].set_value("ccrit", value)

    @property
    def enq(self) -> float:
        """Get or set the Heat of Reaction.
        """ # nopep8
        return self._cards[3].get_value("enq")

    @enq.setter
    def enq(self, value: float) -> None:
        self._cards[3].set_value("enq", value)

    @property
    def tmp0(self) -> float:
        """Get or set the Initial Temperature (default = 298°K).
        """ # nopep8
        return self._cards[3].get_value("tmp0")

    @tmp0.setter
    def tmp0(self, value: float) -> None:
        self._cards[3].set_value("tmp0", value)

    @property
    def grow2(self) -> float:
        """Get or set the Second burn rate coefficient.
        """ # nopep8
        return self._cards[4].get_value("grow2")

    @grow2.setter
    def grow2(self, value: float) -> None:
        self._cards[4].set_value("grow2", value)

    @property
    def ar2(self) -> float:
        """Get or set the Exponent on F (2nd term).
        """ # nopep8
        return self._cards[4].get_value("ar2")

    @ar2.setter
    def ar2(self, value: float) -> None:
        self._cards[4].set_value("ar2", value)

    @property
    def es2(self) -> float:
        """Get or set the Exponent on (1-F) (2nd term).
        """ # nopep8
        return self._cards[4].get_value("es2")

    @es2.setter
    def es2(self, value: float) -> None:
        self._cards[4].set_value("es2", value)

    @property
    def en(self) -> float:
        """Get or set the Pressure Exponent (2nd term).
        """ # nopep8
        return self._cards[4].get_value("en")

    @en.setter
    def en(self, value: float) -> None:
        self._cards[4].set_value("en", value)

    @property
    def fmxgr(self) -> float:
        """Get or set the Maximum F for 1st term.
        """ # nopep8
        return self._cards[4].get_value("fmxgr")

    @fmxgr.setter
    def fmxgr(self, value: float) -> None:
        self._cards[4].set_value("fmxgr", value)

    @property
    def fmngr(self) -> float:
        """Get or set the Minimum F for 2nd term.
        """ # nopep8
        return self._cards[4].get_value("fmngr")

    @fmngr.setter
    def fmngr(self, value: float) -> None:
        self._cards[4].set_value("fmngr", value)

