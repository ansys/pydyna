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

class Mat252(KeywordBase):
    """DYNA MAT_252 keyword"""

    keyword = "MAT"
    subkeyword = "252"
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
                        "flg",
                        int,
                        40,
                        10,
                        kwargs.get("flg", 0)
                    ),
                    Field(
                        "jcfl",
                        int,
                        50,
                        10,
                        kwargs.get("jcfl", 0)
                    ),
                    Field(
                        "dopt",
                        int,
                        60,
                        10,
                        kwargs.get("dopt")
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
                        kwargs.get("lcss")
                    ),
                    Field(
                        "tau0",
                        float,
                        10,
                        10,
                        kwargs.get("tau0")
                    ),
                    Field(
                        "q",
                        float,
                        20,
                        10,
                        kwargs.get("q")
                    ),
                    Field(
                        "b",
                        float,
                        30,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "h",
                        float,
                        40,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "c",
                        float,
                        50,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "gam0",
                        float,
                        60,
                        10,
                        kwargs.get("gam0")
                    ),
                    Field(
                        "gamm",
                        float,
                        70,
                        10,
                        kwargs.get("gamm")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a10",
                        float,
                        0,
                        10,
                        kwargs.get("a10")
                    ),
                    Field(
                        "a20",
                        float,
                        10,
                        10,
                        kwargs.get("a20")
                    ),
                    Field(
                        "a1h",
                        float,
                        20,
                        10,
                        kwargs.get("a1h")
                    ),
                    Field(
                        "a2h",
                        float,
                        30,
                        10,
                        kwargs.get("a2h")
                    ),
                    Field(
                        "a2s",
                        float,
                        40,
                        10,
                        kwargs.get("a2s")
                    ),
                    Field(
                        "pow",
                        float,
                        50,
                        10,
                        kwargs.get("pow")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "srfilt",
                        float,
                        70,
                        10,
                        kwargs.get("srfilt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "d1",
                        float,
                        20,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        30,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        40,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "d4",
                        float,
                        50,
                        10,
                        kwargs.get("d4")
                    ),
                    Field(
                        "d1c",
                        float,
                        60,
                        10,
                        kwargs.get("d1c")
                    ),
                    Field(
                        "d2c",
                        float,
                        70,
                        10,
                        kwargs.get("d2c")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat252.option_specs[0],
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
    def flg(self) -> int:
        """Get or set the Flag to choose between yield functions f and f^, see Remarks.
        EQ.0.0: Cap in tension and nonlinear Drucker & Prager in compression,
        EQ.2.0: Cap in tension. and von Mises in compression
        """ # nopep8
        return self._cards[0].get_value("flg")

    @flg.setter
    def flg(self, value: int) -> None:
        if value not in [0.0, 2.0]:
            raise Exception("""flg must be one of {0.0,2.0}""")
        self._cards[0].set_value("flg", value)

    @property
    def jcfl(self) -> int:
        """Get or set the Johnson & Cook constitutive failure criterion flag, see Remarks.
        EQ.0.0: use triaxiality factor only in tension,
        EQ.1.0: use triaxiality factor in tension and compression
        """ # nopep8
        return self._cards[0].get_value("jcfl")

    @jcfl.setter
    def jcfl(self, value: int) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""jcfl must be one of {0.0,1.0}""")
        self._cards[0].set_value("jcfl", value)

    @property
    def dopt(self) -> typing.Optional[int]:
        """Get or set the Damage criterion flag D or D^, see Remarks.
        EQ.0.0: damage model uses damage plastic strain r.
        damage model uses plastic arc length rv
        """ # nopep8
        return self._cards[0].get_value("dopt")

    @dopt.setter
    def dopt(self, value: int) -> None:
        self._cards[0].set_value("dopt", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Curve ID or Table ID.
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        self._cards[1].set_value("lcss", value)

    @property
    def tau0(self) -> typing.Optional[float]:
        """Get or set the Initial shear yield stress.
        """ # nopep8
        return self._cards[1].get_value("tau0")

    @tau0.setter
    def tau0(self, value: float) -> None:
        self._cards[1].set_value("tau0", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Isotropic nonlinear hardening modulus q.
        """ # nopep8
        return self._cards[1].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[1].set_value("q", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Isotropic exponential decay parameter b.
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[1].set_value("b", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Isotropic linear hardening modulus H
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[1].set_value("h", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate coefficient C
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[1].set_value("c", value)

    @property
    def gam0(self) -> typing.Optional[float]:
        """Get or set the Quasi-static threshold strain rate
        """ # nopep8
        return self._cards[1].get_value("gam0")

    @gam0.setter
    def gam0(self, value: float) -> None:
        self._cards[1].set_value("gam0", value)

    @property
    def gamm(self) -> typing.Optional[float]:
        """Get or set the Maximum threshold strain rate
        """ # nopep8
        return self._cards[1].get_value("gamm")

    @gamm.setter
    def gamm(self, value: float) -> None:
        self._cards[1].set_value("gamm", value)

    @property
    def a10(self) -> typing.Optional[float]:
        """Get or set the Yield function parameter: initial value.
        """ # nopep8
        return self._cards[2].get_value("a10")

    @a10.setter
    def a10(self, value: float) -> None:
        self._cards[2].set_value("a10", value)

    @property
    def a20(self) -> typing.Optional[float]:
        """Get or set the Yield function parameter: initial value.
        """ # nopep8
        return self._cards[2].get_value("a20")

    @a20.setter
    def a20(self, value: float) -> None:
        self._cards[2].set_value("a20", value)

    @property
    def a1h(self) -> typing.Optional[float]:
        """Get or set the Yield function parameter for formative hardening.
        """ # nopep8
        return self._cards[2].get_value("a1h")

    @a1h.setter
    def a1h(self, value: float) -> None:
        self._cards[2].set_value("a1h", value)

    @property
    def a2h(self) -> typing.Optional[float]:
        """Get or set the Yield function parameter for formative hardening.
        """ # nopep8
        return self._cards[2].get_value("a2h")

    @a2h.setter
    def a2h(self, value: float) -> None:
        self._cards[2].set_value("a2h", value)

    @property
    def a2s(self) -> typing.Optional[float]:
        """Get or set the Plastic potential parameter for hydrostatic stress term
        """ # nopep8
        return self._cards[2].get_value("a2s")

    @a2s.setter
    def a2s(self, value: float) -> None:
        self._cards[2].set_value("a2s", value)

    @property
    def pow(self) -> typing.Optional[float]:
        """Get or set the Exponent of the phenomenological damage model
        """ # nopep8
        return self._cards[2].get_value("pow")

    @pow.setter
    def pow(self, value: float) -> None:
        self._cards[2].set_value("pow", value)

    @property
    def srfilt(self) -> typing.Optional[float]:
        """Get or set the Strain rate filtering parameter in exponential moving average with admissible values ranging from 0 to 1
        """ # nopep8
        return self._cards[2].get_value("srfilt")

    @srfilt.setter
    def srfilt(self, value: float) -> None:
        self._cards[2].set_value("srfilt", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook failure parameter d1.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook failure parameter d2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook failure parameter d3
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def d4(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook rate dependent failure parameter d4.
        """ # nopep8
        return self._cards[3].get_value("d4")

    @d4.setter
    def d4(self, value: float) -> None:
        self._cards[3].set_value("d4", value)

    @property
    def d1c(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook damage threshold parameter d1c
        """ # nopep8
        return self._cards[3].get_value("d1c")

    @d1c.setter
    def d1c(self, value: float) -> None:
        self._cards[3].set_value("d1c", value)

    @property
    def d2c(self) -> typing.Optional[float]:
        """Get or set the Johnson & Cook damage threshold parameter d2c
        """ # nopep8
        return self._cards[3].get_value("d2c")

    @d2c.setter
    def d2c(self, value: float) -> None:
        self._cards[3].set_value("d2c", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

