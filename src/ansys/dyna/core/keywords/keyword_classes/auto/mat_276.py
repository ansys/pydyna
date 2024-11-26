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

class Mat276(KeywordBase):
    """DYNA MAT_276 keyword"""

    keyword = "MAT"
    subkeyword = "276"
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
                        "bulk",
                        float,
                        20,
                        10,
                        kwargs.get("bulk")
                    ),
                    Field(
                        "pcf",
                        float,
                        30,
                        10,
                        kwargs.get("pcf")
                    ),
                    Field(
                        "ef",
                        float,
                        40,
                        10,
                        kwargs.get("ef")
                    ),
                    Field(
                        "tref",
                        float,
                        50,
                        10,
                        kwargs.get("tref")
                    ),
                    Field(
                        "a",
                        float,
                        60,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        70,
                        10,
                        kwargs.get("b")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "nt",
                        int,
                        10,
                        10,
                        kwargs.get("nt")
                    ),
                    Field(
                        "bstart",
                        float,
                        20,
                        10,
                        kwargs.get("bstart")
                    ),
                    Field(
                        "tramp",
                        float,
                        30,
                        10,
                        kwargs.get("tramp")
                    ),
                    Field(
                        "lcidk",
                        int,
                        40,
                        10,
                        kwargs.get("lcidk")
                    ),
                    Field(
                        "ntk",
                        int,
                        50,
                        10,
                        kwargs.get("ntk")
                    ),
                    Field(
                        "bstartk",
                        float,
                        60,
                        10,
                        kwargs.get("bstartk")
                    ),
                    Field(
                        "trampk",
                        float,
                        70,
                        10,
                        kwargs.get("trampk")
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
                    Field(
                        "ki",
                        float,
                        20,
                        10,
                        kwargs.get("ki")
                    ),
                    Field(
                        "betaki",
                        float,
                        30,
                        10,
                        kwargs.get("betaki")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat276.option_specs[0],
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
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Elastic bulk modulus.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        self._cards[0].set_value("bulk", value)

    @property
    def pcf(self) -> typing.Optional[float]:
        """Get or set the Tensile pressure elimination flag for solid elements only. If set to unity tensile pressures are set to zero.
        """ # nopep8
        return self._cards[0].get_value("pcf")

    @pcf.setter
    def pcf(self, value: float) -> None:
        self._cards[0].set_value("pcf", value)

    @property
    def ef(self) -> typing.Optional[float]:
        """Get or set the Elastic flag (if equal 1, the layer is elastic. If 0 the layer is viscoelastic).
        """ # nopep8
        return self._cards[0].get_value("ef")

    @ef.setter
    def ef(self, value: float) -> None:
        self._cards[0].set_value("ef", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for shift function (must be greater than zero).
        """ # nopep8
        return self._cards[0].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        self._cards[0].set_value("tref", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Chronological coefficient.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Chronological coefficient.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[0].set_value("b", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for deviatoric behavior if constants, Gi, and Bi are determined via a least squares fit. This relaxation curve is shown below.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def nt(self) -> typing.Optional[int]:
        """Get or set the Number of terms in shear fit. If zero the default is 6. Fewer than NT terms will be used if the fit produces one or more negative shear moduli.Currently, the maximum number is set to 6.
        """ # nopep8
        return self._cards[1].get_value("nt")

    @nt.setter
    def nt(self, value: int) -> None:
        self._cards[1].set_value("nt", value)

    @property
    def bstart(self) -> typing.Optional[float]:
        """Get or set the In the fit, B1 is set to zero, B2 is set to BSTART, B3 is 10 times B2, B4 is 100 times greater than B3 , and so on. If zero, BSTART is determined by an iterative trial and error scheme.
        """ # nopep8
        return self._cards[1].get_value("bstart")

    @bstart.setter
    def bstart(self, value: float) -> None:
        self._cards[1].set_value("bstart", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for loading
        """ # nopep8
        return self._cards[1].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        self._cards[1].set_value("tramp", value)

    @property
    def lcidk(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for bulk behavior if constants, Ki, and BKi are determined via a least squares fit. This relaxation curve is shown below.
        """ # nopep8
        return self._cards[1].get_value("lcidk")

    @lcidk.setter
    def lcidk(self, value: int) -> None:
        self._cards[1].set_value("lcidk", value)

    @property
    def ntk(self) -> typing.Optional[int]:
        """Get or set the Number of terms desired in bulk fit. If zero the default is 6. Currently, the maximum number is set to 6.
        """ # nopep8
        return self._cards[1].get_value("ntk")

    @ntk.setter
    def ntk(self, value: int) -> None:
        self._cards[1].set_value("ntk", value)

    @property
    def bstartk(self) -> typing.Optional[float]:
        """Get or set the In the fit, BK1 is set to zero, BK2 is set to BSTARTK, BK3 is 10 times BK2, BK4 is 100 times greater than BK3 , and so on. If zero, BSTARTK is determined by an iterative trial and error scheme.
        """ # nopep8
        return self._cards[1].get_value("bstartk")

    @bstartk.setter
    def bstartk(self, value: float) -> None:
        self._cards[1].set_value("bstartk", value)

    @property
    def trampk(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for bulk loading.
        """ # nopep8
        return self._cards[1].get_value("trampk")

    @trampk.setter
    def trampk(self, value: float) -> None:
        self._cards[1].set_value("trampk", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Optional shear relaxation modulus for the ith term.
        """ # nopep8
        return self._cards[2].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        self._cards[2].set_value("gi", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the Optional shear decay constant for the ith term.
        """ # nopep8
        return self._cards[2].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        self._cards[2].set_value("betai", value)

    @property
    def ki(self) -> typing.Optional[float]:
        """Get or set the Optional bulk relaxation modulus for the ith term.
        """ # nopep8
        return self._cards[2].get_value("ki")

    @ki.setter
    def ki(self, value: float) -> None:
        self._cards[2].set_value("ki", value)

    @property
    def betaki(self) -> typing.Optional[float]:
        """Get or set the Optional bulk decay constant for the ith term
        """ # nopep8
        return self._cards[2].get_value("betaki")

    @betaki.setter
    def betaki(self, value: float) -> None:
        self._cards[2].set_value("betaki", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

