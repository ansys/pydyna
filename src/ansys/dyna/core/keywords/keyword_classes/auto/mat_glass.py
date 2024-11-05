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

class MatGlass(KeywordBase):
    """DYNA MAT_GLASS keyword"""

    keyword = "MAT"
    subkeyword = "GLASS"
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
                        "imod",
                        float,
                        60,
                        10,
                        kwargs.get("imod", 0.0)
                    ),
                    Field(
                        "ilaw",
                        float,
                        70,
                        10,
                        kwargs.get("ilaw", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fmod",
                        float,
                        0,
                        10,
                        kwargs.get("fmod", 0.0)
                    ),
                    Field(
                        "ft",
                        float,
                        10,
                        10,
                        kwargs.get("ft")
                    ),
                    Field(
                        "fc",
                        float,
                        20,
                        10,
                        kwargs.get("fc")
                    ),
                    Field(
                        "at",
                        float,
                        30,
                        10,
                        kwargs.get("at")
                    ),
                    Field(
                        "bt",
                        int,
                        40,
                        10,
                        kwargs.get("bt")
                    ),
                    Field(
                        "ac",
                        int,
                        50,
                        10,
                        kwargs.get("ac")
                    ),
                    Field(
                        "bc",
                        float,
                        60,
                        10,
                        kwargs.get("bc")
                    ),
                    Field(
                        "ftscl",
                        float,
                        70,
                        10,
                        kwargs.get("ftscl", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sfsti",
                        float,
                        0,
                        10,
                        kwargs.get("sfsti")
                    ),
                    Field(
                        "sfstr",
                        float,
                        10,
                        10,
                        kwargs.get("sfstr")
                    ),
                    Field(
                        "crin",
                        float,
                        20,
                        10,
                        kwargs.get("crin", 0.0)
                    ),
                    Field(
                        "ecrcl",
                        float,
                        30,
                        10,
                        kwargs.get("ecrcl")
                    ),
                    Field(
                        "ncycr",
                        float,
                        40,
                        10,
                        kwargs.get("ncycr")
                    ),
                    Field(
                        "nipf",
                        float,
                        50,
                        10,
                        kwargs.get("nipf")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epscr",
                        float,
                        0,
                        10,
                        kwargs.get("epscr")
                    ),
                    Field(
                        "engcrt",
                        float,
                        10,
                        10,
                        kwargs.get("engcrt")
                    ),
                    Field(
                        "radcrt",
                        float,
                        20,
                        10,
                        kwargs.get("radcrt")
                    ),
                    Field(
                        "ratenl",
                        float,
                        30,
                        10,
                        kwargs.get("ratenl")
                    ),
                    Field(
                        "rfiltf",
                        float,
                        40,
                        10,
                        kwargs.get("rfiltf")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatGlass.option_specs[0],
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
        """Get or set the the material density.
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
    def imod(self) -> float:
        """Get or set the Flag to choose degradation procedure, when critical stress is reached.
        EQ.0.0:	Softening in NCYCR load steps. Define SFSTI, SFSTR, and NCYCR (default).
        EQ.1.0:	Damage model for softening. Define ILAW, AT, BT, AC, and BC.
        EQ.2.0 : Drucker - Prager
        EQ.10.0 : Rankine with modified compressive failure
        EQ.11.0 : Mohr - Coulomb with modified compressive failure
        EQ.12.0 : Drucker - Prager with modified compressive failure
        """ # nopep8
        return self._cards[0].get_value("imod")

    @imod.setter
    def imod(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 10.0, 11.0, 12.0]:
            raise Exception("""imod must be one of {0.0,1.0,2.0,10.0,11.0,12.0}""")
        self._cards[0].set_value("imod", value)

    @property
    def ilaw(self) -> float:
        """Get or set the Flag to choose damage evolution law if IMOD=1.0, see Remarks.
        EQ.0.0:	Same damage evolution for tensile and compressive failure (default).
        EQ.1.0:	Different damage evolution for tensile failure and compressive failure.
        """ # nopep8
        return self._cards[0].get_value("ilaw")

    @ilaw.setter
    def ilaw(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ilaw must be one of {0.0,1.0}""")
        self._cards[0].set_value("ilaw", value)

    @property
    def fmod(self) -> float:
        """Get or set the Flag to choose between failure criteria, see Remarks.
        EQ.0.0: Rankine maximum stress (default),
        EQ.1.0: Mohr-Coulomb,
        EQ.2.0: Drucker-Prager.
        """ # nopep8
        return self._cards[1].get_value("fmod")

    @fmod.setter
    def fmod(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0]:
            raise Exception("""fmod must be one of {0.0,1.0,2.0}""")
        self._cards[1].set_value("fmod", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the GT.0.0:	constant value
        LT.0.0:	load curve ID = |FT| , which defines tensile strength as a function of effective strain rate(RFILTF is recommended).If used with FTSCL>0, |FT| defines a curve for tensile strength vs. strain rate and FTSCL scales the strength values from that curve as long as the material is intact. If cracked, neighbors get non-scaled values from that curve. RATENL is set to zero in that case.
        """ # nopep8
        return self._cards[1].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        self._cards[1].set_value("ft", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Compressive strength.
        """ # nopep8
        return self._cards[1].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        self._cards[1].set_value("fc", value)

    @property
    def at(self) -> typing.Optional[float]:
        """Get or set the Tensile damage evolution parameter α_t. Can be interpreted as the residual load carrying capacity ratio for tensile failure ranging from 0 to 1..
        """ # nopep8
        return self._cards[1].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        self._cards[1].set_value("at", value)

    @property
    def bt(self) -> typing.Optional[int]:
        """Get or set the Tensile damage evolution parameter β_t. It controls the softening velocity for tensile failure.
        """ # nopep8
        return self._cards[1].get_value("bt")

    @bt.setter
    def bt(self, value: int) -> None:
        self._cards[1].set_value("bt", value)

    @property
    def ac(self) -> typing.Optional[int]:
        """Get or set the Compressive damage evolution parameter α_t. Can be interpreted as the residual load carrying capacity ratio for compressive failure ranging from 0 to 1.
        """ # nopep8
        return self._cards[1].get_value("ac")

    @ac.setter
    def ac(self, value: int) -> None:
        self._cards[1].set_value("ac", value)

    @property
    def bc(self) -> typing.Optional[float]:
        """Get or set the Compressive damage evolution parameter β_t. It controls the softening velocity for compressive failure.
        """ # nopep8
        return self._cards[1].get_value("bc")

    @bc.setter
    def bc(self, value: float) -> None:
        self._cards[1].set_value("bc", value)

    @property
    def ftscl(self) -> float:
        """Get or set the Scale factor for the tensile strength: FTmod = FTSCL * FT.
        As soon as the first crack happens in the associated part, tensile strength drops to its original value, FT.
        Default value is 1.0, values >1.0 can be helpful to grasp high force peaks in impact events.
        """ # nopep8
        return self._cards[1].get_value("ftscl")

    @ftscl.setter
    def ftscl(self, value: float) -> None:
        self._cards[1].set_value("ftscl", value)

    @property
    def sfsti(self) -> typing.Optional[float]:
        """Get or set the Scale factor for stiffness in case of failure, e.g. SFSTI = 0.1 means
        that stiffness is reduced to 10% of the stiffness at failure.
        """ # nopep8
        return self._cards[2].get_value("sfsti")

    @sfsti.setter
    def sfsti(self, value: float) -> None:
        self._cards[2].set_value("sfsti", value)

    @property
    def sfstr(self) -> typing.Optional[float]:
        """Get or set the Scale factor for stress in case of failure, e.g. SFSTR = 0.1 means that
        stress is reduced to 10% of the stress at failure.
        """ # nopep8
        return self._cards[2].get_value("sfstr")

    @sfstr.setter
    def sfstr(self, value: float) -> None:
        self._cards[2].set_value("sfstr", value)

    @property
    def crin(self) -> float:
        """Get or set the Flag for crack strain initialization
        EQ.0.0: initial crack strain is strain at failure (default),
        EQ.1.0: initial crack strain is zero.
        """ # nopep8
        return self._cards[2].get_value("crin")

    @crin.setter
    def crin(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""crin must be one of {0.0,1.0}""")
        self._cards[2].set_value("crin", value)

    @property
    def ecrcl(self) -> typing.Optional[float]:
        """Get or set the Crack strain necessary to reactivate certain stress components after	crack closure..
        """ # nopep8
        return self._cards[2].get_value("ecrcl")

    @ecrcl.setter
    def ecrcl(self, value: float) -> None:
        self._cards[2].set_value("ecrcl", value)

    @property
    def ncycr(self) -> typing.Optional[float]:
        """Get or set the Number of cycles in which the stress is reduced to SFSTR*failure stress.
        """ # nopep8
        return self._cards[2].get_value("ncycr")

    @ncycr.setter
    def ncycr(self, value: float) -> None:
        self._cards[2].set_value("ncycr", value)

    @property
    def nipf(self) -> typing.Optional[float]:
        """Get or set the Number of failed through thickness integration points to fail all through thickness integration points.
        """ # nopep8
        return self._cards[2].get_value("nipf")

    @nipf.setter
    def nipf(self, value: float) -> None:
        self._cards[2].set_value("nipf", value)

    @property
    def epscr(self) -> typing.Optional[float]:
        """Get or set the Effective critical strain to trigger element deletion. This can be useful to get rid of highly distorted elements.
        """ # nopep8
        return self._cards[3].get_value("epscr")

    @epscr.setter
    def epscr(self, value: float) -> None:
        self._cards[3].set_value("epscr", value)

    @property
    def engcrt(self) -> typing.Optional[float]:
        """Get or set the Critical energy for nonlocal failure criterion; see Remark 6.
        """ # nopep8
        return self._cards[3].get_value("engcrt")

    @engcrt.setter
    def engcrt(self, value: float) -> None:
        self._cards[3].set_value("engcrt", value)

    @property
    def radcrt(self) -> typing.Optional[float]:
        """Get or set the Critical radius for nonlocal failure criterion; see Remark 6.
        """ # nopep8
        return self._cards[3].get_value("radcrt")

    @radcrt.setter
    def radcrt(self, value: float) -> None:
        self._cards[3].set_value("radcrt", value)

    @property
    def ratenl(self) -> typing.Optional[float]:
        """Get or set the Quasi-static strain rate threshold variable which activates a nonlocal, strain rate dependent tensile strength adaption; see Remark 7.
        """ # nopep8
        return self._cards[3].get_value("ratenl")

    @ratenl.setter
    def ratenl(self, value: float) -> None:
        self._cards[3].set_value("ratenl", value)

    @property
    def rfiltf(self) -> typing.Optional[float]:
        """Get or set the Smoothing factor on the effective strain rate for the evaluation of the current tensile strength if RATENL > 0.0; see Remark 7.
        """ # nopep8
        return self._cards[3].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        self._cards[3].set_value("rfiltf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

