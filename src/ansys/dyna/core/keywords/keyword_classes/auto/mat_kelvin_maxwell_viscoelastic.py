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

class MatKelvinMaxwellViscoelastic(KeywordBase):
    """DYNA MAT_KELVIN-MAXWELL_VISCOELASTIC keyword"""

    keyword = "MAT"
    subkeyword = "KELVIN-MAXWELL_VISCOELASTIC"
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
                        "g0",
                        float,
                        30,
                        10,
                        kwargs.get("g0")
                    ),
                    Field(
                        "gi",
                        float,
                        40,
                        10,
                        kwargs.get("gi")
                    ),
                    Field(
                        "dc",
                        float,
                        50,
                        10,
                        kwargs.get("dc")
                    ),
                    Field(
                        "fo",
                        float,
                        60,
                        10,
                        kwargs.get("fo", 0.0)
                    ),
                    Field(
                        "so",
                        float,
                        70,
                        10,
                        kwargs.get("so", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatKelvinMaxwellViscoelastic.option_specs[0],
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
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus (elastic).
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        self._cards[0].set_value("bulk", value)

    @property
    def g0(self) -> typing.Optional[float]:
        """Get or set the Short-time shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g0")

    @g0.setter
    def g0(self, value: float) -> None:
        self._cards[0].set_value("g0", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Long-time (infinite) shear modulus.
        """ # nopep8
        return self._cards[0].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        self._cards[0].set_value("gi", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Maxwell decay constant, beta, if FO=0.0 or
        Kelvin relaxation constant, tau if FO=1.0
        """ # nopep8
        return self._cards[0].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[0].set_value("dc", value)

    @property
    def fo(self) -> float:
        """Get or set the Formulation option:
        EQ.0.0: Maxwell (default),
        EQ.1.0: Kelvin.
        """ # nopep8
        return self._cards[0].get_value("fo")

    @fo.setter
    def fo(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""fo must be one of {0.0,1.0}""")
        self._cards[0].set_value("fo", value)

    @property
    def so(self) -> float:
        """Get or set the Strain (logarithmic) output option to be plotted as component 7 in D3PLOT file which is the effective plastic strain component (maximum values of each element are updated every timestep).
        EQ.0.0: maximum principal strain that occurs during the calculation (default),
        EQ.1.0: maximum magnitude of the principal strain values that occurs during the calculation,
        EQ.2.0: maximum effective strain that occurs during the calculation.
        """ # nopep8
        return self._cards[0].get_value("so")

    @so.setter
    def so(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0]:
            raise Exception("""so must be one of {0.0,1.0,2.0}""")
        self._cards[0].set_value("so", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

