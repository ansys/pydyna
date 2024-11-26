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

class MatCrushableFoam(KeywordBase):
    """DYNA MAT_CRUSHABLE_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "CRUSHABLE_FOAM"
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
                        "lcid",
                        int,
                        40,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "tsc",
                        float,
                        50,
                        10,
                        kwargs.get("tsc")
                    ),
                    Field(
                        "damp",
                        float,
                        60,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "model",
                        int,
                        70,
                        10,
                        kwargs.get("model", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatCrushableFoam.option_specs[0],
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
        """Get or set the Young’s modulus. For MODEL = 0, E may affect contact stiffness but otherwise is not used.  The final slope of curve LCID determines the elastic stiffness for loading and unloading.  This slope is also used in the time step calculation. For MODEL = 1, E is really used as Young’s modulus in the material law
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
    def lcid(self) -> typing.Optional[int]:
        """Get or set the MODEL.EQ.0:	load curve ID defining yield stress as a function of volumetric strain,  (see Figure 0-1).
        MODEL.EQ.1:	load curve or table ID.If a load curve ID is specified,
        then the load curve defines uniaxial yield stress as a function of equivalent plastic strain.If a table ID is specified,
        then each strain rate references a load curve ID that gives uniaxial yield stress as a function of equivalent plastic strain.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def tsc(self) -> typing.Optional[float]:
        """Get or set the Tensile stress cutoff (only for MODEL = 0). A nonzero, positive value is strongly recommended for realistic behavior.
        """ # nopep8
        return self._cards[0].get_value("tsc")

    @tsc.setter
    def tsc(self, value: float) -> None:
        self._cards[0].set_value("tsc", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Rate sensitivity via damping coefficient (.05 <  recommended value < .50). Only available for MODEL = 0.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[0].set_value("damp", value)

    @property
    def model(self) -> int:
        """Get or set the Choice of material model formulation:
        EQ.0:	von Mises yield condition(default),
        EQ.1 : elliptical yield surface in - space.
        """ # nopep8
        return self._cards[0].get_value("model")

    @model.setter
    def model(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""model must be one of {0,1}""")
        self._cards[0].set_value("model", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

