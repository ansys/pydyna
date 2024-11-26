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

class MatSphIncompressibleStructure(KeywordBase):
    """DYNA MAT_SPH_INCOMPRESSIBLE_STRUCTURE keyword"""

    keyword = "MAT"
    subkeyword = "SPH_INCOMPRESSIBLE_STRUCTURE"
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
                        "beta",
                        float,
                        20,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "rough",
                        float,
                        30,
                        10,
                        kwargs.get("rough")
                    ),
                    Field(
                        "adh",
                        float,
                        40,
                        10,
                        kwargs.get("adh")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSphIncompressibleStructure.option_specs[0],
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
        """Get or set the Mass density. This should be set to the rest density of the fluid.
        The actual mass of the structure will be calculated from the parent surfaces sampled with the *DEFINE_SPH_MESH_SURFACE keyword.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Numerical surface adhesion coefficient. For water, a value of β=1000 m/s^2 is recommended. Only used if IMAT=0 in *CONTROL_SPH.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def rough(self) -> typing.Optional[float]:
        """Get or set the Surface roughness coefficient. A friction force between the structure and the fluid is generated based on the viscosity of the fluid, scaled by this coefficient. A value between 0.0 and 10.0 is usually recommended
        """ # nopep8
        return self._cards[0].get_value("rough")

    @rough.setter
    def rough(self, value: float) -> None:
        self._cards[0].set_value("rough", value)

    @property
    def adh(self) -> typing.Optional[float]:
        """Get or set the Surface adhesion scaling coefficient. It is only used if IMAT=1 in *CONTROL_SPH. An attractive force between fluid and structure is calculated based on surface tension forces in the fluid and then, scaled by ADH.
        """ # nopep8
        return self._cards[0].get_value("adh")

    @adh.setter
    def adh(self, value: float) -> None:
        self._cards[0].set_value("adh", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

