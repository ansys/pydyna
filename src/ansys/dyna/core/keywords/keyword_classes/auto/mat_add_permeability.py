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

class MatAddPermeability(KeywordBase):
    """DYNA MAT_ADD_PERMEABILITY keyword"""

    keyword = "MAT"
    subkeyword = "ADD_PERMEABILITY"
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
                        "perm",
                        float,
                        10,
                        10,
                        kwargs.get("perm")
                    ),
                    Field(
                        "permy",
                        int,
                        20,
                        10,
                        kwargs.get("permy")
                    ),
                    Field(
                        "permz",
                        int,
                        30,
                        10,
                        kwargs.get("permz")
                    ),
                    Field(
                        "thexp",
                        float,
                        40,
                        10,
                        kwargs.get("thexp")
                    ),
                    Field(
                        "lckz",
                        int,
                        50,
                        10,
                        kwargs.get("lckz")
                    ),
                    Field(
                        "pmtyp",
                        int,
                        60,
                        10,
                        kwargs.get("pmtyp", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatAddPermeability.option_specs[0],
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
        """Get or set the Material identification - must be same as the structural material.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def perm(self) -> typing.Optional[float]:
        """Get or set the Permeability or load curve ID defining permeability, depending on the definition of PMTYP below. If PERMY and PERMZ are nonzero, then PERM gives the permeability in the global X direction.  See Remark 3.
        """ # nopep8
        return self._cards[0].get_value("perm")

    @perm.setter
    def perm(self, value: float) -> None:
        self._cards[0].set_value("perm", value)

    @property
    def permy(self) -> typing.Optional[int]:
        """Get or set the Optional permeability or load curve ID defining permeability in the global Y direction, depending on the definition of PMTYP below
        """ # nopep8
        return self._cards[0].get_value("permy")

    @permy.setter
    def permy(self, value: int) -> None:
        self._cards[0].set_value("permy", value)

    @property
    def permz(self) -> typing.Optional[int]:
        """Get or set the Optional permeability or load curve ID defining permeability in the global Z direction, depending on the definition of PMTYP below
        """ # nopep8
        return self._cards[0].get_value("permz")

    @permz.setter
    def permz(self, value: int) -> None:
        self._cards[0].set_value("permz", value)

    @property
    def thexp(self) -> typing.Optional[float]:
        """Get or set the Undrained volumetric thermal expansion coefficient (see Remark 2):
        GE.0.0:	Constant undrained volumetric thermal expansion coefficient
        LT.0.0 : | THEXP | is the ID of a load curve giving the thermal expansion coefficient(y - axis) as a function of temperature(x - axis).
        """ # nopep8
        return self._cards[0].get_value("thexp")

    @thexp.setter
    def thexp(self, value: float) -> None:
        self._cards[0].set_value("thexp", value)

    @property
    def lckz(self) -> typing.Optional[int]:
        """Get or set the Load curve giving factor on PERM as a function of z-coordinate
        """ # nopep8
        return self._cards[0].get_value("lckz")

    @lckz.setter
    def lckz(self, value: int) -> None:
        self._cards[0].set_value("lckz", value)

    @property
    def pmtyp(self) -> int:
        """Get or set the Permeability definition type:
        EQ.0:	PERM is a constant.
        EQ.1 : PERM is a load curve ID giving permeability(y - axis) as a function of the volume ratio of current volume to volume in the stress free state(x - axis).
        EQ.2 : PERM is a load curve ID giving permeability(y - axis) as a function of effective plastic strain(x - axis) of materials other than MAT_072R3.For MAT_072R3, the x - axis is the output selector specified by NOUT; see* MAT_072R3.
        EQ.3:	PERM is a load curve ID giving permeability(y - axis) as a function of effective pressure(x - axis) which is positive when in compression.
        """ # nopep8
        return self._cards[0].get_value("pmtyp")

    @pmtyp.setter
    def pmtyp(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""pmtyp must be one of {0,1,2,3}""")
        self._cards[0].set_value("pmtyp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

