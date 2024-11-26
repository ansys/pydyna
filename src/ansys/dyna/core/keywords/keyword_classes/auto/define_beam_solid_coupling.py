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

class DefineBeamSolidCoupling(KeywordBase):
    """DYNA DEFINE_BEAM_SOLID_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "BEAM_SOLID_COUPLING"
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
                        "lstrid",
                        int,
                        0,
                        10,
                        kwargs.get("lstrid")
                    ),
                    Field(
                        "msolidm",
                        int,
                        10,
                        10,
                        kwargs.get("msolidm")
                    ),
                    Field(
                        "lstrtype",
                        int,
                        20,
                        10,
                        kwargs.get("lstrtype", 0)
                    ),
                    Field(
                        "soltype",
                        int,
                        30,
                        10,
                        kwargs.get("soltype", 0)
                    ),
                    Field(
                        "form",
                        int,
                        40,
                        10,
                        kwargs.get("form", 0)
                    ),
                    Field(
                        "psf",
                        float,
                        50,
                        10,
                        kwargs.get("psf", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineBeamSolidCoupling.option_specs[0],
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
    def lstrid(self) -> typing.Optional[int]:
        """Get or set the Part set ID or part ID of the Lagrangian structure.  LSTRTYPE below indicates the ID type specified by LSTRTYPE.
        """ # nopep8
        return self._cards[0].get_value("lstrid")

    @lstrid.setter
    def lstrid(self, value: int) -> None:
        self._cards[0].set_value("lstrid", value)

    @property
    def msolidm(self) -> typing.Optional[int]:
        """Get or set the Part set ID or part ID of the solid block.  SOLTYPE below indicates the ID type specified by SOLTYPE.
        """ # nopep8
        return self._cards[0].get_value("msolidm")

    @msolidm.setter
    def msolidm(self, value: int) -> None:
        self._cards[0].set_value("msolidm", value)

    @property
    def lstrtype(self) -> int:
        """Get or set the Type of Lagrangian structures set:
        EQ.0:	Part set
        EQ.1:	Part
        """ # nopep8
        return self._cards[0].get_value("lstrtype")

    @lstrtype.setter
    def lstrtype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""lstrtype must be one of {0,1}""")
        self._cards[0].set_value("lstrtype", value)

    @property
    def soltype(self) -> int:
        """Get or set the Type of solid set:
        EQ.0:	Part set
        EQ.1:	Part
        """ # nopep8
        return self._cards[0].get_value("soltype")

    @soltype.setter
    def soltype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""soltype must be one of {0,1}""")
        self._cards[0].set_value("soltype", value)

    @property
    def form(self) -> int:
        """Get or set the Coupling type
        EQ.0: Constrained acceleration and velocity
        EQ.1: Penalty tied in all directions
        """ # nopep8
        return self._cards[0].get_value("form")

    @form.setter
    def form(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""form must be one of {0,1}""")
        self._cards[0].set_value("form", value)

    @property
    def psf(self) -> float:
        """Get or set the Scale factor for penalty stiffness
        """ # nopep8
        return self._cards[0].get_value("psf")

    @psf.setter
    def psf(self, value: float) -> None:
        self._cards[0].set_value("psf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

