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

class Mat097(KeywordBase):
    """DYNA MAT_097 keyword"""

    keyword = "MAT"
    subkeyword = "097"
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
                        "tr",
                        int,
                        20,
                        10,
                        kwargs.get("tr")
                    ),
                    Field(
                        "ts",
                        int,
                        30,
                        10,
                        kwargs.get("ts")
                    ),
                    Field(
                        "tt",
                        int,
                        40,
                        10,
                        kwargs.get("tt")
                    ),
                    Field(
                        "rr",
                        int,
                        50,
                        10,
                        kwargs.get("rr")
                    ),
                    Field(
                        "rs",
                        int,
                        60,
                        10,
                        kwargs.get("rs")
                    ),
                    Field(
                        "rt",
                        int,
                        70,
                        10,
                        kwargs.get("rt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rpst",
                        float,
                        0,
                        10,
                        kwargs.get("rpst")
                    ),
                    Field(
                        "rpsr",
                        float,
                        10,
                        10,
                        kwargs.get("rpsr")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat097.option_specs[0],
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
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition..
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def tr(self) -> typing.Optional[int]:
        """Get or set the Translational constraint code along the r-axis(0 => free, 1 => constrained)
        """ # nopep8
        return self._cards[0].get_value("tr")

    @tr.setter
    def tr(self, value: int) -> None:
        self._cards[0].set_value("tr", value)

    @property
    def ts(self) -> typing.Optional[int]:
        """Get or set the Translational constraint code along the s-axis(0 => free, 1 => constrained)
        """ # nopep8
        return self._cards[0].get_value("ts")

    @ts.setter
    def ts(self, value: int) -> None:
        self._cards[0].set_value("ts", value)

    @property
    def tt(self) -> typing.Optional[int]:
        """Get or set the Translational constraint code along the t-axis(0 => free, 1 => constrained)
        """ # nopep8
        return self._cards[0].get_value("tt")

    @tt.setter
    def tt(self, value: int) -> None:
        self._cards[0].set_value("tt", value)

    @property
    def rr(self) -> typing.Optional[int]:
        """Get or set the Rotational constraint code along the r-axis(0 => free, 1 => constrained)
        """ # nopep8
        return self._cards[0].get_value("rr")

    @rr.setter
    def rr(self, value: int) -> None:
        self._cards[0].set_value("rr", value)

    @property
    def rs(self) -> typing.Optional[int]:
        """Get or set the Rotational constraint code along the s-axis(0 => free, 1 => constrained)
        """ # nopep8
        return self._cards[0].get_value("rs")

    @rs.setter
    def rs(self, value: int) -> None:
        self._cards[0].set_value("rs", value)

    @property
    def rt(self) -> typing.Optional[int]:
        """Get or set the Rotational constraint code along the t-axis(0 => free, 1 => constrained)
        """ # nopep8
        return self._cards[0].get_value("rt")

    @rt.setter
    def rt(self, value: int) -> None:
        self._cards[0].set_value("rt", value)

    @property
    def rpst(self) -> typing.Optional[float]:
        """Get or set the Penalty stiffness scale factor for translational constraints.
        """ # nopep8
        return self._cards[1].get_value("rpst")

    @rpst.setter
    def rpst(self, value: float) -> None:
        self._cards[1].set_value("rpst", value)

    @property
    def rpsr(self) -> typing.Optional[float]:
        """Get or set the Penalty stiffness scale factor for rotational constraints.
        """ # nopep8
        return self._cards[1].get_value("rpsr")

    @rpsr.setter
    def rpsr(self, value: float) -> None:
        self._cards[1].set_value("rpsr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

