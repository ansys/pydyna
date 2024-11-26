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

class DefineSpotweldFailureAdd(KeywordBase):
    """DYNA DEFINE_SPOTWELD_FAILURE_ADD keyword"""

    keyword = "DEFINE"
    subkeyword = "SPOTWELD_FAILURE_ADD"
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
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "tflag",
                        int,
                        10,
                        10,
                        kwargs.get("tflag", 0)
                    ),
                    Field(
                        "dc1",
                        float,
                        20,
                        10,
                        kwargs.get("dc1", 1.183)
                    ),
                    Field(
                        "dc2",
                        float,
                        30,
                        10,
                        kwargs.get("dc2", 0.002963)
                    ),
                    Field(
                        "dc3",
                        float,
                        40,
                        10,
                        kwargs.get("dc3", 0.0458)
                    ),
                    Field(
                        "dc4",
                        float,
                        50,
                        10,
                        kwargs.get("dc4", 0.1)
                    ),
                    Field(
                        "exn",
                        float,
                        60,
                        10,
                        kwargs.get("exn", 2.0)
                    ),
                    Field(
                        "exs",
                        float,
                        70,
                        10,
                        kwargs.get("exs", 2.0)
                    ),
                ],
            ),
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
                        "sn",
                        float,
                        10,
                        10,
                        kwargs.get("sn")
                    ),
                    Field(
                        "ss",
                        float,
                        20,
                        10,
                        kwargs.get("ss")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSpotweldFailureAdd.option_specs[0],
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
    def id(self) -> typing.Optional[int]:
        """Get or set the Identification number of data set, input as FVAL on *MAT_SPOTWELD.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def tflag(self) -> int:
        """Get or set the Thickness flag for nominal stress calculation
        EQ.0:	Use minimum sheet thickness
        EQ.1:	Use average sheet thickness
        EQ.2	Use maximum sheet thickness
        EQ.3:	Use sum of sheet thicknesses.
        """ # nopep8
        return self._cards[0].get_value("tflag")

    @tflag.setter
    def tflag(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""tflag must be one of {0,1,2,3}""")
        self._cards[0].set_value("tflag", value)

    @property
    def dc1(self) -> float:
        """Get or set the Dynamic coefficient C1.
        """ # nopep8
        return self._cards[0].get_value("dc1")

    @dc1.setter
    def dc1(self, value: float) -> None:
        self._cards[0].set_value("dc1", value)

    @property
    def dc2(self) -> float:
        """Get or set the Dynamic coefficient C2.
        """ # nopep8
        return self._cards[0].get_value("dc2")

    @dc2.setter
    def dc2(self, value: float) -> None:
        self._cards[0].set_value("dc2", value)

    @property
    def dc3(self) -> float:
        """Get or set the Dynamic coefficient C3.
        """ # nopep8
        return self._cards[0].get_value("dc3")

    @dc3.setter
    def dc3(self, value: float) -> None:
        self._cards[0].set_value("dc3", value)

    @property
    def dc4(self) -> float:
        """Get or set the Dynamic coefficient C4.
        """ # nopep8
        return self._cards[0].get_value("dc4")

    @dc4.setter
    def dc4(self, value: float) -> None:
        self._cards[0].set_value("dc4", value)

    @property
    def exn(self) -> float:
        """Get or set the Exponent on the normal term.
        """ # nopep8
        return self._cards[0].get_value("exn")

    @exn.setter
    def exn(self, value: float) -> None:
        self._cards[0].set_value("exn", value)

    @property
    def exs(self) -> float:
        """Get or set the Exponent on the shear term.
        """ # nopep8
        return self._cards[0].get_value("exs")

    @exs.setter
    def exs(self, value: float) -> None:
        self._cards[0].set_value("exs", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID number of welded shell material.
        """ # nopep8
        return self._cards[1].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[1].set_value("mid", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Static normal strength of material MID.
        """ # nopep8
        return self._cards[1].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        self._cards[1].set_value("sn", value)

    @property
    def ss(self) -> typing.Optional[float]:
        """Get or set the Static shear strength of material MID.
        """ # nopep8
        return self._cards[1].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        self._cards[1].set_value("ss", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

