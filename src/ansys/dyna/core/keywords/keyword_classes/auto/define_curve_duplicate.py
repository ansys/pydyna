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

class DefineCurveDuplicate(KeywordBase):
    """DYNA DEFINE_CURVE_DUPLICATE keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_DUPLICATE"
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
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "rlcid",
                        int,
                        10,
                        10,
                        kwargs.get("rlcid")
                    ),
                    Field(
                        "sfa",
                        float,
                        20,
                        10,
                        kwargs.get("sfa", 1.0)
                    ),
                    Field(
                        "sfo",
                        float,
                        30,
                        10,
                        kwargs.get("sfo", 1.0)
                    ),
                    Field(
                        "offa",
                        float,
                        40,
                        10,
                        kwargs.get("offa", 0.0)
                    ),
                    Field(
                        "offo",
                        float,
                        50,
                        10,
                        kwargs.get("offo", 0.0)
                    ),
                    Field(
                        "dattyp",
                        int,
                        60,
                        10,
                        kwargs.get("dattyp", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveDuplicate.option_specs[0],
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
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def rlcid(self) -> typing.Optional[int]:
        """Get or set the Reference load curve ID
        """ # nopep8
        return self._cards[0].get_value("rlcid")

    @rlcid.setter
    def rlcid(self, value: int) -> None:
        self._cards[0].set_value("rlcid", value)

    @property
    def sfa(self) -> float:
        """Get or set the Scale factor for abcissa value. This is useful for simple modifications.
        EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        self._cards[0].set_value("sfa", value)

    @property
    def sfo(self) -> float:
        """Get or set the Scale factor for ordinate value (function). This is useful for simple modifications.
        EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfo")

    @sfo.setter
    def sfo(self, value: float) -> None:
        self._cards[0].set_value("sfo", value)

    @property
    def offa(self) -> float:
        """Get or set the Offset for abcissa values.
        """ # nopep8
        return self._cards[0].get_value("offa")

    @offa.setter
    def offa(self, value: float) -> None:
        self._cards[0].set_value("offa", value)

    @property
    def offo(self) -> float:
        """Get or set the Offset for ordinate values (function).
        """ # nopep8
        return self._cards[0].get_value("offo")

    @offo.setter
    def offo(self, value: float) -> None:
        self._cards[0].set_value("offo", value)

    @property
    def dattyp(self) -> int:
        """Get or set the Data type. Usually 0, set to 1 only for general xy data. This affects how offsets are applied. General xy data curves refer to curves whose abcissa values do not increase monotonically. Generally, DATTYP=0 for time dependent curves, force versus displacement curves, and stress strain curves.
        """ # nopep8
        return self._cards[0].get_value("dattyp")

    @dattyp.setter
    def dattyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dattyp must be one of {0,1}""")
        self._cards[0].set_value("dattyp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

