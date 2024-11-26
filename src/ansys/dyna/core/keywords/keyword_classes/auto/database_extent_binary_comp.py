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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseExtentBinaryComp(KeywordBase):
    """DYNA DATABASE_EXTENT_BINARY_COMP keyword"""

    keyword = "DATABASE"
    subkeyword = "EXTENT_BINARY_COMP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "iglb",
                        int,
                        0,
                        10,
                        kwargs.get("iglb", 0)
                    ),
                    Field(
                        "ixyz",
                        int,
                        10,
                        10,
                        kwargs.get("ixyz", 0)
                    ),
                    Field(
                        "ivel",
                        int,
                        20,
                        10,
                        kwargs.get("ivel", 0)
                    ),
                    Field(
                        "iacc",
                        int,
                        30,
                        10,
                        kwargs.get("iacc", 0)
                    ),
                    Field(
                        "istrs",
                        int,
                        40,
                        10,
                        kwargs.get("istrs", 0)
                    ),
                    Field(
                        "istra",
                        int,
                        50,
                        10,
                        kwargs.get("istra", 0)
                    ),
                    Field(
                        "ised",
                        int,
                        60,
                        10,
                        kwargs.get("ised", 0)
                    ),
                ],
            ),
        ]

    @property
    def iglb(self) -> int:
        """Get or set the Output flag for global data
        EQ.0: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("iglb")

    @iglb.setter
    def iglb(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iglb must be one of {0,1}""")
        self._cards[0].set_value("iglb", value)

    @property
    def ixyz(self) -> int:
        """Get or set the Output flag for geometry data
        EQ.0: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("ixyz")

    @ixyz.setter
    def ixyz(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ixyz must be one of {0,1}""")
        self._cards[0].set_value("ixyz", value)

    @property
    def ivel(self) -> int:
        """Get or set the Output flag for velocity data
        EQ.0: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("ivel")

    @ivel.setter
    def ivel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ivel must be one of {0,1}""")
        self._cards[0].set_value("ivel", value)

    @property
    def iacc(self) -> int:
        """Get or set the Output flag for acceleration data
        EQ.0: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("iacc")

    @iacc.setter
    def iacc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iacc must be one of {0,1}""")
        self._cards[0].set_value("iacc", value)

    @property
    def istrs(self) -> int:
        """Get or set the Output flag for stress data
        EQ.0: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("istrs")

    @istrs.setter
    def istrs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""istrs must be one of {0,1}""")
        self._cards[0].set_value("istrs", value)

    @property
    def istra(self) -> int:
        """Get or set the Output flag for strain data
        EQ.0: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("istra")

    @istra.setter
    def istra(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""istra must be one of {0,1}""")
        self._cards[0].set_value("istra", value)

    @property
    def ised(self) -> int:
        """Get or set the Output flag for strain energy density data
        EQ.0: no
        EQ.1: yes.
        """ # nopep8
        return self._cards[0].get_value("ised")

    @ised.setter
    def ised(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ised must be one of {0,1}""")
        self._cards[0].set_value("ised", value)

