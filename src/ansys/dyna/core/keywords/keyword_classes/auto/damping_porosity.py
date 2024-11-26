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

class DampingPorosity(KeywordBase):
    """DYNA DAMPING_POROSITY keyword"""

    keyword = "DAMPING"
    subkeyword = "POROSITY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "flag",
                        int,
                        30,
                        10,
                        kwargs.get("flag", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "stx",
                        float,
                        0,
                        10,
                        kwargs.get("stx", 0.0)
                    ),
                    Field(
                        "sty",
                        float,
                        10,
                        10,
                        kwargs.get("sty", 0.0)
                    ),
                    Field(
                        "stz",
                        float,
                        20,
                        10,
                        kwargs.get("stz", 0.0)
                    ),
                    Field(
                        "srx",
                        float,
                        30,
                        10,
                        kwargs.get("srx", 0.0)
                    ),
                    Field(
                        "sry",
                        float,
                        40,
                        10,
                        kwargs.get("sry", 0.0)
                    ),
                    Field(
                        "srz",
                        float,
                        50,
                        10,
                        kwargs.get("srz", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> int:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID which specifies system damping for parts.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor for load curve. This allows a simple modification of the load curve values.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def flag(self) -> int:
        """Get or set the Set this flag to unity if the global components of the damping forces require separate scale factors.
        """ # nopep8
        return self._cards[0].get_value("flag")

    @flag.setter
    def flag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""flag must be one of {0,1}""")
        self._cards[0].set_value("flag", value)

    @property
    def stx(self) -> float:
        """Get or set the Scale factor on global x translational damping forces.
        """ # nopep8
        return self._cards[1].get_value("stx")

    @stx.setter
    def stx(self, value: float) -> None:
        self._cards[1].set_value("stx", value)

    @property
    def sty(self) -> float:
        """Get or set the Scale factor on global y translational damping forces.
        """ # nopep8
        return self._cards[1].get_value("sty")

    @sty.setter
    def sty(self, value: float) -> None:
        self._cards[1].set_value("sty", value)

    @property
    def stz(self) -> float:
        """Get or set the Scale factor on global z translational damping forces.
        """ # nopep8
        return self._cards[1].get_value("stz")

    @stz.setter
    def stz(self, value: float) -> None:
        self._cards[1].set_value("stz", value)

    @property
    def srx(self) -> float:
        """Get or set the Scale factor on global x rotational damping moments.
        """ # nopep8
        return self._cards[1].get_value("srx")

    @srx.setter
    def srx(self, value: float) -> None:
        self._cards[1].set_value("srx", value)

    @property
    def sry(self) -> float:
        """Get or set the Scale factor on global y rotational damping moments.
        """ # nopep8
        return self._cards[1].get_value("sry")

    @sry.setter
    def sry(self, value: float) -> None:
        self._cards[1].set_value("sry", value)

    @property
    def srz(self) -> float:
        """Get or set the Scale factor on global z rotational damping moments.
        """ # nopep8
        return self._cards[1].get_value("srz")

    @srz.setter
    def srz(self, value: float) -> None:
        self._cards[1].set_value("srz", value)

