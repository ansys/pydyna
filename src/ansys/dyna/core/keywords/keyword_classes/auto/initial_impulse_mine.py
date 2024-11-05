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

class InitialImpulseMine(KeywordBase):
    """DYNA INITIAL_IMPULSE_MINE keyword"""

    keyword = "INITIAL"
    subkeyword = "IMPULSE_MINE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "mtnt",
                        float,
                        10,
                        10,
                        kwargs.get("mtnt", 0.0)
                    ),
                    Field(
                        "rhos",
                        float,
                        20,
                        10,
                        kwargs.get("rhos", 0.0)
                    ),
                    Field(
                        "depth",
                        float,
                        30,
                        10,
                        kwargs.get("depth", 0.0)
                    ),
                    Field(
                        "area",
                        float,
                        40,
                        10,
                        kwargs.get("area", 0.0)
                    ),
                    Field(
                        "scale",
                        float,
                        50,
                        10,
                        kwargs.get("scale", 1.0)
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unit",
                        int,
                        70,
                        10,
                        kwargs.get("unit", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x",
                        float,
                        0,
                        10,
                        kwargs.get("x", 0.0)
                    ),
                    Field(
                        "y",
                        float,
                        10,
                        10,
                        kwargs.get("y", 0.0)
                    ),
                    Field(
                        "z",
                        float,
                        20,
                        10,
                        kwargs.get("z", 0.0)
                    ),
                    Field(
                        "nidmc",
                        int,
                        30,
                        10,
                        kwargs.get("nidmc", 0)
                    ),
                    Field(
                        "gvid",
                        int,
                        40,
                        10,
                        kwargs.get("gvid")
                    ),
                    Field(
                        "tbirth",
                        float,
                        50,
                        10,
                        kwargs.get("tbirth", 0.0)
                    ),
                    Field(
                        "psid",
                        int,
                        60,
                        10,
                        kwargs.get("psid", 0)
                    ),
                    Field(
                        "search",
                        float,
                        70,
                        10,
                        kwargs.get("search", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def mtnt(self) -> float:
        """Get or set the Equivalent mass of TNT
        """ # nopep8
        return self._cards[0].get_value("mtnt")

    @mtnt.setter
    def mtnt(self, value: float) -> None:
        self._cards[0].set_value("mtnt", value)

    @property
    def rhos(self) -> float:
        """Get or set the Density of overburden soil
        """ # nopep8
        return self._cards[0].get_value("rhos")

    @rhos.setter
    def rhos(self, value: float) -> None:
        self._cards[0].set_value("rhos", value)

    @property
    def depth(self) -> float:
        """Get or set the Burial depth from the ground surface to the center of the mine. This value must be a positive
        """ # nopep8
        return self._cards[0].get_value("depth")

    @depth.setter
    def depth(self, value: float) -> None:
        self._cards[0].set_value("depth", value)

    @property
    def area(self) -> float:
        """Get or set the Cross sectional area of the mine
        """ # nopep8
        return self._cards[0].get_value("area")

    @area.setter
    def area(self, value: float) -> None:
        self._cards[0].set_value("area", value)

    @property
    def scale(self) -> float:
        """Get or set the Scale factor for the impulse
        """ # nopep8
        return self._cards[0].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        self._cards[0].set_value("scale", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit system. This must match the units used by finite element model.
        EQ.1: inch, dozen slugs (i.e., lbf-s^2/in), second, psi (default)
        EQ.2: meter, kilogram, second, Pascal
        EQ.3: centimeter, gram, microsecond, megabar
        EQ.4: millimeter, kilogram, millisecond, GPa
        EQ.5: millimeter, metric ton, second, MPa
        EQ.6: millimeter, gram, millisecond, MPa
        """ # nopep8
        return self._cards[0].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6]:
            raise Exception("""unit must be one of {1,2,3,4,5,6}""")
        self._cards[0].set_value("unit", value)

    @property
    def x(self) -> float:
        """Get or set the x- coordinates of mine center.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y-coordinates of mine center.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the z- coordinates of mine center.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

    @property
    def nidmc(self) -> int:
        """Get or set the Optional node ID representing the mine center (see *NODE). If defined then X, Y and Z are ignored
        """ # nopep8
        return self._cards[1].get_value("nidmc")

    @nidmc.setter
    def nidmc(self, value: int) -> None:
        self._cards[1].set_value("nidmc", value)

    @property
    def gvid(self) -> typing.Optional[int]:
        """Get or set the Vector ID representing the vertically upward direction, i.e., normal to the ground surface
        """ # nopep8
        return self._cards[1].get_value("gvid")

    @gvid.setter
    def gvid(self, value: int) -> None:
        self._cards[1].set_value("gvid", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time. Impulse is activated at this time
        """ # nopep8
        return self._cards[1].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[1].set_value("tbirth", value)

    @property
    def psid(self) -> int:
        """Get or set the Part set ID identifying the parts affected by the mine.  See *SET_‌PART.  If the segment set defined by SSID includes segments of more than one part, PSID may be used to load only segments of identified parts. Otherwise, if PSID is set to zero, the part affected by the mine defaults to the part comprised by the nodes of the segment set
        """ # nopep8
        return self._cards[1].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[1].set_value("psid", value)

    @property
    def search(self) -> float:
        """Get or set the Limit the search depth into the structure. Initial nodal velocity is distributed from the segment to a depth equal to the SEARCH value. The
        value must be positive. If set to zero the search depth is unlimited and
        extends through the part(s) identified by PSID
        """ # nopep8
        return self._cards[1].get_value("search")

    @search.setter
    def search(self, value: float) -> None:
        self._cards[1].set_value("search", value)

