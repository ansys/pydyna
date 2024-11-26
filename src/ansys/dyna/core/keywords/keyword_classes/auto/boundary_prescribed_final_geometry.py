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

class BoundaryPrescribedFinalGeometry(KeywordBase):
    """DYNA BOUNDARY_PRESCRIBED_FINAL_GEOMETRY keyword"""

    keyword = "BOUNDARY"
    subkeyword = "PRESCRIBED_FINAL_GEOMETRY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "bpfgid",
                        int,
                        0,
                        10,
                        kwargs.get("bpfgid", 0)
                    ),
                    Field(
                        "lcidf",
                        int,
                        10,
                        10,
                        kwargs.get("lcidf", 0)
                    ),
                    Field(
                        "deathd",
                        float,
                        20,
                        10,
                        kwargs.get("deathd")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        8,
                        kwargs.get("nid")
                    ),
                    Field(
                        "x",
                        float,
                        8,
                        16,
                        kwargs.get("x", 0.0)
                    ),
                    Field(
                        "y",
                        float,
                        24,
                        16,
                        kwargs.get("y", 0.0)
                    ),
                    Field(
                        "z",
                        float,
                        40,
                        16,
                        kwargs.get("z", 0.0)
                    ),
                    Field(
                        "lcid",
                        int,
                        56,
                        8,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "death",
                        float,
                        64,
                        16,
                        kwargs.get("death")
                    ),
                ],
            ),
        ]

    @property
    def bpfgid(self) -> int:
        """Get or set the ID for this set of imposed boundary conditions.
        """ # nopep8
        return self._cards[0].get_value("bpfgid")

    @bpfgid.setter
    def bpfgid(self, value: int) -> None:
        self._cards[0].set_value("bpfgid", value)

    @property
    def lcidf(self) -> int:
        """Get or set the Default load curve ID.  This curve varies between zero and unity
        """ # nopep8
        return self._cards[0].get_value("lcidf")

    @lcidf.setter
    def lcidf(self, value: int) -> None:
        self._cards[0].set_value("lcidf", value)

    @property
    def deathd(self) -> typing.Optional[float]:
        """Get or set the Default death time.  At this time the prescribed motion is inactive and the nodal point is allowed to move freely
        """ # nopep8
        return self._cards[0].get_value("deathd")

    @deathd.setter
    def deathd(self, value: float) -> None:
        self._cards[0].set_value("deathd", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID for which the final position is defined.  Nodes defined in this section must also appear under the *NODE input.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def x(self) -> float:
        """Get or set the x-coordinate of final geometry
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y-coordinate of final geometry
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the z-coordinate of final geometry.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID.  If zero the default curve ID, LCIDF, is used.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def death(self) -> typing.Optional[float]:
        """Get or set the Death time.  If zero the default value, DEATHD, is used.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[1].set_value("death", value)

