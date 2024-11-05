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

class DatabaseTracerDe(KeywordBase):
    """DYNA DATABASE_TRACER_DE keyword"""

    keyword = "DATABASE"
    subkeyword = "TRACER_DE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "time",
                        float,
                        0,
                        10,
                        kwargs.get("time", 0.0)
                    ),
                    Field(
                        "track",
                        int,
                        10,
                        10,
                        kwargs.get("track", 0)
                    ),
                    Field(
                        "x",
                        float,
                        20,
                        10,
                        kwargs.get("x", 0.0)
                    ),
                    Field(
                        "y",
                        float,
                        30,
                        10,
                        kwargs.get("y", 0.0)
                    ),
                    Field(
                        "z",
                        float,
                        40,
                        10,
                        kwargs.get("z", 0.0)
                    ),
                    Field(
                        "ammgid",
                        int,
                        50,
                        10,
                        kwargs.get("ammgid")
                    ),
                    Field(
                        "nid",
                        int,
                        60,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "radius",
                        float,
                        70,
                        10,
                        kwargs.get("radius", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def time(self) -> float:
        """Get or set the Start time for tracer particle
        """ # nopep8
        return self._cards[0].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        self._cards[0].set_value("time", value)

    @property
    def track(self) -> int:
        """Get or set the Tracking option:
        EQ.0: particle follows material,
        EQ.1: particle is fixed in space.
        EQ.2:	particle follows the mesh
        """ # nopep8
        return self._cards[0].get_value("track")

    @track.setter
    def track(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""track must be one of {0,1,2}""")
        self._cards[0].set_value("track", value)

    @property
    def x(self) -> float:
        """Get or set the Initial x-coordinate
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the Initial y-coordinate
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the Initial z-coordinate
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[0].set_value("z", value)

    @property
    def ammgid(self) -> typing.Optional[int]:
        """Get or set the The AMMG ID (ALE multi-material group) of the material being tracked in a multi-material ALE element. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("ammgid")

    @ammgid.setter
    def ammgid(self, value: int) -> None:
        self._cards[0].set_value("ammgid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the An optional node ID defining the initial position of a tracer particle.
        If defined, its coordinates will overwrite the X, Y, Z coordinates
        above. This feature is for TRACK = 0 only and can be applied to ALE tracers and DE tracers
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def radius(self) -> float:
        """Get or set the Radius is used only for the DE option to indicate whether the tracer
        follows and monitors a single discrete element or multiple discrete elements.
        GT.0: The tracer takes the average results of all discrete elements located inside a sphere with radius = RADIUS. That sphere stays centered on the DE tracer.
        LT.0: The discrete element closest to the tracer is used. The magnitude of RADIUS in this case is unimportant.
        """ # nopep8
        return self._cards[0].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[0].set_value("radius", value)

