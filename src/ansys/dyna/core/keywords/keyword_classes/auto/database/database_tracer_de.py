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

"""Module providing the DatabaseTracerDe class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_DATABASETRACERDE_CARD0 = (
    FieldSchema("time", float, 0, 10, 0.0),
    FieldSchema("track", int, 10, 10, 0),
    FieldSchema("x", float, 20, 10, 0.0),
    FieldSchema("y", float, 30, 10, 0.0),
    FieldSchema("z", float, 40, 10, 0.0),
    FieldSchema("ammgid", int, 50, 10, None),
    FieldSchema("nid", int, 60, 10, None),
    FieldSchema("radius", float, 70, 10, 0.0),
)

class DatabaseTracerDe(KeywordBase):
    """DYNA DATABASE_TRACER_DE keyword"""

    keyword = "DATABASE"
    subkeyword = "TRACER_DE"
    _link_fields = {
        "nid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseTracerDe class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASETRACERDE_CARD0,
                **kwargs,
            ),        ]
    @property
    def time(self) -> float:
        """Get or set the Start time for tracer particle
        """ # nopep8
        return self._cards[0].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        """Set the time property."""
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
        """Set the track property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""track must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("track", value)

    @property
    def x(self) -> float:
        """Get or set the Initial x-coordinate
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the Initial y-coordinate
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the Initial z-coordinate
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[0].set_value("z", value)

    @property
    def ammgid(self) -> typing.Optional[int]:
        """Get or set the The AMMG ID (ALE multi-material group) of the material being tracked in a multi-material ALE element. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("ammgid")

    @ammgid.setter
    def ammgid(self, value: int) -> None:
        """Set the ammgid property."""
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
        """Set the nid property."""
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
        """Set the radius property."""
        self._cards[0].set_value("radius", value)

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

