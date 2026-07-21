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

"""Module providing the BoundarySphTrackSurface class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYSPHTRACKSURFACE_CARD0 = (
    FieldSchema("nbck", int, 0, 10, 200),
    FieldSchema("neimod", int, 10, 10, 0),
    FieldSchema("gnei", float, 20, 10, 0.8),
    FieldSchema("gdist", float, 30, 10, 0.25),
)

_BOUNDARYSPHTRACKSURFACE_CARD1 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("idtype", int, 10, 10, 0),
    FieldSchema("maxnei", float, 20, 10, 0.0),
    FieldSchema("mindist", float, 30, 10, 0.0),
    FieldSchema("icont", int, 40, 10, 0),
)

class BoundarySphTrackSurface(KeywordBase):
    """DYNA BOUNDARY_SPH_TRACK_SURFACE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPH_TRACK_SURFACE"

    def __init__(self, **kwargs):
        """Initialize the BoundarySphTrackSurface class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPHTRACKSURFACE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPHTRACKSURFACE_CARD1,
                **kwargs,
            ),
        ]
    @property
    def nbck(self) -> int:
        """Get or set the Cycle frequency to update surface particle tagging
        """ # nopep8
        return self._cards[0].get_value("nbck")

    @nbck.setter
    def nbck(self, value: int) -> None:
        """Set the nbck property."""
        self._cards[0].set_value("nbck", value)

    @property
    def neimod(self) -> int:
        """Get or set the Compute the average number of neighbors:
        EQ.0: Using all SPH parts for which this condition applies. In other words, the average is computed using all the SPH parts specified in ID.
        EQ.1: Per SPH part. In other words, each SPH part has its own average value.
        """ # nopep8
        return self._cards[0].get_value("neimod")

    @neimod.setter
    def neimod(self, value: int) -> None:
        """Set the neimod property."""
        if value not in [0, 1, None]:
            raise Exception("""neimod must be `None` or one of {0,1}.""")
        self._cards[0].set_value("neimod", value)

    @property
    def gnei(self) -> float:
        """Get or set the Global maximum number of neighbors allowed to be considered a surface particle. This value is used when MAXNEI is not provided.
        """ # nopep8
        return self._cards[0].get_value("gnei")

    @gnei.setter
    def gnei(self, value: float) -> None:
        """Set the gnei property."""
        self._cards[0].set_value("gnei", value)

    @property
    def gdist(self) -> float:
        """Get or set the Global minimum distance of the center of mass for a particle to be considered a boundary particle. This value is used when MINDIST is not provided.
        """ # nopep8
        return self._cards[0].get_value("gdist")

    @gdist.setter
    def gdist(self, value: float) -> None:
        """Set the gdist property."""
        self._cards[0].set_value("gdist", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID for which this detection applies.
        """ # nopep8
        return self._cards[1].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[1].set_value("id", value)

    @property
    def idtype(self) -> int:
        """Get or set the ID type:
        EQ.0: Part ID(see *PART)
        EQ.1: Part set ID(see *SET_PART)
        """ # nopep8
        return self._cards[1].get_value("idtype")

    @idtype.setter
    def idtype(self, value: int) -> None:
        """Set the idtype property."""
        if value not in [0, 1, None]:
            raise Exception("""idtype must be `None` or one of {0,1}.""")
        self._cards[1].set_value("idtype", value)

    @property
    def maxnei(self) -> float:
        """Get or set the Maximum number of neighbors allowed for a particle to be considered a surface particle for this part or part set.
        """ # nopep8
        return self._cards[1].get_value("maxnei")

    @maxnei.setter
    def maxnei(self, value: float) -> None:
        """Set the maxnei property."""
        self._cards[1].set_value("maxnei", value)

    @property
    def mindist(self) -> float:
        """Get or set the Minimum distance of center of mass for a particle to be considered a boundary particle for this part or part set.
        """ # nopep8
        return self._cards[1].get_value("mindist")

    @mindist.setter
    def mindist(self, value: float) -> None:
        """Set the mindist property."""
        self._cards[1].set_value("mindist", value)

    @property
    def icont(self) -> int:
        """Get or set the Feature to only include surface particles in contact interfaces:
        EQ.0: Off
        EQ.1: On
        """ # nopep8
        return self._cards[1].get_value("icont")

    @icont.setter
    def icont(self, value: int) -> None:
        """Set the icont property."""
        self._cards[1].set_value("icont", value)

