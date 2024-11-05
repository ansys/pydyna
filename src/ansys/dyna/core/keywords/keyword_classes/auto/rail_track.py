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

class RailTrack(KeywordBase):
    """DYNA RAIL_TRACK keyword"""

    keyword = "RAIL"
    subkeyword = "TRACK"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
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
                        "bsetid1",
                        int,
                        10,
                        10,
                        kwargs.get("bsetid1")
                    ),
                    Field(
                        "norgn1",
                        int,
                        20,
                        10,
                        kwargs.get("norgn1")
                    ),
                    Field(
                        "lcur1",
                        int,
                        30,
                        10,
                        kwargs.get("lcur1")
                    ),
                    Field(
                        "oset1",
                        float,
                        40,
                        10,
                        kwargs.get("oset1", 0.0)
                    ),
                    Field(
                        "sf1",
                        float,
                        50,
                        10,
                        kwargs.get("sf1", 1.0)
                    ),
                    Field(
                        "ga1",
                        float,
                        60,
                        10,
                        kwargs.get("ga1", 0.0)
                    ),
                    Field(
                        "idir",
                        int,
                        70,
                        10,
                        kwargs.get("idir", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "bsetid2",
                        int,
                        10,
                        10,
                        kwargs.get("bsetid2")
                    ),
                    Field(
                        "norgn2",
                        int,
                        20,
                        10,
                        kwargs.get("norgn2")
                    ),
                    Field(
                        "lcur2",
                        int,
                        30,
                        10,
                        kwargs.get("lcur2")
                    ),
                    Field(
                        "oset2",
                        float,
                        40,
                        10,
                        kwargs.get("oset2", 0.0)
                    ),
                    Field(
                        "sf2",
                        float,
                        50,
                        10,
                        kwargs.get("sf2", 1.0)
                    ),
                    Field(
                        "ga2",
                        float,
                        60,
                        10,
                        kwargs.get("ga2", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Track ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def bsetid1(self) -> typing.Optional[int]:
        """Get or set the Beam set ID for rails 1 and 2 containing all beam elements that make up the rail, see *SET_BEAM.
        """ # nopep8
        return self._cards[0].get_value("bsetid1")

    @bsetid1.setter
    def bsetid1(self, value: int) -> None:
        self._cards[0].set_value("bsetid1", value)

    @property
    def norgn1(self) -> typing.Optional[int]:
        """Get or set the Reference node at one end of each rail, used as the origin for the roughness curve. The train will move in a direction away from this node.
        """ # nopep8
        return self._cards[0].get_value("norgn1")

    @norgn1.setter
    def norgn1(self, value: int) -> None:
        self._cards[0].set_value("norgn1", value)

    @property
    def lcur1(self) -> typing.Optional[int]:
        """Get or set the Loadcurve ID (see *DEFINE_CURVE) defining track roughness (vertical displacement from line of beam elements) of the rail as a function of distance from the reference node NORIGIN. Distance from reference node on x-axis of curve, roughness on y-axis. Default: no roughness.
        """ # nopep8
        return self._cards[0].get_value("lcur1")

    @lcur1.setter
    def lcur1(self, value: int) -> None:
        self._cards[0].set_value("lcur1", value)

    @property
    def oset1(self) -> float:
        """Get or set the Origin of curve LCUR is shifted by distance OSET from the reference node.
        """ # nopep8
        return self._cards[0].get_value("oset1")

    @oset1.setter
    def oset1(self, value: float) -> None:
        self._cards[0].set_value("oset1", value)

    @property
    def sf1(self) -> float:
        """Get or set the Roughness values are scaled by SF. Default: 1.0.
        """ # nopep8
        return self._cards[0].get_value("sf1")

    @sf1.setter
    def sf1(self, value: float) -> None:
        self._cards[0].set_value("sf1", value)

    @property
    def ga1(self) -> float:
        """Get or set the Shear stiffness of rail per unit length (used to calculate local rail shear deformation within each beam element). GA = shear modulus x cross-sectional area. Default: local shear deformation is ignored.
        """ # nopep8
        return self._cards[0].get_value("ga1")

    @ga1.setter
    def ga1(self, value: float) -> None:
        self._cards[0].set_value("ga1", value)

    @property
    def idir(self) -> int:
        """Get or set the Contact forces are calculated in local directions relative to the plane containing the two rails at the contact point. IDIR determines which side of the plane is �up�, that is, the direction in which the wheel can lift off the rail. �Up� is either c or -c,where c=a�b.  a is the direction along rail 1 heading away from node NORGN1 and b is the vector from rail 1 to rail 2. Both a and b are determined locally.
        EQ.0:	Whichever out of c or -c has a positive global Z component is up(default).
        EQ.1 : -c is up.
        EQ. - 1 : c is up.
        """ # nopep8
        return self._cards[0].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        if value not in [0, 1, -1]:
            raise Exception("""idir must be one of {0,1,-1}""")
        self._cards[0].set_value("idir", value)

    @property
    def bsetid2(self) -> typing.Optional[int]:
        """Get or set the Beam set ID for rails 1 and 2 containing all beam elements that make up the rail, see *SET_BEAM.
        """ # nopep8
        return self._cards[1].get_value("bsetid2")

    @bsetid2.setter
    def bsetid2(self, value: int) -> None:
        self._cards[1].set_value("bsetid2", value)

    @property
    def norgn2(self) -> typing.Optional[int]:
        """Get or set the Reference node at one end of each rail, used as the origin for the roughness curve. The train will move in a direction away from this node.
        """ # nopep8
        return self._cards[1].get_value("norgn2")

    @norgn2.setter
    def norgn2(self, value: int) -> None:
        self._cards[1].set_value("norgn2", value)

    @property
    def lcur2(self) -> typing.Optional[int]:
        """Get or set the Loadcurve ID (see *DEFINE_CURVE) defining track roughness (vertical displacement from line of beam elements) of the rail as a function of distance from the reference node NORIGIN. Distance from reference node on x-axis of curve, roughness on y-axis. Default: no roughness.
        """ # nopep8
        return self._cards[1].get_value("lcur2")

    @lcur2.setter
    def lcur2(self, value: int) -> None:
        self._cards[1].set_value("lcur2", value)

    @property
    def oset2(self) -> float:
        """Get or set the Origin of curve LCUR is shifted by distance OSET from the reference node.
        """ # nopep8
        return self._cards[1].get_value("oset2")

    @oset2.setter
    def oset2(self, value: float) -> None:
        self._cards[1].set_value("oset2", value)

    @property
    def sf2(self) -> float:
        """Get or set the Roughness values are scaled by SF. Default: 1.0.
        """ # nopep8
        return self._cards[1].get_value("sf2")

    @sf2.setter
    def sf2(self, value: float) -> None:
        self._cards[1].set_value("sf2", value)

    @property
    def ga2(self) -> float:
        """Get or set the Shear stiffness of rail per unit length (used to calculate local rail shear deformation within each beam element). GA = shear modulus x cross-sectional area. Default: local shear deformation is ignored.
        """ # nopep8
        return self._cards[1].get_value("ga2")

    @ga2.setter
    def ga2(self, value: float) -> None:
        self._cards[1].set_value("ga2", value)

