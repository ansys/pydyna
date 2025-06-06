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

"""Module providing the ElementBearing class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ElementBearing(KeywordBase):
    """DYNA ELEMENT_BEARING keyword"""

    keyword = "ELEMENT"
    subkeyword = "BEARING"

    def __init__(self, **kwargs):
        """Initialize the ElementBearing class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "itype",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "n1",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "cid1",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n2",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ci2",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nb",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eball",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "prball",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "erace",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "prrace",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "stresl",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "d",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "di",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "do",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dm",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ao",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ai",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "bo",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "pd",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ipflag",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "xtran",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ytran",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ztran",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "xrot",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yrot",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def itype(self) -> int:
        """Get or set the Bearing type: EQ.0: default
        EQ.1:ball bearing
        EQ.2:  roller bearing
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""itype must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("itype", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node on centerline of shaft
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def cid1(self) -> typing.Optional[int]:
        """Get or set the Coordinate ID on shaft. The local z axis defines the axis of rotation.
        """ # nopep8
        return self._cards[0].get_value("cid1")

    @cid1.setter
    def cid1(self, value: int) -> None:
        """Set the cid1 property."""
        self._cards[0].set_value("cid1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node on centerline of bearing. It should initially coincide with N1.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def ci2(self) -> typing.Optional[int]:
        """Get or set the Coordinate ID on bearing. The local z axis defines the axis of rotation.
        """ # nopep8
        return self._cards[0].get_value("ci2")

    @ci2.setter
    def ci2(self, value: int) -> None:
        """Set the ci2 property."""
        self._cards[0].set_value("ci2", value)

    @property
    def nb(self) -> typing.Optional[int]:
        """Get or set the Number of balls or rollers
        """ # nopep8
        return self._cards[0].get_value("nb")

    @nb.setter
    def nb(self, value: int) -> None:
        """Set the nb property."""
        self._cards[0].set_value("nb", value)

    @property
    def eball(self) -> float:
        """Get or set the Youngs modulus for balls or rollers.
        """ # nopep8
        return self._cards[1].get_value("eball")

    @eball.setter
    def eball(self, value: float) -> None:
        """Set the eball property."""
        self._cards[1].set_value("eball", value)

    @property
    def prball(self) -> float:
        """Get or set the Poissons ratio for balls or rollers.
        """ # nopep8
        return self._cards[1].get_value("prball")

    @prball.setter
    def prball(self, value: float) -> None:
        """Set the prball property."""
        self._cards[1].set_value("prball", value)

    @property
    def erace(self) -> float:
        """Get or set the Youngs modulus for races.
        """ # nopep8
        return self._cards[1].get_value("erace")

    @erace.setter
    def erace(self, value: float) -> None:
        """Set the erace property."""
        self._cards[1].set_value("erace", value)

    @property
    def prrace(self) -> float:
        """Get or set the Poissons ratio for races.
        """ # nopep8
        return self._cards[1].get_value("prrace")

    @prrace.setter
    def prrace(self, value: float) -> None:
        """Set the prrace property."""
        self._cards[1].set_value("prrace", value)

    @property
    def stresl(self) -> float:
        """Get or set the Specified value of the bearing stress required to print a warning message that the value has been reached. If it is 0.0, then no message is printe.
        """ # nopep8
        return self._cards[1].get_value("stresl")

    @stresl.setter
    def stresl(self, value: float) -> None:
        """Set the stresl property."""
        self._cards[1].set_value("stresl", value)

    @property
    def d(self) -> float:
        """Get or set the Diameter of balls or rollers.
        """ # nopep8
        return self._cards[2].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[2].set_value("d", value)

    @property
    def di(self) -> float:
        """Get or set the Bore inner diameter.
        """ # nopep8
        return self._cards[2].get_value("di")

    @di.setter
    def di(self, value: float) -> None:
        """Set the di property."""
        self._cards[2].set_value("di", value)

    @property
    def do(self) -> float:
        """Get or set the Bore outer diameter.
        """ # nopep8
        return self._cards[2].get_value("do")

    @do.setter
    def do(self, value: float) -> None:
        """Set the do property."""
        self._cards[2].set_value("do", value)

    @property
    def dm(self) -> float:
        """Get or set the Pitch diameter. If DM is not specified, it is calculated as the average of DI and DO.
        """ # nopep8
        return self._cards[2].get_value("dm")

    @dm.setter
    def dm(self, value: float) -> None:
        """Set the dm property."""
        self._cards[2].set_value("dm", value)

    @property
    def ao(self) -> float:
        """Get or set the Initial contact angle
        """ # nopep8
        return self._cards[3].get_value("ao")

    @ao.setter
    def ao(self, value: float) -> None:
        """Set the ao property."""
        self._cards[3].set_value("ao", value)

    @property
    def ai(self) -> float:
        """Get or set the Inner groove radius to ball diameter ratio.
        """ # nopep8
        return self._cards[3].get_value("ai")

    @ai.setter
    def ai(self, value: float) -> None:
        """Set the ai property."""
        self._cards[3].set_value("ai", value)

    @property
    def bo(self) -> float:
        """Get or set the Outer race groove radius to ball diameter ratio.
        """ # nopep8
        return self._cards[3].get_value("bo")

    @bo.setter
    def bo(self, value: float) -> None:
        """Set the bo property."""
        self._cards[3].set_value("bo", value)

    @property
    def pd(self) -> float:
        """Get or set the Bearing clearance when no load is applied.
        """ # nopep8
        return self._cards[3].get_value("pd")

    @pd.setter
    def pd(self, value: float) -> None:
        """Set the pd property."""
        self._cards[3].set_value("pd", value)

    @property
    def ipflag(self) -> int:
        """Get or set the Preload flag
        EQ.0: no preload.
        EQ.1: displacement preload specified.
        EQ.2: force preload specified.
        """ # nopep8
        return self._cards[4].get_value("ipflag")

    @ipflag.setter
    def ipflag(self, value: int) -> None:
        """Set the ipflag property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ipflag must be `None` or one of {0,1,2}.""")
        self._cards[4].set_value("ipflag", value)

    @property
    def xtran(self) -> float:
        """Get or set the Displacement or force preload in the local X direction.
        """ # nopep8
        return self._cards[4].get_value("xtran")

    @xtran.setter
    def xtran(self, value: float) -> None:
        """Set the xtran property."""
        self._cards[4].set_value("xtran", value)

    @property
    def ytran(self) -> float:
        """Get or set the Displacement or force preload in the local Y direction.
        """ # nopep8
        return self._cards[4].get_value("ytran")

    @ytran.setter
    def ytran(self, value: float) -> None:
        """Set the ytran property."""
        self._cards[4].set_value("ytran", value)

    @property
    def ztran(self) -> float:
        """Get or set the Displacement or force preload in the local Z direction.
        """ # nopep8
        return self._cards[4].get_value("ztran")

    @ztran.setter
    def ztran(self, value: float) -> None:
        """Set the ztran property."""
        self._cards[4].set_value("ztran", value)

    @property
    def xrot(self) -> float:
        """Get or set the Angle (in radians) or moment preload in local X direction.
        """ # nopep8
        return self._cards[4].get_value("xrot")

    @xrot.setter
    def xrot(self, value: float) -> None:
        """Set the xrot property."""
        self._cards[4].set_value("xrot", value)

    @property
    def yrot(self) -> float:
        """Get or set the Angle (in radians) or moment preload in local Y direction.
        """ # nopep8
        return self._cards[4].get_value("yrot")

    @yrot.setter
    def yrot(self, value: float) -> None:
        """Set the yrot property."""
        self._cards[4].set_value("yrot", value)

