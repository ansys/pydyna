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

class CeseBoundaryCyclicPartSet(KeywordBase):
    """DYNA CESE_BOUNDARY_CYCLIC_PART_SET keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_CYCLIC_PART_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "surfsid1",
                        int,
                        0,
                        10,
                        kwargs.get("surfsid1")
                    ),
                    Field(
                        "surfsid2",
                        int,
                        10,
                        10,
                        kwargs.get("surfsid2")
                    ),
                    Field(
                        "cyctyp",
                        int,
                        20,
                        10,
                        kwargs.get("cyctyp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "axisx1",
                        float,
                        0,
                        10,
                        kwargs.get("axisx1", 0)
                    ),
                    Field(
                        "axisy1",
                        float,
                        10,
                        10,
                        kwargs.get("axisy1", 0)
                    ),
                    Field(
                        "axisz1",
                        float,
                        20,
                        10,
                        kwargs.get("axisz1", 0)
                    ),
                    Field(
                        "dirx",
                        float,
                        30,
                        10,
                        kwargs.get("dirx")
                    ),
                    Field(
                        "diry",
                        float,
                        40,
                        10,
                        kwargs.get("diry")
                    ),
                    Field(
                        "dirz",
                        float,
                        50,
                        10,
                        kwargs.get("dirz")
                    ),
                    Field(
                        "rotang",
                        float,
                        60,
                        10,
                        kwargs.get("rotang")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "transx",
                        float,
                        0,
                        10,
                        kwargs.get("transx")
                    ),
                    Field(
                        "transy",
                        float,
                        10,
                        10,
                        kwargs.get("transy")
                    ),
                    Field(
                        "transz",
                        float,
                        20,
                        10,
                        kwargs.get("transz")
                    ),
                ],
            ),
        ]

    @property
    def surfsid1(self) -> typing.Optional[int]:
        """Get or set the Identifiers of two sets of surface part IDs, each created with a *LSO_ID_SET card, where each surface part ID in each set is referenced in *MESH_SURFACE_ELEMENT cards.
        """ # nopep8
        return self._cards[0].get_value("surfsid1")

    @surfsid1.setter
    def surfsid1(self, value: int) -> None:
        self._cards[0].set_value("surfsid1", value)

    @property
    def surfsid2(self) -> typing.Optional[int]:
        """Get or set the Identifiers of two sets of surface part IDs, each created with a *LSO_ID_SET card, where each surface part ID in each set is referenced in *MESH_SURFACE_ELEMENT cards.
        """ # nopep8
        return self._cards[0].get_value("surfsid2")

    @surfsid2.setter
    def surfsid2(self, value: int) -> None:
        self._cards[0].set_value("surfsid2", value)

    @property
    def cyctyp(self) -> int:
        """Get or set the Relationship between the two cyclic boundary condition surfaces:EQ.0: none assumed (default)
        EQ.1: The first surface is rotated about an axis to match the second surface.
        EQ.2: The faces of the first surface are translated in a given direction to obtain the corresponding faces on the second surface.
        """ # nopep8
        return self._cards[0].get_value("cyctyp")

    @cyctyp.setter
    def cyctyp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""cyctyp must be one of {0,1,2}""")
        self._cards[0].set_value("cyctyp", value)

    @property
    def axisx1(self) -> float:
        """Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("axisx1")

    @axisx1.setter
    def axisx1(self, value: float) -> None:
        self._cards[1].set_value("axisx1", value)

    @property
    def axisy1(self) -> float:
        """Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("axisy1")

    @axisy1.setter
    def axisy1(self, value: float) -> None:
        self._cards[1].set_value("axisy1", value)

    @property
    def axisz1(self) -> float:
        """Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("axisz1")

    @axisz1.setter
    def axisz1(self, value: float) -> None:
        self._cards[1].set_value("axisz1", value)

    @property
    def dirx(self) -> typing.Optional[float]:
        """Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("dirx")

    @dirx.setter
    def dirx(self, value: float) -> None:
        self._cards[1].set_value("dirx", value)

    @property
    def diry(self) -> typing.Optional[float]:
        """Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("diry")

    @diry.setter
    def diry(self, value: float) -> None:
        self._cards[1].set_value("diry", value)

    @property
    def dirz(self) -> typing.Optional[float]:
        """Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
        """ # nopep8
        return self._cards[1].get_value("dirz")

    @dirz.setter
    def dirz(self, value: float) -> None:
        self._cards[1].set_value("dirz", value)

    @property
    def rotang(self) -> typing.Optional[float]:
        """Get or set the The angle of rotation (in degrees) that transforms the centroid of each face on the first surface to the centroid of the corresponding face on the second surface (for CYCTYP.EQ.1).
        """ # nopep8
        return self._cards[1].get_value("rotang")

    @rotang.setter
    def rotang(self, value: float) -> None:
        self._cards[1].set_value("rotang", value)

    @property
    def transx(self) -> typing.Optional[float]:
        """Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
        """ # nopep8
        return self._cards[2].get_value("transx")

    @transx.setter
    def transx(self, value: float) -> None:
        self._cards[2].set_value("transx", value)

    @property
    def transy(self) -> typing.Optional[float]:
        """Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
        """ # nopep8
        return self._cards[2].get_value("transy")

    @transy.setter
    def transy(self, value: float) -> None:
        self._cards[2].set_value("transy", value)

    @property
    def transz(self) -> typing.Optional[float]:
        """Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
        """ # nopep8
        return self._cards[2].get_value("transz")

    @transz.setter
    def transz(self, value: float) -> None:
        self._cards[2].set_value("transz", value)

