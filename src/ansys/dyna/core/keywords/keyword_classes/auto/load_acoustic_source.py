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

class LoadAcousticSource(KeywordBase):
    """DYNA LOAD_ACOUSTIC_SOURCE keyword"""

    keyword = "LOAD"
    subkeyword = "ACOUSTIC_SOURCE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid/ssid",
                        int,
                        0,
                        10,
                        kwargs.get("nid/ssid")
                    ),
                    Field(
                        "srctyp",
                        int,
                        10,
                        10,
                        kwargs.get("srctyp", 1)
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "data1",
                        float,
                        30,
                        10,
                        kwargs.get("data1", 1.0)
                    ),
                    Field(
                        "data2",
                        float,
                        40,
                        10,
                        kwargs.get("data2", 0.0)
                    ),
                    Field(
                        "data3",
                        float,
                        50,
                        10,
                        kwargs.get("data3", 0.0)
                    ),
                    Field(
                        "data4",
                        float,
                        60,
                        10,
                        kwargs.get("data4", 0.0)
                    ),
                    Field(
                        "data5",
                        float,
                        70,
                        10,
                        kwargs.get("data5", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def nid_ssid(self) -> typing.Optional[int]:
        """Get or set the Node ID of the acoustic point source for SRCTYP = 1 and 5.
        Segment set ID of structural faces on the external fluid-structure boundary exposed to the acoustic wave source for SRCTYP = 11 and 12
        """ # nopep8
        return self._cards[0].get_value("nid/ssid")

    @nid_ssid.setter
    def nid_ssid(self, value: int) -> None:
        self._cards[0].set_value("nid/ssid", value)

    @property
    def srctyp(self) -> int:
        """Get or set the Acoustic source type:
        EQ.1:	Harmonic nodal point source.DATA1, DATA2,and LCID define the harmonic nodal source strength Q;
        the phase angle of the nodal source strengthand frequency variation for a point source at node NID.
        EQ.5 : Transient nodal point source.DATA1 and LCID define the transient nodal source strength(Q) and temporal variation for a point source at node NID.
        EQ.11 : Harmonic plane wave : DATA1 and LCID define the magnitude and frequency variation for a harmonic plane wave with direction cosines given in
        DATA2, DATA3,and DATA4.SSID is the segment set ID of the external structural(coupled) surface.
        EQ.12 : Harmonic spherical wave.DATA1, DATA5,and LCID define the magnitude, reference radiusand frequency variation for
        a harmonic spherical wave centered at coordinates x_0, y_0,and z_0 specified with DATA2, DATA3,and DATA4.SSID is the segment set ID of the external structural(coupled) surface
        .
        """ # nopep8
        return self._cards[0].get_value("srctyp")

    @srctyp.setter
    def srctyp(self, value: int) -> None:
        if value not in [1, 5, 11, 12]:
            raise Exception("""srctyp must be one of {1,5,11,12}""")
        self._cards[0].set_value("srctyp", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID of curve specifying the variation of the load with frequency or time. If LCID is undefined, then the loading is constant.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def data1(self) -> float:
        """Get or set the SRCTYP.EQ.1:	Magnitude of the harmonic nodal source strength
        SRCTYP.EQ.5:	Magnitude of the transient nodal source strength
        SRCTYP.EQ.11:	Pressure of the harmonic plane wave
        SRCTYP.EQ.12:	Pressure of the harmonic spherical wave.
        """ # nopep8
        return self._cards[0].get_value("data1")

    @data1.setter
    def data1(self, value: float) -> None:
        self._cards[0].set_value("data1", value)

    @property
    def data2(self) -> float:
        """Get or set the SRCTYP.EQ.1:	Phase angle of the nodal source strength in radians
        SRCTYP.EQ.11:	Direction cosine
        SRCTYP.EQ.12:	x coordinate of center of spherical wave.
        """ # nopep8
        return self._cards[0].get_value("data2")

    @data2.setter
    def data2(self, value: float) -> None:
        self._cards[0].set_value("data2", value)

    @property
    def data3(self) -> float:
        """Get or set the SRCTYP.EQ.11:	Direction cosine
        SRCTYP.EQ.12:	y coordinate of center of spherical wave.
        """ # nopep8
        return self._cards[0].get_value("data3")

    @data3.setter
    def data3(self, value: float) -> None:
        self._cards[0].set_value("data3", value)

    @property
    def data4(self) -> float:
        """Get or set the SRCTYP.EQ.11:	Direction cosine
        SRCTYP.EQ.12:	z coordinate of center of spherical wave.
        """ # nopep8
        return self._cards[0].get_value("data4")

    @data4.setter
    def data4(self, value: float) -> None:
        self._cards[0].set_value("data4", value)

    @property
    def data5(self) -> float:
        """Get or set the For SRCTYP = 12, reference radius, where the pressure equals .
        """ # nopep8
        return self._cards[0].get_value("data5")

    @data5.setter
    def data5(self, value: float) -> None:
        self._cards[0].set_value("data5", value)

