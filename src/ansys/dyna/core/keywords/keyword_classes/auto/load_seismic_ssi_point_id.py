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

"""Module providing the LoadSeismicSsiPointId class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class LoadSeismicSsiPointId(KeywordBase):
    """DYNA LOAD_SEISMIC_SSI_POINT_ID keyword"""

    keyword = "LOAD"
    subkeyword = "SEISMIC_SSI_POINT_ID"

    def __init__(self, **kwargs):
        """Initialize the LoadSeismicSsiPointId class."""
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
                        "heading",
                        str,
                        10,
                        70,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "xp",
                        float,
                        8,
                        16,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yp",
                        float,
                        24,
                        16,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zp",
                        float,
                        40,
                        16,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "gmx",
                        int,
                        56,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "gmy",
                        int,
                        64,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "gmz",
                        int,
                        72,
                        8,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sf",
                        float,
                        0,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "cid",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "birth",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "death",
                        float,
                        30,
                        10,
                        1.E+28,
                        **kwargs,
                    ),
                    Field(
                        "isg",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "igm",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Soil-structure interface ID.
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[1].set_value("ssid", value)

    @property
    def xp(self) -> float:
        """Get or set the Curve multiplier at node N1.
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the Curve multiplier at node N2.
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the Curve multiplier at node N3.
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[1].set_value("zp", value)

    @property
    def gmx(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) x-direction.
        """ # nopep8
        return self._cards[1].get_value("gmx")

    @gmx.setter
    def gmx(self, value: int) -> None:
        """Set the gmx property."""
        self._cards[1].set_value("gmx", value)

    @property
    def gmy(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) y-direction.
        """ # nopep8
        return self._cards[1].get_value("gmy")

    @gmy.setter
    def gmy(self, value: int) -> None:
        """Set the gmy property."""
        self._cards[1].set_value("gmy", value)

    @property
    def gmz(self) -> typing.Optional[int]:
        """Get or set the Acceleration load curve or ground motion ID for motion in the (local) z-direction.
        """ # nopep8
        return self._cards[1].get_value("gmz")

    @gmz.setter
    def gmz(self, value: int) -> None:
        """Set the gmz property."""
        self._cards[1].set_value("gmz", value)

    @property
    def sf(self) -> float:
        """Get or set the Ground motion scale factor.
        """ # nopep8
        return self._cards[2].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[2].set_value("sf", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
        """ # nopep8
        return self._cards[2].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[2].set_value("cid", value)

    @property
    def birth(self) -> float:
        """Get or set the Time at which specified ground motion is activated.
        """ # nopep8
        return self._cards[2].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[2].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Time at which specified ground motion is removed.
        """ # nopep8
        return self._cards[2].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        """Set the death property."""
        self._cards[2].set_value("death", value)

    @property
    def isg(self) -> int:
        """Get or set the Definition of soil-structure interface:
        EQ.0: SSID is the ID for the soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.For the DECONV keyword option, ISG = 0 additionally flags that the free-field within motion is computed at depth
        EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure.For the DECONV, ISG = 1 additionally flags that the free-field outcrop motion is computed at depth.
        """ # nopep8
        return self._cards[2].get_value("isg")

    @isg.setter
    def isg(self, value: int) -> None:
        """Set the isg property."""
        if value not in [0, 1, None]:
            raise Exception("""isg must be `None` or one of {0,1}.""")
        self._cards[2].set_value("isg", value)

    @property
    def igm(self) -> int:
        """Get or set the Specification of ground motions GMX, GMY, GMZ:
        EQ.0: ground motions are specified as acceleration load curves. See *DEFINE_CURVE
        EQ.1: Both ground accelerations and velocities specified using *DEFINE_GROUND_MOTION
        .
        """ # nopep8
        return self._cards[2].get_value("igm")

    @igm.setter
    def igm(self, value: int) -> None:
        """Set the igm property."""
        if value not in [0, 1, None]:
            raise Exception("""igm must be `None` or one of {0,1}.""")
        self._cards[2].set_value("igm", value)

