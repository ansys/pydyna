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

class LoadSegmentSetAngle(KeywordBase):
    """DYNA LOAD_SEGMENT_SET_ANGLE keyword"""

    keyword = "LOAD"
    subkeyword = "SEGMENT_SET_ANGLE"

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
                        "ssid",
                        int,
                        10,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        30,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "ioptp",
                        int,
                        40,
                        10,
                        kwargs.get("ioptp", 0)
                    ),
                    Field(
                        "ioptd",
                        int,
                        50,
                        10,
                        kwargs.get("ioptd", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "na",
                        int,
                        20,
                        10,
                        kwargs.get("na")
                    ),
                    Field(
                        "ni",
                        int,
                        30,
                        10,
                        kwargs.get("ni")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve or function ID defining the traction as a function of the angle.  If IOPT=0 below, define the abscissa between 0 and 2??radians or 0 and 360 degrees if IOPD=1.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor on value of the load curve or function.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def ioptp(self) -> int:
        """Get or set the Flag for periodicity. The default (IOPTP=0) requires the load curve to be defined between 0 and 2?. This is useful, for example, for modeling an engine that is running at a steady state since each rotation will experience the same loading. To model a transient response, IOPTP=1 uses a load curve defined over the full range of angles, permitting a different response on the second and subsequent revolutions.
        """ # nopep8
        return self._cards[0].get_value("ioptp")

    @ioptp.setter
    def ioptp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ioptp must be one of {0,1}""")
        self._cards[0].set_value("ioptp", value)

    @property
    def ioptd(self) -> int:
        """Get or set the Flag for specifying if the load curve or function argument is in radians (IOPTD=0, the default) or degrees (IOPTD=1).
        """ # nopep8
        return self._cards[0].get_value("ioptd")

    @ioptd.setter
    def ioptd(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ioptd must be one of {0,1}""")
        self._cards[0].set_value("ioptd", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the The node specifying the tail of the rotating vector
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the The node specifying the head of the rotating vector
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def na(self) -> typing.Optional[int]:
        """Get or set the The node specifying the head of the vector defining the axis of rotation. The node N1 specifies the tail.
        """ # nopep8
        return self._cards[1].get_value("na")

    @na.setter
    def na(self, value: int) -> None:
        self._cards[1].set_value("na", value)

    @property
    def ni(self) -> typing.Optional[int]:
        """Get or set the The node specifying the orientation of the vector at an angle of zero. If the initial angle is zero, NI should be equal to N2.
        """ # nopep8
        return self._cards[1].get_value("ni")

    @ni.setter
    def ni(self, value: int) -> None:
        self._cards[1].set_value("ni", value)

