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

class FrequencyDomainAcousticIncidentWave(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_INCIDENT_WAVE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_INCIDENT_WAVE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "type",
                        int,
                        0,
                        10,
                        kwargs.get("type", 1)
                    ),
                    Field(
                        "mag",
                        float,
                        10,
                        10,
                        kwargs.get("mag")
                    ),
                    Field(
                        "xc",
                        float,
                        20,
                        10,
                        kwargs.get("xc")
                    ),
                    Field(
                        "yc",
                        float,
                        30,
                        10,
                        kwargs.get("yc")
                    ),
                    Field(
                        "zc",
                        float,
                        40,
                        10,
                        kwargs.get("zc")
                    ),
                ],
            ),
        ]

    @property
    def type(self) -> int:
        """Get or set the Type of incident sound wave:
        EQ.1: plane wave.
        EQ.2: spherical wave.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""type must be one of {1,2}""")
        self._cards[0].set_value("type", value)

    @property
    def mag(self) -> typing.Optional[float]:
        """Get or set the Magnitude of the incident sound wave.
        """ # nopep8
        return self._cards[0].get_value("mag")

    @mag.setter
    def mag(self, value: float) -> None:
        self._cards[0].set_value("mag", value)

    @property
    def xc(self) -> typing.Optional[float]:
        """Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).
        """ # nopep8
        return self._cards[0].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[0].set_value("xc", value)

    @property
    def yc(self) -> typing.Optional[float]:
        """Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).
        """ # nopep8
        return self._cards[0].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[0].set_value("yc", value)

    @property
    def zc(self) -> typing.Optional[float]:
        """Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).
        """ # nopep8
        return self._cards[0].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[0].set_value("zc", value)

