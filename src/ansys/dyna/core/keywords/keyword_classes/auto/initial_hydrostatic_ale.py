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

class InitialHydrostaticAle(KeywordBase):
    """DYNA INITIAL_HYDROSTATIC_ALE keyword"""

    keyword = "INITIAL"
    subkeyword = "HYDROSTATIC_ALE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "alesid",
                        int,
                        0,
                        10,
                        kwargs.get("alesid")
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "vecid",
                        int,
                        20,
                        10,
                        kwargs.get("vecid")
                    ),
                    Field(
                        "grav",
                        float,
                        30,
                        10,
                        kwargs.get("grav")
                    ),
                    Field(
                        "pbase",
                        float,
                        40,
                        10,
                        kwargs.get("pbase", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "mmgblo",
                        int,
                        10,
                        10,
                        kwargs.get("mmgblo")
                    ),
                ],
            ),
        ]

    @property
    def alesid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("alesid")

    @alesid.setter
    def alesid(self, value: int) -> None:
        self._cards[0].set_value("alesid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type for the SID above:  EQ.0:  SID is a part set ID ; EQ.1:  SID is a part ID.
        EQ.2: Solid set ID (SSID).
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""stype must be one of {0,1,2}""")
        self._cards[0].set_value("stype", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the A vector ID defining the direction of gravitational acceleration.
        """ # nopep8
        return self._cards[0].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[0].set_value("vecid", value)

    @property
    def grav(self) -> typing.Optional[float]:
        """Get or set the Magnitude of the gravitational acceleration.
        """ # nopep8
        return self._cards[0].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        self._cards[0].set_value("grav", value)

    @property
    def pbase(self) -> float:
        """Get or set the The "base" pressure of each fluid layer.  This is the ambient pressure at the top of each ALE material (fluid) layer to be initialized.  Each layer must be represented by one ALE multi-material group ID (AMMG).
        """ # nopep8
        return self._cards[0].get_value("pbase")

    @pbase.setter
    def pbase(self, value: float) -> None:
        self._cards[0].set_value("pbase", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID defining the top location of a material/fluid layer.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def mmgblo(self) -> typing.Optional[int]:
        """Get or set the AMMG ID of the fluid layer immediately below this NID. Each node is defined in association with one AMMG layer below it. See Remark 3.  In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID.
        """ # nopep8
        return self._cards[1].get_value("mmgblo")

    @mmgblo.setter
    def mmgblo(self, value: int) -> None:
        self._cards[1].set_value("mmgblo", value)

