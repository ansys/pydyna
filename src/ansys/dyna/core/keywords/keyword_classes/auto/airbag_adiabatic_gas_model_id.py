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

class AirbagAdiabaticGasModelId(KeywordBase):
    """DYNA AIRBAG_ADIABATIC_GAS_MODEL_ID keyword"""

    keyword = "AIRBAG"
    subkeyword = "ADIABATIC_GAS_MODEL_ID"

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
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "sidtyp",
                        int,
                        10,
                        10,
                        kwargs.get("sidtyp", 0)
                    ),
                    Field(
                        "rbid",
                        int,
                        20,
                        10,
                        kwargs.get("rbid", 0)
                    ),
                    Field(
                        "vsca",
                        float,
                        30,
                        10,
                        kwargs.get("vsca", 1.0)
                    ),
                    Field(
                        "psca",
                        float,
                        40,
                        10,
                        kwargs.get("psca", 1.0)
                    ),
                    Field(
                        "vini",
                        float,
                        50,
                        10,
                        kwargs.get("vini", 0.0)
                    ),
                    Field(
                        "mwd",
                        float,
                        60,
                        10,
                        kwargs.get("mwd", 0.0)
                    ),
                    Field(
                        "spsf",
                        float,
                        70,
                        10,
                        kwargs.get("spsf", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "psf",
                        float,
                        0,
                        10,
                        kwargs.get("psf", 1.0)
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "gamma",
                        float,
                        20,
                        10,
                        kwargs.get("gamma")
                    ),
                    Field(
                        "po",
                        float,
                        30,
                        10,
                        kwargs.get("po")
                    ),
                    Field(
                        "pe",
                        float,
                        40,
                        10,
                        kwargs.get("pe")
                    ),
                    Field(
                        "ro",
                        float,
                        50,
                        10,
                        kwargs.get("ro")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Airbag ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[1].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set type:
        EQ.0: segment,
        EQ.1: part IDs.
        """ # nopep8
        return self._cards[1].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sidtyp must be one of {0,1}""")
        self._cards[1].set_value("sidtyp", value)

    @property
    def rbid(self) -> int:
        """Get or set the Rigid body part ID for user defined activation subroutine:
        EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
        EQ.0: the control volume is active from time zero,
        EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
        """ # nopep8
        return self._cards[1].get_value("rbid")

    @rbid.setter
    def rbid(self, value: int) -> None:
        self._cards[1].set_value("rbid", value)

    @property
    def vsca(self) -> float:
        """Get or set the Volume scale factor, V-sca (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("vsca")

    @vsca.setter
    def vsca(self, value: float) -> None:
        self._cards[1].set_value("vsca", value)

    @property
    def psca(self) -> float:
        """Get or set the Pressure scale factor, P-sca (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("psca")

    @psca.setter
    def psca(self, value: float) -> None:
        self._cards[1].set_value("psca", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial filled volume, V-ini (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        self._cards[1].set_value("vini", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        self._cards[1].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
        """ # nopep8
        return self._cards[1].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        self._cards[1].set_value("spsf", value)

    @property
    def psf(self) -> float:
        """Get or set the Pressure scale factor (default=1.0).
        """ # nopep8
        return self._cards[2].get_value("psf")

    @psf.setter
    def psf(self, value: float) -> None:
        self._cards[2].set_value("psf", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Optional load curve for preload flag. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[2].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[2].set_value("lcid", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Ratio of specific heats.
        """ # nopep8
        return self._cards[2].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[2].set_value("gamma", value)

    @property
    def po(self) -> typing.Optional[float]:
        """Get or set the Initial pressure (gauge).
        """ # nopep8
        return self._cards[2].get_value("po")

    @po.setter
    def po(self, value: float) -> None:
        self._cards[2].set_value("po", value)

    @property
    def pe(self) -> typing.Optional[float]:
        """Get or set the Ambient pressure.
        """ # nopep8
        return self._cards[2].get_value("pe")

    @pe.setter
    def pe(self, value: float) -> None:
        self._cards[2].set_value("pe", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Initial density of gas.
        """ # nopep8
        return self._cards[2].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[2].set_value("ro", value)

