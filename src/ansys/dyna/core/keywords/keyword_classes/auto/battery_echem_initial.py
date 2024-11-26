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

class BatteryEchemInitial(KeywordBase):
    """DYNA BATTERY_ECHEM_INITIAL keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_INITIAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "echemid",
                        int,
                        0,
                        10,
                        kwargs.get("echemid")
                    ),
                    Field(
                        "mid",
                        int,
                        10,
                        10,
                        kwargs.get("mid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lic",
                        float,
                        0,
                        10,
                        kwargs.get("lic")
                    ),
                    Field(
                        "lisic",
                        float,
                        10,
                        10,
                        kwargs.get("lisic")
                    ),
                    Field(
                        "phi2ic",
                        float,
                        20,
                        10,
                        kwargs.get("phi2ic")
                    ),
                    Field(
                        "phi1ic",
                        float,
                        30,
                        10,
                        kwargs.get("phi1ic")
                    ),
                    Field(
                        "curic",
                        float,
                        40,
                        10,
                        kwargs.get("curic")
                    ),
                    Field(
                        "fluxic",
                        float,
                        50,
                        10,
                        kwargs.get("fluxic")
                    ),
                ],
            ),
        ]

    @property
    def echemid(self) -> typing.Optional[int]:
        """Get or set the Identifier of the electrochemistry control card to use
        """ # nopep8
        return self._cards[0].get_value("echemid")

    @echemid.setter
    def echemid(self, value: int) -> None:
        self._cards[0].set_value("echemid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Identifier of the battery material to use. Currently not used
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def lic(self) -> typing.Optional[float]:
        """Get or set the Initial concentration of Lithium ions
        """ # nopep8
        return self._cards[1].get_value("lic")

    @lic.setter
    def lic(self, value: float) -> None:
        self._cards[1].set_value("lic", value)

    @property
    def lisic(self) -> typing.Optional[float]:
        """Get or set the Initial concentration of Lithium ions in the solid particles
        """ # nopep8
        return self._cards[1].get_value("lisic")

    @lisic.setter
    def lisic(self, value: float) -> None:
        self._cards[1].set_value("lisic", value)

    @property
    def phi2ic(self) -> typing.Optional[float]:
        """Get or set the Initial condition of the electrolyte potential
        """ # nopep8
        return self._cards[1].get_value("phi2ic")

    @phi2ic.setter
    def phi2ic(self, value: float) -> None:
        self._cards[1].set_value("phi2ic", value)

    @property
    def phi1ic(self) -> typing.Optional[float]:
        """Get or set the Initial condition of the electrode potential
        """ # nopep8
        return self._cards[1].get_value("phi1ic")

    @phi1ic.setter
    def phi1ic(self, value: float) -> None:
        self._cards[1].set_value("phi1ic", value)

    @property
    def curic(self) -> typing.Optional[float]:
        """Get or set the Initial operating current
        """ # nopep8
        return self._cards[1].get_value("curic")

    @curic.setter
    def curic(self, value: float) -> None:
        self._cards[1].set_value("curic", value)

    @property
    def fluxic(self) -> typing.Optional[float]:
        """Get or set the Initial pore-wall flux
        """ # nopep8
        return self._cards[1].get_value("fluxic")

    @fluxic.setter
    def fluxic(self, value: float) -> None:
        self._cards[1].set_value("fluxic", value)

