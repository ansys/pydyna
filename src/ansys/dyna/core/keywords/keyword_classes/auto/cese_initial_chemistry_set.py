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

class CeseInitialChemistrySet(KeywordBase):
    """DYNA CESE_INITIAL_CHEMISTRY_SET keyword"""

    keyword = "CESE"
    subkeyword = "INITIAL_CHEMISTRY_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "setid",
                        int,
                        0,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "chemid",
                        int,
                        10,
                        10,
                        kwargs.get("chemid")
                    ),
                    Field(
                        "compid",
                        int,
                        20,
                        10,
                        kwargs.get("compid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "uic",
                        float,
                        0,
                        10,
                        kwargs.get("uic")
                    ),
                    Field(
                        "vic",
                        float,
                        10,
                        10,
                        kwargs.get("vic")
                    ),
                    Field(
                        "wic",
                        float,
                        20,
                        10,
                        kwargs.get("wic")
                    ),
                    Field(
                        "rhoic",
                        float,
                        30,
                        10,
                        kwargs.get("rhoic")
                    ),
                    Field(
                        "pic",
                        float,
                        40,
                        10,
                        kwargs.get("pic")
                    ),
                    Field(
                        "tic",
                        float,
                        50,
                        10,
                        kwargs.get("tic")
                    ),
                    Field(
                        "hic",
                        float,
                        60,
                        10,
                        kwargs.get("hic")
                    ),
                ],
            ),
        ]

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Identifier of the CESE element set to initialize.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def chemid(self) -> typing.Optional[int]:
        """Get or set the Identifier of chemistry control card to use.
        """ # nopep8
        return self._cards[0].get_value("chemid")

    @chemid.setter
    def chemid(self, value: int) -> None:
        self._cards[0].set_value("chemid", value)

    @property
    def compid(self) -> typing.Optional[int]:
        """Get or set the Identifier of chemical composition to use.
        """ # nopep8
        return self._cards[0].get_value("compid")

    @compid.setter
    def compid(self, value: int) -> None:
        self._cards[0].set_value("compid", value)

    @property
    def uic(self) -> typing.Optional[float]:
        """Get or set the X-component of the fluid velocity.
        """ # nopep8
        return self._cards[1].get_value("uic")

    @uic.setter
    def uic(self, value: float) -> None:
        self._cards[1].set_value("uic", value)

    @property
    def vic(self) -> typing.Optional[float]:
        """Get or set the Y-component of the fluid velocity.
        """ # nopep8
        return self._cards[1].get_value("vic")

    @vic.setter
    def vic(self, value: float) -> None:
        self._cards[1].set_value("vic", value)

    @property
    def wic(self) -> typing.Optional[float]:
        """Get or set the Z-component of the fluid velocity.
        """ # nopep8
        return self._cards[1].get_value("wic")

    @wic.setter
    def wic(self, value: float) -> None:
        self._cards[1].set_value("wic", value)

    @property
    def rhoic(self) -> typing.Optional[float]:
        """Get or set the Initial fluid density.
        """ # nopep8
        return self._cards[1].get_value("rhoic")

    @rhoic.setter
    def rhoic(self, value: float) -> None:
        self._cards[1].set_value("rhoic", value)

    @property
    def pic(self) -> typing.Optional[float]:
        """Get or set the Initial fluid pressure.
        """ # nopep8
        return self._cards[1].get_value("pic")

    @pic.setter
    def pic(self, value: float) -> None:
        self._cards[1].set_value("pic", value)

    @property
    def tic(self) -> typing.Optional[float]:
        """Get or set the Initial fluid temperature.
        """ # nopep8
        return self._cards[1].get_value("tic")

    @tic.setter
    def tic(self, value: float) -> None:
        self._cards[1].set_value("tic", value)

    @property
    def hic(self) -> typing.Optional[float]:
        """Get or set the Initial fluid enthalpy. However, when CHEMID refers to a ZND 1-step reaction card, this is the progressive variable (degree of combustion).
        """ # nopep8
        return self._cards[1].get_value("hic")

    @hic.setter
    def hic(self, value: float) -> None:
        self._cards[1].set_value("hic", value)

