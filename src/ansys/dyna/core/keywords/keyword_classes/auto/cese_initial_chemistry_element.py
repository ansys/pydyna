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

class CeseInitialChemistryElement(KeywordBase):
    """DYNA CESE_INITIAL_CHEMISTRY_ELEMENT keyword"""

    keyword = "CESE"
    subkeyword = "INITIAL_CHEMISTRY_ELEMENT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "chemid",
                        int,
                        0,
                        10,
                        kwargs.get("chemid")
                    ),
                    Field(
                        "compid",
                        int,
                        10,
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
            Card(
                [
                    Field(
                        "ele1",
                        int,
                        0,
                        10,
                        kwargs.get("ele1")
                    ),
                    Field(
                        "ele2",
                        int,
                        10,
                        10,
                        kwargs.get("ele2")
                    ),
                    Field(
                        "ele3",
                        int,
                        20,
                        10,
                        kwargs.get("ele3")
                    ),
                    Field(
                        "ele4",
                        int,
                        30,
                        10,
                        kwargs.get("ele4")
                    ),
                    Field(
                        "ele5",
                        int,
                        40,
                        10,
                        kwargs.get("ele5")
                    ),
                    Field(
                        "ele6",
                        int,
                        50,
                        10,
                        kwargs.get("ele6")
                    ),
                    Field(
                        "ele7",
                        int,
                        60,
                        10,
                        kwargs.get("ele7")
                    ),
                    Field(
                        "ele8",
                        int,
                        70,
                        10,
                        kwargs.get("ele8")
                    ),
                ],
            ),
        ]

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

    @property
    def ele1(self) -> typing.Optional[int]:
        """Get or set the User element numbers to initialize.
        """ # nopep8
        return self._cards[2].get_value("ele1")

    @ele1.setter
    def ele1(self, value: int) -> None:
        self._cards[2].set_value("ele1", value)

    @property
    def ele2(self) -> typing.Optional[int]:
        """Get or set the User element numbers to initialize.
        """ # nopep8
        return self._cards[2].get_value("ele2")

    @ele2.setter
    def ele2(self, value: int) -> None:
        self._cards[2].set_value("ele2", value)

    @property
    def ele3(self) -> typing.Optional[int]:
        """Get or set the User element numbers to initialize.
        """ # nopep8
        return self._cards[2].get_value("ele3")

    @ele3.setter
    def ele3(self, value: int) -> None:
        self._cards[2].set_value("ele3", value)

    @property
    def ele4(self) -> typing.Optional[int]:
        """Get or set the User element numbers to initialize.
        """ # nopep8
        return self._cards[2].get_value("ele4")

    @ele4.setter
    def ele4(self, value: int) -> None:
        self._cards[2].set_value("ele4", value)

    @property
    def ele5(self) -> typing.Optional[int]:
        """Get or set the User element numbers to initialize.
        """ # nopep8
        return self._cards[2].get_value("ele5")

    @ele5.setter
    def ele5(self, value: int) -> None:
        self._cards[2].set_value("ele5", value)

    @property
    def ele6(self) -> typing.Optional[int]:
        """Get or set the User element numbers to initialize.
        """ # nopep8
        return self._cards[2].get_value("ele6")

    @ele6.setter
    def ele6(self, value: int) -> None:
        self._cards[2].set_value("ele6", value)

    @property
    def ele7(self) -> typing.Optional[int]:
        """Get or set the User element numbers to initialize.
        """ # nopep8
        return self._cards[2].get_value("ele7")

    @ele7.setter
    def ele7(self, value: int) -> None:
        self._cards[2].set_value("ele7", value)

    @property
    def ele8(self) -> typing.Optional[int]:
        """Get or set the User element numbers to initialize.
        """ # nopep8
        return self._cards[2].get_value("ele8")

    @ele8.setter
    def ele8(self, value: int) -> None:
        self._cards[2].set_value("ele8", value)

