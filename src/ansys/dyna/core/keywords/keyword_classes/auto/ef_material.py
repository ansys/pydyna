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

class EfMaterial(KeywordBase):
    """DYNA EF_MATERIAL keyword"""

    keyword = "EF"
    subkeyword = "MATERIAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nmat",
                        int,
                        0,
                        10,
                        kwargs.get("nmat")
                    ),
                    Field(
                        "name",
                        str,
                        10,
                        70,
                        kwargs.get("name")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mtyp",
                        int,
                        0,
                        10,
                        kwargs.get("mtyp")
                    ),
                    Field(
                        "exe",
                        float,
                        10,
                        10,
                        kwargs.get("exe")
                    ),
                    Field(
                        "eye",
                        float,
                        20,
                        10,
                        kwargs.get("eye")
                    ),
                    Field(
                        "eze",
                        float,
                        30,
                        10,
                        kwargs.get("eze")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rhos",
                        float,
                        0,
                        10,
                        kwargs.get("rhos")
                    ),
                    Field(
                        "rhod",
                        float,
                        10,
                        10,
                        kwargs.get("rhod")
                    ),
                    Field(
                        "taus",
                        float,
                        20,
                        10,
                        kwargs.get("taus")
                    ),
                    Field(
                        "taud",
                        float,
                        30,
                        10,
                        kwargs.get("taud")
                    ),
                    Field(
                        "rdiffr",
                        float,
                        40,
                        10,
                        kwargs.get("rdiffr", 1)
                    ),
                    Field(
                        "rdifft",
                        float,
                        50,
                        10,
                        kwargs.get("rdifft", 1)
                    ),
                ],
            ),
        ]

    @property
    def nmat(self) -> typing.Optional[int]:
        """Get or set the Specifies the material ID, of the exchange factor material.
        """ # nopep8
        return self._cards[0].get_value("nmat")

    @nmat.setter
    def nmat(self, value: int) -> None:
        self._cards[0].set_value("nmat", value)

    @property
    def name(self) -> typing.Optional[str]:
        """Get or set the Specifies the material’s name.  This parameter is used only to make the output file easier to read.
        """ # nopep8
        return self._cards[0].get_value("name")

    @name.setter
    def name(self, value: str) -> None:
        self._cards[0].set_value("name", value)

    @property
    def mtyp(self) -> typing.Optional[int]:
        """Get or set the Specifies if and how emission occurs:
        EQ. - 2:	There is to be no emission and F_ij = 1 is written to the output file for this surface.
        EQ. - 1 : There is to be no emission and F_ij = 0 is written to the output file for this surface.
        EQ.0 : Emission is to be distributed in θ according to :ε(θ) = cos ^ r(θ)
        EQ.1 : Beam emission is to occur in the direction {E_X,E_Y,E_Z}
        EQ.2 : This specifies that emission according to user specified function.
        """ # nopep8
        return self._cards[1].get_value("mtyp")

    @mtyp.setter
    def mtyp(self, value: int) -> None:
        self._cards[1].set_value("mtyp", value)

    @property
    def exe(self) -> typing.Optional[float]:
        """Get or set the Specifies the x component of emission for a type 1 material
        """ # nopep8
        return self._cards[1].get_value("exe")

    @exe.setter
    def exe(self, value: float) -> None:
        self._cards[1].set_value("exe", value)

    @property
    def eye(self) -> typing.Optional[float]:
        """Get or set the Specifies the y component of emission for a type 1 material
        """ # nopep8
        return self._cards[1].get_value("eye")

    @eye.setter
    def eye(self, value: float) -> None:
        self._cards[1].set_value("eye", value)

    @property
    def eze(self) -> typing.Optional[float]:
        """Get or set the Specifies the z component of emission for a type 1 material
        """ # nopep8
        return self._cards[1].get_value("eze")

    @eze.setter
    def eze(self, value: float) -> None:
        self._cards[1].set_value("eze", value)

    @property
    def rhos(self) -> typing.Optional[float]:
        """Get or set the Specifies the specular reflectance
        """ # nopep8
        return self._cards[2].get_value("rhos")

    @rhos.setter
    def rhos(self, value: float) -> None:
        self._cards[2].set_value("rhos", value)

    @property
    def rhod(self) -> typing.Optional[float]:
        """Get or set the Specifies the diffuse reflectance
        """ # nopep8
        return self._cards[2].get_value("rhod")

    @rhod.setter
    def rhod(self, value: float) -> None:
        self._cards[2].set_value("rhod", value)

    @property
    def taus(self) -> typing.Optional[float]:
        """Get or set the Specifies the specular transmittance.
        """ # nopep8
        return self._cards[2].get_value("taus")

    @taus.setter
    def taus(self, value: float) -> None:
        self._cards[2].set_value("taus", value)

    @property
    def taud(self) -> typing.Optional[float]:
        """Get or set the Specifies the diffuse transmittance.
        """ # nopep8
        return self._cards[2].get_value("taud")

    @taud.setter
    def taud(self, value: float) -> None:
        self._cards[2].set_value("taud", value)

    @property
    def rdiffr(self) -> float:
        """Get or set the LS-DYNA simulates diffuse reflection according to the equation: ε(θ)=cos^r (θ).  The user specifies the value for r with RDIFFR.
        """ # nopep8
        return self._cards[2].get_value("rdiffr")

    @rdiffr.setter
    def rdiffr(self, value: float) -> None:
        self._cards[2].set_value("rdiffr", value)

    @property
    def rdifft(self) -> float:
        """Get or set the LS-DYNA simulates diffuse transmittance according to the equation: ε(θ)=cos^r (θ).  The user specifies the value for r with RDIFFT.
        """ # nopep8
        return self._cards[2].get_value("rdifft")

    @rdifft.setter
    def rdifft(self, value: float) -> None:
        self._cards[2].set_value("rdifft", value)

