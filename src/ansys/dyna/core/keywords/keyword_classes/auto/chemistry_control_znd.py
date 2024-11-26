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

class ChemistryControlZnd(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_ZND keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_ZND"

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
                ],
            ),
            Card(
                [
                    Field(
                        "f",
                        float,
                        0,
                        10,
                        kwargs.get("f")
                    ),
                    Field(
                        "eplus",
                        float,
                        10,
                        10,
                        kwargs.get("eplus")
                    ),
                    Field(
                        "q0",
                        float,
                        20,
                        10,
                        kwargs.get("q0")
                    ),
                    Field(
                        "gam",
                        float,
                        30,
                        10,
                        kwargs.get("gam")
                    ),
                    Field(
                        "xyzd",
                        float,
                        40,
                        10,
                        kwargs.get("xyzd")
                    ),
                    Field(
                        "detdir",
                        int,
                        50,
                        10,
                        kwargs.get("detdir")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Identifier for this full chemistry calculation.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def f(self) -> typing.Optional[float]:
        """Get or set the Overdriven factor.
        """ # nopep8
        return self._cards[1].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        self._cards[1].set_value("f", value)

    @property
    def eplus(self) -> typing.Optional[float]:
        """Get or set the EPLUS parameter of the ZND model.
        """ # nopep8
        return self._cards[1].get_value("eplus")

    @eplus.setter
    def eplus(self, value: float) -> None:
        self._cards[1].set_value("eplus", value)

    @property
    def q0(self) -> typing.Optional[float]:
        """Get or set the Q0 parameter of the ZND model.
        """ # nopep8
        return self._cards[1].get_value("q0")

    @q0.setter
    def q0(self, value: float) -> None:
        self._cards[1].set_value("q0", value)

    @property
    def gam(self) -> typing.Optional[float]:
        """Get or set the GAM parameter of the ZND model.
        """ # nopep8
        return self._cards[1].get_value("gam")

    @gam.setter
    def gam(self, value: float) -> None:
        self._cards[1].set_value("gam", value)

    @property
    def xyzd(self) -> typing.Optional[float]:
        """Get or set the Position of the detonation front in the DETDIR direction.
        """ # nopep8
        return self._cards[1].get_value("xyzd")

    @xyzd.setter
    def xyzd(self, value: float) -> None:
        self._cards[1].set_value("xyzd", value)

    @property
    def detdir(self) -> typing.Optional[int]:
        """Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)
        """ # nopep8
        return self._cards[1].get_value("detdir")

    @detdir.setter
    def detdir(self, value: int) -> None:
        self._cards[1].set_value("detdir", value)

