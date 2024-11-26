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

class EmContact(KeywordBase):
    """DYNA EM_CONTACT keyword"""

    keyword = "EM"
    subkeyword = "CONTACT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "contid",
                        int,
                        0,
                        10,
                        kwargs.get("contid")
                    ),
                    Field(
                        "conttype",
                        int,
                        10,
                        10,
                        kwargs.get("conttype", 1)
                    ),
                    Field(
                        "psidm",
                        int,
                        20,
                        10,
                        kwargs.get("psidm")
                    ),
                    Field(
                        "psids",
                        int,
                        30,
                        10,
                        kwargs.get("psids")
                    ),
                    Field(
                        "eps1",
                        float,
                        40,
                        10,
                        kwargs.get("eps1", 0.3)
                    ),
                    Field(
                        "eps2",
                        float,
                        50,
                        10,
                        kwargs.get("eps2", 0.3)
                    ),
                    Field(
                        "eps3",
                        float,
                        60,
                        10,
                        kwargs.get("eps3", 0.3)
                    ),
                    Field(
                        "d0",
                        float,
                        70,
                        10,
                        kwargs.get("d0")
                    ),
                ],
            ),
        ]

    @property
    def contid(self) -> typing.Optional[int]:
        """Get or set the Electromagnetic contact ID.
        """ # nopep8
        return self._cards[0].get_value("contid")

    @contid.setter
    def contid(self, value: int) -> None:
        self._cards[0].set_value("contid", value)

    @property
    def conttype(self) -> int:
        """Get or set the Type of EM contact.
        EQ.1: Face to face.
        """ # nopep8
        return self._cards[0].get_value("conttype")

    @conttype.setter
    def conttype(self, value: int) -> None:
        self._cards[0].set_value("conttype", value)

    @property
    def psidm(self) -> typing.Optional[int]:
        """Get or set the Set of master parts ID.
        """ # nopep8
        return self._cards[0].get_value("psidm")

    @psidm.setter
    def psidm(self, value: int) -> None:
        self._cards[0].set_value("psidm", value)

    @property
    def psids(self) -> typing.Optional[int]:
        """Get or set the Set of slave parts ID.
        """ # nopep8
        return self._cards[0].get_value("psids")

    @psids.setter
    def psids(self, value: int) -> None:
        self._cards[0].set_value("psids", value)

    @property
    def eps1(self) -> float:
        """Get or set the Contact Coefficients for contact detection conditions.
        """ # nopep8
        return self._cards[0].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        self._cards[0].set_value("eps1", value)

    @property
    def eps2(self) -> float:
        """Get or set the Contact Coefficients for contact detection conditions.
        """ # nopep8
        return self._cards[0].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        self._cards[0].set_value("eps2", value)

    @property
    def eps3(self) -> float:
        """Get or set the Contact Coefficients for contact detection conditions.
        """ # nopep8
        return self._cards[0].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        self._cards[0].set_value("eps3", value)

    @property
    def d0(self) -> typing.Optional[float]:
        """Get or set the Contact condition 3 when COTYPE = 1.
        """ # nopep8
        return self._cards[0].get_value("d0")

    @d0.setter
    def d0(self, value: float) -> None:
        self._cards[0].set_value("d0", value)

