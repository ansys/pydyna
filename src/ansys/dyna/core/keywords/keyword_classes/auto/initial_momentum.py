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

"""Module providing the InitialMomentum class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InitialMomentum(KeywordBase):
    """DYNA INITIAL_MOMENTUM keyword"""

    keyword = "INITIAL"
    subkeyword = "MOMENTUM"

    def __init__(self, **kwargs):
        """Initialize the InitialMomentum class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mx",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "my",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "mz",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dept",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def mx(self) -> float:
        """Get or set the Initial x-momentum.
        """ # nopep8
        return self._cards[0].get_value("mx")

    @mx.setter
    def mx(self, value: float) -> None:
        """Set the mx property."""
        self._cards[0].set_value("mx", value)

    @property
    def my(self) -> float:
        """Get or set the Initial y-momentum.
        """ # nopep8
        return self._cards[0].get_value("my")

    @my.setter
    def my(self, value: float) -> None:
        """Set the my property."""
        self._cards[0].set_value("my", value)

    @property
    def mz(self) -> float:
        """Get or set the Initial z-momentum.
        """ # nopep8
        return self._cards[0].get_value("mz")

    @mz.setter
    def mz(self, value: float) -> None:
        """Set the mz property."""
        self._cards[0].set_value("mz", value)

    @property
    def dept(self) -> float:
        """Get or set the Deposition time.
        """ # nopep8
        return self._cards[0].get_value("dept")

    @dept.setter
    def dept(self, value: float) -> None:
        """Set the dept property."""
        self._cards[0].set_value("dept", value)

