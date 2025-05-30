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

"""Module providing the ConstrainedSpotweldFilteredForce class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ConstrainedSpotweldFilteredForce(KeywordBase):
    """DYNA CONSTRAINED_SPOTWELD_FILTERED_FORCE keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "SPOTWELD_FILTERED_FORCE"

    def __init__(self, **kwargs):
        """Initialize the ConstrainedSpotweldFilteredForce class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "wid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sn",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ss",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "n",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "m",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tf",
                        float,
                        60,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                    Field(
                        "ep",
                        float,
                        70,
                        10,
                        1.0E+20,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nf",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tw",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def wid(self) -> typing.Optional[int]:
        """Get or set the Optional weld ID
        """ # nopep8
        return self._cards[0].get_value("wid")

    @wid.setter
    def wid(self, value: int) -> None:
        """Set the wid property."""
        self._cards[0].set_value("wid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node ID for node 1.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node ID for node 2.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Normal force at spotweld failure (optional).
        """ # nopep8
        return self._cards[1].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        """Set the sn property."""
        self._cards[1].set_value("sn", value)

    @property
    def ss(self) -> typing.Optional[float]:
        """Get or set the Shear force at spotweld failure (optional).
        """ # nopep8
        return self._cards[1].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        """Set the ss property."""
        self._cards[1].set_value("ss", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent for normal spotweld force (optional).
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponent for shear spotweld force (optional).
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[1].set_value("m", value)

    @property
    def tf(self) -> float:
        """Get or set the Failure time for nodal constraint set (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("tf")

    @tf.setter
    def tf(self, value: float) -> None:
        """Set the tf property."""
        self._cards[1].set_value("tf", value)

    @property
    def ep(self) -> float:
        """Get or set the Effective plastic strain at failure (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("ep")

    @ep.setter
    def ep(self, value: float) -> None:
        """Set the ep property."""
        self._cards[1].set_value("ep", value)

    @property
    def nf(self) -> typing.Optional[int]:
        """Get or set the Number of force vectors stored for filtering.
        """ # nopep8
        return self._cards[2].get_value("nf")

    @nf.setter
    def nf(self, value: int) -> None:
        """Set the nf property."""
        self._cards[2].set_value("nf", value)

    @property
    def tw(self) -> typing.Optional[float]:
        """Get or set the Time window for filtering.
        """ # nopep8
        return self._cards[2].get_value("tw")

    @tw.setter
    def tw(self, value: float) -> None:
        """Set the tw property."""
        self._cards[2].set_value("tw", value)

