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

"""Module providing the LoadThermalVariableShellSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class LoadThermalVariableShellSet(KeywordBase):
    """DYNA LOAD_THERMAL_VARIABLE_SHELL_SET keyword"""

    keyword = "LOAD"
    subkeyword = "THERMAL_VARIABLE_SHELL_SET"

    def __init__(self, **kwargs):
        """Initialize the LoadThermalVariableShellSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tbase",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tscale",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tcurve",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "tcurdr",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "zco",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the load ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Shell set ID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def tbase(self) -> typing.Optional[float]:
        """Get or set the Base temperature
        """ # nopep8
        return self._cards[1].get_value("tbase")

    @tbase.setter
    def tbase(self, value: float) -> None:
        """Set the tbase property."""
        self._cards[1].set_value("tbase", value)

    @property
    def tscale(self) -> typing.Optional[float]:
        """Get or set the Scale factor on temperature from load curve
        """ # nopep8
        return self._cards[1].get_value("tscale")

    @tscale.setter
    def tscale(self, value: float) -> None:
        """Set the tscale property."""
        self._cards[1].set_value("tscale", value)

    @property
    def tcurve(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for temperature vs time
        """ # nopep8
        return self._cards[1].get_value("tcurve")

    @tcurve.setter
    def tcurve(self, value: int) -> None:
        """Set the tcurve property."""
        self._cards[1].set_value("tcurve", value)

    @property
    def tcurdr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID used during dynamic relaxation
        """ # nopep8
        return self._cards[1].get_value("tcurdr")

    @tcurdr.setter
    def tcurdr(self, value: int) -> None:
        """Set the tcurdr property."""
        self._cards[1].set_value("tcurdr", value)

    @property
    def zco(self) -> typing.Optional[float]:
        """Get or set the Relative coordinate through-thickness (-1.0 to +1.0)
        """ # nopep8
        return self._cards[1].get_value("zco")

    @zco.setter
    def zco(self, value: float) -> None:
        """Set the zco property."""
        self._cards[1].set_value("zco", value)

