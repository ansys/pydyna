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

"""Module providing the InitialGasMixture class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InitialGasMixture(KeywordBase):
    """DYNA INITIAL_GAS_MIXTURE keyword"""

    keyword = "INITIAL"
    subkeyword = "GAS_MIXTURE"

    def __init__(self, **kwargs):
        """Initialize the InitialGasMixture class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "mmgid",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "temp",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ro1",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ro2",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ro3",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ro4",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ro5",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ro6",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ro7",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ro8",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID for initialization.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set Type:
        EQ.0: Set Part
        EQ.1: Part.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, None]:
            raise Exception("""stype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype", value)

    @property
    def mmgid(self) -> typing.Optional[int]:
        """Get or set the ALE Multi-material group ID of the material that may be present at t = 0 in the ALE mesh set defined by SID.  For general ALE, it must be AMMGID.
        For S - ALE, either AMMGID or AMMG name(AMMGNM) could be used here.Please refer to * ALE_STRUCTURED_MULTI - MATERIALS_GROUP for more details..
        """ # nopep8
        return self._cards[0].get_value("mmgid")

    @mmgid.setter
    def mmgid(self, value: int) -> None:
        """Set the mmgid property."""
        self._cards[0].set_value("mmgid", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the Initial temperature value
        """ # nopep8
        return self._cards[0].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        """Set the temp property."""
        self._cards[0].set_value("temp", value)

    @property
    def ro1(self) -> float:
        """Get or set the Initial densities for up to eight different gas species.
        """ # nopep8
        return self._cards[1].get_value("ro1")

    @ro1.setter
    def ro1(self, value: float) -> None:
        """Set the ro1 property."""
        self._cards[1].set_value("ro1", value)

    @property
    def ro2(self) -> float:
        """Get or set the Initial densities for up to eight different gas species.
        """ # nopep8
        return self._cards[1].get_value("ro2")

    @ro2.setter
    def ro2(self, value: float) -> None:
        """Set the ro2 property."""
        self._cards[1].set_value("ro2", value)

    @property
    def ro3(self) -> float:
        """Get or set the Initial densities for up to eight different gas species.
        """ # nopep8
        return self._cards[1].get_value("ro3")

    @ro3.setter
    def ro3(self, value: float) -> None:
        """Set the ro3 property."""
        self._cards[1].set_value("ro3", value)

    @property
    def ro4(self) -> float:
        """Get or set the Initial densities for up to eight different gas species.
        """ # nopep8
        return self._cards[1].get_value("ro4")

    @ro4.setter
    def ro4(self, value: float) -> None:
        """Set the ro4 property."""
        self._cards[1].set_value("ro4", value)

    @property
    def ro5(self) -> float:
        """Get or set the Initial densities for up to eight different gas species.
        """ # nopep8
        return self._cards[1].get_value("ro5")

    @ro5.setter
    def ro5(self, value: float) -> None:
        """Set the ro5 property."""
        self._cards[1].set_value("ro5", value)

    @property
    def ro6(self) -> float:
        """Get or set the Initial densities for up to eight different gas species.
        """ # nopep8
        return self._cards[1].get_value("ro6")

    @ro6.setter
    def ro6(self, value: float) -> None:
        """Set the ro6 property."""
        self._cards[1].set_value("ro6", value)

    @property
    def ro7(self) -> float:
        """Get or set the Initial densities for up to eight different gas species.
        """ # nopep8
        return self._cards[1].get_value("ro7")

    @ro7.setter
    def ro7(self, value: float) -> None:
        """Set the ro7 property."""
        self._cards[1].set_value("ro7", value)

    @property
    def ro8(self) -> float:
        """Get or set the Initial densities for up to eight different gas species.
        """ # nopep8
        return self._cards[1].get_value("ro8")

    @ro8.setter
    def ro8(self, value: float) -> None:
        """Set the ro8 property."""
        self._cards[1].set_value("ro8", value)

