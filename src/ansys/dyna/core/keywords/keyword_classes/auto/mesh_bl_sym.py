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

"""Module providing the MeshBlSym class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MeshBlSym(KeywordBase):
    """DYNA MESH_BL_SYM keyword"""

    keyword = "MESH"
    subkeyword = "BL_SYM"

    def __init__(self, **kwargs):
        """Initialize the MeshBlSym class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid5",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid6",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid7",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid8",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part identifiers for the surface element. This is the surface with symmetry.
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part identifiers for the surface element. This is the surface with symmetry.
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[0].set_value("pid2", value)

    @property
    def pid3(self) -> typing.Optional[int]:
        """Get or set the Part identifiers for the surface element. This is the surface with symmetry.
        """ # nopep8
        return self._cards[0].get_value("pid3")

    @pid3.setter
    def pid3(self, value: int) -> None:
        """Set the pid3 property."""
        self._cards[0].set_value("pid3", value)

    @property
    def pid4(self) -> typing.Optional[int]:
        """Get or set the Part identifiers for the surface element. This is the surface with symmetry.
        """ # nopep8
        return self._cards[0].get_value("pid4")

    @pid4.setter
    def pid4(self, value: int) -> None:
        """Set the pid4 property."""
        self._cards[0].set_value("pid4", value)

    @property
    def pid5(self) -> typing.Optional[int]:
        """Get or set the Part identifiers for the surface element. This is the surface with symmetry.
        """ # nopep8
        return self._cards[0].get_value("pid5")

    @pid5.setter
    def pid5(self, value: int) -> None:
        """Set the pid5 property."""
        self._cards[0].set_value("pid5", value)

    @property
    def pid6(self) -> typing.Optional[int]:
        """Get or set the Part identifiers for the surface element. This is the surface with symmetry.
        """ # nopep8
        return self._cards[0].get_value("pid6")

    @pid6.setter
    def pid6(self, value: int) -> None:
        """Set the pid6 property."""
        self._cards[0].set_value("pid6", value)

    @property
    def pid7(self) -> typing.Optional[int]:
        """Get or set the Part identifiers for the surface element. This is the surface with symmetry.
        """ # nopep8
        return self._cards[0].get_value("pid7")

    @pid7.setter
    def pid7(self, value: int) -> None:
        """Set the pid7 property."""
        self._cards[0].set_value("pid7", value)

    @property
    def pid8(self) -> typing.Optional[int]:
        """Get or set the Part identifiers for the surface element. This is the surface with symmetry.
        """ # nopep8
        return self._cards[0].get_value("pid8")

    @pid8.setter
    def pid8(self, value: int) -> None:
        """Set the pid8 property."""
        self._cards[0].set_value("pid8", value)

