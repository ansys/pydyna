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

"""Module providing the LoadShellElement class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class LoadShellElement(KeywordBase):
    """DYNA LOAD_SHELL_ELEMENT keyword"""

    keyword = "LOAD"
    subkeyword = "SHELL_ELEMENT"

    def __init__(self, **kwargs):
        """Initialize the LoadShellElement class."""
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
                        "heading",
                        str,
                        10,
                        70,
                        **kwargs,
                    ),
                ],
            ),
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
                        "lcid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "at",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Shell element ID, see *ELEMENT_SHELL.
        """ # nopep8
        return self._cards[1].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[1].set_value("eid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[1].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[1].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Arrival time for pressure or birth time of pressure.
        """ # nopep8
        return self._cards[1].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        """Set the at property."""
        self._cards[1].set_value("at", value)

