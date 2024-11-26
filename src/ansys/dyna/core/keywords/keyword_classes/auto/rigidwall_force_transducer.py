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

class RigidwallForceTransducer(KeywordBase):
    """DYNA RIGIDWALL_FORCE_TRANSDUCER keyword"""

    keyword = "RIGIDWALL"
    subkeyword = "FORCE_TRANSDUCER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "tid",
                        int,
                        0,
                        10,
                        kwargs.get("tid", 0)
                    ),
                    Field(
                        "rwid",
                        int,
                        10,
                        10,
                        kwargs.get("rwid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "heading",
                        str,
                        0,
                        80,
                        kwargs.get("heading")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid", 0)
                    ),
                ],
            ),
        ]

    @property
    def tid(self) -> int:
        """Get or set the Transducer ID.
        """ # nopep8
        return self._cards[0].get_value("tid")

    @tid.setter
    def tid(self, value: int) -> None:
        self._cards[0].set_value("tid", value)

    @property
    def rwid(self) -> int:
        """Get or set the Rigid wall ID.
        """ # nopep8
        return self._cards[0].get_value("rwid")

    @rwid.setter
    def rwid(self, value: int) -> None:
        self._cards[0].set_value("rwid", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        self._cards[1].set_value("heading", value)

    @property
    def nsid(self) -> int:
        """Get or set the Node set ID.
        """ # nopep8
        return self._cards[2].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[2].set_value("nsid", value)

