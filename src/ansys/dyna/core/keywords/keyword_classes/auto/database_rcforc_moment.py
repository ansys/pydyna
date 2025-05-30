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

"""Module providing the DatabaseRcforcMoment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseRcforcMoment(KeywordBase):
    """DYNA DATABASE_RCFORC_MOMENT keyword"""

    keyword = "DATABASE"
    subkeyword = "RCFORC_MOMENT"

    def __init__(self, **kwargs):
        """Initialize the DatabaseRcforcMoment class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nodes",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nodem",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def nodes(self) -> typing.Optional[int]:
        """Get or set the Node  for moment calculation on slave surface.
        """ # nopep8
        return self._cards[0].get_value("nodes")

    @nodes.setter
    def nodes(self, value: int) -> None:
        """Set the nodes property."""
        self._cards[0].set_value("nodes", value)

    @property
    def nodem(self) -> typing.Optional[int]:
        """Get or set the Node for moment calculation on master surface.
        """ # nopep8
        return self._cards[0].get_value("nodem")

    @nodem.setter
    def nodem(self, value: int) -> None:
        """Set the nodem property."""
        self._cards[0].set_value("nodem", value)

