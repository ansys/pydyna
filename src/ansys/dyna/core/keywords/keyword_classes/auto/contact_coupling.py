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

class ContactCoupling(KeywordBase):
    """DYNA CONTACT_COUPLING keyword"""

    keyword = "CONTACT"
    subkeyword = "COUPLING"

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
                        **kwargs,
                    ),
                ],
            ),
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
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID for coupling
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID for coupling
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[1].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the EQ.0:part set
        EQ.1:shell element set
        EQ.2:solid element
        EQ.3 thick shell element set
        """ # nopep8
        return self._cards[1].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""stype must be `None` or one of {0,1,2,3}""")
        self._cards[1].set_value("stype", value)

