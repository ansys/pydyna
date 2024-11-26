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

class ControlMppContactGroupable(KeywordBase):
    """DYNA CONTROL_MPP_CONTACT_GROUPABLE keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_CONTACT_GROUPABLE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "grp",
                        int,
                        0,
                        10,
                        kwargs.get("grp", 1)
                    ),
                ],
            ),
        ]

    @property
    def grp(self) -> int:
        """Get or set the The sum of these available options (in any combination that makes sense):
        1: Turn on GROUPABLE for all non-tied contacts
        2: Turn on GROUPABLE for all tied contacts
        4: Turn off GROUPABLE for all non-tied contacts
        8: Turn off GROUPABLE for all tied contacts.
        """ # nopep8
        return self._cards[0].get_value("grp")

    @grp.setter
    def grp(self, value: int) -> None:
        if value not in [1, 2, 4, 8]:
            raise Exception("""grp must be one of {1,2,4,8}""")
        self._cards[0].set_value("grp", value)

