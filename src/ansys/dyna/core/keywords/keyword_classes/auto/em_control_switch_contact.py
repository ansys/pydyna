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

class EmControlSwitchContact(KeywordBase):
    """DYNA EM_CONTROL_SWITCH_CONTACT keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_SWITCH_CONTACT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                    Field(
                        "ncylfem",
                        int,
                        10,
                        10,
                        kwargs.get("ncylfem", 0)
                    ),
                    Field(
                        "ncylfem",
                        int,
                        20,
                        10,
                        kwargs.get("ncylfem", 0)
                    ),
                ],
            ),
        ]

    @property
    def lcid(self) -> int:
        """Get or set the Load Curve ID.Negative values switch the contact detection off, positive values switch it back on.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def ncylfem(self) -> int:
        """Get or set the Determines the number of cycles before FEM matrix recomputation.If defined this will overwrite the previous NCYCLFEM as long as the contact detection is turned on.
        """ # nopep8
        return self._cards[0].get_value("ncylfem")

    @ncylfem.setter
    def ncylfem(self, value: int) -> None:
        self._cards[0].set_value("ncylfem", value)

    @property
    def ncylfem(self) -> int:
        """Get or set the Determines the number of cycles before BEM matrix recomputation.If defined this will overwrite the previous NCYCLBEM as long as the contact detection is turned on.
        """ # nopep8
        return self._cards[0].get_value("ncylfem")

    @ncylfem.setter
    def ncylfem(self, value: int) -> None:
        self._cards[0].set_value("ncylfem", value)

