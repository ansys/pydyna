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

class ElementMassNodeSet(KeywordBase):
    """DYNA ELEMENT_MASS_NODE_SET keyword"""

    keyword = "ELEMENT"
    subkeyword = "MASS_NODE_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        8,
                        kwargs.get("eid")
                    ),
                    Field(
                        "nsid",
                        int,
                        8,
                        8,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "mass",
                        float,
                        16,
                        16,
                        kwargs.get("mass", 0.0)
                    ),
                    Field(
                        "pid",
                        int,
                        32,
                        8,
                        kwargs.get("pid")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number must be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID. Nodes in this set to which the mass is assigned.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def mass(self) -> float:
        """Get or set the Mass value.
        """ # nopep8
        return self._cards[0].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        self._cards[0].set_value("mass", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID. This input is optional.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

