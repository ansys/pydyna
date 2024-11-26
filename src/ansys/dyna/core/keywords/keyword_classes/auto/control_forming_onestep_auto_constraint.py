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

class ControlFormingOnestepAutoConstraint(KeywordBase):
    """DYNA CONTROL_FORMING_ONESTEP_AUTO_CONSTRAINT keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_ONESTEP_AUTO_CONSTRAINT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "icon",
                        int,
                        0,
                        10,
                        kwargs.get("icon")
                    ),
                    Field(
                        "node1",
                        int,
                        10,
                        10,
                        kwargs.get("node1")
                    ),
                    Field(
                        "node2",
                        int,
                        20,
                        10,
                        kwargs.get("node2")
                    ),
                    Field(
                        "node3",
                        int,
                        30,
                        10,
                        kwargs.get("node3")
                    ),
                ],
            ),
        ]

    @property
    def icon(self) -> typing.Optional[int]:
        """Get or set the Automatic nodal constraining option to eliminate the rigid body motion:EQ. 1: Apply.
        """ # nopep8
        return self._cards[0].get_value("icon")

    @icon.setter
    def icon(self, value: int) -> None:
        self._cards[0].set_value("icon", value)

    @property
    def node1(self) -> typing.Optional[int]:
        """Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.
        """ # nopep8
        return self._cards[0].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        self._cards[0].set_value("node1", value)

    @property
    def node2(self) -> typing.Optional[int]:
        """Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.
        """ # nopep8
        return self._cards[0].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        self._cards[0].set_value("node2", value)

    @property
    def node3(self) -> typing.Optional[int]:
        """Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.
        """ # nopep8
        return self._cards[0].get_value("node3")

    @node3.setter
    def node3(self, value: int) -> None:
        self._cards[0].set_value("node3", value)

