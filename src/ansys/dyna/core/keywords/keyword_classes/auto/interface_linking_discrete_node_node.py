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

"""Module providing the InterfaceLinkingDiscreteNodeNode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InterfaceLinkingDiscreteNodeNode(KeywordBase):
    """DYNA INTERFACE_LINKING_DISCRETE_NODE_NODE keyword"""

    keyword = "INTERFACE"
    subkeyword = "LINKING_DISCRETE_NODE_NODE"

    def __init__(self, **kwargs):
        """Initialize the InterfaceLinkingDiscreteNodeNode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ifid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID to be moved by interface file, see also *NODE.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def ifid(self) -> typing.Optional[int]:
        """Get or set the Interface ID in interface file.
        """ # nopep8
        return self._cards[0].get_value("ifid")

    @ifid.setter
    def ifid(self, value: int) -> None:
        """Set the ifid property."""
        self._cards[0].set_value("ifid", value)

