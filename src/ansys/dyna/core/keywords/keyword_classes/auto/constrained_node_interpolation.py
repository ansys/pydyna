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

class ConstrainedNodeInterpolation(KeywordBase):
    """DYNA CONSTRAINED_NODE_INTERPOLATION keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODE_INTERPOLATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "numcn",
                        int,
                        10,
                        10,
                        kwargs.get("numcn")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cn1",
                        int,
                        0,
                        10,
                        kwargs.get("cn1")
                    ),
                    Field(
                        "w1",
                        float,
                        10,
                        10,
                        kwargs.get("w1")
                    ),
                    Field(
                        "cn2",
                        int,
                        20,
                        10,
                        kwargs.get("cn2")
                    ),
                    Field(
                        "w2",
                        float,
                        30,
                        10,
                        kwargs.get("w2")
                    ),
                    Field(
                        "cn3",
                        int,
                        40,
                        10,
                        kwargs.get("cn3")
                    ),
                    Field(
                        "w3",
                        float,
                        50,
                        10,
                        kwargs.get("w3")
                    ),
                    Field(
                        "cn4",
                        int,
                        60,
                        10,
                        kwargs.get("cn4")
                    ),
                    Field(
                        "w4",
                        float,
                        70,
                        10,
                        kwargs.get("w4")
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID of the interpolation node as defined in *NODE (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def numcn(self) -> typing.Optional[int]:
        """Get or set the Number of nodes controling the interpolation node.
        """ # nopep8
        return self._cards[0].get_value("numcn")

    @numcn.setter
    def numcn(self, value: int) -> None:
        self._cards[0].set_value("numcn", value)

    @property
    def cn1(self) -> typing.Optional[int]:
        """Get or set the Node ID of controlling node i.
        """ # nopep8
        return self._cards[1].get_value("cn1")

    @cn1.setter
    def cn1(self, value: int) -> None:
        self._cards[1].set_value("cn1", value)

    @property
    def w1(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of controlling node i.
        """ # nopep8
        return self._cards[1].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        self._cards[1].set_value("w1", value)

    @property
    def cn2(self) -> typing.Optional[int]:
        """Get or set the Node ID of controlling node i.
        """ # nopep8
        return self._cards[1].get_value("cn2")

    @cn2.setter
    def cn2(self, value: int) -> None:
        self._cards[1].set_value("cn2", value)

    @property
    def w2(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of controlling node i.
        """ # nopep8
        return self._cards[1].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        self._cards[1].set_value("w2", value)

    @property
    def cn3(self) -> typing.Optional[int]:
        """Get or set the Node ID of controlling node i.
        """ # nopep8
        return self._cards[1].get_value("cn3")

    @cn3.setter
    def cn3(self, value: int) -> None:
        self._cards[1].set_value("cn3", value)

    @property
    def w3(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of controlling node i.
        """ # nopep8
        return self._cards[1].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        self._cards[1].set_value("w3", value)

    @property
    def cn4(self) -> typing.Optional[int]:
        """Get or set the Node ID of controlling node i.
        """ # nopep8
        return self._cards[1].get_value("cn4")

    @cn4.setter
    def cn4(self, value: int) -> None:
        self._cards[1].set_value("cn4", value)

    @property
    def w4(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of controlling node i.
        """ # nopep8
        return self._cards[1].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        self._cards[1].set_value("w4", value)

