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

class ElementShellBeta(KeywordBase):
    """DYNA ELEMENT_SHELL_BETA keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_BETA"

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
                        "pid",
                        int,
                        8,
                        8,
                        kwargs.get("pid")
                    ),
                    Field(
                        "n1",
                        int,
                        16,
                        8,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        24,
                        8,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        int,
                        32,
                        8,
                        kwargs.get("n3")
                    ),
                    Field(
                        "n4",
                        int,
                        40,
                        8,
                        kwargs.get("n4")
                    ),
                    Field(
                        "n5",
                        int,
                        48,
                        8,
                        kwargs.get("n5")
                    ),
                    Field(
                        "n6",
                        int,
                        56,
                        8,
                        kwargs.get("n6")
                    ),
                    Field(
                        "n7",
                        int,
                        64,
                        8,
                        kwargs.get("n7")
                    ),
                    Field(
                        "n8",
                        int,
                        72,
                        8,
                        kwargs.get("n8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "thic1",
                        float,
                        0,
                        16,
                        kwargs.get("thic1", 0.0)
                    ),
                    Field(
                        "thic2",
                        float,
                        16,
                        16,
                        kwargs.get("thic2", 0.0)
                    ),
                    Field(
                        "thic3",
                        float,
                        32,
                        16,
                        kwargs.get("thic3", 0.0)
                    ),
                    Field(
                        "thic4",
                        float,
                        48,
                        16,
                        kwargs.get("thic4", 0.0)
                    ),
                    Field(
                        "beta",
                        float,
                        64,
                        16,
                        kwargs.get("beta", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "thic5",
                        float,
                        0,
                        16,
                        kwargs.get("thic5", 0.0)
                    ),
                    Field(
                        "thic6",
                        float,
                        16,
                        16,
                        kwargs.get("thic6", 0.0)
                    ),
                    Field(
                        "thic7",
                        float,
                        32,
                        16,
                        kwargs.get("thic7", 0.0)
                    ),
                    Field(
                        "thic8",
                        float,
                        48,
                        16,
                        kwargs.get("thic8", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[0].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 5.
        """ # nopep8
        return self._cards[0].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        self._cards[0].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 6.
        """ # nopep8
        return self._cards[0].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        self._cards[0].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 7.
        """ # nopep8
        return self._cards[0].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        self._cards[0].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 8.
        """ # nopep8
        return self._cards[0].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        self._cards[0].set_value("n8", value)

    @property
    def thic1(self) -> float:
        """Get or set the Shell thickness at node 1.
        """ # nopep8
        return self._cards[1].get_value("thic1")

    @thic1.setter
    def thic1(self, value: float) -> None:
        self._cards[1].set_value("thic1", value)

    @property
    def thic2(self) -> float:
        """Get or set the Shell thickness at node 2.
        """ # nopep8
        return self._cards[1].get_value("thic2")

    @thic2.setter
    def thic2(self, value: float) -> None:
        self._cards[1].set_value("thic2", value)

    @property
    def thic3(self) -> float:
        """Get or set the Shell thickness at node 3.
        """ # nopep8
        return self._cards[1].get_value("thic3")

    @thic3.setter
    def thic3(self, value: float) -> None:
        self._cards[1].set_value("thic3", value)

    @property
    def thic4(self) -> float:
        """Get or set the Shell thickness at node 4.
        """ # nopep8
        return self._cards[1].get_value("thic4")

    @thic4.setter
    def thic4(self, value: float) -> None:
        self._cards[1].set_value("thic4", value)

    @property
    def beta(self) -> float:
        """Get or set the Orthotropic material angle offset measured from the reference (1-2 element side) axis, the angle is in degrees.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[1].set_value("beta", value)

    @property
    def thic5(self) -> float:
        """Get or set the Shell thickness at node 5.
        """ # nopep8
        return self._cards[2].get_value("thic5")

    @thic5.setter
    def thic5(self, value: float) -> None:
        self._cards[2].set_value("thic5", value)

    @property
    def thic6(self) -> float:
        """Get or set the Shell thickness at node 6.
        """ # nopep8
        return self._cards[2].get_value("thic6")

    @thic6.setter
    def thic6(self, value: float) -> None:
        self._cards[2].set_value("thic6", value)

    @property
    def thic7(self) -> float:
        """Get or set the Shell thickness at node 7.
        """ # nopep8
        return self._cards[2].get_value("thic7")

    @thic7.setter
    def thic7(self, value: float) -> None:
        self._cards[2].set_value("thic7", value)

    @property
    def thic8(self) -> float:
        """Get or set the Shell thickness at node 8.
        """ # nopep8
        return self._cards[2].get_value("thic8")

    @thic8.setter
    def thic8(self, value: float) -> None:
        self._cards[2].set_value("thic8", value)

