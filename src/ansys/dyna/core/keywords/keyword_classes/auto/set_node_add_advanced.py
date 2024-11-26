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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SetNodeAddAdvanced(KeywordBase):
    """DYNA SET_NODE_ADD_ADVANCED keyword"""

    keyword = "SET"
    subkeyword = "NODE_ADD_ADVANCED"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "da1",
                        float,
                        10,
                        10,
                        kwargs.get("da1", 0.0)
                    ),
                    Field(
                        "da2",
                        float,
                        20,
                        10,
                        kwargs.get("da2", 0.0)
                    ),
                    Field(
                        "da3",
                        float,
                        30,
                        10,
                        kwargs.get("da3", 0.0)
                    ),
                    Field(
                        "da4",
                        float,
                        40,
                        10,
                        kwargs.get("da4", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid1",
                        int,
                        0,
                        10,
                        kwargs.get("sid1")
                    ),
                    Field(
                        "type1",
                        int,
                        10,
                        10,
                        kwargs.get("type1", 1)
                    ),
                    Field(
                        "sid2",
                        int,
                        20,
                        10,
                        kwargs.get("sid2")
                    ),
                    Field(
                        "type2",
                        int,
                        30,
                        10,
                        kwargs.get("type2", 1)
                    ),
                    Field(
                        "sid3",
                        int,
                        40,
                        10,
                        kwargs.get("sid3")
                    ),
                    Field(
                        "type3",
                        int,
                        50,
                        10,
                        kwargs.get("type3", 1)
                    ),
                    Field(
                        "sid4",
                        int,
                        60,
                        10,
                        kwargs.get("sid4")
                    ),
                    Field(
                        "type4",
                        int,
                        70,
                        10,
                        kwargs.get("type4", 1)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetNodeAddAdvanced.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set ID. All node sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        self._cards[0].set_value("da4", value)

    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the set ID.
        """ # nopep8
        return self._cards[1].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        self._cards[1].set_value("sid1", value)

    @property
    def type1(self) -> int:
        """Get or set the Type Code for SIDn.
        """ # nopep8
        return self._cards[1].get_value("type1")

    @type1.setter
    def type1(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""type1 must be one of {1,2,3,4,5,6,7}""")
        self._cards[1].set_value("type1", value)

    @property
    def sid2(self) -> typing.Optional[int]:
        """Get or set the set ID.
        """ # nopep8
        return self._cards[1].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        self._cards[1].set_value("sid2", value)

    @property
    def type2(self) -> int:
        """Get or set the Type Code for SIDn.
        """ # nopep8
        return self._cards[1].get_value("type2")

    @type2.setter
    def type2(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""type2 must be one of {1,2,3,4,5,6,7}""")
        self._cards[1].set_value("type2", value)

    @property
    def sid3(self) -> typing.Optional[int]:
        """Get or set the set ID.
        """ # nopep8
        return self._cards[1].get_value("sid3")

    @sid3.setter
    def sid3(self, value: int) -> None:
        self._cards[1].set_value("sid3", value)

    @property
    def type3(self) -> int:
        """Get or set the Type Code for SIDn.
        """ # nopep8
        return self._cards[1].get_value("type3")

    @type3.setter
    def type3(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""type3 must be one of {1,2,3,4,5,6,7}""")
        self._cards[1].set_value("type3", value)

    @property
    def sid4(self) -> typing.Optional[int]:
        """Get or set the set ID.
        """ # nopep8
        return self._cards[1].get_value("sid4")

    @sid4.setter
    def sid4(self, value: int) -> None:
        self._cards[1].set_value("sid4", value)

    @property
    def type4(self) -> int:
        """Get or set the Type Code for SIDn..
        """ # nopep8
        return self._cards[1].get_value("type4")

    @type4.setter
    def type4(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""type4 must be one of {1,2,3,4,5,6,7}""")
        self._cards[1].set_value("type4", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

