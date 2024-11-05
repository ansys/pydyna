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

class DefineMultiscale(KeywordBase):
    """DYNA DEFINE_MULTISCALE keyword"""

    keyword = "DEFINE"
    subkeyword = "MULTISCALE"
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
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "bset",
                        int,
                        10,
                        10,
                        kwargs.get("bset")
                    ),
                    Field(
                        "id",
                        int,
                        20,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "bset",
                        int,
                        30,
                        10,
                        kwargs.get("bset")
                    ),
                    Field(
                        "id",
                        int,
                        40,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "bset",
                        int,
                        50,
                        10,
                        kwargs.get("bset")
                    ),
                    Field(
                        "id",
                        int,
                        60,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "bset",
                        int,
                        70,
                        10,
                        kwargs.get("bset")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineMultiscale.option_specs[0],
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
    def id(self) -> typing.Optional[int]:
        """Get or set the MULTISCALE local model ID to use.  See *INCLUDE_‌MULTISCALE.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def bset(self) -> typing.Optional[int]:
        """Get or set the Beam set which uses this multi-scale local model ID for failure modeling
        """ # nopep8
        return self._cards[0].get_value("bset")

    @bset.setter
    def bset(self, value: int) -> None:
        self._cards[0].set_value("bset", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the MULTISCALE local model ID to use.  See *INCLUDE_‌MULTISCALE.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def bset(self) -> typing.Optional[int]:
        """Get or set the Beam set which uses this multi-scale local model ID for failure modeling
        """ # nopep8
        return self._cards[0].get_value("bset")

    @bset.setter
    def bset(self, value: int) -> None:
        self._cards[0].set_value("bset", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the MULTISCALE local model ID to use.  See *INCLUDE_‌MULTISCALE.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def bset(self) -> typing.Optional[int]:
        """Get or set the Beam set which uses this multi-scale local model ID for failure modeling
        """ # nopep8
        return self._cards[0].get_value("bset")

    @bset.setter
    def bset(self, value: int) -> None:
        self._cards[0].set_value("bset", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the MULTISCALE local model ID to use.  See *INCLUDE_‌MULTISCALE.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def bset(self) -> typing.Optional[int]:
        """Get or set the Beam set which uses this multi-scale local model ID for failure modeling
        """ # nopep8
        return self._cards[0].get_value("bset")

    @bset.setter
    def bset(self, value: int) -> None:
        self._cards[0].set_value("bset", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

