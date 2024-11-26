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

class FrequencyDomainModeGenerate(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_MODE_GENERATE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_MODE_GENERATE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "m1beg",
                        int,
                        0,
                        10,
                        kwargs.get("m1beg")
                    ),
                    Field(
                        "m1end",
                        int,
                        10,
                        10,
                        kwargs.get("m1end")
                    ),
                    Field(
                        "m2beg",
                        int,
                        20,
                        10,
                        kwargs.get("m2beg")
                    ),
                    Field(
                        "m2end",
                        int,
                        30,
                        10,
                        kwargs.get("m2end")
                    ),
                    Field(
                        "m3beg",
                        int,
                        40,
                        10,
                        kwargs.get("m3beg")
                    ),
                    Field(
                        "m3end",
                        int,
                        50,
                        10,
                        kwargs.get("m3end")
                    ),
                    Field(
                        "m4beg",
                        int,
                        60,
                        10,
                        kwargs.get("m4beg")
                    ),
                    Field(
                        "m4end",
                        int,
                        70,
                        10,
                        kwargs.get("m4end")
                    ),
                ],
            ),
        ]

    @property
    def m1beg(self) -> typing.Optional[int]:
        """Get or set the First mode ID in block 1.
        """ # nopep8
        return self._cards[0].get_value("m1beg")

    @m1beg.setter
    def m1beg(self, value: int) -> None:
        self._cards[0].set_value("m1beg", value)

    @property
    def m1end(self) -> typing.Optional[int]:
        """Get or set the Last mode ID in block 1. All mode ID’s between and including M1BEG and M1END are added to the list.
        """ # nopep8
        return self._cards[0].get_value("m1end")

    @m1end.setter
    def m1end(self, value: int) -> None:
        self._cards[0].set_value("m1end", value)

    @property
    def m2beg(self) -> typing.Optional[int]:
        """Get or set the First mode ID in block 2.
        """ # nopep8
        return self._cards[0].get_value("m2beg")

    @m2beg.setter
    def m2beg(self, value: int) -> None:
        self._cards[0].set_value("m2beg", value)

    @property
    def m2end(self) -> typing.Optional[int]:
        """Get or set the Last mode ID in block 2. All mode ID’s between and including M2BEG and M2END are added to the list.
        """ # nopep8
        return self._cards[0].get_value("m2end")

    @m2end.setter
    def m2end(self, value: int) -> None:
        self._cards[0].set_value("m2end", value)

    @property
    def m3beg(self) -> typing.Optional[int]:
        """Get or set the First mode ID in block 3.
        """ # nopep8
        return self._cards[0].get_value("m3beg")

    @m3beg.setter
    def m3beg(self, value: int) -> None:
        self._cards[0].set_value("m3beg", value)

    @property
    def m3end(self) -> typing.Optional[int]:
        """Get or set the Last mode ID in block 3. All mode ID’s between and including M3BEG and M3END are added to the list.
        """ # nopep8
        return self._cards[0].get_value("m3end")

    @m3end.setter
    def m3end(self, value: int) -> None:
        self._cards[0].set_value("m3end", value)

    @property
    def m4beg(self) -> typing.Optional[int]:
        """Get or set the First mode ID in block 4.
        """ # nopep8
        return self._cards[0].get_value("m4beg")

    @m4beg.setter
    def m4beg(self, value: int) -> None:
        self._cards[0].set_value("m4beg", value)

    @property
    def m4end(self) -> typing.Optional[int]:
        """Get or set the Last mode ID in block 4. All mode ID’s between and including M4BEG and M4END are added to the list.
        """ # nopep8
        return self._cards[0].get_value("m4end")

    @m4end.setter
    def m4end(self, value: int) -> None:
        self._cards[0].set_value("m4end", value)

