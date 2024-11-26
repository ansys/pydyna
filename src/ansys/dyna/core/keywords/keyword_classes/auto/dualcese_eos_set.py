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

class DualceseEosSet(KeywordBase):
    """DYNA DUALCESE_EOS_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "EOS_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eossid",
                        int,
                        0,
                        10,
                        kwargs.get("eossid")
                    ),
                    Field(
                        "eosinid",
                        int,
                        10,
                        10,
                        kwargs.get("eosinid")
                    ),
                    Field(
                        "eosrctid",
                        int,
                        20,
                        10,
                        kwargs.get("eosrctid")
                    ),
                    Field(
                        "eosprdi",
                        int,
                        30,
                        10,
                        kwargs.get("eosprdi")
                    ),
                ],
            ),
        ]

    @property
    def eossid(self) -> typing.Optional[int]:
        """Get or set the Set ID of the EOS mixture of a given multiphase fluid
        """ # nopep8
        return self._cards[0].get_value("eossid")

    @eossid.setter
    def eossid(self, value: int) -> None:
        self._cards[0].set_value("eossid", value)

    @property
    def eosinid(self) -> typing.Optional[int]:
        """Get or set the EOS ID of the inert component of the multiphase mixture
        """ # nopep8
        return self._cards[0].get_value("eosinid")

    @eosinid.setter
    def eosinid(self, value: int) -> None:
        self._cards[0].set_value("eosinid", value)

    @property
    def eosrctid(self) -> typing.Optional[int]:
        """Get or set the EOS ID of the reactant phase of the multiphase mixture
        """ # nopep8
        return self._cards[0].get_value("eosrctid")

    @eosrctid.setter
    def eosrctid(self, value: int) -> None:
        self._cards[0].set_value("eosrctid", value)

    @property
    def eosprdi(self) -> typing.Optional[int]:
        """Get or set the EOS ID of the product phase of the multiphase mixture
        """ # nopep8
        return self._cards[0].get_value("eosprdi")

    @eosprdi.setter
    def eosprdi(self, value: int) -> None:
        self._cards[0].set_value("eosprdi", value)

