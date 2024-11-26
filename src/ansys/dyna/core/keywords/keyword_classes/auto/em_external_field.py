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

class EmExternalField(KeywordBase):
    """DYNA EM_EXTERNAL_FIELD keyword"""

    keyword = "EM"
    subkeyword = "EXTERNAL_FIELD"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "fieldid",
                        int,
                        0,
                        10,
                        kwargs.get("fieldid")
                    ),
                    Field(
                        "ftype",
                        int,
                        10,
                        10,
                        kwargs.get("ftype", 1)
                    ),
                    Field(
                        "fdef",
                        int,
                        20,
                        10,
                        kwargs.get("fdef", 1)
                    ),
                    Field(
                        "lcidx",
                        int,
                        30,
                        10,
                        kwargs.get("lcidx")
                    ),
                    Field(
                        "lcidy",
                        int,
                        40,
                        10,
                        kwargs.get("lcidy")
                    ),
                    Field(
                        "lcidz",
                        int,
                        50,
                        10,
                        kwargs.get("lcidz")
                    ),
                ],
            ),
        ]

    @property
    def fieldid(self) -> typing.Optional[int]:
        """Get or set the External Field ID.
        """ # nopep8
        return self._cards[0].get_value("fieldid")

    @fieldid.setter
    def fieldid(self, value: int) -> None:
        self._cards[0].set_value("fieldid", value)

    @property
    def ftype(self) -> int:
        """Get or set the Field type:
        EQ.1: Magnetic field.
        EQ.2: Electric field.
        EQ.3: charge density (resistive heating solver only).
        """ # nopep8
        return self._cards[0].get_value("ftype")

    @ftype.setter
    def ftype(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""ftype must be one of {1,2,3}""")
        self._cards[0].set_value("ftype", value)

    @property
    def fdef(self) -> int:
        """Get or set the Field defined by:
        EQ.1:Load curves.
        EQ.2: define function (FTYPE = 3 only). If a define function is used, the following parameters are accepted : x, y, z,time, emdt, pot, curr, sigma.
        """ # nopep8
        return self._cards[0].get_value("fdef")

    @fdef.setter
    def fdef(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""fdef must be one of {1,2}""")
        self._cards[0].set_value("fdef", value)

    @property
    def lcidx(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the X component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
        """ # nopep8
        return self._cards[0].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        self._cards[0].set_value("lcidx", value)

    @property
    def lcidy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the Y component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
        """ # nopep8
        return self._cards[0].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        self._cards[0].set_value("lcidy", value)

    @property
    def lcidz(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the Z component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
        """ # nopep8
        return self._cards[0].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        self._cards[0].set_value("lcidz", value)

