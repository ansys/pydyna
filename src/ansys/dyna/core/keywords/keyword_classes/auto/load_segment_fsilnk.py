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

class LoadSegmentFsilnk(KeywordBase):
    """DYNA LOAD_SEGMENT_FSILNK keyword"""

    keyword = "LOAD"
    subkeyword = "SEGMENT_FSILNK"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "filename",
                        str,
                        0,
                        80,
                        kwargs.get("filename")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nint",
                        int,
                        0,
                        10,
                        kwargs.get("nint")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "id1",
                        int,
                        0,
                        10,
                        kwargs.get("id1")
                    ),
                    Field(
                        "id2",
                        int,
                        10,
                        10,
                        kwargs.get("id2")
                    ),
                    Field(
                        "id3",
                        int,
                        20,
                        10,
                        kwargs.get("id3")
                    ),
                    Field(
                        "id4",
                        int,
                        30,
                        10,
                        kwargs.get("id4")
                    ),
                    Field(
                        "id5",
                        int,
                        40,
                        10,
                        kwargs.get("id5")
                    ),
                    Field(
                        "id6",
                        int,
                        50,
                        10,
                        kwargs.get("id6")
                    ),
                    Field(
                        "id7",
                        int,
                        60,
                        10,
                        kwargs.get("id7")
                    ),
                    Field(
                        "id8",
                        int,
                        70,
                        10,
                        kwargs.get("id8")
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Filename of the interface linking file
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def nint(self) -> typing.Optional[int]:
        """Get or set the Number of couplings for which the previous run provides pressure data
        """ # nopep8
        return self._cards[1].get_value("nint")

    @nint.setter
    def nint(self, value: int) -> None:
        self._cards[1].set_value("nint", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID (see *DEFINE_CURVE) or function ID (see *DE-FINE_FUNCTION). The curve referred to by LCID provides a scale
        factor as a function of time. The pressure data that is read in from
        the fsilnk file is scaled according to this value
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def id1(self) -> typing.Optional[int]:
        """Get or set the These must match COUPIDs from the *CONSTRAINED_LA-	GRANGE_IN_SOLID card from the previous runs. These IDs
        specify which of the first run's couplings are propagated into this
        run through pressure data read from the fsilnk file.
        """ # nopep8
        return self._cards[2].get_value("id1")

    @id1.setter
    def id1(self, value: int) -> None:
        self._cards[2].set_value("id1", value)

    @property
    def id2(self) -> typing.Optional[int]:
        """Get or set the These must match COUPIDs from the *CONSTRAINED_LA-	GRANGE_IN_SOLID card from the previous runs. These IDs
        specify which of the first run's couplings are propagated into this
        run through pressure data read from the fsilnk file.
        """ # nopep8
        return self._cards[2].get_value("id2")

    @id2.setter
    def id2(self, value: int) -> None:
        self._cards[2].set_value("id2", value)

    @property
    def id3(self) -> typing.Optional[int]:
        """Get or set the These must match COUPIDs from the *CONSTRAINED_LA-	GRANGE_IN_SOLID card from the previous runs. These IDs
        specify which of the first run's couplings are propagated into this
        run through pressure data read from the fsilnk file.
        """ # nopep8
        return self._cards[2].get_value("id3")

    @id3.setter
    def id3(self, value: int) -> None:
        self._cards[2].set_value("id3", value)

    @property
    def id4(self) -> typing.Optional[int]:
        """Get or set the These must match COUPIDs from the *CONSTRAINED_LA-	GRANGE_IN_SOLID card from the previous runs. These IDs
        specify which of the first run's couplings are propagated into this
        run through pressure data read from the fsilnk file.
        """ # nopep8
        return self._cards[2].get_value("id4")

    @id4.setter
    def id4(self, value: int) -> None:
        self._cards[2].set_value("id4", value)

    @property
    def id5(self) -> typing.Optional[int]:
        """Get or set the These must match COUPIDs from the *CONSTRAINED_LA-	GRANGE_IN_SOLID card from the previous runs. These IDs
        specify which of the first run's couplings are propagated into this
        run through pressure data read from the fsilnk file.
        """ # nopep8
        return self._cards[2].get_value("id5")

    @id5.setter
    def id5(self, value: int) -> None:
        self._cards[2].set_value("id5", value)

    @property
    def id6(self) -> typing.Optional[int]:
        """Get or set the These must match COUPIDs from the *CONSTRAINED_LA-	GRANGE_IN_SOLID card from the previous runs. These IDs
        specify which of the first run's couplings are propagated into this
        run through pressure data read from the fsilnk file.
        """ # nopep8
        return self._cards[2].get_value("id6")

    @id6.setter
    def id6(self, value: int) -> None:
        self._cards[2].set_value("id6", value)

    @property
    def id7(self) -> typing.Optional[int]:
        """Get or set the These must match COUPIDs from the *CONSTRAINED_LA-	GRANGE_IN_SOLID card from the previous runs. These IDs
        specify which of the first run's couplings are propagated into this
        run through pressure data read from the fsilnk file.
        """ # nopep8
        return self._cards[2].get_value("id7")

    @id7.setter
    def id7(self, value: int) -> None:
        self._cards[2].set_value("id7", value)

    @property
    def id8(self) -> typing.Optional[int]:
        """Get or set the These must match COUPIDs from the *CONSTRAINED_LA-	GRANGE_IN_SOLID card from the previous runs. These IDs
        specify which of the first run's couplings are propagated into this
        run through pressure data read from the fsilnk file.
        """ # nopep8
        return self._cards[2].get_value("id8")

    @id8.setter
    def id8(self, value: int) -> None:
        self._cards[2].set_value("id8", value)

