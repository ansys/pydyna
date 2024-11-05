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

class AirbagShellReferenceGeometryRdtId(KeywordBase):
    """DYNA AIRBAG_SHELL_REFERENCE_GEOMETRY_RDT_ID keyword"""

    keyword = "AIRBAG"
    subkeyword = "SHELL_REFERENCE_GEOMETRY_RDT_ID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
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
                        "sx",
                        float,
                        10,
                        10,
                        kwargs.get("sx")
                    ),
                    Field(
                        "sy",
                        float,
                        20,
                        10,
                        kwargs.get("sy")
                    ),
                    Field(
                        "sz",
                        float,
                        30,
                        10,
                        kwargs.get("sz")
                    ),
                    Field(
                        "nid",
                        int,
                        40,
                        10,
                        kwargs.get("nid")
                    ),
                ],
            ),
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
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Card ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def sx(self) -> typing.Optional[float]:
        """Get or set the Scale factor for X direction.
        """ # nopep8
        return self._cards[0].get_value("sx")

    @sx.setter
    def sx(self, value: float) -> None:
        self._cards[0].set_value("sx", value)

    @property
    def sy(self) -> typing.Optional[float]:
        """Get or set the Scale factor for Y direction.
        """ # nopep8
        return self._cards[0].get_value("sy")

    @sy.setter
    def sy(self, value: float) -> None:
        self._cards[0].set_value("sy", value)

    @property
    def sz(self) -> typing.Optional[float]:
        """Get or set the Scale factor for Z direction.
        """ # nopep8
        return self._cards[0].get_value("sz")

    @sz.setter
    def sz(self, value: float) -> None:
        self._cards[0].set_value("sz", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID for origin. Default is the first node ID defined in this keyword.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID.
        """ # nopep8
        return self._cards[1].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[1].set_value("eid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Optional part ID, see *PART, the part ID is not used in this section.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[1].set_value("n4", value)

