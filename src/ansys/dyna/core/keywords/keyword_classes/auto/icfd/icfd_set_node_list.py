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

"""Module providing the IcfdSetNodeList class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDSETNODELIST_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
)

_ICFDSETNODELIST_CARD1 = (
    FieldSchema("nid1", int, 0, 10, None),
    FieldSchema("nid2", int, 10, 10, None),
    FieldSchema("nid3", int, 20, 10, None),
    FieldSchema("nid4", int, 30, 10, None),
    FieldSchema("nid5", int, 40, 10, None),
    FieldSchema("nid6", int, 50, 10, None),
    FieldSchema("nid7", int, 60, 10, None),
    FieldSchema("nid8", int, 70, 10, None),
)

class IcfdSetNodeList(KeywordBase):
    """DYNA ICFD_SET_NODE_LIST keyword"""

    keyword = "ICFD"
    subkeyword = "SET_NODE_LIST"

    def __init__(self, **kwargs):
        """Initialize the IcfdSetNodeList class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDSETNODELIST_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ICFDSETNODELIST_CARD1,
                **kwargs,
            ),        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Associated Part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node ID n.
        """ # nopep8
        return self._cards[1].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[1].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node ID n.
        """ # nopep8
        return self._cards[1].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[1].set_value("nid2", value)

    @property
    def nid3(self) -> typing.Optional[int]:
        """Get or set the Node ID n.
        """ # nopep8
        return self._cards[1].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        """Set the nid3 property."""
        self._cards[1].set_value("nid3", value)

    @property
    def nid4(self) -> typing.Optional[int]:
        """Get or set the Node ID n.
        """ # nopep8
        return self._cards[1].get_value("nid4")

    @nid4.setter
    def nid4(self, value: int) -> None:
        """Set the nid4 property."""
        self._cards[1].set_value("nid4", value)

    @property
    def nid5(self) -> typing.Optional[int]:
        """Get or set the Node ID n.
        """ # nopep8
        return self._cards[1].get_value("nid5")

    @nid5.setter
    def nid5(self, value: int) -> None:
        """Set the nid5 property."""
        self._cards[1].set_value("nid5", value)

    @property
    def nid6(self) -> typing.Optional[int]:
        """Get or set the Node ID n.
        """ # nopep8
        return self._cards[1].get_value("nid6")

    @nid6.setter
    def nid6(self, value: int) -> None:
        """Set the nid6 property."""
        self._cards[1].set_value("nid6", value)

    @property
    def nid7(self) -> typing.Optional[int]:
        """Get or set the Node ID n.
        """ # nopep8
        return self._cards[1].get_value("nid7")

    @nid7.setter
    def nid7(self, value: int) -> None:
        """Set the nid7 property."""
        self._cards[1].set_value("nid7", value)

    @property
    def nid8(self) -> typing.Optional[int]:
        """Get or set the Node ID n.
        """ # nopep8
        return self._cards[1].get_value("nid8")

    @nid8.setter
    def nid8(self, value: int) -> None:
        """Set the nid8 property."""
        self._cards[1].set_value("nid8", value)

