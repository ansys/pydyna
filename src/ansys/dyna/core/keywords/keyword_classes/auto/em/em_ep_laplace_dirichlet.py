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

"""Module providing the EmEpLaplaceDirichlet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMEPLAPLACEDIRICHLET_CARD0 = (
    FieldSchema("ldid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("stype", int, 20, 10, 1),
    FieldSchema("sid1", int, 30, 10, None),
    FieldSchema("sid0", int, 40, 10, None),
)

class EmEpLaplaceDirichlet(KeywordBase):
    """DYNA EM_EP_LAPLACE_DIRICHLET keyword"""

    keyword = "EM"
    subkeyword = "EP_LAPLACE_DIRICHLET"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmEpLaplaceDirichlet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPLAPLACEDIRICHLET_CARD0,
                **kwargs,
            ),
        ]
    @property
    def ldid(self) -> typing.Optional[int]:
        """Get or set the ID of the Laplace system to solve (define a new ID with each new card)
        """ # nopep8
        return self._cards[0].get_value("ldid")

    @ldid.setter
    def ldid(self, value: int) -> None:
        """Set the ldid property."""
        self._cards[0].set_value("ldid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID on which the system is solved
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type for the boundary condition:
        EQ.1: Segment set
        EQ.2: Node set
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [1, 2, None]:
            raise Exception("""stype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("stype", value)

    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Set on which a potential of value 1 is prescribed
        """ # nopep8
        return self._cards[0].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        """Set the sid1 property."""
        self._cards[0].set_value("sid1", value)

    @property
    def sid0(self) -> typing.Optional[int]:
        """Get or set the Set on which a potential of value 0 is prescribed
        """ # nopep8
        return self._cards[0].get_value("sid0")

    @sid0.setter
    def sid0(self, value: int) -> None:
        """Set the sid0 property."""
        self._cards[0].set_value("sid0", value)

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

