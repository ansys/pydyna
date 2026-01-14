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

"""Module providing the IcfdBoundaryPrescribedLevelset class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ICFDBOUNDARYPRESCRIBEDLEVELSET_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("ptid", int, 10, 10, None),
    FieldSchema("axe", int, 20, 10, None),
)

class IcfdBoundaryPrescribedLevelset(KeywordBase):
    """DYNA ICFD_BOUNDARY_PRESCRIBED_LEVELSET keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PRESCRIBED_LEVELSET"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryPrescribedLevelset class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYPRESCRIBEDLEVELSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID of the fluid surface where a fluid height will be imposed.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ptid(self) -> typing.Optional[int]:
        """Get or set the Point ID specifying the origin of the fluid surface. See ICFD_DEFINE_POINT.
        """ # nopep8
        return self._cards[0].get_value("ptid")

    @ptid.setter
    def ptid(self, value: int) -> None:
        """Set the ptid property."""
        self._cards[0].set_value("ptid", value)

    @property
    def axe(self) -> typing.Optional[int]:
        """Get or set the Global axis specifying the direction of the fluid (X=1, Y=2, Z=3).
        """ # nopep8
        return self._cards[0].get_value("axe")

    @axe.setter
    def axe(self, value: int) -> None:
        """Set the axe property."""
        self._cards[0].set_value("axe", value)

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

