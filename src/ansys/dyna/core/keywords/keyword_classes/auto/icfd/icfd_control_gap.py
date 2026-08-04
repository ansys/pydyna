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

"""Module providing the IcfdControlGap class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLGAP_CARD0 = (
    FieldSchema("hgap", float, 0, 10, None),
    FieldSchema("pgap", int, 10, 10, 0),
    FieldSchema("perm", float, 20, 10, 1e-05),
)

_ICFDCONTROLGAP_CARD1 = (
    FieldSchema("pid1", int, 0, 10, None),
    FieldSchema("pid2", int, 10, 10, None),
    FieldSchema("pid3", int, 20, 10, None),
    FieldSchema("pid4", int, 30, 10, None),
    FieldSchema("pid5", int, 40, 10, None),
    FieldSchema("pid6", int, 50, 10, None),
    FieldSchema("pid7", int, 60, 10, None),
    FieldSchema("pid8", int, 70, 10, None),
)

class IcfdControlGap(KeywordBase):
    """DYNA ICFD_CONTROL_GAP keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_GAP"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlGap class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLGAP_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLGAP_CARD1,
                **kwargs,
            ),
        ]
    @property
    def hgap(self) -> typing.Optional[float]:
        """Get or set the Threshold distance: If the distance between surfaces is less than this value the flow motion is blocked in the gap. Currently there is only one universal value of HGAP for all the surfaces listed in Card 2.
        """ # nopep8
        return self._cards[0].get_value("hgap")

    @hgap.setter
    def hgap(self, value: float) -> None:
        """Set the hgap property."""
        self._cards[0].set_value("hgap", value)

    @property
    def pgap(self) -> int:
        """Get or set the Flag for flow treatment of fluid elements in the gap:
        EQ.0: Element exclusion contact treatment
        EQ.1: Porous media description
        """ # nopep8
        return self._cards[0].get_value("pgap")

    @pgap.setter
    def pgap(self, value: int) -> None:
        """Set the pgap property."""
        self._cards[0].set_value("pgap", value)

    @property
    def perm(self) -> float:
        """Get or set the Permeability coefficient. Its value determines the ratio between the fluid and porous representation in the contact region. Low values induce a Darcy description.
        """ # nopep8
        return self._cards[0].get_value("perm")

    @perm.setter
    def perm(self, value: float) -> None:
        """Set the perm property."""
        self._cards[0].set_value("perm", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part IDs of the surfaces involved in the gap closure treatment
        """ # nopep8
        return self._cards[1].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[1].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part IDs of the surfaces involved in the gap closure treatment
        """ # nopep8
        return self._cards[1].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[1].set_value("pid2", value)

    @property
    def pid3(self) -> typing.Optional[int]:
        """Get or set the Part IDs of the surfaces involved in the gap closure treatment
        """ # nopep8
        return self._cards[1].get_value("pid3")

    @pid3.setter
    def pid3(self, value: int) -> None:
        """Set the pid3 property."""
        self._cards[1].set_value("pid3", value)

    @property
    def pid4(self) -> typing.Optional[int]:
        """Get or set the Part IDs of the surfaces involved in the gap closure treatment
        """ # nopep8
        return self._cards[1].get_value("pid4")

    @pid4.setter
    def pid4(self, value: int) -> None:
        """Set the pid4 property."""
        self._cards[1].set_value("pid4", value)

    @property
    def pid5(self) -> typing.Optional[int]:
        """Get or set the Part IDs of the surfaces involved in the gap closure treatment
        """ # nopep8
        return self._cards[1].get_value("pid5")

    @pid5.setter
    def pid5(self, value: int) -> None:
        """Set the pid5 property."""
        self._cards[1].set_value("pid5", value)

    @property
    def pid6(self) -> typing.Optional[int]:
        """Get or set the Part IDs of the surfaces involved in the gap closure treatment
        """ # nopep8
        return self._cards[1].get_value("pid6")

    @pid6.setter
    def pid6(self, value: int) -> None:
        """Set the pid6 property."""
        self._cards[1].set_value("pid6", value)

    @property
    def pid7(self) -> typing.Optional[int]:
        """Get or set the Part IDs of the surfaces involved in the gap closure treatment
        """ # nopep8
        return self._cards[1].get_value("pid7")

    @pid7.setter
    def pid7(self, value: int) -> None:
        """Set the pid7 property."""
        self._cards[1].set_value("pid7", value)

    @property
    def pid8(self) -> typing.Optional[int]:
        """Get or set the Part IDs of the surfaces involved in the gap closure treatment
        """ # nopep8
        return self._cards[1].get_value("pid8")

    @pid8.setter
    def pid8(self, value: int) -> None:
        """Set the pid8 property."""
        self._cards[1].set_value("pid8", value)

