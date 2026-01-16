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

"""Module providing the InitialStressDepth class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_INITIALSTRESSDEPTH_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("ro_g", float, 10, 10, None),
    FieldSchema("zdatum", float, 20, 10, None),
    FieldSchema("kfact", float, 30, 10, 0.0),
    FieldSchema("lc", int, 40, 10, None),
    FieldSchema("lch", int, 50, 10, None),
    FieldSchema("lck0", int, 60, 10, None),
)

class InitialStressDepth(KeywordBase):
    """DYNA INITIAL_STRESS_DEPTH keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_DEPTH"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialStressDepth class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALSTRESSDEPTH_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ro_g(self) -> typing.Optional[float]:
        """Get or set the Stress per unit elevation above datum (usually = density x gravity)
        """ # nopep8
        return self._cards[0].get_value("ro_g")

    @ro_g.setter
    def ro_g(self, value: float) -> None:
        """Set the ro_g property."""
        self._cards[0].set_value("ro_g", value)

    @property
    def zdatum(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of datum
        """ # nopep8
        return self._cards[0].get_value("zdatum")

    @zdatum.setter
    def zdatum(self, value: float) -> None:
        """Set the zdatum property."""
        self._cards[0].set_value("zdatum", value)

    @property
    def kfact(self) -> float:
        """Get or set the X- and Y-stress = KFACT x Z-stress
        """ # nopep8
        return self._cards[0].get_value("kfact")

    @kfact.setter
    def kfact(self, value: float) -> None:
        """Set the kfact property."""
        self._cards[0].set_value("kfact", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Optional curve of stress vs z-coordinate (ZDATUM is ignored with this option)
        """ # nopep8
        return self._cards[0].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        """Set the lc property."""
        self._cards[0].set_value("lc", value)

    @property
    def lch(self) -> typing.Optional[int]:
        """Get or set the Optional curve of horizontal stress versus z-coordinate (KFACT is ignored with this option)
        """ # nopep8
        return self._cards[0].get_value("lch")

    @lch.setter
    def lch(self, value: int) -> None:
        """Set the lch property."""
        self._cards[0].set_value("lch", value)

    @property
    def lck0(self) -> typing.Optional[int]:
        """Get or set the Optional curve of K0 (ratio of horizontal_stress/vertical_stress) versus coordinate. KFACT and LCH are ignored with this option. The axis of the curve is the coordinate, the axis is K0.)
        """ # nopep8
        return self._cards[0].get_value("lck0")

    @lck0.setter
    def lck0(self, value: int) -> None:
        """Set the lck0 property."""
        self._cards[0].set_value("lck0", value)

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

