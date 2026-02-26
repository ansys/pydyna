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

"""Module providing the ControlImplicitModalDynamicDampingFrequencyRange class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLIMPLICITMODALDYNAMICDAMPINGFREQUENCYRANGE_CARD0 = (
    FieldSchema("freq1", float, 0, 10, None),
    FieldSchema("zeta1", float, 10, 10, None),
    FieldSchema("freq2", float, 20, 10, None),
    FieldSchema("zeta2", float, 30, 10, None),
    FieldSchema("freq3", float, 40, 10, None),
    FieldSchema("zeta3", float, 50, 10, None),
    FieldSchema("freq4", float, 60, 10, None),
    FieldSchema("zeta4", float, 70, 10, None),
)

class ControlImplicitModalDynamicDampingFrequencyRange(KeywordBase):
    """DYNA CONTROL_IMPLICIT_MODAL_DYNAMIC_DAMPING_FREQUENCY_RANGE keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_MODAL_DYNAMIC_DAMPING_FREQUENCY_RANGE"

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitModalDynamicDampingFrequencyRange class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITMODALDYNAMICDAMPINGFREQUENCYRANGE_CARD0,
                **kwargs,
            ),        ]
    @property
    def freq1(self) -> typing.Optional[float]:
        """Get or set the Frequency value.
        """ # nopep8
        return self._cards[0].get_value("freq1")

    @freq1.setter
    def freq1(self, value: float) -> None:
        """Set the freq1 property."""
        self._cards[0].set_value("freq1", value)

    @property
    def zeta1(self) -> typing.Optional[float]:
        """Get or set the Modal Dynamic damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("zeta1")

    @zeta1.setter
    def zeta1(self, value: float) -> None:
        """Set the zeta1 property."""
        self._cards[0].set_value("zeta1", value)

    @property
    def freq2(self) -> typing.Optional[float]:
        """Get or set the Frequency value.
        """ # nopep8
        return self._cards[0].get_value("freq2")

    @freq2.setter
    def freq2(self, value: float) -> None:
        """Set the freq2 property."""
        self._cards[0].set_value("freq2", value)

    @property
    def zeta2(self) -> typing.Optional[float]:
        """Get or set the Modal Dynamic damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("zeta2")

    @zeta2.setter
    def zeta2(self, value: float) -> None:
        """Set the zeta2 property."""
        self._cards[0].set_value("zeta2", value)

    @property
    def freq3(self) -> typing.Optional[float]:
        """Get or set the Frequency value.
        """ # nopep8
        return self._cards[0].get_value("freq3")

    @freq3.setter
    def freq3(self, value: float) -> None:
        """Set the freq3 property."""
        self._cards[0].set_value("freq3", value)

    @property
    def zeta3(self) -> typing.Optional[float]:
        """Get or set the Modal Dynamic damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("zeta3")

    @zeta3.setter
    def zeta3(self, value: float) -> None:
        """Set the zeta3 property."""
        self._cards[0].set_value("zeta3", value)

    @property
    def freq4(self) -> typing.Optional[float]:
        """Get or set the Frequency value.
        """ # nopep8
        return self._cards[0].get_value("freq4")

    @freq4.setter
    def freq4(self, value: float) -> None:
        """Set the freq4 property."""
        self._cards[0].set_value("freq4", value)

    @property
    def zeta4(self) -> typing.Optional[float]:
        """Get or set the Modal Dynamic damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("zeta4")

    @zeta4.setter
    def zeta4(self, value: float) -> None:
        """Set the zeta4 property."""
        self._cards[0].set_value("zeta4", value)

