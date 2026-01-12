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

"""Module providing the ControlImplicitModalDynamicDampingSpecific class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLIMPLICITMODALDYNAMICDAMPINGSPECIFIC_CARD0 = (
    FieldSchema("mid1", int, 0, 10, None),
    FieldSchema("zeta1", float, 10, 10, None),
    FieldSchema("mid2", int, 20, 10, None),
    FieldSchema("zeta2", float, 30, 10, None),
    FieldSchema("mid3", int, 40, 10, None),
    FieldSchema("zeta3", float, 50, 10, None),
    FieldSchema("mid4", int, 60, 10, None),
    FieldSchema("zeta4", float, 70, 10, None),
)

class ControlImplicitModalDynamicDampingSpecific(KeywordBase):
    """DYNA CONTROL_IMPLICIT_MODAL_DYNAMIC_DAMPING_SPECIFIC keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_MODAL_DYNAMIC_DAMPING_SPECIFIC"

    def __init__(self, **kwargs):
        """Initialize the ControlImplicitModalDynamicDampingSpecific class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLIMPLICITMODALDYNAMICDAMPINGSPECIFIC_CARD0,
                **kwargs,
            ),        ]
    @property
    def mid1(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[0].get_value("mid1")

    @mid1.setter
    def mid1(self, value: int) -> None:
        """Set the mid1 property."""
        self._cards[0].set_value("mid1", value)

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
    def mid2(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[0].get_value("mid2")

    @mid2.setter
    def mid2(self, value: int) -> None:
        """Set the mid2 property."""
        self._cards[0].set_value("mid2", value)

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
    def mid3(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[0].get_value("mid3")

    @mid3.setter
    def mid3(self, value: int) -> None:
        """Set the mid3 property."""
        self._cards[0].set_value("mid3", value)

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
    def mid4(self) -> typing.Optional[int]:
        """Get or set the Mode ID.
        """ # nopep8
        return self._cards[0].get_value("mid4")

    @mid4.setter
    def mid4(self, value: int) -> None:
        """Set the mid4 property."""
        self._cards[0].set_value("mid4", value)

    @property
    def zeta4(self) -> typing.Optional[float]:
        """Get or set the Modal Dynamic damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("zeta4")

    @zeta4.setter
    def zeta4(self, value: float) -> None:
        """Set the zeta4 property."""
        self._cards[0].set_value("zeta4", value)

