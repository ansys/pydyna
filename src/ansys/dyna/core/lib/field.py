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

import copy
import dataclasses
import typing

import pandas as pd


@dataclasses.dataclass
class Flag:
    value: bool = None
    true_value: str = None
    false_value: str = None


class Field:
    def __init__(self, name: str, type: type, offset: int, width: int, value: typing.Any = None):
        self._name = name
        self._type = type
        self._offset = offset
        self._width = width
        self._value = value

    def __repr__(self) -> str:
        return f"Field({self.name}, {self.type}, {self.offset}, {self.width}, {self.value})"

    @property
    def name(self) -> str:
        return self._name

    @name.setter
    def name(self, value: str) -> None:
        self._name = value

    @property
    def type(self) -> type:
        return self._type

    @type.setter
    def type(self, value: type) -> None:
        self._type = value

    @property
    def offset(self) -> int:
        return self._offset

    @offset.setter
    def offset(self, value: int) -> None:
        self._offset = value

    @property
    def width(self) -> int:
        return self._width

    @width.setter
    def width(self, value: int) -> None:
        self._width = value

    @property
    def value(self) -> typing.Any:
        if self._truthy_value(self._value) and type(self._value) == Flag:
            return self._value.value
        return self._value

    def _truthy_value(self, value) -> bool:
        if pd.isna(value):
            return False
        return True if value else False

    def _no_value_flag(self, value) -> bool:
        return self._truthy_value(value) and type(value) is Flag

    @value.setter
    def value(self, value: typing.Any) -> None:
        no_value_flag = self._no_value_flag(value)
        if no_value_flag:
            self._value.value = value
        else:
            self._value = value

    def io_info(self) -> typing.Tuple[str, typing.Type]:
        """Return the value and type used for io."""
        no_value_flag = self._no_value_flag(self._value)
        if no_value_flag:
            value = self._value.true_value if self._value.value else self._value.false_value
            return value, str
        return self.value, self.type


def to_long(field: Field, offset: int) -> Field:
    field = copy.copy(field)
    width = field.width
    if width < 20:
        width = 20
    field.offset = offset
    field.width = width
    offset += width
    return field
