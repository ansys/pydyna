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

import pytest

from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec, Options


# Define field schemas as module-level tuples for reuse
_FIELD_SCHEMAS_A = (FieldSchema("a", int, 0, 10),)
_FIELD_SCHEMAS_B = (FieldSchema("b", int, 0, 10),)


class OptionAPIImplementation:
    def __init__(self, **kwargs):
        self._active_options = set(["FOO"])
        self._option_specs = {
                "FOO": OptionSpec("FOO", 1, 0),
                "BAR": OptionSpec("BAR", 1, 0)
        }
        _cards = [
            OptionCardSet(
                option_spec = self._option_specs["FOO"],
                cards = [
                    Card.from_field_schemas(_FIELD_SCHEMAS_A),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = self._option_specs["BAR"],
                cards = [
                    Card.from_field_schemas(_FIELD_SCHEMAS_B),
                ],
                **kwargs
            ),
        ]

    def is_option_active(self, option: str) -> bool:
        return option in self._active_options

    def activate_option(self, option: str) -> None:
        self._active_options.add(option)

    def deactivate_option(self, option: str) -> None:
        if option in self._active_options:
            self._active_options.remove(option)

    def get_option_spec(self, name: str) -> OptionSpec:
        return self._option_specs[name]

    @property
    def option_specs(self) -> typing.Iterable[OptionSpec]:
        return [self._option_specs["FOO"], self._option_specs["BAR"]]



def test_options_basic():
    impl = OptionAPIImplementation()
    options = Options(impl)
    assert options["FOO"].active is True
    assert options["BAR"].active is False
    options["BAR"].active = True
    assert options["BAR"].active is True



def test_options_union():
    impl = OptionAPIImplementation()
    options = Options(impl)
    assert options["FOO"].active is True
    assert options["BAR"].active is False
    options["BAR"].active = True
    assert options["BAR"].active is True
    assert options["FOO"].active is False
