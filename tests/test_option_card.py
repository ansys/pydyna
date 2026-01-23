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

import io
import typing

import pytest

from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec, Options
from ansys.dyna.core.lib.parameters import ParameterSet


# Define field schemas as module-level tuples for reuse
_FIELD_SCHEMAS_A = (FieldSchema("a", int, 0, 10),)
_FIELD_SCHEMAS_B = (FieldSchema("b", int, 0, 10),)

# Field schemas for parameter testing (simulating CONTACT SOFT option)
_FIELD_SCHEMAS_WITH_PARAMS = (
    FieldSchema("soft", int, 0, 10, None),
    FieldSchema("sofscl", float, 10, 10, 0.1),
    FieldSchema("lcidab", int, 20, 10, 0),
)


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


@pytest.mark.keywords
def test_options_basic():
    impl = OptionAPIImplementation()
    options = Options(impl)
    assert options["FOO"].active is True
    assert options["BAR"].active is False
    options["BAR"].active = True
    assert options["BAR"].active is True


@pytest.mark.keywords
def test_options_union():
    impl = OptionAPIImplementation()
    options = Options(impl)
    assert options["FOO"].active is True
    assert options["BAR"].active is False
    options["BAR"].active = True
    assert options["BAR"].active is True
    assert options["FOO"].active is False


class MockKeywordForOptionTests:
    """Mock keyword implementing OptionsInterface for testing option cards with parameters."""

    def __init__(self):
        self._active_options = set()
        self._option_specs = {
            "SOFT": OptionSpec("SOFT", 1, 0),
        }

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
        return [self._option_specs["SOFT"]]


class TestOptionCardParameters:
    """Test parameter substitution in OptionCardSet."""

    @pytest.mark.keywords
    def test_option_card_with_single_parameter(self):
        """Test option card with one parameter reference."""
        # Setup
        parameter_set = ParameterSet()
        parameter_set.add("sftval", 2)

        mock_keyword = MockKeywordForOptionTests()
        option_card_set = OptionCardSet(
            option_spec=mock_keyword._option_specs["SOFT"],
            cards=[Card.from_field_schemas(_FIELD_SCHEMAS_WITH_PARAMS)],
            keyword=mock_keyword,
        )

        # Read card with parameter (Note: param name must fit in 10-char field with & prefix)
        card_text = """   &sftval       0.1         0"""
        buf = io.StringIO(card_text)
        option_card_set.read(buf, parameter_set)

        # Verify
        assert option_card_set.cards[0].get_value("soft") == 2
        assert option_card_set.cards[0].get_value("sofscl") == 0.1
        assert option_card_set.cards[0].get_value("lcidab") == 0

    @pytest.mark.keywords
    def test_option_card_with_multiple_parameters(self):
        """Test option card with multiple parameter references."""
        # Setup
        parameter_set = ParameterSet()
        parameter_set.add("sft", 1)
        parameter_set.add("scl", 0.25)
        parameter_set.add("lc", 100)

        mock_keyword = MockKeywordForOptionTests()
        option_card_set = OptionCardSet(
            option_spec=mock_keyword._option_specs["SOFT"],
            cards=[Card.from_field_schemas(_FIELD_SCHEMAS_WITH_PARAMS)],
            keyword=mock_keyword,
        )

        # Read card with multiple parameters
        card_text = """      &sft      &scl       &lc"""
        buf = io.StringIO(card_text)
        option_card_set.read(buf, parameter_set)

        # Verify
        assert option_card_set.cards[0].get_value("soft") == 1
        assert option_card_set.cards[0].get_value("sofscl") == 0.25
        assert option_card_set.cards[0].get_value("lcidab") == 100

    @pytest.mark.keywords
    def test_option_card_with_negative_parameter(self):
        """Test option card with negative parameter (-&param)."""
        # Setup
        parameter_set = ParameterSet()
        parameter_set.add("pval", 2)
        parameter_set.add("scl", 0.5)

        mock_keyword = MockKeywordForOptionTests()
        option_card_set = OptionCardSet(
            option_spec=mock_keyword._option_specs["SOFT"],
            cards=[Card.from_field_schemas(_FIELD_SCHEMAS_WITH_PARAMS)],
            keyword=mock_keyword,
        )

        # Read card with negative parameter
        card_text = """    -&pval     -&scl         0"""
        buf = io.StringIO(card_text)
        option_card_set.read(buf, parameter_set)

        # Verify
        assert option_card_set.cards[0].get_value("soft") == -2
        assert option_card_set.cards[0].get_value("sofscl") == -0.5
        assert option_card_set.cards[0].get_value("lcidab") == 0

    @pytest.mark.keywords
    def test_option_card_without_parameters_still_works(self):
        """Regression test: option cards without parameters still work."""
        # Setup (no parameter set)
        mock_keyword = MockKeywordForOptionTests()
        option_card_set = OptionCardSet(
            option_spec=mock_keyword._option_specs["SOFT"],
            cards=[Card.from_field_schemas(_FIELD_SCHEMAS_WITH_PARAMS)],
            keyword=mock_keyword,
        )

        # Read card with literal values
        card_text = """         2       0.1       100"""
        buf = io.StringIO(card_text)
        option_card_set.read(buf, None)

        # Verify
        assert option_card_set.cards[0].get_value("soft") == 2
        assert option_card_set.cards[0].get_value("sofscl") == 0.1
        assert option_card_set.cards[0].get_value("lcidab") == 100

    @pytest.mark.keywords
    def test_option_card_parameter_not_defined_raises_error(self):
        """Test that undefined parameters raise KeyError."""
        # Setup
        parameter_set = ParameterSet()
        # Note: not defining the parameter

        mock_keyword = MockKeywordForOptionTests()
        option_card_set = OptionCardSet(
            option_spec=mock_keyword._option_specs["SOFT"],
            cards=[Card.from_field_schemas(_FIELD_SCHEMAS_WITH_PARAMS)],
            keyword=mock_keyword,
        )

        # Try to read card with undefined parameter
        card_text = """&undefined       0.1         0"""
        buf = io.StringIO(card_text)

        with pytest.raises(KeyError):
            option_card_set.read(buf, parameter_set)

    @pytest.mark.keywords
    def test_option_card_parameter_type_conversion(self):
        """Test parameter type conversion (int, float)."""
        # Setup - define parameters with different types
        parameter_set = ParameterSet()
        parameter_set.add("intv", 5)  # Integer
        parameter_set.add("fltv", 0.75)  # Float
        parameter_set.add("intf", 10)  # Int used as float

        mock_keyword = MockKeywordForOptionTests()
        option_card_set = OptionCardSet(
            option_spec=mock_keyword._option_specs["SOFT"],
            cards=[Card.from_field_schemas(_FIELD_SCHEMAS_WITH_PARAMS)],
            keyword=mock_keyword,
        )

        # Read card - int field gets int, float field gets float
        card_text = """     &intv     &fltv     &intf"""
        buf = io.StringIO(card_text)
        option_card_set.read(buf, parameter_set)

        # Verify types are correctly converted
        assert option_card_set.cards[0].get_value("soft") == 5  # int
        assert option_card_set.cards[0].get_value("sofscl") == 0.75  # float
        assert option_card_set.cards[0].get_value("lcidab") == 10  # int

    @pytest.mark.keywords
    def test_option_card_mixed_parameters_and_literals(self):
        """Test option card with mix of parameters and literal values."""
        # Setup
        parameter_set = ParameterSet()
        parameter_set.add("sval", 1)

        mock_keyword = MockKeywordForOptionTests()
        option_card_set = OptionCardSet(
            option_spec=mock_keyword._option_specs["SOFT"],
            cards=[Card.from_field_schemas(_FIELD_SCHEMAS_WITH_PARAMS)],
            keyword=mock_keyword,
        )

        # Read card with mix of parameter and literals
        card_text = """     &sval       0.2        50"""
        buf = io.StringIO(card_text)
        option_card_set.read(buf, parameter_set)

        # Verify
        assert option_card_set.cards[0].get_value("soft") == 1  # From parameter
        assert option_card_set.cards[0].get_value("sofscl") == 0.2  # Literal
        assert option_card_set.cards[0].get_value("lcidab") == 50  # Literal
