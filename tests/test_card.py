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

import pytest

from ansys.dyna.core.lib.card import (
    Card,
    Field,
    _field_signature,
    _format_spec_cache,
    _get_cached_format_spec,
    _get_cached_schema,
    _schema_cache,
)
from ansys.dyna.core.lib.field import Flag
from ansys.dyna.core.lib.field_schema import CardSchema, FieldSchema
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.parameters import ParameterSet


@pytest.mark.keywords
def test_load_card_errors(string_utils):
    """Error test for loading a card."""
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]

    card = Card(fields)
    with pytest.raises(Exception):
        # cards can only load a readable buffer
        card.read("")

    with pytest.warns(UserWarning, match="Detected out of bound card characters"):
        # error if the line that is too long
        buf = "                                           "
        card.read(string_utils.as_buffer(buf))


@pytest.mark.keywords
def test_load_card_parameters(string_utils):
    """Error test for loading a card."""
    fields = [
        Field("a", float, 0, 10, None),
        Field("b", float, 10, 10, None),
        Field("c", float, 20, 10, None),
        Field("d", float, 30, 10, None),
        Field("e", float, 40, 10, None),
        Field("f", float, 50, 10, None),
        Field("g", float, 60, 10, None),
        Field("h", float, 70, 10, None),
    ]

    card = Card(fields)
    parameter_set = ParameterSet()
    parameter_set.add("vdct", 1.12)
    buf = "                                             &vdct"
    card.read(string_utils.as_buffer(buf), parameter_set)
    assert card.get_value("e") == 1.12


@pytest.mark.keywords
def test_load_card_basic(string_utils):
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]
    card = Card(fields)
    card.read(string_utils.as_buffer("                    "))
    assert card.get_value("foo") == None
    assert card.get_value("bar") == None
    card = Card(fields)
    card.read(string_utils.as_buffer("         8         4"))
    assert card.get_value("foo") == 8
    assert card.get_value("bar") == 4


@pytest.mark.keywords
def test_load_card_long(string_utils):
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]
    card = Card(fields, format=format_type.long)
    buf = string_utils.as_buffer("                                       4")
    card.read(buf)
    assert card.get_value("foo") == None
    assert card.get_value("bar") == 4


@pytest.mark.keywords
def test_write_inactive_card():
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]
    card = Card(fields, lambda: False, format=format_type.long)
    assert card.write() == ""


# =============================================================================
# Tests for the new Card schema/values architecture
# =============================================================================


class TestFieldSchema:
    """Tests for FieldSchema class."""

    @pytest.mark.keywords
    def test_field_schema_from_field_basic(self):
        """Test creating FieldSchema from a basic Field."""
        field = Field("test", int, 0, 10, 42)
        schema = FieldSchema.from_field(field)

        assert schema.name == "test"
        assert schema.type == int
        assert schema.offset == 0
        assert schema.width == 10
        assert schema.default == 42
        assert not schema.is_flag()

    @pytest.mark.keywords
    def test_field_schema_from_field_with_flag(self):
        """Test creating FieldSchema from a Flag Field."""
        flag = Flag(value=True, true_value="YES", false_value="NO")
        field = Field("active", str, 0, 10, flag)
        schema = FieldSchema.from_field(field)

        assert schema.name == "active"
        assert schema.is_flag()
        assert schema.default.true_value == "YES"
        assert schema.default.false_value == "NO"

    @pytest.mark.keywords
    def test_field_schema_to_field_roundtrip(self):
        """Test that to_field creates a correct Field from schema."""
        original = Field("count", int, 10, 10, 99)
        schema = FieldSchema.from_field(original)
        reconstructed = schema.to_field(value=123)

        assert reconstructed.name == "count"
        assert reconstructed.type == int
        assert reconstructed.offset == 10
        assert reconstructed.width == 10
        assert reconstructed.value == 123

    @pytest.mark.keywords
    def test_field_schema_to_field_flag_roundtrip(self):
        """Test flag field roundtrip through schema."""
        flag = Flag(value=False, true_value="ON", false_value="OFF")
        original = Field("enabled", str, 0, 10, flag)
        schema = FieldSchema.from_field(original)
        reconstructed = schema.to_field(value=True)

        assert reconstructed.name == "enabled"
        assert reconstructed._is_flag()
        assert reconstructed.value is True


class TestCardSchema:
    """Tests for CardSchema class."""

    @pytest.mark.keywords
    def test_card_schema_from_fields(self):
        """Test creating CardSchema from a list of Fields."""
        fields = [
            Field("a", int, 0, 10, None),
            Field("b", float, 10, 10, None),
            Field("c", str, 20, 10, None),
        ]
        schema = CardSchema.from_fields(fields)

        assert len(schema) == 3
        assert schema.get_index("a") == 0
        assert schema.get_index("b") == 1
        assert schema.get_index("c") == 2

    @pytest.mark.keywords
    def test_card_schema_to_fields(self):
        """Test reconstructing Fields from CardSchema with values."""
        fields = [
            Field("x", int, 0, 10, None),
            Field("y", float, 10, 10, None),
        ]
        schema = CardSchema.from_fields(fields)
        values = [42, 3.14]
        reconstructed = schema.to_fields(values)

        assert len(reconstructed) == 2
        assert reconstructed[0].name == "x"
        assert reconstructed[0].value == 42
        assert reconstructed[1].name == "y"
        assert reconstructed[1].value == 3.14


class TestFieldSignature:
    """Tests for _field_signature function."""

    @pytest.mark.keywords
    def test_field_signature_basic(self):
        """Test signature for basic field."""
        field = Field("test", int, 0, 10, None)
        sig = _field_signature(field)

        assert sig == ("test", int, 0, 10)

    @pytest.mark.keywords
    def test_field_signature_with_flag(self):
        """Test signature for flag field includes flag info."""
        flag = Flag(value=True, true_value="Y", false_value="N")
        field = Field("flag_field", str, 0, 10, flag)
        sig = _field_signature(field)

        assert sig == ("flag_field", str, 0, 10, "FLAG", "Y", "N")

    @pytest.mark.keywords
    def test_field_signature_different_values_same_signature(self):
        """Two fields with same schema but different values have same signature."""
        field1 = Field("num", int, 0, 10, 100)
        field2 = Field("num", int, 0, 10, 200)

        assert _field_signature(field1) == _field_signature(field2)

    @pytest.mark.keywords
    def test_field_signature_different_schema_different_signature(self):
        """Fields with different schemas have different signatures."""
        field1 = Field("a", int, 0, 10, None)
        field2 = Field("b", int, 0, 10, None)  # different name
        field3 = Field("a", float, 0, 10, None)  # different type

        assert _field_signature(field1) != _field_signature(field2)
        assert _field_signature(field1) != _field_signature(field3)


class TestSchemaCaching:
    """Tests for schema caching functionality."""

    @pytest.mark.keywords
    def test_schema_cache_hit(self):
        """Test that identical field lists use cached schema."""
        fields1 = [
            Field("x", int, 0, 10, None),
            Field("y", int, 10, 10, None),
        ]
        fields2 = [
            Field("x", int, 0, 10, 999),  # different value
            Field("y", int, 10, 10, 888),  # different value
        ]

        schema1, sig1 = _get_cached_schema(fields1)
        schema2, sig2 = _get_cached_schema(fields2)

        # Same signature means same schema object
        assert sig1 == sig2
        assert schema1 is schema2

    @pytest.mark.keywords
    def test_schema_cache_miss(self):
        """Test that different field structures create different schemas."""
        fields1 = [Field("a", int, 0, 10, None)]
        fields2 = [Field("b", int, 0, 10, None)]  # different name

        schema1, sig1 = _get_cached_schema(fields1)
        schema2, sig2 = _get_cached_schema(fields2)

        assert sig1 != sig2
        assert schema1 is not schema2


class TestCardSchemaIntegration:
    """Integration tests for Card with schema architecture."""

    @pytest.mark.keywords
    def test_card_stores_schema_and_values_separately(self):
        """Test that Card stores schema reference and values list."""
        fields = [
            Field("a", int, 0, 10, 1),
            Field("b", int, 10, 10, 2),
        ]
        card = Card(fields)

        # Card should have _schema and _values attributes
        assert hasattr(card, "_schema")
        assert hasattr(card, "_values")
        assert hasattr(card, "_signature")

        # Values should be stored in list
        assert card._values == [1, 2]

    @pytest.mark.keywords
    def test_multiple_cards_share_schema(self):
        """Test that multiple cards with same structure share schema."""
        fields1 = [Field("x", int, 0, 10, 100)]
        fields2 = [Field("x", int, 0, 10, 200)]

        card1 = Card(fields1)
        card2 = Card(fields2)

        # Same schema, different values
        assert card1._schema is card2._schema
        assert card1._values != card2._values

    @pytest.mark.keywords
    def test_card_get_value_uses_index_lookup(self):
        """Test that get_value uses efficient index-based lookup."""
        fields = [
            Field("first", int, 0, 10, 10),
            Field("second", float, 10, 10, 2.5),
            Field("third", str, 20, 10, "hello"),
        ]
        card = Card(fields)

        assert card.get_value("first") == 10
        assert card.get_value("second") == 2.5
        assert card.get_value("third") == "hello"

    @pytest.mark.keywords
    def test_card_set_value_updates_values_list(self):
        """Test that set_value updates the internal values list."""
        fields = [
            Field("count", int, 0, 10, 0),
        ]
        card = Card(fields)

        card.set_value("count", 42)

        assert card._values[0] == 42
        assert card.get_value("count") == 42

    @pytest.mark.keywords
    def test_card_fields_property_creates_fields_lazily(self):
        """Test that _fields property creates Field objects from schema+values."""
        fields = [
            Field("a", int, 0, 10, 1),
            Field("b", int, 10, 10, 2),
        ]
        card = Card(fields)

        # Modify values
        card.set_value("a", 100)
        card.set_value("b", 200)

        # _fields property should create new Fields with current values
        lazy_fields = card._fields
        assert lazy_fields[0].value == 100
        assert lazy_fields[1].value == 200

    @pytest.mark.keywords
    def test_card_load_updates_values_not_schema(self, string_utils):
        """Test that loading data updates values but reuses schema."""
        fields = [
            Field("x", int, 0, 10, None),
            Field("y", int, 10, 10, None),
        ]
        card = Card(fields)
        original_schema = card._schema

        card.read(string_utils.as_buffer("         5        10"))

        # Schema should be unchanged
        assert card._schema is original_schema
        # Values should be updated
        assert card._values == [5, 10]

    @pytest.mark.keywords
    def test_card_write_uses_lazy_fields(self):
        """Test that write creates proper output from schema+values."""
        fields = [
            Field("a", int, 0, 10, 42),
            Field("b", int, 10, 10, 99),
        ]
        card = Card(fields)

        output = card.write(comment=False)

        assert "42" in output
        assert "99" in output

    @pytest.mark.keywords
    def test_card_with_flag_field(self, string_utils):
        """Test Card handles Flag fields correctly through schema."""
        flag = Flag(value=None, true_value="YES", false_value="NO")
        fields = [
            Field("enabled", str, 0, 10, flag),
            Field("count", int, 10, 10, None),
        ]
        card = Card(fields)

        # Load some data
        card.read(string_utils.as_buffer("       YES         5"))

        # Verify values
        assert card.get_value("count") == 5


class TestFromFieldSchemas:
    """Tests for Card.from_field_schemas() fast path."""

    @pytest.mark.keywords
    def test_from_field_schemas_basic(self):
        """Test basic Card creation from FieldSchema tuple."""
        field_schemas = (
            FieldSchema("eid", int, 0, 10, 0),
            FieldSchema("pid", int, 10, 10, 0),
        )
        card = Card.from_field_schemas(field_schemas)

        assert card.get_value("eid") == 0
        assert card.get_value("pid") == 0

    @pytest.mark.keywords
    def test_from_field_schemas_with_values(self):
        """Test Card creation with initial values."""
        field_schemas = (
            FieldSchema("x", int, 0, 10, 0),
            FieldSchema("y", float, 10, 10, 0.0),
        )
        card = Card.from_field_schemas(field_schemas, values=[42, 3.14])

        assert card.get_value("x") == 42
        assert card.get_value("y") == 3.14

    @pytest.mark.keywords
    def test_from_field_schemas_caches_schema(self):
        """Test that same tuple reuses cached schema."""
        field_schemas = (
            FieldSchema("a", int, 0, 10, 0),
            FieldSchema("b", int, 10, 10, 0),
        )
        card1 = Card.from_field_schemas(field_schemas)
        card2 = Card.from_field_schemas(field_schemas)

        # Same tuple should produce same cached schema
        assert card1._schema is card2._schema
        assert card1._signature is card2._signature

    @pytest.mark.keywords
    def test_from_field_schemas_different_tuples_different_schemas(self):
        """Test that different tuples create different schemas."""
        schemas1 = (FieldSchema("a", int, 0, 10, 0),)
        schemas2 = (FieldSchema("b", int, 0, 10, 0),)

        card1 = Card.from_field_schemas(schemas1)
        card2 = Card.from_field_schemas(schemas2)

        assert card1._schema is not card2._schema

    @pytest.mark.keywords
    def test_from_field_schemas_set_value(self):
        """Test set_value works on Card from field schemas."""
        field_schemas = (FieldSchema("val", int, 0, 10, 0),)
        card = Card.from_field_schemas(field_schemas)

        card.set_value("val", 999)
        assert card.get_value("val") == 999

    @pytest.mark.keywords
    def test_from_field_schemas_write(self):
        """Test write works on Card from field schemas."""
        field_schemas = (
            FieldSchema("x", int, 0, 10, 0),
            FieldSchema("y", int, 10, 10, 0),
        )
        card = Card.from_field_schemas(field_schemas, values=[100, 200])

        output = card.write()
        assert "100" in output
        assert "200" in output

    @pytest.mark.keywords
    def test_from_field_schemas_read(self, string_utils):
        """Test read works on Card from field schemas."""
        field_schemas = (
            FieldSchema("a", int, 0, 10, 0),
            FieldSchema("b", float, 10, 10, 0.0),
        )
        card = Card.from_field_schemas(field_schemas)

        card.read(string_utils.as_buffer("        42       3.5"))

        assert card.get_value("a") == 42
        assert card.get_value("b") == 3.5

    @pytest.mark.keywords
    def test_from_field_schemas_with_flag(self, string_utils):
        """Test Card.from_field_schemas with Flag field."""
        flag = Flag(value=None, true_value="Y", false_value="N")
        field_schemas = (
            FieldSchema("enabled", str, 0, 10, flag),
            FieldSchema("count", int, 10, 10, 0),
        )
        card = Card.from_field_schemas(field_schemas)

        card.read(string_utils.as_buffer("         Y        10"))

        assert card.get_value("count") == 10


class TestFormatSpecCaching:
    """Tests for FormatSpec caching."""

    @pytest.mark.keywords
    def test_format_spec_cached_per_format_type(self):
        """Test that FormatSpec is cached separately for each format type."""
        fields = [Field("x", int, 0, 10, None)]
        schema, signature = _get_cached_schema(fields)

        spec_default = _get_cached_format_spec(signature, schema, format_type.default)
        spec_long = _get_cached_format_spec(signature, schema, format_type.long)

        # Different format types should have different specs
        assert spec_default is not spec_long

        # Same format type should return cached spec
        spec_default_again = _get_cached_format_spec(signature, schema, format_type.default)
        assert spec_default is spec_default_again
