# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""Tests for handler base classes and metadata system."""

import logging
from unittest.mock import patch

import pytest

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.handlers.handler_base import (
    HandlerMetadata,
    KeywordHandler,
    _HANDLER_METADATA,
    get_all_handler_metadata,
    get_handler_metadata,
    handler,
    validate_handler_settings,
)



class TestHandlerMetadata:
    """Test HandlerMetadata dataclass."""

    def test_metadata_creation_minimal(self):
        """Test creating HandlerMetadata with minimal required fields."""

        class DummyHandler(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        metadata = HandlerMetadata(name="test-handler", handler_class=DummyHandler)

        assert metadata.name == "test-handler"
        assert metadata.handler_class == DummyHandler
        assert metadata.phase == "handle"
        assert metadata.description == ""
        assert metadata.input_schema is None
        assert metadata.output_description == ""

    def test_metadata_creation_full(self):
        """Test creating HandlerMetadata with all fields."""

        class DummyHandler(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        schema = {"type": "array", "items": {"type": "object"}}
        metadata = HandlerMetadata(
            name="test-handler",
            handler_class=DummyHandler,
            phase="post_process",
            description="Test description",
            input_schema=schema,
            output_description="Adds test field",
        )

        assert metadata.name == "test-handler"
        assert metadata.handler_class == DummyHandler
        assert metadata.phase == "post_process"
        assert metadata.description == "Test description"
        assert metadata.input_schema == schema
        assert metadata.output_description == "Adds test field"



class TestHandlerDecorator:
    """Test handler decorator registration."""

    def test_handler_decorator_registration(self):
        """Test that @handler decorator registers handler metadata."""
        # Clear any existing test handler
        _HANDLER_METADATA.pop("test-decorator-handler", None)

        @handler(name="test-decorator-handler", description="Test handler")
        class TestDecoratorHandler(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        # Check handler was registered
        assert "test-decorator-handler" in _HANDLER_METADATA
        metadata = _HANDLER_METADATA["test-decorator-handler"]
        assert metadata.name == "test-decorator-handler"
        assert metadata.handler_class == TestDecoratorHandler
        assert metadata.description == "Test handler"

    def test_handler_decorator_with_schema(self):
        """Test handler decorator with input schema."""
        _HANDLER_METADATA.pop("test-schema-handler", None)

        schema = {
            "type": "array",
            "items": {"type": "object", "properties": {"index": {"type": "integer"}}},
        }

        @handler(
            name="test-schema-handler",
            input_schema=schema,
            output_description="Modifies cards",
        )
        class TestSchemaHandler(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        metadata = _HANDLER_METADATA["test-schema-handler"]
        assert metadata.input_schema == schema
        assert metadata.output_description == "Modifies cards"

    def test_handler_decorator_stores_metadata_on_class(self):
        """Test that decorator stores metadata on the class itself."""
        _HANDLER_METADATA.pop("test-class-meta", None)

        @handler(name="test-class-meta")
        class TestClassMeta(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        assert hasattr(TestClassMeta, "_handler_metadata")
        assert TestClassMeta._handler_metadata.name == "test-class-meta"



class TestGetHandlerMetadata:
    """Test get_handler_metadata function."""

    def test_get_existing_handler(self):
        """Test retrieving metadata for an existing handler."""
        _HANDLER_METADATA.pop("test-get-handler", None)

        @handler(name="test-get-handler")
        class TestGetHandler(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        metadata = get_handler_metadata("test-get-handler")
        assert metadata is not None
        assert metadata.name == "test-get-handler"
        assert metadata.handler_class == TestGetHandler

    def test_get_nonexistent_handler(self):
        """Test retrieving metadata for non-existent handler returns None."""
        metadata = get_handler_metadata("nonexistent-handler-xyz")
        assert metadata is None



class TestGetAllHandlerMetadata:
    """Test get_all_handler_metadata function."""

    def test_get_all_handlers(self):
        """Test retrieving all handler metadata."""
        # Register some test handlers
        _HANDLER_METADATA.pop("test-all-1", None)
        _HANDLER_METADATA.pop("test-all-2", None)

        @handler(name="test-all-1")
        class TestAll1(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        @handler(name="test-all-2")
        class TestAll2(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        all_metadata = get_all_handler_metadata()
        assert isinstance(all_metadata, dict)
        assert "test-all-1" in all_metadata
        assert "test-all-2" in all_metadata

    def test_get_all_returns_copy(self):
        """Test that get_all_handler_metadata returns a copy, not the original."""
        all_metadata = get_all_handler_metadata()
        original_keys = set(all_metadata.keys())

        # Modify the returned dict
        all_metadata["fake-handler"] = None

        # Original should be unchanged
        all_metadata_again = get_all_handler_metadata()
        assert "fake-handler" not in all_metadata_again
        assert set(all_metadata_again.keys()) == original_keys



class TestValidateHandlerSettings:
    """Test validate_handler_settings function."""

    def test_validate_with_valid_settings(self):
        """Test validation with valid settings passes."""
        _HANDLER_METADATA.pop("test-validate-valid", None)

        schema = {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {"index": {"type": "integer"}, "name": {"type": "string"}},
                "required": ["index"],
            },
        }

        @handler(name="test-validate-valid", input_schema=schema)
        class TestValidateValid(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        # Should not raise
        validate_handler_settings("test-validate-valid", [{"index": 1, "name": "test"}])
        validate_handler_settings("test-validate-valid", [{"index": 1}, {"index": 2}])

    def test_validate_with_invalid_settings(self):
        """Test validation with invalid settings raises ValueError."""
        _HANDLER_METADATA.pop("test-validate-invalid", None)

        schema = {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {"index": {"type": "integer"}},
                "required": ["index"],
            },
        }

        @handler(name="test-validate-invalid", input_schema=schema)
        class TestValidateInvalid(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        # Missing required field
        with pytest.raises(ValueError, match="Invalid settings"):
            validate_handler_settings("test-validate-invalid", [{"name": "test"}])

        # Wrong type
        with pytest.raises(ValueError, match="Invalid settings"):
            validate_handler_settings("test-validate-invalid", [{"index": "not-an-int"}])

    def test_validate_nonexistent_handler(self):
        """Test validation for non-existent handler raises ValueError."""
        with pytest.raises(ValueError, match="Handler 'nonexistent-xyz' not found"):
            validate_handler_settings("nonexistent-xyz", [])

    def test_validate_handler_without_schema(self, caplog):
        """Test validation skips if no schema defined."""
        _HANDLER_METADATA.pop("test-no-schema", None)

        @handler(name="test-no-schema")
        class TestNoSchema(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        with caplog.at_level(logging.DEBUG):
            validate_handler_settings("test-no-schema", [{"anything": "goes"}])

        # Should log that validation is skipped
        assert any("No input schema defined" in record.message for record in caplog.records)

    def test_validate_without_jsonschema_installed(self, caplog):
        """Test validation is skipped if jsonschema not installed."""
        _HANDLER_METADATA.pop("test-no-jsonschema", None)

        schema = {"type": "array"}

        @handler(name="test-no-jsonschema", input_schema=schema)
        class TestNoJsonschema(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        # Mock the import statement inside validate_handler_settings to simulate jsonschema not being installed
        import builtins
        real_import = builtins.__import__

        def mock_import(name, *args, **kwargs):
            if name == "jsonschema":
                raise ImportError("No module named 'jsonschema'")
            return real_import(name, *args, **kwargs)

        with patch.object(builtins, "__import__", mock_import):
            with caplog.at_level(logging.WARNING):
                # Should not raise, just log warning
                validate_handler_settings("test-no-jsonschema", [{"anything": "goes"}])

            # Check warning was logged
            assert any(
                "jsonschema package not installed" in record.message for record in caplog.records
            )

    def test_validate_with_schema_error(self):
        """Test validation with invalid schema raises ValueError."""
        _HANDLER_METADATA.pop("test-schema-error", None)

        # Invalid schema (missing 'type' in property)
        bad_schema = {
            "type": "array",
            "items": {"type": "object", "properties": {"index": {"invalid_key": "value"}}},
        }

        @handler(name="test-schema-error", input_schema=bad_schema)
        class TestSchemaError(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        # This might pass basic validation, so let's test with actual schema error
        # We'll create a deliberately broken schema that jsonschema will reject
        _HANDLER_METADATA["test-schema-error"].input_schema = {
            "type": "invalid_type_that_does_not_exist"
        }

        with pytest.raises(ValueError, match="Invalid"):
            validate_handler_settings("test-schema-error", [])



class TestKeywordHandler:
    """Test KeywordHandler abstract base class."""

    def test_cannot_instantiate_abstract_class(self):
        """Test that KeywordHandler cannot be instantiated directly."""
        with pytest.raises(TypeError):
            KeywordHandler()

    def test_handle_is_only_required_method(self):
        """Test that implementing handle() is sufficient for instantiation.

        post_process() has a default implementation so it's optional.
        Python ABC doesn't check method signatures, only that the method exists.
        """

        class MinimalHandler(KeywordHandler):
            def handle(self, kwd_data):
                pass

        # Should not raise - handle() is implemented, post_process() has default
        handler = MinimalHandler()
        assert isinstance(handler, KeywordHandler)

    def test_must_implement_handle(self):
        """Test that subclasses must implement handle method."""

        class IncompleteHandler(KeywordHandler):
            def post_process(self, kwd_data, settings):
                pass

        with pytest.raises(TypeError):
            IncompleteHandler()

    def test_complete_handler_can_be_instantiated(self):
        """Test that complete handler implementation can be instantiated."""

        class CompleteHandler(KeywordHandler):
            def handle(self, kwd_data, settings):
                pass

            def post_process(self, kwd_data):
                pass

        # Should not raise
        handler_instance = CompleteHandler()
        assert isinstance(handler_instance, KeywordHandler)

    def test_handler_methods_are_called(self):
        """Test that handler methods can be called with correct arguments."""

        class TrackingHandler(KeywordHandler):
            def __init__(self):
                self.handle_called = False
                self.post_process_called = False

            def handle(self, kwd_data, settings):
                self.handle_called = True
                assert isinstance(kwd_data, KeywordData)
                assert isinstance(settings, list)

            def post_process(self, kwd_data):
                self.post_process_called = True
                assert isinstance(kwd_data, KeywordData)

        handler_instance = TrackingHandler()
        kwd_data = KeywordData(
            keyword="TEST", subkeyword="KEYWORD", title="*TEST_KEYWORD", cards=[]
        )

        handler_instance.handle(kwd_data, [{"test": "setting"}])
        assert handler_instance.handle_called

        handler_instance.post_process(kwd_data)
        assert handler_instance.post_process_called