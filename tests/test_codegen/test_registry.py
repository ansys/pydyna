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

"""Tests for handler registry functionality."""

import pytest

from keyword_generation.handlers.registry import HandlerRegistry, create_default_registry, discover_handlers


@pytest.mark.codegen
class TestHandlerRegistry:
    """Test HandlerRegistry functionality."""

    def test_registry_creation(self):
        """Test creating an empty registry."""
        registry = HandlerRegistry()
        assert len(registry.get_handler_names()) == 0

    def test_default_registry_has_handlers(self):
        """Test that default registry discovers and registers handlers."""
        registry = create_default_registry()
        handler_names = registry.get_handler_names()

        # Verify we have handlers registered
        assert len(handler_names) > 0

        # Verify critical handlers are present
        expected_handlers = [
            "reorder-card",
            "table-card",
            "override-field",
            "replace-card",
            "insert-card",
            "series-card",
            "add-option",
            "card-set",
            "conditional-card",
            "rename-property",
            "skip-card",
            "table-card-group",
            "external-card-implementation",
            "shared-field",
        ]
        for handler in expected_handlers:
            assert handler in handler_names, f"Expected handler '{handler}' not found in registry"

    def test_handler_execution_order(self):
        """Test that handlers are registered in the correct critical order."""
        registry = create_default_registry()
        handler_names = registry.get_handler_names()

        # Verify critical ordering constraints
        reorder_idx = handler_names.index("reorder-card")
        card_set_idx = handler_names.index("card-set")
        conditional_idx = handler_names.index("conditional-card")

        # reorder-card must be first (other handlers use positional indices)
        assert reorder_idx == 0, "reorder-card must run first"

        # card-set must run before conditional-card
        # (conditional-card needs to modify cards via shared references)
        assert (
            card_set_idx < conditional_idx
        ), "card-set must run before conditional-card for reference semantics"

    def test_discover_handlers(self):
        """Test that handler discovery finds all decorated handlers."""
        metadata = discover_handlers()
        assert len(metadata) > 0

        # Check that metadata has expected structure
        for name, meta in metadata.items():
            assert hasattr(meta, "name")
            assert hasattr(meta, "handler_class")
            assert hasattr(meta, "dependencies")
            assert meta.name == name

    def test_apply_all_with_no_settings(self, sample_keyword_data):
        """Test that apply_all handles empty settings gracefully."""
        registry = create_default_registry()
        settings = {}

        # Should not raise an error
        registry.apply_all(sample_keyword_data, settings)

        # Keyword data should be unchanged
        assert sample_keyword_data.keyword == "SECTION"
        assert len(sample_keyword_data.cards) == 1

    def test_apply_all_with_nonexistent_handler(self, sample_keyword_data):
        """Test that apply_all ignores settings for unregistered handlers."""
        registry = HandlerRegistry()
        settings = {"nonexistent-handler": [{"some": "setting"}]}

        # Should not raise an error (just ignores unknown handler)
        registry.apply_all(sample_keyword_data, settings)

    def test_post_process_all(self, sample_keyword_data):
        """Test that post_process_all runs for all handlers."""
        registry = create_default_registry()

        # Should not raise an error
        registry.post_process_all(sample_keyword_data)

