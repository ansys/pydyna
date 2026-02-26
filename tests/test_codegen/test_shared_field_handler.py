# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Tests for the shared-field handler."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.handlers.shared_field import SharedFieldHandler, SharedFieldSettings


class TestSharedFieldSettings:
    """Test SharedFieldSettings dataclass functionality."""

    def test_settings_creation(self):
        """Test creating settings with field_name and card_indices."""
        settings = SharedFieldSettings(field_name="pid", card_indices=[0, 1, 2])
        assert settings.field_name == "pid"
        assert settings.card_indices == [0, 1, 2]

    def test_from_dict(self):
        """Test from_dict parsing."""
        data = {"field-name": "secid", "card-indices": [1, 3, 5]}
        settings = SharedFieldSettings.from_dict(data)
        assert settings.field_name == "secid"
        assert settings.card_indices == [1, 3, 5]


class TestSharedFieldHandler:
    """Test SharedFieldHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with duplicate fields across cards."""
        return KeywordData(
            keyword="SECTION",
            subkeyword="SHELL",
            title="*SECTION_SHELL",
            cards=[
                Card(
                    index=0,
                    fields=[
                        Field(name="pid", type="int", position=0, width=10),
                        Field(name="secid", type="int", position=10, width=10),
                    ],
                ),
                Card(
                    index=1,
                    fields=[
                        Field(name="pid", type="int", position=0, width=10),
                        Field(name="elform", type="int", position=10, width=10),
                    ],
                ),
                Card(
                    index=2,
                    fields=[
                        Field(name="pid", type="int", position=0, width=10),
                        Field(name="t1", type="float", position=10, width=10),
                    ],
                ),
            ],
        )

    @pytest.fixture
    def handler(self):
        """Create a SharedFieldHandler instance."""
        return SharedFieldHandler()

    def test_handle_positive_indices(self, handler, sample_kwd_data):
        """Test marking shared fields with positive card indices (> 0)."""
        # NOTE: index 0 is treated as negative by the handler (uses `c > 0`)
        settings = [{"name": "pid", "cards": [1, 2]}]

        handler.handle(sample_kwd_data, settings)

        # First occurrence (across all cards with 'pid') gets card_indices
        assert sample_kwd_data.cards[0]["fields"][0]["card_indices"] == [1, 2]
        assert sample_kwd_data.cards[0]["fields"][0].get("redundant") != True

        # Subsequent occurrences marked redundant
        assert sample_kwd_data.cards[1]["fields"][0]["redundant"] is True
        assert sample_kwd_data.cards[2]["fields"][0]["redundant"] is True

    def test_handle_preserves_other_fields(self, handler, sample_kwd_data):
        """Test that only the specified field is marked, not others."""
        settings = [{"name": "pid", "cards": [1, 2]}]

        handler.handle(sample_kwd_data, settings)

        # Non-shared fields should not be affected
        assert sample_kwd_data.cards[0]["fields"][1].get("card_indices") is None
        assert sample_kwd_data.cards[0]["fields"][1].get("redundant") != True
        assert sample_kwd_data.cards[1]["fields"][1].get("redundant") != True

    def test_handle_negative_indices_deferred(self, handler, sample_kwd_data):
        """Test that negative indices are stored for post_process."""
        settings = [{"name": "sid", "cards": [-1, -2]}]

        handler.handle(sample_kwd_data, settings)

        # Should be stored in negative_shared_fields
        assert len(sample_kwd_data.negative_shared_fields) == 1
        assert sample_kwd_data.negative_shared_fields[0]["name"] == "sid"
        assert sample_kwd_data.negative_shared_fields[0]["cards"] == [-1, -2]

    def test_handle_with_index_zero_treated_as_negative(self, handler, sample_kwd_data):
        """Test that index 0 is treated as negative (deferred to post_process)."""
        # Handler uses `c > 0` so 0 is treated as negative
        settings = [{"name": "pid", "cards": [0]}]

        handler.handle(sample_kwd_data, settings)

        # Should be stored in negative_shared_fields
        assert len(sample_kwd_data.negative_shared_fields) == 1

    def test_post_process_negative_indices_in_options(self, handler, sample_kwd_data):
        """Test post_process handles negative indices in option cards."""
        # Set up option groups with cards
        sample_kwd_data.options = [
            {
                "name": "OPTION1",
                "cards": [
                    Card(
                        index=1,
                        fields=[
                            Field(name="sid", type="int", position=0, width=10),
                            Field(name="val1", type="float", position=10, width=10),
                        ],
                    ),
                ],
            },
            {
                "name": "OPTION2",
                "cards": [
                    Card(
                        index=2,
                        fields=[
                            Field(name="sid", type="int", position=0, width=10),
                            Field(name="val2", type="float", position=10, width=10),
                        ],
                    ),
                ],
            },
        ]

        # Add negative shared field
        sample_kwd_data.negative_shared_fields = [
            {"name": "sid", "cards": [-1, -2], "applied_card_indices": False}
        ]

        handler.post_process(sample_kwd_data)

        # First occurrence in options gets card_indices
        option1_sid = sample_kwd_data.options[0]["cards"][0]["fields"][0]
        assert option1_sid["card_indices"] == [1, 2]
        assert option1_sid.get("redundant") != True

        # Second occurrence marked redundant
        option2_sid = sample_kwd_data.options[1]["cards"][0]["fields"][0]
        assert option2_sid["redundant"] is True

    def test_post_process_option_card_search_first(self, handler, sample_kwd_data):
        """Test that post_process searches option cards first, regardless of index."""
        # CRITICAL TEST: Option card with index=2, but we have 3 base cards (0,1,2)
        # The handler MUST search options first, not assume index >= num_cards
        sample_kwd_data.options = [
            {
                "name": "OPTION",
                "cards": [
                    Card(
                        index=2,  # Same as base card index!
                        fields=[Field(name="opt_field", type="int", position=0, width=10)],
                    ),
                    Card(
                        index=3,
                        fields=[Field(name="opt_field", type="int", position=0, width=10)],
                    ),
                ],
            }
        ]

        sample_kwd_data.negative_shared_fields = [
            {"name": "opt_field", "cards": [-2, -3], "applied_card_indices": False}
        ]

        # Should not raise an assertion error
        handler.post_process(sample_kwd_data)

        # Verify it processed the option cards, not base cards
        option_field1 = sample_kwd_data.options[0]["cards"][0]["fields"][0]
        option_field2 = sample_kwd_data.options[0]["cards"][1]["fields"][0]
        assert option_field1["card_indices"] == [2, 3]
        assert option_field2["redundant"] is True

    def test_post_process_insufficient_fields_skips_quietly(self, handler, sample_kwd_data, caplog):
        """Test post_process logs warning when fewer than 2 fields found."""
        sample_kwd_data.options = [
            {
                "name": "OPTION1",
                "cards": [
                    Card(
                        index=1,
                        fields=[Field(name="unique_field", type="int", position=0, width=10)],
                    ),
                ],
            }
        ]

        sample_kwd_data.negative_shared_fields = [
            {"name": "unique_field", "cards": [-1], "applied_card_indices": False}
        ]

        with caplog.at_level("WARNING"):
            handler.post_process(sample_kwd_data)

        # Should log warning about insufficient occurrences
        assert "Shared field skipped" in caplog.text
        assert "insufficient occurrences" in caplog.text

    def test_post_process_empty_negative_shared_fields(self, handler, sample_kwd_data):
        """Test post_process with no negative shared fields."""
        # Should not raise any errors
        handler.post_process(sample_kwd_data)
