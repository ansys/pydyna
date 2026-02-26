"""Tests for card-set handler.

Tests the most complex handler which groups cards into reusable sets with:
- Reference semantics (later handlers modify same cards)
- Discriminator fields (field values determine card variants)
- Internal conditionals (self.field references within set)
- Shared fields across mutually-exclusive cards
- Options within card sets
"""

import pytest

from codegen.keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from codegen.keyword_generation.data_model.label_registry import LabelRegistry
from codegen.keyword_generation.handlers.card_set import (
    CardSetHandler,
    CardSetSettings,
    DiscriminatorConfig,
    InternalConditional,
    SharedFieldConfig,
)


def make_registry_with_labels(cards, **labels):
    """Helper to create LabelRegistry with field-name-to-index labels."""
    return LabelRegistry.from_cards(cards, keyword="TEST", initial_labels=labels)


class TestDiscriminatorConfig:
    """Test DiscriminatorConfig dataclass."""

    def test_discriminator_creation(self):
        """Test creating a discriminator config."""
        disc = DiscriminatorConfig(field="ftype", cards_with_field=[0, 1, 2])

        assert disc.field == "ftype"
        assert disc.cards_with_field == [0, 1, 2]

    def test_from_dict(self):
        """Test parsing from dict."""
        disc = DiscriminatorConfig.from_dict(
            {"field": "ftype", "cards-with-field": [0, 1, 2]}
        )

        assert disc.field == "ftype"
        assert disc.cards_with_field == [0, 1, 2]


class TestInternalConditional:
    """Test InternalConditional dataclass."""

    def test_conditional_creation(self):
        """Test creating internal conditional."""
        cond = InternalConditional(index=1, func="self.ftype == 1")

        assert cond.index == 1
        assert cond.func == "self.ftype == 1"

    def test_from_dict(self):
        """Test parsing from dict."""
        cond = InternalConditional.from_dict({"index": 2, "func": "self.active"})

        assert cond.index == 2
        assert cond.func == "self.active"


class TestSharedFieldConfig:
    """Test SharedFieldConfig dataclass."""

    def test_shared_field_creation(self):
        """Test creating shared field config."""
        sf = SharedFieldConfig(name="ftype", card_indices=[0, 1], source_index=0)

        assert sf.name == "ftype"
        assert sf.card_indices == [0, 1]
        assert sf.source_index == 0

    def test_from_dict_with_source_index(self):
        """Test parsing from dict with explicit source-index."""
        sf = SharedFieldConfig.from_dict(
            {"name": "ftype", "card-indices": [0, 1], "source-index": 1}
        )

        assert sf.name == "ftype"
        assert sf.card_indices == [0, 1]
        assert sf.source_index == 1

    def test_from_dict_defaults_to_first_index(self):
        """Test source-index defaults to first card index."""
        sf = SharedFieldConfig.from_dict(
            {"name": "ftype", "card-indices": [1, 2, 3]}
        )

        assert sf.source_index == 1  # First in card-indices


class TestCardSetSettings:
    """Test CardSetSettings dataclass."""

    def test_settings_creation(self):
        """Test creating basic card set settings."""
        settings = CardSetSettings(
            name="LoadSet",
            source_refs=["load_1", "load_2"],
            target_ref="load_1",
        )

        assert settings.name == "LoadSet"
        assert settings.source_refs == ["load_1", "load_2"]
        assert settings.target_ref == "load_1"

    def test_bounded_property(self):
        """Test bounded property based on length_func."""
        unbounded = CardSetSettings(name="A", source_refs=["a"], target_ref="a")
        bounded = CardSetSettings(
            name="B", source_refs=["b"], target_ref="b", length_func="len(self.items)"
        )

        assert unbounded.bounded is False
        assert bounded.bounded is True

    def test_resolve_source_indices(self):
        """Test resolving source refs to indices."""
        cards = [
            Card(index=0, fields=[Field(name="a", type="int", position=0, width=10)]),
            Card(index=1, fields=[Field(name="b", type="int", position=0, width=10)]),
            Card(index=2, fields=[Field(name="c", type="int", position=0, width=10)]),
        ]
        registry = make_registry_with_labels(cards, a=0, b=1, c=2)

        settings = CardSetSettings(
            name="Test", source_refs=["b", "c"], target_ref="a"
        )
        indices = settings.resolve_source_indices(registry, cards)

        assert indices == [1, 2]

    def test_resolve_source_indices_requires_refs(self):
        """Test error when source_refs missing."""
        settings = CardSetSettings(name="Test", target_ref="a")
        registry = LabelRegistry(_labels={}, _keyword="TEST")

        with pytest.raises(ValueError, match="must have source-refs"):
            settings.resolve_source_indices(registry, [])

    def test_resolve_target_index(self):
        """Test resolving target ref to index."""
        cards = [
            Card(index=0, fields=[Field(name="a", type="int", position=0, width=10)]),
            Card(index=1, fields=[Field(name="b", type="int", position=0, width=10)]),
        ]
        registry = make_registry_with_labels(cards, a=0, b=1)

        settings = CardSetSettings(
            name="Test", source_refs=["a"], target_ref="b"
        )
        target_idx = settings.resolve_target_index(registry, cards)

        assert target_idx == 1

    def test_resolve_target_index_requires_ref(self):
        """Test error when target_ref missing."""
        settings = CardSetSettings(name="Test", source_refs=["a"])
        registry = LabelRegistry(_labels={}, _keyword="TEST")

        with pytest.raises(ValueError, match="must have target-ref"):
            settings.resolve_target_index(registry, [])

    def test_from_dict_basic(self):
        """Test parsing basic settings from dict."""
        settings = CardSetSettings.from_dict({
            "name": "FiberSet",
            "source-refs": ["fiber_a", "fiber_b"],
            "target-ref": "fiber_a",
        })

        assert settings.name == "FiberSet"
        assert settings.source_refs == ["fiber_a", "fiber_b"]
        assert settings.target_ref == "fiber_a"

    def test_from_dict_with_discriminator(self):
        """Test parsing settings with discriminator."""
        settings = CardSetSettings.from_dict({
            "name": "Test",
            "source-refs": ["a"],
            "target-ref": "a",
            "discriminator": {
                "field": "ftype",
                "cards-with-field": [0, 1, 2]
            }
        })

        assert settings.discriminator is not None
        assert settings.discriminator.field == "ftype"
        assert settings.discriminator.cards_with_field == [0, 1, 2]

    def test_from_dict_with_internal_conditionals(self):
        """Test parsing settings with internal conditionals."""
        settings = CardSetSettings.from_dict({
            "name": "Test",
            "source-refs": ["a"],
            "target-ref": "a",
            "internal-conditionals": [
                {"index": 1, "func": "self.ftype == 1"},
                {"index": 2, "func": "self.ftype == 2"}
            ]
        })

        assert settings.internal_conditionals is not None
        assert len(settings.internal_conditionals) == 2
        assert settings.internal_conditionals[0].index == 1
        assert settings.internal_conditionals[1].func == "self.ftype == 2"

    def test_from_dict_with_shared_fields(self):
        """Test parsing settings with shared fields."""
        settings = CardSetSettings.from_dict({
            "name": "Test",
            "source-refs": ["a"],
            "target-ref": "a",
            "shared-fields": [
                {"name": "ftype", "card-indices": [0, 1], "source-index": 0}
            ]
        })

        assert settings.shared_fields is not None
        assert len(settings.shared_fields) == 1
        assert settings.shared_fields[0].name == "ftype"
        assert settings.shared_fields[0].card_indices == [0, 1]

    def test_from_dict_with_all_fields(self):
        """Test parsing complete settings."""
        settings = CardSetSettings.from_dict({
            "name": "CompleteSet",
            "source-refs": ["a", "b"],
            "target-ref": "a",
            "length-func": "len(self.items)",
            "active-func": "self.nitems > 0",
            "options": [{"name": "OPT"}],
            "item-name": "item",
            "items-name": "items",
        })

        assert settings.name == "CompleteSet"
        assert settings.length_func == "len(self.items)"
        assert settings.active_func == "self.nitems > 0"
        assert settings.options == [{"name": "OPT"}]
        assert settings.item_name == "item"
        assert settings.items_name == "items"


class TestCardSetHandler:
    """Test CardSetHandler functionality."""

    @pytest.fixture
    def handler(self):
        """Create handler instance."""
        return CardSetHandler()

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample keyword data with cards and registry."""
        cards = [
            Card(index=0, fields=[Field(name="header", type="int", position=0, width=10)]),
            Card(index=1, fields=[Field(name="load_a", type="float", position=0, width=10)]),
            Card(index=2, fields=[Field(name="load_b", type="float", position=0, width=10)]),
            Card(index=3, fields=[Field(name="load_c", type="float", position=0, width=10)]),
            Card(index=4, fields=[Field(name="footer", type="int", position=0, width=10)]),
        ]
        registry = make_registry_with_labels(
            cards, header=0, load_a=1, load_b=2, load_c=3, footer=4
        )

        return KeywordData(
            keyword="LOAD",
            subkeyword="DATA",
            title="*LOAD_DATA",
            classname="",
            cards=cards,
            options=[],
            label_registry=registry,
        )

    def test_handle_basic_card_set(self, handler, sample_kwd_data):
        """Test creating a basic card set."""
        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a", "load_b", "load_c"],
            "target-ref": "load_a",
            "target-name": "",
        }]

        handler.handle(sample_kwd_data, settings)

        # Verify card_sets created
        assert sample_kwd_data.card_sets is not None
        assert len(sample_kwd_data.card_sets["sets"]) == 1

        card_set = sample_kwd_data.card_sets["sets"][0]
        assert card_set["name"] == "LoadSet"
        assert len(card_set["source_cards"]) == 3

        # Verify cards marked for removal
        assert sample_kwd_data.cards[1].mark_for_removal == 1
        assert sample_kwd_data.cards[2].mark_for_removal == 1
        assert sample_kwd_data.cards[3].mark_for_removal == 1

    def test_handle_card_indices_rewritten(self, handler, sample_kwd_data):
        """Test card indices are rewritten to 0, 1, 2 within set."""
        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a", "load_b", "load_c"],
            "target-ref": "load_a",
        }]

        handler.handle(sample_kwd_data, settings)

        card_set = sample_kwd_data.card_sets["sets"][0]
        # Indices should be sequential 0, 1, 2 within the set
        assert card_set["source_cards"][0]["index"] == 0
        assert card_set["source_cards"][1]["index"] == 1
        assert card_set["source_cards"][2]["index"] == 2

        # Original indices preserved in source_index
        assert card_set["source_cards"][0]["source_index"] == 1
        assert card_set["source_cards"][1]["source_index"] == 2
        assert card_set["source_cards"][2]["source_index"] == 3

    def test_handle_reference_semantics(self, handler, sample_kwd_data):
        """Test that source_cards reference same objects as cards list (no copy)."""
        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a", "load_b"],
            "target-ref": "load_a",
        }]

        handler.handle(sample_kwd_data, settings)

        card_set = sample_kwd_data.card_sets["sets"][0]
        # Should be same object (identity check)
        assert card_set["source_cards"][0] is sample_kwd_data.cards[1]
        assert card_set["source_cards"][1] is sample_kwd_data.cards[2]

        # Modifications to one should appear in the other
        sample_kwd_data.cards[1]["test_property"] = "modified"
        assert card_set["source_cards"][0]["test_property"] == "modified"

    def test_handle_with_length_func(self, handler, sample_kwd_data):
        """Test bounded card set with length function."""
        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a", "load_b"],
            "target-ref": "load_a",
            "length-func": "len(self.loads)",
        }]

        handler.handle(sample_kwd_data, settings)

        card_set = sample_kwd_data.card_sets["sets"][0]
        assert card_set["bounded"] is True

        # Check insertion has length func
        insertion = sample_kwd_data.card_insertions[0]
        assert insertion.card["length_func"] == "len(self.loads)"
        assert insertion.card["bounded"] is True

    def test_handle_with_active_func(self, handler, sample_kwd_data):
        """Test card set with active function."""
        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a"],
            "target-ref": "load_a",
            "active-func": "self.nloads > 0",
        }]

        handler.handle(sample_kwd_data, settings)

        insertion = sample_kwd_data.card_insertions[0]
        assert insertion.card["active_func"] == "self.nloads > 0"

    def test_handle_with_item_names(self, handler, sample_kwd_data):
        """Test card set with custom item names."""
        settings = [{
            "name": "FiberSet",
            "source-refs": ["load_a"],
            "target-ref": "load_a",
            "item-name": "fiber_family",
            "items-name": "fiber_families",
        }]

        handler.handle(sample_kwd_data, settings)

        card_set = sample_kwd_data.card_sets["sets"][0]
        assert card_set["item_name"] == "fiber_family"
        assert card_set["items_name"] == "fiber_families"

    def test_handle_default_item_names(self, handler, sample_kwd_data):
        """Test card set defaults to 'set' and 'sets'."""
        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a"],
            "target-ref": "load_a",
        }]

        handler.handle(sample_kwd_data, settings)

        card_set = sample_kwd_data.card_sets["sets"][0]
        assert card_set["item_name"] == "set"
        assert card_set["items_name"] == "sets"

    def test_handle_with_discriminator(self, handler, sample_kwd_data):
        """Test card set with discriminator field."""
        settings = [{
            "name": "FiberSet",
            "source-refs": ["load_a", "load_b"],
            "target-ref": "load_a",
            "discriminator": {
                "field": "ftype",
                "cards-with-field": [0, 1]
            }
        }]

        handler.handle(sample_kwd_data, settings)

        card_set = sample_kwd_data.card_sets["sets"][0]
        assert "discriminator" in card_set
        assert card_set["discriminator"]["field"] == "ftype"
        assert card_set["discriminator"]["cards_with_field"] == [0, 1]

    def test_handle_with_internal_conditionals(self, handler, sample_kwd_data):
        """Test card set with internal conditionals."""
        settings = [{
            "name": "FiberSet",
            "source-refs": ["load_a", "load_b", "load_c"],
            "target-ref": "load_a",
            "internal-conditionals": [
                {"index": 1, "func": "self.ftype == 1"},
                {"index": 2, "func": "self.ftype == 2"}
            ]
        }]

        handler.handle(sample_kwd_data, settings)

        card_set = sample_kwd_data.card_sets["sets"][0]
        assert "internal_conditionals" in card_set
        assert len(card_set["internal_conditionals"]) == 2

        # Conditionals should be applied to source cards
        assert card_set["source_cards"][1]["func"] == "self.ftype == 1"
        assert card_set["source_cards"][2]["func"] == "self.ftype == 2"

    def test_handle_with_shared_fields(self, handler):
        """Test card set with shared fields."""
        # Create cards with same field in multiple cards
        cards = [
            Card(index=0, fields=[Field(name="header", type="int", position=0, width=10)]),
            Card(index=1, fields=[
                Field(name="ftype", type="int", position=0, width=10, help="Fiber type", used=True)
            ]),
            Card(index=2, fields=[
                Field(name="ftype", type="int", position=0, width=10, help="Fiber type", used=True)
            ]),
        ]
        registry = make_registry_with_labels(cards, header=0, ftype=1, ftype2=2)

        kwd_data = KeywordData(
            keyword="FIBER",
            subkeyword="DATA",
            title="*FIBER_DATA",
            classname="",
            cards=cards,
            options=[],
            label_registry=registry,
        )

        settings = [{
            "name": "FiberSet",
            "source-refs": ["ftype", "ftype2"],  # Use distinct label refs
            "target-ref": "header",
            "shared-fields": [
                {"name": "ftype", "card-indices": [0, 1], "source-index": 0}
            ]
        }]

        handler = CardSetHandler()
        handler.handle(kwd_data, settings)

        card_set = kwd_data.card_sets["sets"][0]
        assert "shared_fields" in card_set
        assert len(card_set["shared_fields"]) == 1

        sf = card_set["shared_fields"][0]
        assert sf["name"] == "ftype"
        assert sf["card_indices"] == [0, 1]
        assert sf["type"] == "int"
        assert sf["help"] == "Fiber type"

        # Fields in cards should be marked redundant
        assert card_set["source_cards"][0]["fields"][0].redundant is True
        assert card_set["source_cards"][1]["fields"][0].redundant is True

    def test_handle_with_options(self, handler, sample_kwd_data):
        """Test card set including options."""
        # Add an option to the keyword
        sample_kwd_data.options = [{
            "name": "OPTION_A",
            "cards": [
                Card(index=0, fields=[Field(name="opt_field", type="int", position=0, width=10)])
            ]
        }]

        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a"],
            "target-ref": "load_a",
            "source-options": [0]
        }]

        handler.handle(sample_kwd_data, settings)

        card_set = sample_kwd_data.card_sets["sets"][0]
        assert "options" in card_set
        assert len(card_set["options"]) == 1
        assert card_set["options"][0]["name"] == "OPTION_A"

        # Option should be marked for removal
        assert sample_kwd_data.options[0]["mark_for_removal"] == 1

        # has_options flag should be set
        assert sample_kwd_data.card_sets["options"] is True

    def test_handle_multiple_card_sets(self, handler, sample_kwd_data):
        """Test creating multiple card sets with different target-names."""
        settings = [
            {
                "name": "LoadSetA",
                "source-refs": ["load_a"],
                "target-ref": "load_a",
                "target-name": "SET_A",  # Non-default target
            },
            {
                "name": "LoadSetB",
                "source-refs": ["load_c"],
                "target-ref": "load_c",
                "target-name": "SET_B",  # Non-default target
            }
        ]

        handler.handle(sample_kwd_data, settings)

        assert len(sample_kwd_data.card_sets["sets"]) == 2
        assert sample_kwd_data.card_sets["sets"][0]["name"] == "LoadSetA"
        assert sample_kwd_data.card_sets["sets"][1]["name"] == "LoadSetB"

    def test_handle_multiple_default_targets_raises(self, handler, sample_kwd_data):
        """Test error when multiple default targets (empty target-name)."""
        settings = [
            {
                "name": "SetA",
                "source-refs": ["load_a"],
                "target-ref": "load_a",
                "target-name": "",  # Default target
            },
            {
                "name": "SetB",
                "source-refs": ["load_b"],
                "target-ref": "load_b",
                "target-name": "",  # Second default target
            }
        ]

        with pytest.raises(Exception, match="only one card set on the base keyword"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_without_registry_raises(self, handler, sample_kwd_data):
        """Test error when label registry not available."""
        sample_kwd_data.label_registry = None

        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a"],
            "target-ref": "load_a",
        }]

        with pytest.raises(ValueError, match="requires a label registry"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_creates_insertion(self, handler, sample_kwd_data):
        """Test that card insertion is created correctly."""
        settings = [{
            "name": "LoadSet",
            "source-refs": ["load_a"],
            "target-ref": "load_b",
        }]

        handler.handle(sample_kwd_data, settings)

        assert len(sample_kwd_data.card_insertions) == 1
        insertion = sample_kwd_data.card_insertions[0]
        assert insertion.target_index == 2  # load_b is at index 2
        assert insertion.card["set"]["name"] == "LoadSet"

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test post_process does nothing."""
        handler.post_process(sample_kwd_data)
        # No assertions - just verify it doesn't raise
