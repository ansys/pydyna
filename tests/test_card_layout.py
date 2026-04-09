# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT

"""Tests for ansys.dyna.core.lib.card_layout — pure layout functions.

These tests validate the card assembly logic in isolation, using lightweight
stubs instead of full keyword/card infrastructure.
"""

import pytest

from ansys.dyna.core.lib.card_interface import CardInterface, ReadResult
from ansys.dyna.core.lib.card_layout import (
    get_active_options,
    get_all_cards,
    get_main_option_cards_by_index,
    get_non_option_cards,
    get_post_option_cards,
    get_post_options_with_no_title_order,
    get_pre_option_cards,
    get_sorted_option_cards,
)
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionsInterface, OptionSpec


class StubCard(CardInterface):
    """Minimal CardInterface implementation for testing layout logic."""

    def __init__(self, name: str = ""):
        self.name = name

    def read(self, buf, parameter_set=None):
        return ReadResult()

    def write(self, format=None, buf=None, comment=None):
        return None

    @property
    def format(self):
        return format_type.default

    @format.setter
    def format(self, value):
        pass

    @property
    def active(self):
        return True

    def __repr__(self):
        return f"StubCard({self.name!r})"


class StubOptionsAPI(OptionsInterface):
    """Minimal OptionsInterface backed by a set of active option names."""

    def __init__(self, active_names=None):
        self._active = set(active_names or [])
        self._specs = []

    def is_option_active(self, name: str) -> bool:
        return name in self._active

    def activate_option(self, name: str) -> None:
        self._active.add(name)

    def deactivate_option(self, name: str) -> None:
        self._active.discard(name)

    def get_option_spec(self, name: str) -> OptionSpec:
        for s in self._specs:
            if s.name == name:
                return s
        raise Exception(f"No spec {name}")

    @property
    def option_specs(self):
        return iter(self._specs)


def _make_option_card_set(name, card_order, title_order, inner_card_names=None, keyword=None):
    spec = OptionSpec(name, card_order, title_order)
    inner = [StubCard(n) for n in (inner_card_names or [name])]
    return OptionCardSet(option_spec=spec, cards=inner, keyword=keyword)


# ---------------------------------------------------------------------------
# get_non_option_cards
# ---------------------------------------------------------------------------


class TestGetNonOptionCards:
    def test_empty_list(self):
        assert get_non_option_cards([]) == []

    def test_no_option_cards(self):
        cards = [StubCard("a"), StubCard("b")]
        assert get_non_option_cards(cards) == cards

    def test_filters_out_option_card_sets(self):
        regular = StubCard("a")
        opt = _make_option_card_set("ID", "post/1", 1)
        assert get_non_option_cards([regular, opt]) == [regular]

    def test_all_option_cards(self):
        opt1 = _make_option_card_set("ID", "post/1", 1)
        opt2 = _make_option_card_set("TITLE", "pre/1", 2)
        assert get_non_option_cards([opt1, opt2]) == []


# ---------------------------------------------------------------------------
# get_sorted_option_cards
# ---------------------------------------------------------------------------


class TestGetSortedOptionCards:
    def test_empty_list(self):
        assert get_sorted_option_cards([]) == []

    def test_returns_only_option_cards(self):
        regular = StubCard("a")
        opt = _make_option_card_set("ID", "post/1", 1)
        result = get_sorted_option_cards([regular, opt])
        assert result == [opt]

    def test_sorts_by_position(self):
        pre = _make_option_card_set("TITLE", "pre/1", 2)
        post = _make_option_card_set("ID", "post/1", 1)
        result = get_sorted_option_cards([post, pre])
        assert result == [pre, post]

    def test_sorts_post_ascending(self):
        post1 = _make_option_card_set("A", "post/1", 0)
        post2 = _make_option_card_set("B", "post/2", 0)
        result = get_sorted_option_cards([post2, post1])
        assert result == [post1, post2]


# ---------------------------------------------------------------------------
# get_post_options_with_no_title_order
# ---------------------------------------------------------------------------


class TestGetPostOptionsWithNoTitleOrder:
    def test_empty_list(self):
        assert get_post_options_with_no_title_order([]) == []

    def test_ignores_options_with_title_order(self):
        opt = _make_option_card_set("TITLE", "post/1", title_order=2)
        assert get_post_options_with_no_title_order([opt]) == []

    def test_returns_post_options_with_zero_title_order(self):
        opt = _make_option_card_set("MPP", "post/1", title_order=0)
        result = get_post_options_with_no_title_order([opt])
        assert result == [opt]

    def test_raises_on_pre_with_zero_title_order(self):
        opt = _make_option_card_set("BAD", "pre/1", title_order=0)
        with pytest.raises(ValueError, match="must not have 'pre' placement"):
            get_post_options_with_no_title_order([opt])

    def test_skips_main_with_zero_title_order(self):
        opt = _make_option_card_set("X", "main/0", title_order=0)
        assert get_post_options_with_no_title_order([opt]) == []


# ---------------------------------------------------------------------------
# get_active_options
# ---------------------------------------------------------------------------


class TestGetActiveOptions:
    def test_no_active_options(self):
        opt = _make_option_card_set("ID", "post/1", 1)
        api = StubOptionsAPI(active_names=[])
        assert get_active_options([opt], api) == []

    def test_returns_active_options(self):
        opt = _make_option_card_set("ID", "post/1", 1)
        api = StubOptionsAPI(active_names=["ID"])
        assert get_active_options([opt], api) == [opt]

    def test_filters_inactive(self):
        active_opt = _make_option_card_set("ID", "post/1", 1)
        inactive_opt = _make_option_card_set("TITLE", "pre/1", 2)
        api = StubOptionsAPI(active_names=["ID"])
        result = get_active_options([active_opt, inactive_opt], api)
        assert result == [active_opt]

    def test_returns_sorted(self):
        pre = _make_option_card_set("TITLE", "pre/1", 2)
        post = _make_option_card_set("ID", "post/1", 1)
        api = StubOptionsAPI(active_names=["ID", "TITLE"])
        result = get_active_options([post, pre], api)
        assert result == [pre, post]


# ---------------------------------------------------------------------------
# get_pre_option_cards / get_post_option_cards
# ---------------------------------------------------------------------------


class TestGetPrePostOptionCards:
    def test_pre_option_cards(self):
        inner = StubCard("title_card")
        spec = OptionSpec("TITLE", "pre/1", 2)
        opt = OptionCardSet(option_spec=spec, cards=[inner])
        api = StubOptionsAPI(active_names=["TITLE"])
        result = get_pre_option_cards([opt], api)
        assert result == [inner]

    def test_post_option_cards(self):
        inner = StubCard("id_card")
        spec = OptionSpec("ID", "post/1", 1)
        opt = OptionCardSet(option_spec=spec, cards=[inner])
        api = StubOptionsAPI(active_names=["ID"])
        result = get_post_option_cards([opt], api)
        assert result == [inner]

    def test_pre_excludes_post(self):
        pre_inner = StubCard("pre")
        post_inner = StubCard("post")
        pre_opt = OptionCardSet(option_spec=OptionSpec("TITLE", "pre/1", 2), cards=[pre_inner])
        post_opt = OptionCardSet(option_spec=OptionSpec("ID", "post/1", 1), cards=[post_inner])
        api = StubOptionsAPI(active_names=["TITLE", "ID"])
        assert get_pre_option_cards([pre_opt, post_opt], api) == [pre_inner]
        assert get_post_option_cards([pre_opt, post_opt], api) == [post_inner]

    def test_inactive_options_excluded(self):
        inner = StubCard("x")
        opt = OptionCardSet(option_spec=OptionSpec("ID", "post/1", 1), cards=[inner])
        api = StubOptionsAPI(active_names=[])
        assert get_post_option_cards([opt], api) == []


# ---------------------------------------------------------------------------
# get_main_option_cards_by_index
# ---------------------------------------------------------------------------


class TestGetMainOptionCardsByIndex:
    def test_empty_when_no_main_options(self):
        opt = _make_option_card_set("ID", "post/1", 1)
        api = StubOptionsAPI(active_names=["ID"])
        assert get_main_option_cards_by_index([opt], api) == {}

    def test_groups_by_index(self):
        inner0 = StubCard("after_card_0")
        inner2 = StubCard("after_card_2")
        opt0 = OptionCardSet(option_spec=OptionSpec("A", "main/0", 0), cards=[inner0])
        opt2 = OptionCardSet(option_spec=OptionSpec("B", "main/2", 0), cards=[inner2])
        api = StubOptionsAPI(active_names=["A", "B"])
        result = get_main_option_cards_by_index([opt0, opt2], api)
        assert result == {0: [inner0], 2: [inner2]}

    def test_multiple_at_same_index(self):
        inner_a = StubCard("a")
        inner_b = StubCard("b")
        opt_a = OptionCardSet(option_spec=OptionSpec("A", "main/1", 0), cards=[inner_a])
        opt_b = OptionCardSet(option_spec=OptionSpec("B", "main/1", 0), cards=[inner_b])
        api = StubOptionsAPI(active_names=["A", "B"])
        result = get_main_option_cards_by_index([opt_a, opt_b], api)
        assert result == {1: [inner_a, inner_b]}


# ---------------------------------------------------------------------------
# get_all_cards — the main assembler
# ---------------------------------------------------------------------------


class TestGetAllCards:
    def test_no_options(self):
        a, b = StubCard("a"), StubCard("b")
        api = StubOptionsAPI()
        assert get_all_cards([a, b], api) == [a, b]

    def test_pre_then_main_then_post(self):
        main_a = StubCard("main_a")
        main_b = StubCard("main_b")
        pre_inner = StubCard("pre")
        post_inner = StubCard("post")
        pre_opt = OptionCardSet(option_spec=OptionSpec("TITLE", "pre/1", 2), cards=[pre_inner])
        post_opt = OptionCardSet(option_spec=OptionSpec("ID", "post/1", 1), cards=[post_inner])
        api = StubOptionsAPI(active_names=["TITLE", "ID"])

        result = get_all_cards([main_a, pre_opt, post_opt, main_b], api)
        assert result == [pre_inner, main_a, main_b, post_inner]

    def test_inactive_options_excluded(self):
        main = StubCard("main")
        pre_inner = StubCard("pre")
        pre_opt = OptionCardSet(option_spec=OptionSpec("TITLE", "pre/1", 2), cards=[pre_inner])
        api = StubOptionsAPI(active_names=[])

        result = get_all_cards([main, pre_opt], api)
        assert result == [main]

    def test_main_option_inserted_after_index(self):
        card0 = StubCard("card0")
        card1 = StubCard("card1")
        card2 = StubCard("card2")
        inserted = StubCard("inserted_after_1")
        main_opt = OptionCardSet(option_spec=OptionSpec("X", "main/1", 0), cards=[inserted])
        api = StubOptionsAPI(active_names=["X"])

        result = get_all_cards([card0, card1, main_opt, card2], api)
        assert result == [card0, card1, inserted, card2]

    def test_multiple_pre_sorted_correctly(self):
        """pre/2 should appear before pre/1 (higher index = further before main)."""
        main = StubCard("main")
        inner1 = StubCard("pre1")
        inner2 = StubCard("pre2")
        opt1 = OptionCardSet(option_spec=OptionSpec("A", "pre/1", 0), cards=[inner1])
        opt2 = OptionCardSet(option_spec=OptionSpec("B", "pre/2", 0), cards=[inner2])
        api = StubOptionsAPI(active_names=["A", "B"])

        result = get_all_cards([main, opt1, opt2], api)
        assert result == [inner2, inner1, main]

    def test_complex_layout(self):
        """PRE + MAIN insertion + POST all together."""
        card0 = StubCard("card0")
        card1 = StubCard("card1")
        pre_inner = StubCard("pre")
        main_inner = StubCard("after_card0")
        post_inner = StubCard("post")
        pre_opt = OptionCardSet(option_spec=OptionSpec("T", "pre/1", 1), cards=[pre_inner])
        main_opt = OptionCardSet(option_spec=OptionSpec("M", "main/0", 0), cards=[main_inner])
        post_opt = OptionCardSet(option_spec=OptionSpec("I", "post/1", 2), cards=[post_inner])
        api = StubOptionsAPI(active_names=["T", "M", "I"])

        result = get_all_cards([card0, pre_opt, main_opt, post_opt, card1], api)
        assert result == [pre_inner, card0, main_inner, card1, post_inner]

    def test_empty_card_list(self):
        api = StubOptionsAPI()
        assert get_all_cards([], api) == []

    def test_only_option_cards_all_inactive(self):
        opt = _make_option_card_set("ID", "post/1", 1)
        api = StubOptionsAPI(active_names=[])
        assert get_all_cards([opt], api) == []

    def test_only_option_cards_all_active(self):
        inner = StubCard("id")
        opt = OptionCardSet(option_spec=OptionSpec("ID", "post/1", 1), cards=[inner])
        api = StubOptionsAPI(active_names=["ID"])
        assert get_all_cards([opt], api) == [inner]
