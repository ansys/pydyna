# Copyright (C) 2021 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""
Tests for parameter retention feature.

This module tests the ability to retain original parameter references (e.g., &myvar)
when writing a deck back to a keyword file, rather than writing substituted values.
"""

import io
import pytest

from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.kwd_line_formatter import load_dataline


class TestParameterSetScoping:
    """Test URI scoping functionality in ParameterSet."""

    def test_scope_context_manager_pushes_and_pops(self):
        """Test that scope() pushes segment on entry and pops on exit."""
        ps = ParameterSet()
        
        assert ps._uri_stack == []
        
        with ps.scope("keyword1"):
            assert ps._uri_stack == ["keyword1"]
            
            with ps.scope("card0"):
                assert ps._uri_stack == ["keyword1", "card0"]
            
            assert ps._uri_stack == ["keyword1"]
        
        assert ps._uri_stack == []

    def test_scope_pops_on_exception(self):
        """Test that scope() pops even when an exception occurs."""
        ps = ParameterSet()
        
        try:
            with ps.scope("keyword1"):
                with ps.scope("card0"):
                    raise ValueError("test error")
        except ValueError:
            pass
        
        assert ps._uri_stack == []

    def test_get_current_uri(self):
        """Test get_current_uri returns joined path."""
        ps = ParameterSet()
        
        assert ps.get_current_uri() == ""
        
        with ps.scope("kwd"):
            assert ps.get_current_uri() == "kwd"
            
            with ps.scope("card0"):
                assert ps.get_current_uri() == "kwd/card0"


class TestParameterSetRefRecording:
    """Test parameter reference recording in ParameterSet."""

    def test_record_ref_stores_at_current_uri(self):
        """Test record_ref stores ref at current URI path."""
        ps = ParameterSet()
        
        with ps.scope("keyword1"):
            with ps.scope("card0"):
                ps.record_ref("0", "&myvar")
        
        assert ps._refs == {"keyword1/card0/0": "&myvar"}

    def test_record_ref_multiple_fields(self):
        """Test recording multiple refs in same card."""
        ps = ParameterSet()
        
        with ps.scope("kwd"):
            with ps.scope("card0"):
                ps.record_ref("0", "&var1")
                ps.record_ref("2", "&var2")
        
        assert ps._refs == {
            "kwd/card0/0": "&var1",
            "kwd/card0/2": "&var2",
        }

    def test_record_ref_multiple_cards(self):
        """Test recording refs across multiple cards."""
        ps = ParameterSet()
        
        with ps.scope("kwd"):
            with ps.scope("card0"):
                ps.record_ref("0", "&a")
            with ps.scope("card1"):
                ps.record_ref("0", "&b")
        
        assert ps._refs == {
            "kwd/card0/0": "&a",
            "kwd/card1/0": "&b",
        }

    def test_record_ref_negative_parameter(self):
        """Test recording negative parameter refs."""
        ps = ParameterSet()
        
        with ps.scope("kwd"):
            with ps.scope("card0"):
                ps.record_ref("0", "-&myvar")
        
        assert ps._refs == {"kwd/card0/0": "-&myvar"}


class TestParameterSetRefRetrieval:
    """Test parameter reference retrieval from ParameterSet."""

    def test_get_ref_returns_stored_ref(self):
        """Test get_ref returns the stored reference."""
        ps = ParameterSet()
        ps._refs["kwd/card0/0"] = "&myvar"
        
        ref = ps.get_ref("kwd", "card0", "0")
        assert ref == "&myvar"

    def test_get_ref_returns_none_for_missing(self):
        """Test get_ref returns None for non-existent URI."""
        ps = ParameterSet()
        
        ref = ps.get_ref("kwd", "card0", "0")
        assert ref is None

    def test_get_ref_with_different_paths(self):
        """Test get_ref correctly distinguishes different URIs."""
        ps = ParameterSet()
        ps._refs["kwd/card0/0"] = "&a"
        ps._refs["kwd/card0/1"] = "&b"
        ps._refs["kwd/card1/0"] = "&c"
        
        assert ps.get_ref("kwd", "card0", "0") == "&a"
        assert ps.get_ref("kwd", "card0", "1") == "&b"
        assert ps.get_ref("kwd", "card1", "0") == "&c"
        assert ps.get_ref("kwd", "card1", "1") is None


class TestLoadDatalineRefRecording:
    """Test that load_dataline records parameter refs during parsing."""

    def test_load_dataline_records_ref(self):
        """Test that loading a line with parameter records the ref."""
        ps = ParameterSet()
        ps.add("myval", 100.0)
        
        spec = [(0, 10, float), (10, 10, float)]
        line = "    &myval      50.0"
        
        with ps.scope("kwd"):
            with ps.scope("card0"):
                values = load_dataline(spec, line, ps)
        
        # Values should be substituted
        assert values[0] == 100.0
        assert values[1] == 50.0
        
        # Ref should be recorded
        assert ps.get_ref("kwd", "card0", "0") == "&myval"
        assert ps.get_ref("kwd", "card0", "1") is None  # Not a parameter

    def test_load_dataline_records_negative_ref(self):
        """Test that negative parameter refs are recorded correctly."""
        ps = ParameterSet()
        ps.add("val", 50.0)
        
        spec = [(0, 10, float)]
        line = "    -&val "  # 10 chars exactly: 4 spaces + "-&val" + 1 space
        
        with ps.scope("kwd"):
            with ps.scope("card0"):
                values = load_dataline(spec, line, ps)
        
        assert values[0] == -50.0
        assert ps.get_ref("kwd", "card0", "0") == "-&val"

    def test_load_dataline_multiple_params(self):
        """Test recording multiple parameter refs in one line."""
        ps = ParameterSet()
        ps.add("a", 1.0)
        ps.add("b", 2.0)
        ps.add("c", 3.0)
        
        spec = [(0, 10, float), (10, 10, float), (20, 10, float), (30, 10, float)]
        line = "        &a        &b      10.0        &c"
        
        with ps.scope("kwd"):
            with ps.scope("card0"):
                values = load_dataline(spec, line, ps)
        
        assert values == (1.0, 2.0, 10.0, 3.0)
        assert ps.get_ref("kwd", "card0", "0") == "&a"
        assert ps.get_ref("kwd", "card0", "1") == "&b"
        assert ps.get_ref("kwd", "card0", "2") is None
        assert ps.get_ref("kwd", "card0", "3") == "&c"


class TestCardReadRefRecording:
    """Test that Card.read records parameter refs."""

    def test_card_read_records_ref(self):
        """Test Card.read with parameter records the ref."""
        ps = ParameterSet()
        ps.add("secid", 100)
        
        card = Card(
            [
                Field("secid", int, 0, 10, None),
                Field("elform", int, 10, 10, 1),
            ]
        )
        
        line = "    &secid         2"
        buf = io.StringIO(line)
        
        # We need to scope at the keyword level (simulating what KeywordBase does)
        with ps.scope("keyword123"):
            with ps.scope("card0"):
                card.read(buf, ps)
        
        assert card.get_value("secid") == 100
        assert card.get_value("elform") == 2
        
        # Ref should be recorded
        assert ps.get_ref("keyword123", "card0", "0") == "&secid"
        assert ps.get_ref("keyword123", "card0", "1") is None


class TestCardWriteRetainParameters:
    """Test Card.write with retain_parameters option."""

    def test_card_write_without_retain_writes_values(self):
        """Test normal card write outputs substituted values."""
        card = Card(
            [
                Field("secid", int, 0, 10, 100),
                Field("elform", int, 10, 10, 2),
            ]
        )
        card.set_value("secid", 100)
        card.set_value("elform", 2)
        
        output = card.write(comment=False)
        
        # Should contain the numeric values
        assert "100" in output
        assert "2" in output
        assert "&" not in output

    def test_card_write_with_retain_writes_refs(self):
        """Test card write with retain_parameters outputs refs."""
        ps = ParameterSet()
        ps.add("secid", 100)
        ps._refs["kwd/card0/0"] = "&secid"
        
        card = Card(
            [
                Field("secid", int, 0, 10, 100),
                Field("elform", int, 10, 10, 2),
            ]
        )
        card.set_value("secid", 100)
        card.set_value("elform", 2)
        
        output = card.write(
            comment=False,
            retain_parameters=True,
            parameter_set=ps,
            uri_prefix="kwd/card0",
        )
        
        # Should contain the parameter reference for secid
        assert "&secid" in output
        # elform should still be numeric
        assert "2" in output

    def test_card_write_retain_without_refs_writes_values(self):
        """Test retain_parameters with no refs still writes values."""
        ps = ParameterSet()
        # No refs recorded
        
        card = Card(
            [
                Field("secid", int, 0, 10, 100),
            ]
        )
        card.set_value("secid", 100)
        
        output = card.write(
            comment=False,
            retain_parameters=True,
            parameter_set=ps,
            uri_prefix="kwd/card0",
        )
        
        assert "100" in output
        assert "&" not in output

    def test_card_write_retain_negative_ref(self):
        """Test retain_parameters handles negative refs."""
        ps = ParameterSet()
        ps.add("offset", 50.0)
        ps._refs["kwd/card0/0"] = "-&offset"
        
        card = Card(
            [
                Field("value", float, 0, 10, -50.0),
            ]
        )
        card.set_value("value", -50.0)
        
        output = card.write(
            comment=False,
            retain_parameters=True,
            parameter_set=ps,
            uri_prefix="kwd/card0",
        )
        
        assert "-&offset" in output


class TestRoundTrip:
    """Test reading and writing with parameter retention."""

    def test_card_roundtrip_with_parameter(self):
        """Test read then write preserves parameter reference."""
        ps = ParameterSet()
        ps.add("myval", 42)
        
        # Read
        card = Card(
            [
                Field("id", int, 0, 10, None),
                Field("val", int, 10, 10, None),
            ]
        )
        
        line = "    &myval        99"
        buf = io.StringIO(line)
        
        with ps.scope("kwd"):
            with ps.scope("card0"):
                card.read(buf, ps)
        
        # Verify read
        assert card.get_value("id") == 42
        assert card.get_value("val") == 99
        
        # Write with retain_parameters
        output = card.write(
            comment=False,
            retain_parameters=True,
            parameter_set=ps,
            uri_prefix="kwd/card0",
        )
        
        # Should preserve the parameter reference
        assert "&myval" in output
        assert "99" in output
