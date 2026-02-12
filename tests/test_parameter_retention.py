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
from ansys.dyna.core.lib.field_schema import FieldSchema
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

        card = Card.from_field_schemas(
            (
                FieldSchema("secid", int, 0, 10, None),
                FieldSchema("elform", int, 10, 10, 1),
            )
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
        card = Card.from_field_schemas(
            (
                FieldSchema("secid", int, 0, 10, 100),
                FieldSchema("elform", int, 10, 10, 2),
            )
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

        card = Card.from_field_schemas(
            (
                FieldSchema("secid", int, 0, 10, 100),
                FieldSchema("elform", int, 10, 10, 2),
            )
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

        card = Card.from_field_schemas(
            (
                FieldSchema("secid", int, 0, 10, 100),
            )
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

        card = Card.from_field_schemas(
            (
                FieldSchema("value", float, 0, 10, -50.0),
            )
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
        card = Card.from_field_schemas(
            (
                FieldSchema("id", int, 0, 10, None),
                FieldSchema("val", int, 10, 10, None),
            )
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


class TestDeckWithSeriesAndTableCards:
    """Test parameter retention with decks containing series and table cards."""

    
    def test_deck_with_series_card_and_parameters(self):
        """Test that series card parameters are retained when retain_parameters=True.

        When writing with retain_parameters=True, parameter references like &n1
        should appear in the output instead of the substituted numeric values.
        """
        from ansys.dyna.core.lib.deck import Deck

        deck_string = """*PARAMETER
Rn1,102
Rn2,107
*SET_NODE_LIST
         1       0.0       0.0       0.0       0.0    MECH         1
       100       101      &n1       103       104       105       106      &n2
       108       109       110"""

        deck = Deck()
        deck.loads(deck_string)

        # Verify read correctly - parameters should be substituted during read
        set_node = deck.keywords[1]
        assert set_node.sid == 1
        assert len(set_node.nodes) == 11
        assert set_node.nodes[0] == 100
        assert set_node.nodes[2] == 102  # &n1 substituted to 102
        assert set_node.nodes[7] == 107  # &n2 substituted to 107
        assert set_node.nodes[10] == 110

        # Write without retain_parameters (default) - should have substituted values
        output = deck.write()
        assert "*SET_NODE_LIST" in output
        assert "102" in output  # substituted value
        assert "107" in output  # substituted value
        assert "&n1" not in output  # parameter ref should NOT appear
        assert "&n2" not in output  # parameter ref should NOT appear

        # Write with retain_parameters=True - should have parameter references
        output_retain = deck.write(retain_parameters=True)
        assert "*SET_NODE_LIST" in output_retain
        assert "&n1" in output_retain  # parameter ref SHOULD appear
        assert "&n2" in output_retain  # parameter ref SHOULD appear

    
    def test_deck_with_table_card_and_parameters(self):
        """Test that table card parameters are retained when retain_parameters=True.

        When writing with retain_parameters=True, parameter references like &x1
        should appear in the output instead of the substituted numeric values.
        """
        from ansys.dyna.core.lib.deck import Deck

        deck_string = """*PARAMETER
Rx1,-0.25
Ry1,0.30
*NODE
$#   nid               x               y               z      tc      rc
     100            &x1            &y1             0.0       0       0
     101      -0.2687006       0.2687006             0.0       0       0
     102      -0.1607270       0.3880294             0.0       0       0"""

        deck = Deck()
        deck.loads(deck_string)

        # Verify read correctly - parameters should be substituted during read
        node_kwd = deck.keywords[1]
        assert len(node_kwd.nodes) == 3
        assert node_kwd.nodes["nid"].iloc[0] == 100
        assert node_kwd.nodes["x"].iloc[0] == -0.25  # &x1 substituted
        assert node_kwd.nodes["y"].iloc[0] == 0.30  # &y1 substituted

        # Write without retain_parameters (default) - should have substituted values
        output = deck.write()
        assert "*NODE" in output
        assert "-0.25" in output or "-.25" in output  # substituted value
        assert "&x1" not in output  # parameter ref should NOT appear
        assert "&y1" not in output  # parameter ref should NOT appear

        # Write with retain_parameters=True - should have parameter references
        output_retain = deck.write(retain_parameters=True)
        assert "*NODE" in output_retain
        assert "&x1" in output_retain  # parameter ref SHOULD appear
        assert "&y1" in output_retain  # parameter ref SHOULD appear

    
    def test_deck_with_card_and_series_card_parameters(self):
        """Test deck with parameters in both Card fields and SeriesCard data.

        When writing with retain_parameters=True:
        - Card field parameter &da1 should be retained
        - SeriesCard parameters &n1, &n2 should be retained
        """
        from ansys.dyna.core.lib.deck import Deck

        deck_string = """*PARAMETER
Rda1,0.5
Rn1,101
Rn2,103
*SET_NODE_LIST
         1     &da1       0.0       0.0       0.0    MECH         1
       100      &n1       102      &n2"""

        deck = Deck()
        deck.loads(deck_string)

        # Verify read correctly - parameters should be substituted during read
        assert len(deck.keywords) == 2
        set_node = deck.keywords[1]
        assert set_node.sid == 1
        assert set_node.da1 == 0.5  # &da1 substituted in Card field
        assert len(set_node.nodes) == 4
        assert set_node.nodes[0] == 100
        assert set_node.nodes[1] == 101  # &n1 substituted
        assert set_node.nodes[3] == 103  # &n2 substituted

        # Write without retain_parameters - should have substituted values
        output = deck.write()
        assert "*SET_NODE_LIST" in output
        assert "0.5" in output or ".5" in output  # substituted value for da1
        assert "&da1" not in output  # parameter ref should NOT appear
        assert "&n1" not in output
        assert "&n2" not in output

        # Write with retain_parameters=True - should have parameter references
        output_retain = deck.write(retain_parameters=True)
        assert "*SET_NODE_LIST" in output_retain
        assert "&da1" in output_retain  # parameter ref SHOULD appear
        assert "&n1" in output_retain  # parameter ref SHOULD appear
        assert "&n2" in output_retain  # parameter ref SHOULD appear

    
    def test_deck_with_table_card_group_and_parameters(self):
        """Test that a deck with TableCardGroup containing parameters works.

        TableCardGroup is used for keywords where each row needs multiple
        related table cards (e.g., element ID + thickness values).
        """
        from ansys.dyna.core.lib.deck import Deck
        from ansys.dyna.core.lib.parameters import ParameterSet
        import ansys.dyna.core.keywords as kwd

        # Build parameter set with the parameter value
        parameter_set = ParameterSet()
        parameter_set.add("t1", 1.98)

        # Create ELEMENT_SHELL_THICKNESS with a parameter reference
        # Note: thic1 field is 16 chars wide, so &t1 fits perfectly
        elements = kwd.ElementShellThickness()
        elements.loads(
            """*ELEMENT_SHELL_THICKNESS
       1       1       1     105       2       2
             &t1 1.97992622E+000 1.97992622E+000 1.97992622E+000 1.49965326E+002
       2       1     136     133    2834    2834
 1.98166233E+000 1.98166233E+000 1.98296441E+000 1.98296441E+000 1.46006557E+002""",
            parameters=parameter_set,
        )

        # Verify parameters were resolved correctly
        assert len(elements.elements) == 2
        assert elements.elements["eid"].iloc[0] == 1
        assert elements.elements["thic1"].iloc[0] == pytest.approx(1.98, rel=1e-3)  # &t1 resolved

        # Add to deck
        deck = Deck()
        deck.extend([elements])

        # Write without retain_parameters - should show resolved value
        output = deck.write()
        assert "*ELEMENT_SHELL_THICKNESS" in output
        assert "&t1" not in output

        # Write with retain_parameters - should show parameter reference
        output_retain = deck.write(retain_parameters=True)
        assert "*ELEMENT_SHELL_THICKNESS" in output_retain
        assert "&t1" in output_retain  # parameter ref should appear


class TestCSVFormatRetention:
    """Test parameter retention with CSV (comma-delimited) format."""

    
    def test_table_card_csv_roundtrip_with_parameter(self):
        """Test CSV format TableCard read→write preserves parameter refs."""
        from ansys.dyna.core.lib.table_card import TableCard

        ps = ParameterSet()
        ps.add("xval", 100.5)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
            ],
            None,
        )

        # CSV format input with parameter
        card_text = """1,&xval,20.0
2,200.0,30.0"""
        buf = io.StringIO(card_text)

        with ps.scope("kwd"):
            table.read(buf, ps)

        # Values should be substituted
        assert table.table["x"][0] == 100.5
        assert table.table["y"][0] == 20.0

        # Write with retain_parameters
        output = table.write(
            comment=False,
            retain_parameters=True,
            parameter_set=ps,
            uri_prefix="kwd",
        )

        # Should contain the parameter reference
        assert "&xval" in output

    
    def test_deck_csv_format_table_card_retention(self):
        """Test deck-level CSV format table card parameter retention."""
        from ansys.dyna.core.lib.deck import Deck

        # NODE keyword supports CSV format
        deck_string = """*PARAMETER
Rx1,100.5
*NODE
1,&x1,20.0,30.0
2,200.0,40.0,50.0"""

        deck = Deck()
        deck.loads(deck_string)

        # Verify read
        node_kwd = deck.keywords[1]
        assert node_kwd.nodes["x"].iloc[0] == 100.5  # &x1 substituted

        # Write with retain_parameters
        output = deck.write(retain_parameters=True)
        assert "&x1" in output


class TestBoundedCardRetention:
    """Test parameter retention with bounded card variants."""

    
    def test_bounded_series_card_retention(self):
        """Test bounded SeriesCard retains parameter refs."""
        from ansys.dyna.core.lib.series_card import SeriesCard

        ps = ParameterSet()
        ps.add("val", 42.0)

        series = SeriesCard(
            name="values",
            fields_per_card=4,
            element_width=10,
            input_type=float,
            length_func=lambda: 4,  # Bounded to 4 elements
        )

        card_text = """     &val      20.0      30.0      40.0"""
        buf = io.StringIO(card_text)

        with ps.scope("kwd"):
            with ps.scope("series"):
                series.read(buf, ps)

        # Values should be substituted
        assert len(series.data) == 4
        assert series.data[0] == 42.0

        # Write with retain_parameters
        output = series.write(
            format=None,
            buf=None,
            comment=False,
            retain_parameters=True,
            parameter_set=ps,
            uri_prefix="kwd/series",
        )

        assert "&val" in output

    
    def test_bounded_table_card_retention(self):
        """Test bounded TableCard retains parameter refs."""
        from ansys.dyna.core.lib.table_card import TableCard

        ps = ParameterSet()
        ps.add("x1", 10.0)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
            ],
            lambda: 2,  # Bounded to 2 rows
        )

        card_text = """       1        &x1
       2       20.0"""
        buf = io.StringIO(card_text)

        with ps.scope("kwd"):
            table.read(buf, ps)

        assert len(table.table) == 2
        assert table.table["x"][0] == 10.0

        output = table.write(
            comment=False,
            retain_parameters=True,
            parameter_set=ps,
            uri_prefix="kwd",
        )

        assert "&x1" in output

    
    def test_bounded_table_card_group_retention(self):
        """Test bounded TableCardGroup retains parameter refs."""
        from ansys.dyna.core.lib.table_card_group import TableCardGroup
        from ansys.dyna.core.lib.field_schema import FieldSchema

        ps = ParameterSet()
        ps.add("id1", 100)
        ps.add("val1", 10.5)

        card_schemas = [
            (FieldSchema("id", int, 0, 8, None), FieldSchema("value1", float, 8, 16, None)),
            (FieldSchema("value2", float, 0, 16, None), FieldSchema("value3", float, 16, 16, None)),
        ]

        group = TableCardGroup(
            card_schemas,
            lambda: 1,  # Bounded to 1 row
        )

        card_text = """    &id1      &val1
            5.0            6.0"""
        buf = io.StringIO(card_text)

        with ps.scope("kwd"):
            group.read(buf, ps)

        assert len(group.table) == 1
        assert group.table["id"][0] == 100
        assert group.table["value1"][0] == 10.5

        output = group.write(
            comment=False,
            retain_parameters=True,
            parameter_set=ps,
            uri_prefix="kwd",
        )

        assert "&id1" in output
        assert "&val1" in output


class TestPostExpandRetention:
    """Test parameter retention after deck expansion (includes)."""

    
    def test_deck_expand_preserves_parameter_refs(self, tmp_path):
        """Test that deck.expand() preserves parameter refs for retention."""
        from ansys.dyna.core.lib.deck import Deck

        # Create include file
        include_content = """*SET_NODE_LIST
         1       0.0       0.0       0.0       0.0    MECH         1
       100      &n1       102"""

        include_path = tmp_path / "include.k"
        include_path.write_text(include_content)

        # Main deck with parameter and include
        deck_string = f"""*PARAMETER
Rn1,101
*INCLUDE
{include_path}"""

        deck = Deck()
        deck.loads(deck_string)

        # Expand includes - returns a NEW deck
        expanded_deck = deck.expand(cwd=str(tmp_path))

        # Verify the included keyword has the substituted value
        set_node = None
        for kw in expanded_deck.keywords:
            if hasattr(kw, "nodes"):
                set_node = kw
                break

        assert set_node is not None
        assert set_node.nodes[1] == 101  # &n1 substituted

        # Write with retain_parameters
        output = expanded_deck.write(retain_parameters=True)

        # The parameter ref should be retained
        assert "&n1" in output


class TestOptionCardRetention:
    """Test parameter retention with option cards."""

    
    def test_option_card_field_retention(self):
        """Test that parameters in option card fields are retained."""
        from ansys.dyna.core.lib.deck import Deck

        # MAT_001 with TITLE option - mid field can have parameter
        # Field widths: mid=10, ro=10, e=10, pr=10, da=10, db=10
        deck_string = """*PARAMETER
Rmid,100
*MAT_ELASTIC_TITLE
my_material_title
$#     mid        ro         e        pr        da        db  not used
      &mid    7870.02.07000E11     0.292       0.0       0.0       0.0"""

        deck = Deck()
        deck.loads(deck_string)

        # Verify the material keyword read correctly
        mat = deck.keywords[1]
        assert mat.mid == 100  # &mid substituted

        # Write without retain_parameters - should have numeric value
        output = deck.write()
        assert "*MAT_ELASTIC" in output
        assert "100" in output
        assert "&mid" not in output

        # Write with retain_parameters - should have parameter reference
        output_retain = deck.write(retain_parameters=True)
        assert "&mid" in output_retain

    
    def test_option_card_with_multiple_parameters(self):
        """Test option card with parameters in base and option cards."""
        from ansys.dyna.core.lib.deck import Deck

        # SET_NODE_LIST with parameters
        deck_string = """*PARAMETER
Rsid,1
Rda1,0.5
Rn1,100
*SET_NODE_LIST
      &sid     &da1       0.0       0.0       0.0    MECH         1
      &n1       101       102"""

        deck = Deck()
        deck.loads(deck_string)

        # Verify read
        set_node = deck.keywords[1]
        assert set_node.sid == 1  # &sid substituted
        assert set_node.da1 == 0.5  # &da1 substituted
        assert set_node.nodes[0] == 100  # &n1 substituted

        # Write with retain_parameters
        output = deck.write(retain_parameters=True)
        assert "&sid" in output
        assert "&da1" in output
        assert "&n1" in output
