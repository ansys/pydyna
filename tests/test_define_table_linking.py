import pytest

from ansys.dyna.core import Deck

def test_define_table_linking_simple(ref_string):
    deck_string = ref_string.test_define_table_simple_ref
    deck = Deck()
    deck.loads(deck_string)

    tables = deck.get(type="DEFINE", filter=lambda k: getattr(k, "subkeyword", None) == "TABLE")
    assert len(tables) == 1
    table = tables[0]
    assert len(table.linked_curves) >= 1
    assert table.linked_curves[0].lcid == table.tbid

def test_define_table_linking_interleaved(ref_string):
    deck_string = ref_string.test_define_table_interleaved_ref
    deck = Deck()
    deck.loads(deck_string)

    tables = sorted(deck.get(type="DEFINE", filter=lambda k: getattr(k, "subkeyword", None) == "TABLE"), key=lambda x: x.tbid)
    assert len(tables) == 2
    table_a = [t for t in tables if t.tbid == 10000001][0]
    table_b = [t for t in tables if t.tbid == 20000001][0]
    assert len(table_a.linked_curves) >= 1
    assert table_a.linked_curves[0].lcid == 10000001
    assert len(table_b.linked_curves) >= 1
    assert table_b.linked_curves[0].lcid == 20000001

def test_define_table_linking_curve_before_table(ref_string):
    deck_string = ref_string.test_define_table_before_ref
    deck = Deck()
    deck.loads(deck_string)

    curves = deck.get(type="DEFINE", filter=lambda k: getattr(k, "subkeyword", None) == "CURVE")
    tables = deck.get(type="DEFINE", filter=lambda k: getattr(k, "subkeyword", None) == "TABLE")
    
    # First curve appears before any table, so no table has it linked
    # Second curve appears after table_a, so it should be in table_a.linked_curves
    table_a = [t for t in tables if t.tbid == 10000001][0]
    assert len(table_a.linked_curves) >= 1
    assert table_a.linked_curves[0] is curves[1]
