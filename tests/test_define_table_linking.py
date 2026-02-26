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
