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

"""Column properties of table cards and table card groups (issue #1332).

Each column of a table is a keyword property. The getter returns the column as a
Series. The setter takes a scalar, which is copied to every row, or a Series or
array-like, which is assigned by position: missing values pad a short one, and a
long one grows the table.
"""

import numpy as np
import pandas as pd
import pytest

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.lib.card import Field
from ansys.dyna.core.lib.table_card import TableCard

_PARTS = """*KEYWORD
*PART
$#                                                                         title
part one
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
      1010         1         1         0         0         0         0         0
part two
      1020         2         1         0         0         0         0         0
*END"""


class TestGetter:
    def test_table_card_group_column_is_series(self):
        deck = Deck()
        deck.loads(_PARTS)
        part = deck.keywords[0]
        assert isinstance(part.pid, pd.Series)
        assert part.pid.tolist() == [1010, 1020]
        assert part.heading.tolist() == ["part one", "part two"]

    def test_table_card_column_is_series(self):
        node = kwd.Node(nid=[1, 2], x=[0.5, 1.5])
        assert isinstance(node.nid, pd.Series)
        assert node.nid.tolist() == [1, 2]
        assert node.x.tolist() == [0.5, 1.5]

    def test_empty_table(self):
        part = kwd.Part()
        assert len(part.pid) == 0
        assert part.pid.dtype == pd.Int32Dtype()

    def test_getter_follows_the_table(self):
        part = kwd.Part(pid=[1, 2])
        part.parts = pd.DataFrame({"pid": [5, 6, 7]})
        assert part.pid.tolist() == [5, 6, 7]


class TestScalarSetter:
    def test_copies_to_every_row(self):
        part = kwd.Part(pid=[1, 2, 3])
        part.mid = 9
        assert part.mid.tolist() == [9, 9, 9]
        assert part.pid.tolist() == [1, 2, 3]

    def test_empty_unbounded_table_gets_one_row(self):
        node = kwd.Node()
        node.nid = 7
        assert len(node.nodes) == 1
        assert node.nid.tolist() == [7]
        assert node.x.tolist() == [0.0]

    def test_bounded_table_keeps_its_rows(self):
        card = TableCard([Field("a", int, 0, 10), Field("b", float, 10, 10)], lambda: 2)
        card.set_column("a", 4)
        assert card.table["a"].tolist() == [4, 4]

    def test_empty_bounded_table_stays_empty(self):
        card = TableCard([Field("a", int, 0, 10)], lambda: 0)
        card.set_column("a", 4)
        assert len(card.table) == 0


class TestArraySetter:
    def test_same_length(self):
        part = kwd.Part(pid=[1, 2])
        part.secid = [3, 4]
        assert part.secid.tolist() == [3, 4]

    def test_short_value_is_padded_with_missing_values(self):
        part = kwd.Part(pid=[1, 2, 3])
        part.secid = [3]
        assert part.secid[0] == 3
        assert part.secid[1:].isna().all()

    def test_long_value_grows_the_table_with_defaults(self):
        part = kwd.Part(pid=[1])
        part.pid = [1, 2, 3]
        assert len(part.parts) == 3
        assert part.pid.tolist() == [1, 2, 3]
        # eosid has a schema default of 0, secid has none.
        assert part.eosid.tolist() == [0, 0, 0]
        assert part.secid.isna().all()

    def test_series(self):
        node = kwd.Node(nid=[1, 2])
        node.x = pd.Series([1.0, 2.0])
        assert node.x.tolist() == [1.0, 2.0]

    def test_series_with_non_default_index_raises(self):
        node = kwd.Node(nid=[1, 2])
        with pytest.raises(ValueError, match="default index"):
            node.x = pd.Series([1.0, 2.0], index=[1, 0])

    def test_numpy_array(self):
        node = kwd.Node()
        node.nid = np.arange(1, 4)
        assert node.nid.tolist() == [1, 2, 3]

    def test_bounded_table_can_not_grow(self):
        card = TableCard([Field("a", int, 0, 10)], lambda: 2)
        with pytest.raises(ValueError, match="bounded"):
            card.set_column("a", [1, 2, 3])

    def test_unknown_column_raises(self):
        card = TableCard([Field("a", int, 0, 10)], None)
        with pytest.raises(KeyError):
            card.set_column("b", 1)


class TestDtype:
    @pytest.mark.parametrize("value", [1.5, [1, 1.5]])
    def test_lossy_cast_to_int_raises(self, value):
        part = kwd.Part(pid=[1, 2])
        with pytest.raises((TypeError, ValueError)):
            part.mid = value

    def test_int_column_keeps_its_dtype(self):
        part = kwd.Part(pid=[1, 2])
        part.mid = [1.0, 2.0]
        assert part.mid.dtype == pd.Int32Dtype()
        part.mid = pd.Series([3, None])
        assert part.mid.dtype == pd.Int32Dtype()
        assert part.mid.isna().tolist() == [False, True]

    def test_float_column_keeps_its_dtype(self):
        node = kwd.Node(nid=[1, 2])
        node.x = [1, 2]
        assert node.x.dtype == np.float64

    def test_missing_string_is_written_blank(self):
        part = kwd.Part(pid=[1, 2])
        part.heading = ["first"]
        lines = part.write().splitlines()
        assert lines[2].strip() == "first"
        assert "None" not in part.write()
        assert "nan" not in part.write()


class TestConstructor:
    def test_scalar_gives_one_row(self):
        part = kwd.Part(pid=1)
        assert part.pid.tolist() == [1]

    def test_longest_value_sets_the_length(self):
        part = kwd.Part(pid=[1, 2, 3], mid=5, secid=[7])
        assert part.pid.tolist() == [1, 2, 3]
        assert part.mid.tolist() == [5, 5, 5]
        assert part.secid[0] == 7
        assert part.secid[1:].isna().all()

    def test_order_of_kwargs_does_not_matter(self):
        first = kwd.Part(mid=5, pid=[1, 2, 3])
        second = kwd.Part(pid=[1, 2, 3], mid=5)
        pd.testing.assert_frame_equal(first.parts, second.parts)
        assert first.mid.tolist() == [5, 5, 5]

    def test_table_card(self):
        node = kwd.Node(nid=[1, 2], z=3.0)
        assert node.nid.tolist() == [1, 2]
        assert node.z.tolist() == [3.0, 3.0]

    def test_written_deck_reads_back(self):
        part = kwd.Part(pid=[1, 2], secid=[3, 4], mid=5)
        deck = Deck()
        deck.loads("*KEYWORD\n" + part.write() + "\n*END")
        read = deck.keywords[0]
        assert read.pid.tolist() == [1, 2]
        assert read.secid.tolist() == [3, 4]
        assert read.mid.tolist() == [5, 5]
