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

"""*LOAD_SEGMENT and *LOAD_SEGMENT_ID hold any number of segments.

Each segment is one card, followed by an N6-N8 card only when N5 is non-zero.
"""

import warnings

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd

import pytest

_FOUR_NODE_SEGMENTS = """\
         1    40.000                3001      3002      3003      3004
         1    40.000                3002      3005      3006      3003
"""

_MIXED_SEGMENTS = """\
         1       1.0       0.0       101       102       103       104       105
       106       107       108
         2       2.0       0.0       201       202       203       204
"""

_SEGMENT_HEADER = "$#    lcid        sf        at        n1        n2        n3        n4        n5"
_MIDSIDE_HEADER = "$#      n6        n7        n8"


def _nodes(segment):
    return [segment.n1, segment.n2, segment.n3, segment.n4, segment.n5, segment.n6, segment.n7, segment.n8]


@pytest.fixture(params=["*LOAD_SEGMENT\n", "*LOAD_SEGMENT_ID\n        10hinge\n"], ids=["plain", "id"])
def title_and_id(request):
    return request.param


def test_reads_every_four_node_segment(title_and_id):
    keyword = kwd.LoadSegment()
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        keyword.loads(title_and_id + _FOUR_NODE_SEGMENTS)
    assert len(keyword.sets) == 2
    assert [(s.lcid, s.sf) for s in keyword.sets] == [(1, 40.0), (1, 40.0)]
    assert _nodes(keyword.sets[0]) == [3001, 3002, 3003, 3004, None, None, None, None]
    assert _nodes(keyword.sets[1]) == [3002, 3005, 3006, 3003, None, None, None, None]


def test_reads_midside_card_only_after_segment_with_n5(title_and_id):
    keyword = kwd.LoadSegment()
    keyword.loads(title_and_id + _MIXED_SEGMENTS)
    assert len(keyword.sets) == 2
    assert _nodes(keyword.sets[0]) == [101, 102, 103, 104, 105, 106, 107, 108]
    assert keyword.sets[1].lcid == 2
    assert keyword.sets[1].sf == 2.0
    assert _nodes(keyword.sets[1]) == [201, 202, 203, 204, None, None, None, None]


def test_writes_midside_card_only_for_segment_with_n5(title_and_id):
    keyword = kwd.LoadSegment()
    keyword.loads(title_and_id + _MIXED_SEGMENTS)
    lines = keyword.write().splitlines()
    data_lines = lines[lines.index(_SEGMENT_HEADER) + 1 :]
    assert data_lines == [
        "         1       1.0       0.0       101       102       103       104       105",
        _MIDSIDE_HEADER,
        "       106       107       108",
        "         2       2.0       0.0       201       202       203       204          ",
    ]


def test_round_trip(title_and_id):
    keyword = kwd.LoadSegment()
    keyword.loads(title_and_id + _MIXED_SEGMENTS)
    reread = kwd.LoadSegment()
    reread.loads(keyword.write())
    assert reread.options["ID"].active == keyword.options["ID"].active
    assert [_nodes(s) for s in reread.sets] == [_nodes(s) for s in keyword.sets]
    assert reread.write() == keyword.write()


def test_empty_keyword_writes_title_only():
    assert kwd.LoadSegment().write() == "*LOAD_SEGMENT"


def test_empty_keyword_with_id_option_writes_id_card_only():
    keyword = kwd.LoadSegment()
    keyword.options["ID"].active = True
    assert keyword.write() == "\n".join(
        [
            "*LOAD_SEGMENT_ID",
            "$#      id                                                               heading",
            " " * 80,
        ]
    )


def test_four_node_segment_writes_single_line():
    keyword = kwd.LoadSegment(lcid=1, sf=40.0, n1=3001, n2=3002, n3=3003, n4=3004)
    assert len(keyword.sets) == 1
    assert keyword.write() == "\n".join(
        [
            "*LOAD_SEGMENT",
            _SEGMENT_HEADER,
            "         1      40.0       0.0      3001      3002      3003      3004          ",
        ]
    )


def test_add_set_appends_segments():
    keyword = kwd.LoadSegment()
    keyword.add_set(lcid=1, n1=1, n2=2, n3=3, n4=4)
    keyword.add_set(lcid=1, n1=1, n2=2, n3=3, n4=4, n5=5, n6=6, n7=7, n8=8)
    assert keyword.write() == "\n".join(
        [
            "*LOAD_SEGMENT",
            _SEGMENT_HEADER,
            "         1       1.0       0.0         1         2         3         4          ",
            "         1       1.0       0.0         1         2         3         4         5",
            "         6         7         8",
        ]
    )


def test_scalar_properties_require_exactly_one_segment():
    keyword = kwd.LoadSegment()
    keyword.loads("*LOAD_SEGMENT\n" + _FOUR_NODE_SEGMENTS)
    with pytest.raises(LookupError):
        keyword.lcid


def test_load_segment_id_title_activates_id_option():
    deck = Deck()
    deck.loads("*KEYWORD\n*LOAD_SEGMENT_ID\n        10hinge\n" + _FOUR_NODE_SEGMENTS + "*END\n")
    (keyword,) = deck.keywords
    assert isinstance(keyword, kwd.LoadSegment)
    assert keyword.options["ID"].active
    assert keyword.id == 10
    assert keyword.heading == "hinge"
    assert len(keyword.sets) == 2
    assert keyword.write().splitlines()[0] == "*LOAD_SEGMENT_ID"


def test_plain_title_leaves_id_option_inactive():
    keyword = kwd.LoadSegment()
    keyword.loads("*LOAD_SEGMENT\n" + _FOUR_NODE_SEGMENTS)
    assert not keyword.options["ID"].active
    assert keyword.write().splitlines()[:2] == ["*LOAD_SEGMENT", _SEGMENT_HEADER]


def test_id_card_kwargs_do_not_create_segment():
    keyword = kwd.LoadSegment(id=5, heading="h")
    assert keyword.id == 5
    assert len(keyword.sets) == 0
    keyword.options["ID"].active = True
    keyword.add_set(lcid=2, n1=1, n2=2, n3=3, n4=4)
    assert keyword.write() == "\n".join(
        [
            "*LOAD_SEGMENT_ID",
            "$#      id                                                               heading",
            "         5h                                                                     ",
            _SEGMENT_HEADER,
            "         2       1.0       0.0         1         2         3         4          ",
        ]
    )


def test_id_and_segment_kwargs_together():
    keyword = kwd.LoadSegment(id=5, lcid=3, n1=1)
    assert keyword.id == 5
    assert len(keyword.sets) == 1
    assert keyword.lcid == 3
    assert keyword.n1 == 1


def test_setting_id_activates_id_option():
    keyword = kwd.LoadSegment(lcid=1, n1=1, n2=2, n3=3, n4=4)
    keyword.id = 7
    assert keyword.options["ID"].active
    assert keyword.write().splitlines()[:3] == [
        "*LOAD_SEGMENT_ID",
        "$#      id                                                               heading",
        "         7                                                                      ",
    ]


def test_deck_load_keeps_all_segments():
    segments = "".join(
        f"         1    40.000{'':10}{n:10d}{n + 1:10d}{n + 2:10d}{n + 3:10d}\n" for n in range(3001, 3601)
    )
    deck = Deck()
    deck.loads("*KEYWORD\n*LOAD_SEGMENT\n" + segments + "*END\n")
    (keyword,) = deck.keywords
    assert isinstance(keyword, kwd.LoadSegment)
    assert len(keyword.sets) == 600
    assert _nodes(keyword.sets[-1])[:5] == [3600, 3601, 3602, 3603, None]
