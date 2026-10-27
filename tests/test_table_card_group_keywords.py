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

"""Keywords whose table card group has a card that only some rows have.

*ELEMENT_SHELL_THICKNESS, _BETA and _MCID have a THIC5-THIC8 line only for elements
with mid-side nodes (issue #1350). *CONTROL_MPP_DECOMPOSITION_TRANSFORMATION has a
V7-V9 line only for transformations of type VEC3, C2R, S2R and MAT.
"""

import warnings

import numpy as np
import pandas as pd
import pytest

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd

_E8 = (
    "       {e}       1       1       2       3       4       5       6       7       8\n"
    "             1.0             1.0             1.0             1.0             0.0\n"
    "             {t}             {t}             {t}             {t}\n"
)
_E4 = (
    "       {e}       1      11      12      13      14\n"
    "             2.0             2.0             2.0             2.0             0.0\n"
)

_SHELL_TITLES = ["*ELEMENT_SHELL_THICKNESS", "*ELEMENT_SHELL_BETA", "*ELEMENT_SHELL_MCID"]

_SHELL_BLOCKS = [
    pytest.param(_E8.format(e=1, t="1.5"), [1], [1.5], id="A: 8-node shell"),
    pytest.param(_E8.format(e=1, t="2.0"), [1], [2.0], id="B: 8-node shell, whole THIC5-8"),
    pytest.param(_E8.format(e=1, t="1.5") + _E4.format(e=2), [1, 2], [1.5, np.nan], id="C: 8-node, 4-node"),
    pytest.param(_E8.format(e=1, t="1.5") + _E8.format(e=2, t="2.5"), [1, 2], [1.5, 2.5], id="8-node shells"),
    pytest.param(_E4.format(e=1) + _E8.format(e=2, t="1.5"), [1, 2], [np.nan, 1.5], id="4-node, 8-node"),
    pytest.param(_E4.format(e=1) + _E4.format(e=2), [1, 2], [np.nan, np.nan], id="4-node shells"),
]


def _load(text: str):
    deck = Deck()
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        deck.loads(text)
    [keyword] = deck.all_keywords
    assert not isinstance(keyword, str)
    return keyword


def _data_lines(keyword) -> list:
    return [line for line in keyword.write().split("\n") if not line.startswith("$#")]


@pytest.mark.parametrize("title", _SHELL_TITLES)
@pytest.mark.parametrize("body, eids, thic5", _SHELL_BLOCKS)
def test_deck_reads_shell_blocks(title, body, eids, thic5):
    elements = _load(f"*KEYWORD\n{title}\n{body}*END\n").elements
    assert elements["eid"].tolist() == eids
    assert elements["thic1"].notna().all()
    for name in ["thic5", "thic6", "thic7", "thic8"]:
        np.testing.assert_array_equal(elements[name].to_numpy(), thic5)


@pytest.mark.parametrize("title", _SHELL_TITLES)
@pytest.mark.parametrize("body, eids, thic5", _SHELL_BLOCKS)
def test_shell_blocks_round_trip(title, body, eids, thic5):
    keyword = _load(f"*KEYWORD\n{title}\n{body}*END\n")
    reread = _load(f"*KEYWORD\n{keyword.write()}\n*END\n")
    pd.testing.assert_frame_equal(reread.elements, keyword.elements)


def test_write_mixed_shells():
    keyword = _load(f"*KEYWORD\n*ELEMENT_SHELL_THICKNESS\n{_E8.format(e=1, t='1.5')}{_E4.format(e=2)}*END\n")
    assert keyword.write() == (
        "*ELEMENT_SHELL_THICKNESS\n"
        "$#   eid     pid      n1      n2      n3      n4      n5      n6      n7      n8\n"
        "       1       1       1       2       3       4       5       6       7       8\n"
        "$#         thic1           thic2           thic3           thic4            beta\n"
        "             1.0             1.0             1.0             1.0             0.0\n"
        "$#         thic5           thic6           thic7           thic8\n"
        "             1.5             1.5             1.5             1.5\n"
        "       2       1      11      12      13      14                                \n"
        "             2.0             2.0             2.0             2.0             0.0"
    )


def test_shell_elements_from_dataframe():
    keyword = kwd.ElementShellThickness()
    keyword.elements = pd.DataFrame(
        {
            "eid": [1, 2, 3],
            "pid": [1, 1, 1],
            "n1": [1, 11, 21],
            "n2": [2, 12, 22],
            "n3": [3, 13, 23],
            "n4": [4, 14, 24],
            "n5": [5, None, 0],
            "thic1": [1.0, 2.0, 3.0],
            "thic5": [1.5, None, None],
        }
    )
    lines = _data_lines(keyword)
    assert len(lines) == 1 + 3 + 2 + 2
    assert lines[3] == "             1.5             0.0             0.0             0.0"


def test_control_mpp_transformations_from_dataframe():
    keyword = kwd.ControlMppDecompositionTransformation()
    keyword.transformation = pd.DataFrame(
        {"type": ["RX", "VEC3", "SY", "C2R"], "v1": [90.0, 1.0, 0.0, 2.0], "v7": [None, 7.0, None, 8.0]}
    )
    lines = _data_lines(keyword)
    assert [line[:10].strip() for line in lines[1:]] == ["RX", "VEC3", "7.0", "SY", "C2R", "8.0"]
