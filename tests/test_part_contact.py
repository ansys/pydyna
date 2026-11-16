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

"""*PART_CONTACT is a repeating parts table, so IDPOFF offsets its pid (issue #1369)."""

import warnings

import pytest

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd

_PART_CONTACT = """\
*KEYWORD
*PART_CONTACT
$#                                                                         title
shell-one
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         1         2         3         0         0         0         0         0
$#      fs        fd        dc        vc      optt       sft       ssf    cparm8
       0.1       0.2       0.3       0.4       1.0       1.1       1.2       2.0
shell-zero
         0         1         1         0         0         0         0         0
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
*ELEMENT_SHELL
$#   eid     pid      n1      n2      n3      n4      n5      n6      n7      n8
         1         1         1         2         3         4         0         0         0         0
*END
"""


def _load_part_contact() -> kwd.PartContact:
    deck = Deck()
    deck.loads(_PART_CONTACT)
    part = deck.keywords[0]
    assert isinstance(part, kwd.PartContact)
    return part


def test_part_contact_reads_repeating_rows():
    """Each title, part card, and contact card is one row of the parts table."""
    part = _load_part_contact()

    assert part.pid.tolist() == [1, 0]
    assert part.title.tolist() == ["shell-one", "shell-zero"]
    assert part.secid.tolist() == [2, 1]
    assert part.mid.tolist() == [3, 1]
    assert part.fs.tolist() == pytest.approx([0.1, 0.0])
    assert part.fd.tolist() == pytest.approx([0.2, 0.0])
    assert part.ssf.tolist() == pytest.approx([1.2, 0.0])
    assert part.cparm8.tolist() == pytest.approx([2.0, 0.0])


def test_part_contact_write_roundtrip():
    """Writing and reading again preserves every part row, including the contact card."""
    part = _load_part_contact()
    written = part.write()

    reloaded = kwd.PartContact()
    reloaded.loads(written)

    assert reloaded.write() == written
    assert reloaded.pid.tolist() == [1, 0]
    assert reloaded.title.tolist() == ["shell-one", "shell-zero"]
    assert reloaded.fs.tolist() == pytest.approx([0.1, 0.0])
    assert reloaded.optt.tolist() == pytest.approx([1.0, 0.0])
    assert reloaded.cparm8.tolist() == pytest.approx([2.0, 0.0])


def test_part_contact_idpoff_on_expand(tmp_path):
    """*INCLUDE_TRANSFORM IDPOFF offsets *PART_CONTACT pids and does not warn."""
    include = tmp_path / "sub-keyfile.k"
    include.write_text(_PART_CONTACT, encoding="utf-8")

    deck = Deck()
    transform = kwd.IncludeTransform(filename="sub-keyfile.k")
    transform.idpoff = 100
    deck.append(transform)

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        expanded = deck.expand(cwd=str(tmp_path))

    assert not any("not transformed" in str(item.message) for item in caught)
    part = expanded.keywords[0]
    element = expanded.keywords[1]
    assert isinstance(part, kwd.PartContact)
    # pid=1 is offset; pid=0 is left alone, matching *PART
    assert part.pid.tolist() == [101, 0]
    assert part.fs.tolist() == pytest.approx([0.1, 0.0])
    assert element.elements["pid"].tolist() == [101]

    rewritten = kwd.PartContact()
    rewritten.loads(part.write())
    assert rewritten.pid.tolist() == [101, 0]
