import pytest
from ansys.dyna.core import keywords as kwd

RAW_TEXT = """*PART_COMPOSITE
$#                                                                         title
Test
$#     pid    elform      shrf    unused    unused      hgid    unused    tshear
         1         1       1.0                             0                   0
$#    mid1    thick1        b1     tmid1      mid2    thick2        b2     tmid2
         1       1.0       1.0         1         2       2.0       2.0         2
         3       3.0      1.03         3         4       4.0       4.0         4
"""


def test_part_composite_reads_header_fields():
    """Basic header fields (title, pid) should always parse correctly."""
    part = kwd.PartComposite()
    part.loads(RAW_TEXT)
    assert part.title == "Test"
    assert part.pid == 1


def test_part_composite_multiple_layer_cards():
    """PartComposite must read all layer cards, not just the first (issue #1252)."""
    part = kwd.PartComposite()
    part.loads(RAW_TEXT)
    layers = part.layers
    assert len(layers) == 2, f"Expected 2 layer rows, got {len(layers)}"
    assert layers["mid1"].tolist() == [1, 3]
    assert layers["thick1"].tolist() == [1.0, 3.0]
    assert layers["b1"].tolist() == [1.0, 1.03]
    assert layers["tmid1"].tolist() == [1, 3]
    assert layers["mid2"].tolist() == [2, 4]
    assert layers["thick2"].tolist() == [2.0, 4.0]
    assert layers["b2"].tolist() == [2.0, 4.0]
    assert layers["tmid2"].tolist() == [2, 4]


def test_part_composite_roundtrip():
    """Round-trip loads→write must reproduce all layer rows."""
    part = kwd.PartComposite()
    part.loads(RAW_TEXT)
    output = part.write()
    part2 = kwd.PartComposite()
    part2.loads(output)
    assert len(part2.layers) == 2, "Round-trip lost layer rows"