# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT

"""Tests for issue #1252: PART_COMPOSITE should support multiple layer cards."""

import pytest
from ansys.dyna.core import keywords as kwd


RAW_PART_COMPOSITE = """\
*PART_COMPOSITE
$#                                                                         title
Test
$#     pid    elform      shrf    unused    unused      hgid    unused    tshear
         1         1       1.0                             0                   0
$#    mid1    thick1        b1     tmid1      mid2    thick2        b2     tmid2
         1       1.0       1.0         1         2       2.0       2.0         2
         3       3.0      1.03         3         4       4.0       4.0         4
"""


def test_part_composite_loads_multiple_layer_cards():
    """PART_COMPOSITE.loads() should capture all layer rows, not just the first."""
    part = kwd.PartComposite()
    part.loads(RAW_PART_COMPOSITE)

    # Both layer rows must be present in the output
    output = part.write()
    assert "3" in output  # mid from second layer row
    assert "3.0" in output  # thick from second layer row
    assert output.count("1.0") >= 2  # appears in both layer rows


def test_part_composite_roundtrip_multiple_layers():
    """Roundtrip: text loaded then written should preserve all layer cards."""
    part = kwd.PartComposite()
    part.loads(RAW_PART_COMPOSITE)
    output = part.write()

    # Reload and compare
    part2 = kwd.PartComposite()
    part2.loads(output)
    assert part2.write() == output
