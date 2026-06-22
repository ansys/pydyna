# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT

"""Tests for MAT_295 fiber families using CardSet with discriminator (issue #995).

The MAT_295 keyword uses fiber families with a discriminator field (FTYPE) that
determines which mutually-exclusive card is active. The manual subclass adds
before_read support to handle the self-referential conditional pattern.
"""

import io
import pytest

from ansys.dyna.core.keywords.keyword_classes.manual.mat_295 import (
    Mat295,
)

from ansys.dyna.core.lib.deck import Deck


class TestMat295FiberFamilies:
    """Test MAT_295 fiber families with different FTYPE values."""

    def test_fiber_family_ftype1(self):
        """Test creating a fiber family with ftype=1 (Holzapfel-Gasser-Ogden)."""
        mat = Mat295()
        mat.atype = 1
        mat.nf = 1

        families = mat.fiber_families
        assert len(families) == 1

        families[0].theta = 0.5
        families[0].a = 1.0
        families[0].b = 0.5
        families[0].ftype = 1
        families[0].fcid = 100
        families[0].k1 = 0.1
        families[0].k2 = 0.2

        assert families[0].ftype == 1
        assert families[0].theta == 0.5
        assert families[0].fcid == 100

    def test_fiber_family_ftype2(self):
        """Test creating a fiber family with ftype=2 (Freed-Doehring)."""
        mat = Mat295()
        mat.atype = 1
        mat.nf = 1

        families = mat.fiber_families
        families[0].theta = 0.3
        families[0].a = 0.8
        families[0].b = 0.4
        families[0].ftype = 2
        families[0].flcid = 200
        families[0].e = 1000.0
        families[0].r0norm = 0.5
        families[0].h0norm = 0.1

        assert families[0].ftype == 2
        assert families[0].flcid == 200
        assert families[0].e == 1000.0

    def test_multiple_fiber_families_different_ftype(self):
        """Test creating multiple fiber families with different ftype values (issue #995)."""
        mat = Mat295()
        mat.mid = 1
        mat.rho = 1000.0
        mat.atype = 1
        mat.nf = 2

        families = mat.fiber_families
        assert len(families) == 2

        # Family 0: ftype=1
        families[0].theta = 0.5
        families[0].a = 1.0
        families[0].b = 0.5
        families[0].ftype = 1
        families[0].fcid = 100
        families[0].k1 = 0.1
        families[0].k2 = 0.2

        # Family 1: ftype=2
        families[1].theta = 0.3
        families[1].a = 0.8
        families[1].b = 0.4
        families[1].ftype = 2
        families[1].flcid = 200
        families[1].e = 1000.0
        families[1].r0norm = 0.5
        families[1].h0norm = 0.1

        assert families[0].ftype == 1
        assert families[1].ftype == 2

    def test_write_multiple_fiber_families(self):
        """Test writing MAT_295 with multiple fiber families of different ftype."""
        mat = Mat295()
        mat.mid = 1
        mat.rho = 1000.0
        mat.atype = 1
        mat.nf = 2

        families = mat.fiber_families

        # Family 0: ftype=1
        families[0].theta = 0.5
        families[0].a = 1.0
        families[0].b = 0.5
        families[0].ftype = 1
        families[0].fcid = 100
        families[0].k1 = 0.1
        families[0].k2 = 0.2

        # Family 1: ftype=2
        families[1].theta = 0.3
        families[1].a = 0.8
        families[1].b = 0.4
        families[1].ftype = 2
        families[1].flcid = 200
        families[1].e = 1000.0
        families[1].r0norm = 0.5
        families[1].h0norm = 0.1

        output = mat.write()

        # Verify the output contains the expected data
        assert "*MAT_295" in output
        assert "ANISO" in output

        # Both fiber families should be present
        # Family 0 card 0: theta=0.5, a=1.0, b=0.5
        assert "0.5" in output
        assert "1.0" in output

        # Family 0 card 1: ftype=1, fcid=100
        lines = output.split("\n")
        # Check that ftype=1 line has fcid=100
        ftype1_found = False
        for line in lines:
            if "100" in line and "0.1" in line:
                ftype1_found = True
                break
        assert ftype1_found, "ftype=1 card with fcid=100 not found"

        # Check that ftype=2 line has flcid=200
        ftype2_found = False
        for line in lines:
            if "200" in line and "1000" in line:
                ftype2_found = True
                break
        assert ftype2_found, "ftype=2 card with flcid=200 not found"

    def test_roundtrip_multiple_fiber_families(self):
        """Test round-trip: write and read back fiber families with different ftype."""
        # Create original
        mat = Mat295()
        mat.mid = 1
        mat.rho = 1000.0
        mat.atype = 1
        mat.nf = 2

        families = mat.fiber_families
        families[0].theta = 0.5
        families[0].a = 1.0
        families[0].b = 0.5
        families[0].ftype = 1
        families[0].fcid = 100
        families[0].k1 = 0.1
        families[0].k2 = 0.2

        families[1].theta = 0.3
        families[1].a = 0.8
        families[1].b = 0.4
        families[1].ftype = 2
        families[1].flcid = 200
        families[1].e = 1000.0
        families[1].r0norm = 0.5
        families[1].h0norm = 0.1

        # Write
        output = mat.write()

        # Read back
        deck = Deck()
        deck.loads(output)
        assert len(deck.keywords) == 1

        mat2 = deck.keywords[0]
        assert mat2.mid == 1
        assert mat2.atype == 1
        assert mat2.nf == 2
        assert len(mat2.fiber_families) == 2

        # Check family 0 (ftype=1)
        ff0 = mat2.fiber_families[0]
        assert ff0.ftype == 1
        assert ff0.theta == 0.5
        assert ff0.a == 1.0
        assert ff0.b == 0.5
        assert ff0.fcid == 100
        assert ff0.k1 == pytest.approx(0.1)

        # Check family 1 (ftype=2)
        ff1 = mat2.fiber_families[1]
        assert ff1.ftype == 2
        assert ff1.theta == pytest.approx(0.3)
        assert ff1.a == pytest.approx(0.8)
        assert ff1.b == pytest.approx(0.4)
        assert ff1.flcid == 200
        assert ff1.e == 1000.0

    def test_no_extra_blank_lines_one_fiber_family(self):
        """Test that one fiber family writes without extra blank lines."""
        mat = Mat295()
        mat.mid = 1
        mat.nf = 1
        mat.atype = -1

        families = mat.fiber_families
        families[0].theta = 30.0
        families[0].a = 1.0
        families[0].b = 2.0
        families[0].ftype = 1
        families[0].k1 = 0.00049
        families[0].k2 = 9.01

        output = mat.write()
        lines = output.split("\n")

        # Find the ANISO section and check for consecutive blank lines
        aniso_idx = None
        for i, line in enumerate(lines):
            if "ANISO" in line:
                aniso_idx = i
                break

        assert aniso_idx is not None, "ANISO line not found in output"

        # Check for consecutive blank lines (double blank lines)
        fiber_section = lines[aniso_idx : aniso_idx + 10]
        for i in range(len(fiber_section) - 1):
            if fiber_section[i].strip() == "" and fiber_section[i + 1].strip() == "":
                pytest.fail(
                    f"Found consecutive blank lines at index {aniso_idx + i}: "
                    f"'{fiber_section[i]}' and '{fiber_section[i + 1]}'"
                )

    def test_no_extra_blank_lines_two_fiber_families(self):
        """Test that two fiber families write without extra blank lines between them."""
        mat = Mat295()
        mat.mid = 2
        mat.nf = 2
        mat.atype = -1

        # First fiber family
        families = mat.fiber_families
        families[0].theta = 30.0
        families[0].a = 1.0
        families[0].b = 2.0
        families[0].ftype = 1
        families[0].k1 = 0.00049
        families[0].k2 = 9.01

        # Second fiber family
        families[1].theta = 45.0
        families[1].a = 0.5
        families[1].b = 1.5
        families[1].ftype = 1
        families[1].k1 = 0.0003
        families[1].k2 = 8.5

        output = mat.write()
        lines = output.split("\n")

        # Find the ANISO section
        aniso_idx = None
        for i, line in enumerate(lines):
            if "ANISO" in line:
                aniso_idx = i
                break

        assert aniso_idx is not None, "ANISO line not found in output"

        # Check for consecutive blank lines in fiber family section
        # The fiber families should be written back-to-back without extra blank lines
        fiber_section = lines[aniso_idx : aniso_idx + 12]
        for i in range(len(fiber_section) - 1):
            if fiber_section[i].strip() == "" and fiber_section[i + 1].strip() == "":
                pytest.fail(
                    f"Found consecutive blank lines at index {aniso_idx + i}: "
                    f"'{fiber_section[i]}' and '{fiber_section[i + 1]}'"
                )

        # Additionally verify both fiber families are present in sequence
        # Family 0 data line (theta=30.0)
        family0_found = any("30.0" in line for line in fiber_section)
        # Family 1 data line (theta=45.0)
        family1_found = any("45.0" in line for line in fiber_section)

        assert family0_found, "Fiber family 0 data not found in output"
        assert family1_found, "Fiber family 1 data not found in output"

    def test_no_extra_blank_lines_file_write(self, tmp_path):
        """Test that writing to a file produces no extra blank lines.

        This specifically tests the Windows text mode issue where newline
        handling could cause double carriage returns (\\r\\r\\n).
        """
        mat = Mat295()
        mat.mid = 3
        mat.nf = 2
        mat.atype = -1

        families = mat.fiber_families
        families[0].theta = 30.0
        families[0].a = 1.0
        families[0].b = 2.0
        families[0].ftype = 1
        families[0].k1 = 0.00049
        families[0].k2 = 9.01

        families[1].theta = 45.0
        families[1].a = 0.5
        families[1].b = 1.5
        families[1].ftype = 1
        families[1].k1 = 0.0003
        families[1].k2 = 8.5

        # Write to a file
        filepath = tmp_path / "mat295_test.kwd"
        with open(filepath, "w") as f:
            mat.write(buf=f)

        # Read the file in binary mode to check for double carriage returns
        with open(filepath, "rb") as f:
            content = f.read()

        # Check for \r\r\n (double carriage return + line feed) which indicates
        # the bug where inactive cards caused extra blank lines
        assert b"\r\r\n" not in content, (
            "Found double carriage return (\\r\\r\\n) in file output. "
            "This indicates extra blank lines between fiber families."
        )

        # Also verify the file can be read back correctly
        deck = Deck()
        deck.import_file(str(filepath))
        assert len(deck.keywords) == 1

        mat2 = deck.keywords[0]
        assert mat2.nf == 2
        assert len(mat2.fiber_families) == 2
