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

"""Manual MAT_295 subclass that extends the auto-generated version.

This module provides a minimal manual subclass of the auto-generated Mat295 class.
The auto-generated class now includes:
- FiberFamily CardSet with discriminator support for ftype field
- Auto-generated _read_data method with peek-ahead for ftype (via lib function)
- Unified ftype property that sets on both cards 1 and 2
- Internal conditionals for cards 1 (ftype==1) and 2 (ftype==2)
- fiber_families property (via items-name manifest setting)
- No add_set method (bounded=true in manifest)

This subclass only adds:
- before_read method for detecting ANISO/ACTIVE option lines

COMPARISON WITH FULL MANUAL IMPLEMENTATION:
-------------------------------------------
The full manual implementation in mat_295.py contains ~2000+ lines of manually
maintained code. This subclass approach reduces that to ~50 lines by leveraging
the auto-generated code with discriminator support.

See mat_295.py for the original full manual implementation which serves as the
reference implementation for testing.
"""

import typing

from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_295 import Mat295 as _AutoMat295
from ansys.dyna.core.lib.kwd_line_formatter import read_line


class Mat295(_AutoMat295):
    """MAT_295 keyword with fiber_families accessor and before_read hook.

    This subclass extends the auto-generated Mat295 to add:
    1. fiber_families property - returns List[FiberFamily] bounded by nf
    2. before_read method - detects ANISO/ACTIVE option lines before parsing

    The auto-generated base class handles:
    - All card definitions including FiberFamily CardSet
    - Discriminator-based _read_data for ftype field
    - Unified ftype property across cards 1 and 2
    - Internal conditionals for ftype==1 and ftype==2 cards
    """

    def before_read(self, buf: typing.TextIO) -> None:
        """Peek into the buffer before reading to activate conditional cards.

        The ANISO and ACTIVE cards need to be activated before reading because
        their conditional functions depend on atype and actype values which
        are read from those very cards.
        """
        pos = buf.tell()
        while True:
            line, end = read_line(buf)
            if end:
                break
            if line.startswith("ANISO"):
                self.atype = 1
            elif line.startswith("ACTIVE"):
                self.actype = 1
        buf.seek(pos)

    # NOTE: The fiber_families property is now auto-generated in the base class
    # via the items-name="fiber_families" manifest setting, so no override needed.


class MatAnisotropicHyperelastic(Mat295):
    """Alias for MAT_ANISOTROPIC_HYPERELASTIC keyword."""

    subkeyword = "ANISOTROPIC_HYPERELASTIC"
