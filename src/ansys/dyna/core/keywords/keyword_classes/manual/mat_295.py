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

"""Manual MAT_295 subclass with before_read support.

This subclass of the auto-generated Mat295 adds a `before_read` method to handle
the self-referential conditional pattern where card 6 (ANISO) has an active_func
that depends on `atype`, but `atype` is a field ON that card.

The solution is to peek ahead in the buffer before reading to detect "ANISO" and
"ACTIVE" title lines and pre-set the corresponding fields so the conditionals
evaluate correctly during the normal read process.

The auto-generated class handles everything else including:
- FiberFamily CardSet with discriminator support for ftype-dependent cards
- All field properties and card definitions
- Option card handling
"""

import typing

from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_295 import Mat295 as Mat295Auto
from ansys.dyna.core.lib.kwd_line_formatter import read_line


class Mat295(Mat295Auto):
    """MAT_295 keyword with before_read support for self-referential conditionals.

    This subclass adds peek-ahead logic to detect ANISO and ACTIVE title cards
    before reading, allowing the conditional cards to be properly activated.
    """

    def before_read(self, buf: typing.TextIO) -> None:
        """Peek into the buffer before reading to activate conditional cards.

        The ANISO and ACTIVE cards need to be activated before reading because
        their conditional functions depend on atype and actype values which
        are read from those very cards. This method peeks ahead to find these
        title lines and pre-sets the fields.
        """
        pos = buf.tell()
        while True:
            line, end = read_line(buf)
            if end:
                break
            if line.startswith("ANISO"):
                self._cards[6].set_value("atype", 1)
            elif line.startswith("ACTIVE"):
                self._cards[9].set_value("actype", 1)
        buf.seek(pos)


class MatAnisotropicHyperelastic(Mat295):
    """Alias for MAT_ANISOTROPIC_HYPERELASTIC keyword."""

    subkeyword = "ANISOTROPIC_HYPERELASTIC"
