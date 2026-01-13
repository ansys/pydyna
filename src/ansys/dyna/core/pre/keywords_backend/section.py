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

"""Section keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class SectionKeywordsMixin:
    """Mixin class providing section keyword creation methods."""

    def create_section_shell(
        self,
        secid: int,
        elform: int = 2,
        shrf: float = 1.0,
        nip: int = 2,
        propt: float = 1.0,
        t1: float = 0.0,
        t2: float = 0.0,
        t3: float = 0.0,
        t4: float = 0.0,
    ) -> bool:
        """Create a SECTION_SHELL keyword.

        Parameters
        ----------
        secid : int
            Section ID.
        elform : int
            Element formulation.
        shrf : float
            Shear correction factor.
        nip : int
            Number of integration points.
        propt : float
            Printout option.
        t1, t2, t3, t4 : float
            Shell thickness at nodes.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SectionShell()
        kw.secid = secid
        kw.elform = elform
        kw.shrf = shrf
        kw.nip = nip
        # propt=0 means "use default" in the stub API, but SectionShell
        # only accepts {1, 2, 3, None}. Convert 0 to None (which also uses default).
        kw.propt = propt if propt in [1, 2, 3] else None
        kw.t1 = t1
        kw.t2 = t2
        kw.t3 = t3
        kw.t4 = t4

        self._deck.append(kw)
        logger.debug(f"Created SECTION_SHELL with secid={secid}")
        return True

    def create_section_solid(
        self,
        secid: int,
        elform: int = 1,
    ) -> bool:
        """Create a SECTION_SOLID keyword.

        Parameters
        ----------
        secid : int
            Section ID.
        elform : int
            Element formulation.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.SectionSolid()
        kw.secid = secid
        kw.elform = elform

        self._deck.append(kw)
        logger.debug(f"Created SECTION_SOLID with secid={secid}")
        return True
