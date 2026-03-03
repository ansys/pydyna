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

"""Mixin providing validation for SECTION_TSHELL keywords."""

import logging
import typing
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ansys.dyna.core.keywords.keyword_classes.auto.section.section_tshell import SectionTshellCardSet

logger = logging.getLogger(__name__)


class SectionTShellValidationMixin:
    """Mixin that validates SECTION_TSHELL card set items.

    For each section defined in the card set, enforces that when ``icomp == 1``
    (layered composite mode), ``nip`` must be a positive integer since each
    through-thickness integration point maps to one composite layer.

    Attributes expected from the host class:
        sets: list of SectionTshellCardSet items, each having ``icomp`` and ``nip``.
    """

    if TYPE_CHECKING:
        @property
        def sets(self) -> typing.List["SectionTshellCardSet"]:
            ...

    def _is_valid(self) -> typing.Tuple[bool, str]:
        """Validate all card set items.

        Returns
        -------
        tuple[bool, str]
            ``(True, "")`` if all sections are valid, otherwise
            ``(False, <message>)`` describing the first violation found.
        """
        for i, section in enumerate(self.sets):
            if section.icomp == 1 and not (section.nip is not None and section.nip > 0):
                return (
                    False,
                    f"sets[{i}]: nip must be a positive integer when icomp is 1",
                )
        return True, ""
