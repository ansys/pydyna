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

"""
Keywords Backend
================

Main KeywordsBackend class composed from domain-specific mixins.
"""

import logging

from ansys.dyna.core.pre.keywords_backend.base import KeywordsBackendBase
from ansys.dyna.core.pre.keywords_backend.boundary import BoundaryKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.control import ControlKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.database import DatabaseKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.em import EMKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.icfd import ICFDKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.initial import InitialKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.material import MaterialKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.misc import MiscKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.section import SectionKeywordsMixin
from ansys.dyna.core.pre.keywords_backend.set import SetKeywordsMixin

logger = logging.getLogger(__name__)


class KeywordsBackend(
    KeywordsBackendBase,
    ControlKeywordsMixin,
    DatabaseKeywordsMixin,
    MaterialKeywordsMixin,
    SectionKeywordsMixin,
    SetKeywordsMixin,
    InitialKeywordsMixin,
    BoundaryKeywordsMixin,
    EMKeywordsMixin,
    ICFDKeywordsMixin,
    MiscKeywordsMixin,
):
    """Keywords-based backend for the pre module.

    This class provides a local implementation of the pre module functionality
    using the keywords Deck class instead of gRPC calls.

    The implementation is composed from domain-specific mixins:

    - ControlKeywordsMixin: CONTROL_* keywords
    - DatabaseKeywordsMixin: DATABASE_* keywords
    - MaterialKeywordsMixin: MAT_* keywords
    - SectionKeywordsMixin: SECTION_* keywords
    - SetKeywordsMixin: SET_* keywords
    - InitialKeywordsMixin: INITIAL_* keywords
    - BoundaryKeywordsMixin: BOUNDARY_* keywords
    - EMKeywordsMixin: EM_* keywords
    - ICFDKeywordsMixin: ICFD_* and MESH_* keywords
    - MiscKeywordsMixin: DEFINE_CURVE, PART, CONTACT, etc.
    """

    def __init__(self):
        """Initialize the keywords backend."""
        super().__init__()
        logger.debug("Initialized KeywordsBackend with all mixins")
