# Copyright (C) 2023 - 2026 Synopsys, Inc. and ANSYS, Inc. All rights reserved.
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

"""Base Transform class."""

import warnings

from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.lib.import_handler import ImportContext


class Transform:
    """Transform base class."""

    def __init__(self, xform: kwd.IncludeTransform, context: ImportContext):
        """Initialize a transform with the include transform and its import context."""
        self._xform: kwd.IncludeTransform = xform
        self._context: ImportContext = context

    def transform(self, keyword) -> None:
        """Transform the given keyword."""
        raise Exception("Implementation required for transform")

    def _warn_not_transformed(self, keyword) -> None:
        """Warn that ``keyword`` was left unchanged, including the include file and line."""
        name = f"{keyword.keyword}_{keyword.subkeyword}"
        warnings.warn(f"keyword {name} not transformed!{self._context.format_location()}", stacklevel=2)
