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
"""Import handler that links `DefineCurve` keywords to their parent `DefineTable`."""

from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.manual.define_table import DefineTable
from ansys.dyna.core.lib.import_handler import ImportHandler


class DefineTableProcessor(ImportHandler):
    """Import handler that links `DefineCurve` keywords to their parent `DefineTable`.

    Behavior:
    - When a `DefineCurve` is imported immediately after a `DefineTable`,
      it is appended to the table's _linked_curves list.
    - The linking is based on deck import order, not ID matching, which allows
      users to customize the behavior or use pydyna to validate/fix keyword files.
    """

    def __init__(self):
        self._current_table = None

    def after_import(self, context, keyword):
        """Track tables and link curves to the most recent table."""
        if isinstance(keyword, DefineTable):
            self._current_table = keyword
            return

        if isinstance(keyword, DefineCurve):
            if self._current_table is not None:
                self._current_table._linked_curves.append(keyword)
            return

        # For any other keyword, clear current table
        self._current_table = None
