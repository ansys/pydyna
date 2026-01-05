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

"""Import handler used by the import deck feature"""

import dataclasses
import typing
import warnings

if typing.TYPE_CHECKING:
    from ansys.dyna.core.lib.keyword_base import KeywordBase


@dataclasses.dataclass
class ImportContext:
    """Optional transformation to apply, using type `IncludeTransform`"""

    xform: typing.Any = None

    """Deck into which the import is occurring."""
    deck: typing.Any = None

    """Path of file that is importing."""
    path: str = None


class ImportHandler:
    """Base class for import handlers."""

    def before_import(self, context: ImportContext, keyword: str, buffer: typing.TextIO):
        """Event called before reading a keyword.

        `keyword` is the string label of the keyword
        `buffer` is a copy of the buffer to read from.

        Usage:
        Return True if the keyword is to be imported as usual.
        Return False if the keyword is not to be imported.
        """
        return True

    def after_import(self, context: ImportContext, keyword: typing.Union[str, "KeywordBase"]):
        """Event called after a keyword is imported.

        `keyword` is the imported keyword. It could be a string or a keyword object

        Depending on the `context` is a
        """
        pass

    def on_error(self, error):
        # TODO - use logging
        warnings.warn(f"error in importhandler {self}: {error}")
