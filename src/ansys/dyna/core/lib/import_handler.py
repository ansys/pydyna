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
    """Context for keyword import operations.

    Attributes
    ----------
    xform : Any, optional
        Optional transformation to apply, using type `IncludeTransform`.
    deck : Any, optional
        Deck into which the import is occurring.
    path : str, optional
        Path of file that is importing.
    keyword_overrides : Dict[str, type], optional
        Dictionary mapping keyword names (e.g., "*MAT_295") to keyword classes.
        When a keyword is imported, if its name matches a key in this dictionary,
        the corresponding class will be used instead of the default from TypeMapping.

        This allows using alternative or legacy keyword implementations when loading
        keyword files.

        Example
        -------
        >>> from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import (
        ...     Mat295Legacy,
        ... )
        >>> context = ImportContext(keyword_overrides={"*MAT_295": Mat295Legacy})
        >>> deck.loads(data, context=context)
    strict : bool, optional
        If True, raise errors when keyword parsing fails for any reason
        (undefined parameters, invalid field values, malformed data, etc.).
        If False (default), keywords that fail to parse are retained as raw
        strings and a warning is emitted. Default is False for backward
        compatibility.
        TODO: Consider making strict=True the default in a future version.
    """

    xform: typing.Any = None
    deck: typing.Any = None
    path: str = None
    keyword_overrides: typing.Dict[str, type] = dataclasses.field(default_factory=dict)
    strict: bool = False


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
        """Handle errors during import."""
        # TODO - use logging
        warnings.warn(f"error in importhandler {self}: {error}")
