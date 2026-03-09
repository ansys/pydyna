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
    line_number : int, optional
        The 1-based line number in ``path`` where the current keyword block
        starts.  Set by the deck loader as it reads through the file so that
        error messages can include precise location information.
    """

    xform: typing.Any = None
    deck: typing.Any = None
    path: str = None
    keyword_overrides: typing.Dict[str, type] = dataclasses.field(default_factory=dict)
    strict: bool = False
    line_number: int = None

    def format_location(self) -> str:
        """Format location info (path and line number) as a string for messages.

        Returns a string like " in 'path/to/file.k' line 42" or "" if no
        location info is available.
        """
        loc = ""
        if self.path is not None:
            loc = f" in '{self.path}'"
        if self.line_number is not None:
            loc += f" line {self.line_number}"
        return loc


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

    def on_error(
        self,
        error: BaseException,
        context: typing.Optional["ImportContext"] = None,
        result: typing.Any = None,
    ) -> None:
        """Called when an error occurs during keyword import.

        Handlers can override this to handle or log errors as needed.
        The default implementation does nothing.

        Parameters
        ----------
        error : BaseException
            The exception that was raised.
        context : ImportContext, optional
            The import context at the time of the error, which may include
            ``path`` and ``line_number`` for location information.
        result : DeckLoaderResult, optional
            If provided, handlers may add warning messages via
            ``result.add_warning(message)`` for programmatic inspection.
        """
        pass
