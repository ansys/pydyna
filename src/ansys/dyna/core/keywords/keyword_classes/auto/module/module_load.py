# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

"""Module providing the ModuleLoad class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MODULELOAD_CARD0 = (
    FieldSchema("mdlid", int, 0, 20, None),
    FieldSchema("title", str, 20, 60, None),
)

_MODULELOAD_CARD1 = (
    FieldSchema("filename", str, 0, 80, None),
)

class ModuleLoad(KeywordBase):
    """DYNA MODULE_LOAD keyword"""

    keyword = "MODULE"
    subkeyword = "LOAD"

    def __init__(self, **kwargs):
        """Initialize the ModuleLoad class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MODULELOAD_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MODULELOAD_CARD1,
                **kwargs,
            ),        ]
    @property
    def mdlid(self) -> typing.Optional[int]:
        """Get or set the Module identification. A unique string label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mdlid")

    @mdlid.setter
    def mdlid(self, value: int) -> None:
        """Set the mdlid property."""
        self._cards[0].set_value("mdlid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Description of the module.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[0].set_value("title", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name of the library to be loaded, 80 characters maximum. If the file name has no path component, LS-DYNA will search in all directories specified in *MODULE_PATH first. If not found and the file name starts with �+� (a plus sign), LS-DYNA will continue to search all directories specified in the system environment variable LD_LIBRARY_PATH..
        """ # nopep8
        return self._cards[1].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[1].set_value("filename", value)

