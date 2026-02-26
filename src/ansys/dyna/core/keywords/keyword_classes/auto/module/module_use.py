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

"""Module providing the ModuleUse class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MODULEUSE_CARD0 = (
    FieldSchema("mdlid", int, 0, 20, None),
)

_MODULEUSE_CARD1 = (
    FieldSchema("type", str, 0, 20, None),
    FieldSchema("param1", str, 20, 20, None),
    FieldSchema("param2", str, 40, 20, None),
)

class ModuleUse(KeywordBase):
    """DYNA MODULE_USE keyword"""

    keyword = "MODULE"
    subkeyword = "USE"

    def __init__(self, **kwargs):
        """Initialize the ModuleUse class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MODULEUSE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MODULEUSE_CARD1,
                **kwargs,
            ),        ]
    @property
    def mdlid(self) -> typing.Optional[int]:
        """Get or set the Module identification defined in *MODULE_LOAD.
        """ # nopep8
        return self._cards[0].get_value("mdlid")

    @mdlid.setter
    def mdlid(self, value: int) -> None:
        """Set the mdlid property."""
        self._cards[0].set_value("mdlid", value)

    @property
    def type(self) -> typing.Optional[str]:
        """Get or set the Rule type.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: str) -> None:
        """Set the type property."""
        self._cards[1].set_value("type", value)

    @property
    def param1(self) -> typing.Optional[str]:
        """Get or set the Type dependent parameter.
        """ # nopep8
        return self._cards[1].get_value("param1")

    @param1.setter
    def param1(self, value: str) -> None:
        """Set the param1 property."""
        self._cards[1].set_value("param1", value)

    @property
    def param2(self) -> typing.Optional[str]:
        """Get or set the Type dependent parameter.
        """ # nopep8
        return self._cards[1].get_value("param2")

    @param2.setter
    def param2(self, value: str) -> None:
        """Set the param2 property."""
        self._cards[1].set_value("param2", value)

