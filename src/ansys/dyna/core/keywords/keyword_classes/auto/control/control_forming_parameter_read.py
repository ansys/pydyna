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

"""Module providing the ControlFormingParameterRead class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLFORMINGPARAMETERREAD_CARD0 = (
    FieldSchema("filename", str, 0, 80, None),
)

_CONTROLFORMINGPARAMETERREAD_CARD1 = (
    FieldSchema("paraname", str, 0, 10, None),
    FieldSchema("imethod", int, 10, 10, None),
    FieldSchema("line", int, 20, 10, None),
    FieldSchema("nbegpa", int, 30, 10, None),
    FieldSchema("nendpa_", int, 40, 10, None, "nendpa "),
    FieldSchema("value_", float, 50, 10, None, "value "),
)

class ControlFormingParameterRead(KeywordBase):
    """DYNA CONTROL_FORMING_PARAMETER_READ keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_PARAMETER_READ"

    def __init__(self, **kwargs):
        """Initialize the ControlFormingParameterRead class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGPARAMETERREAD_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGPARAMETERREAD_CARD1,
                **kwargs,
            ),        ]
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the file name will be opened to read
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def paraname(self) -> typing.Optional[str]:
        """Get or set the The name of the parameters in the parameter list
        """ # nopep8
        return self._cards[1].get_value("paraname")

    @paraname.setter
    def paraname(self, value: str) -> None:
        """Set the paraname property."""
        self._cards[1].set_value("paraname", value)

    @property
    def imethod(self) -> typing.Optional[int]:
        """Get or set the which method to be used
        """ # nopep8
        return self._cards[1].get_value("imethod")

    @imethod.setter
    def imethod(self, value: int) -> None:
        """Set the imethod property."""
        self._cards[1].set_value("imethod", value)

    @property
    def line(self) -> typing.Optional[int]:
        """Get or set the the line # in filename
        """ # nopep8
        return self._cards[1].get_value("line")

    @line.setter
    def line(self, value: int) -> None:
        """Set the line property."""
        self._cards[1].set_value("line", value)

    @property
    def nbegpa(self) -> typing.Optional[int]:
        """Get or set the the value defined between nbegpa and nendpa will be read for paraname
        """ # nopep8
        return self._cards[1].get_value("nbegpa")

    @nbegpa.setter
    def nbegpa(self, value: int) -> None:
        """Set the nbegpa property."""
        self._cards[1].set_value("nbegpa", value)

    @property
    def nendpa_(self) -> typing.Optional[int]:
        """Get or set the the value defined between nbegpa and nendpa will be read for paraname
        """ # nopep8
        return self._cards[1].get_value("nendpa_")

    @nendpa_.setter
    def nendpa_(self, value: int) -> None:
        """Set the nendpa_ property."""
        self._cards[1].set_value("nendpa_", value)

    @property
    def value_(self) -> typing.Optional[float]:
        """Get or set the the value of the defined parameter
        """ # nopep8
        return self._cards[1].get_value("value_")

    @value_.setter
    def value_(self, value: float) -> None:
        """Set the value_ property."""
        self._cards[1].set_value("value_", value)

