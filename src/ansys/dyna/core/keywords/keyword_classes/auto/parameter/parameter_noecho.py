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

"""Module providing the ParameterNoecho class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_PARAMETERNOECHO_CARD0 = (
    FieldSchema("prmr1", str, 0, 10, None),
    FieldSchema("val1", str, 10, 10, None),
    FieldSchema("prmr2", str, 20, 10, None),
    FieldSchema("val2", str, 30, 10, None),
    FieldSchema("prmr3", str, 40, 10, None),
    FieldSchema("val3", str, 50, 10, None),
    FieldSchema("prmr4", str, 60, 10, None),
    FieldSchema("val4", str, 70, 10, None),
)

class ParameterNoecho(KeywordBase):
    """DYNA PARAMETER_NOECHO keyword"""

    keyword = "PARAMETER"
    subkeyword = "NOECHO"

    def __init__(self, **kwargs):
        """Initialize the ParameterNoecho class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _PARAMETERNOECHO_CARD0,
                **kwargs,
            ),        ]
    @property
    def prmr1(self) -> typing.Optional[str]:
        """Get or set the Define the nth parameter in a field of 10, Within this field the first character must be either an 'R' for a real number or an 'I' for an integer. Lower or upper case for 'I' or 'R' is okay. Following the type designation, define the name of the parameter using up to, but not exceeding 7 characters. For example, when defining a shell thickness named, 'SHLTHK', both inputs 'RSHLTHK' or 'R  SHLTHK' can be used and placed anywhere in the field of 10. When referencing SHLTHK in the input field, place a '&' at the first column of its field followed by the name of the parameter without blanks
        """ # nopep8
        return self._cards[0].get_value("prmr1")

    @prmr1.setter
    def prmr1(self, value: str) -> None:
        """Set the prmr1 property."""
        self._cards[0].set_value("prmr1", value)

    @property
    def val1(self) -> typing.Optional[str]:
        """Get or set the Define the numerical value of the n parameter as either a real or integer number consistent with proceeding definition for PRMRn
        """ # nopep8
        return self._cards[0].get_value("val1")

    @val1.setter
    def val1(self, value: str) -> None:
        """Set the val1 property."""
        self._cards[0].set_value("val1", value)

    @property
    def prmr2(self) -> typing.Optional[str]:
        """Get or set the Define the 2nd parameter
        """ # nopep8
        return self._cards[0].get_value("prmr2")

    @prmr2.setter
    def prmr2(self, value: str) -> None:
        """Set the prmr2 property."""
        self._cards[0].set_value("prmr2", value)

    @property
    def val2(self) -> typing.Optional[str]:
        """Get or set the Define the 2nd numerical value
        """ # nopep8
        return self._cards[0].get_value("val2")

    @val2.setter
    def val2(self, value: str) -> None:
        """Set the val2 property."""
        self._cards[0].set_value("val2", value)

    @property
    def prmr3(self) -> typing.Optional[str]:
        """Get or set the Define the 3rd parameter
        """ # nopep8
        return self._cards[0].get_value("prmr3")

    @prmr3.setter
    def prmr3(self, value: str) -> None:
        """Set the prmr3 property."""
        self._cards[0].set_value("prmr3", value)

    @property
    def val3(self) -> typing.Optional[str]:
        """Get or set the Define the 3rd numerical value
        """ # nopep8
        return self._cards[0].get_value("val3")

    @val3.setter
    def val3(self, value: str) -> None:
        """Set the val3 property."""
        self._cards[0].set_value("val3", value)

    @property
    def prmr4(self) -> typing.Optional[str]:
        """Get or set the Define the 4th parameter
        """ # nopep8
        return self._cards[0].get_value("prmr4")

    @prmr4.setter
    def prmr4(self, value: str) -> None:
        """Set the prmr4 property."""
        self._cards[0].set_value("prmr4", value)

    @property
    def val4(self) -> typing.Optional[str]:
        """Get or set the Define the 4th numerical value
        """ # nopep8
        return self._cards[0].get_value("val4")

    @val4.setter
    def val4(self, value: str) -> None:
        """Set the val4 property."""
        self._cards[0].set_value("val4", value)

