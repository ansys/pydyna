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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ParameterExpressionNoecho(KeywordBase):
    """DYNA PARAMETER_EXPRESSION_NOECHO keyword"""

    keyword = "PARAMETER"
    subkeyword = "EXPRESSION_NOECHO"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "prmr",
                        str,
                        0,
                        10,
                        kwargs.get("prmr")
                    ),
                    Field(
                        "expression",
                        str,
                        10,
                        70,
                        kwargs.get("expression")
                    ),
                ],
            ),
        ]

    @property
    def prmr(self) -> typing.Optional[str]:
        """Get or set the Define the nth parameter in a field of 10.  Within this field the first character must be either an R for a real number or an I for an integer.  Lower or upper case for I or R is okay.  Following the type designation, define the name of the parameter using up to, but not exceeding seven characters.  For example, when defining a shell thickness named, SHLTHK, both inputs RSHLTHK or R   SHLTHK can be used and placed anywhere in the field of 10
        """ # nopep8
        return self._cards[0].get_value("prmr")

    @prmr.setter
    def prmr(self, value: str) -> None:
        self._cards[0].set_value("prmr", value)

    @property
    def expression(self) -> typing.Optional[str]:
        """Get or set the General expression which is evaluated, having the result stored in PRMRn.  The following functions are available: sin, cos, tan, csc, sec, ctn, asin, acos, atan, atan2, sinh, cosh, tanh, asinh, acosh, atanh, min, max, sqrt, mod, abs, sign, LS_INTEGER, aint, nint, anint, LS_REAL, exp, log, log10, LS_REAL, and general arithmetic expressions involving +, -, *, /, and **.  The standard rules regarding operator precedence are obeyed, and nested parentheses are allowed. The expression can reference previously defined parameters (with or without the leading &).  The expression can be continued on multiple lines simply by leaving the first 10 characters of the continuation line blank.
        """ # nopep8
        return self._cards[0].get_value("expression")

    @expression.setter
    def expression(self, value: str) -> None:
        self._cards[0].set_value("expression", value)

