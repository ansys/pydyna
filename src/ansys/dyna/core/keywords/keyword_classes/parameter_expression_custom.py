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

"""Custom ParameterExpression classes that prevent premature parameter substitution.

Expression strings contain &param references that are expression operands, not
parameter substitutions. These custom classes override loads() to pass parameters=None,
preventing substitution during keyword parsing. Expressions are evaluated later by
ParameterHandler.after_import().

Import Strategy:
- Imported in manual_keywords.py (not auto/parameter/__init__.py)
- Import order: auto_keywords.py → manual_keywords.py (custom classes win)
- Never modify auto-generated files (codegen overwrites them)
"""

import typing

from ansys.dyna.core.keywords.keyword_classes.auto.parameter.parameter_expression import (
    ParameterExpression as BaseParameterExpression,
)
from ansys.dyna.core.keywords.keyword_classes.auto.parameter.parameter_expression_local import (
    ParameterExpressionLocal as BaseParameterExpressionLocal,
)
from ansys.dyna.core.keywords.keyword_classes.auto.parameter.parameter_expression_noecho import (
    ParameterExpressionNoecho as BaseParameterExpressionNoecho,
)
from ansys.dyna.core.lib.parameters import ParameterSet


class ParameterExpression(BaseParameterExpression):
    """PARAMETER_EXPRESSION with custom loads() that preserves expression text.

    Prevents parameter substitution during loading. Expressions evaluated later.
    """

    def loads(self, data: str, parameters: typing.Optional[ParameterSet] = None) -> None:
        """Load keyword data without parameter substitution."""
        # Call parent loads() but with parameters=None to disable substitution
        super().loads(data, parameters=None)


class ParameterExpressionLocal(BaseParameterExpressionLocal):
    """PARAMETER_EXPRESSION_LOCAL keyword with custom loading that preserves expression text.

    Overrides loads() to prevent parameter substitution in expression strings.
    """

    def loads(self, data: str, parameters: typing.Optional[ParameterSet] = None) -> None:
        """Load keyword data without parameter substitution."""
        super().loads(data, parameters=None)


class ParameterExpressionNoecho(BaseParameterExpressionNoecho):
    """PARAMETER_EXPRESSION_NOECHO keyword with custom loading that preserves expression text.

    Overrides loads() to prevent parameter substitution in expression strings.
    """

    def loads(self, data: str, parameters: typing.Optional[ParameterSet] = None) -> None:
        """Load keyword data without parameter substitution."""
        super().loads(data, parameters=None)
