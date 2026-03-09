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

"""Custom PARAMETER_EXPRESSION keywords with raw parsing.

Expression keywords contain &param references as expression operands,
not parameter substitutions. These classes override _read_data to use
raw parsing from lib/parameter_keyword_helpers.py.
"""

import typing
import warnings

from ansys.dyna.core.keywords.keyword_classes.auto.parameter.parameter_expression import (
    ParameterExpression as BaseParameterExpression,
)
from ansys.dyna.core.keywords.keyword_classes.auto.parameter.parameter_expression_local import (
    ParameterExpressionLocal as BaseParameterExpressionLocal,
)
from ansys.dyna.core.keywords.keyword_classes.auto.parameter.parameter_expression_noecho import (
    ParameterExpressionNoecho as BaseParameterExpressionNoecho,
)
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.parameter_keyword_helpers import (
    _read_parameter_expression_card,
    _read_parameter_expression_table,
)

if typing.TYPE_CHECKING:
    from ansys.dyna.core.lib.import_handler import ImportContext
    from ansys.dyna.core.lib.parameters import ParameterSet


class ParameterExpression(BaseParameterExpression):
    """PARAMETER_EXPRESSION with raw parsing that preserves expression text."""

    def _read_data(
        self,
        buf: typing.TextIO,
        parameters: "ParameterSet",
        import_context: typing.Optional["ImportContext"] = None,
    ) -> None:
        """Load table data without parameter substitution."""
        card = self._cards[0]
        warn_list = _read_parameter_expression_table(buf, card)
        for msg in warn_list:
            enriched = Cards._enrich_warning_with_context(msg, import_context)
            warnings.warn(enriched)
        self._try_read_options_with_no_title(buf, parameters)


class ParameterExpressionLocal(BaseParameterExpressionLocal):
    """PARAMETER_EXPRESSION_LOCAL with raw parsing that preserves expression text."""

    def _read_data(
        self,
        buf: typing.TextIO,
        parameters: "ParameterSet",
        import_context: typing.Optional["ImportContext"] = None,
    ) -> None:
        """Load table data without parameter substitution."""
        card = self._cards[0]
        warn_list = _read_parameter_expression_table(buf, card)
        for msg in warn_list:
            enriched = Cards._enrich_warning_with_context(msg, import_context)
            warnings.warn(enriched)
        self._try_read_options_with_no_title(buf, parameters)


class ParameterExpressionNoecho(BaseParameterExpressionNoecho):
    """PARAMETER_EXPRESSION_NOECHO with raw parsing that preserves expression text."""

    def _read_data(
        self,
        buf: typing.TextIO,
        parameters: "ParameterSet",
        import_context: typing.Optional["ImportContext"] = None,
    ) -> None:
        """Load card data without parameter substitution."""
        card = self._cards[0]
        warn_list = _read_parameter_expression_card(buf, card)
        for msg in warn_list:
            enriched = Cards._enrich_warning_with_context(msg, import_context)
            warnings.warn(enriched)
        self._try_read_options_with_no_title(buf, parameters)
