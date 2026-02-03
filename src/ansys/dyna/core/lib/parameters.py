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

"""Parameter classes."""

import contextlib
import logging
import typing
import warnings

from ansys.dyna.core.lib.import_handler import ImportContext, ImportHandler

if typing.TYPE_CHECKING:
    from ansys.dyna.core.lib.keyword_base import KeywordBase

logger = logging.getLogger(__name__)


def _evaluate_expressions_if_needed():
    """Lazy import to avoid circular dependency."""
    from ansys.dyna.core.lib.expression_evaluator import evaluate_parameter_expressions

    return evaluate_parameter_expressions


class ParameterSet:
    """Hierarchical parameter storage for LS-DYNA deck parameters.

    Child scopes see parent parameters but local parameters don't leak upward.
    """

    def __init__(self, parent: typing.Optional["ParameterSet"] = None):
        """Initialize a ParameterSet with optional parent scope.

        Parameters
        ----------
        parent : ParameterSet, optional
            Parent scope for parameter lookup. If None, this is a root scope.
        """
        self._params = dict()  # Global parameters in this scope
        self._local_params = dict()  # Local-only parameters in this scope
        self._parent = parent  # Parent scope for lookup
        self._uri_stack: typing.List[str] = []  # Stack for building current URI path
        self._refs: typing.Dict[str, str] = {}  # URI -> parameter reference string (e.g., "&myvar")
        logger.debug(f"Created ParameterSet with parent={parent is not None}")

    def get(self, param: str) -> typing.Any:
        """Get a parameter by name, checking local then parent scopes.

        Parameters
        ----------
        param : str
            Parameter name to lookup.

        Returns
        -------
        Any
            Parameter value.

        Raises
        ------
        KeyError
            If parameter is not found in this scope or any parent scope.
        """
        # Check local scope first (both global and local params)
        if param in self._params:
            logger.debug(f"Found parameter '{param}' in global params: {self._params[param]}")
            return self._params[param]
        
        if param in self._local_params:
            logger.debug(f"Found parameter '{param}' in local params: {self._local_params[param]}")
            return self._local_params[param]

        # Check parent scope
        if self._parent is not None:
            logger.debug(f"Parameter '{param}' not in local scope, checking parent")
            return self._parent.get(param)

        # Not found anywhere
        logger.debug(f"Parameter '{param}' not found in any scope")
        raise KeyError(param)

    def add(self, param: str, value: typing.Any) -> None:
        """Add a parameter to the local scope.

        This method is for global parameters (PARAMETER keyword).
        They are added to the local scope but will be visible to child scopes.

        Parameters
        ----------
        param : str
            Parameter name.
        value : Any
            Parameter value.
        """
        logger.debug(f"Adding global parameter '{param}' = {value} to local scope")
        self._params[param] = value

    def add_local(self, param: str, value: typing.Any) -> None:
        """Add a parameter to local scope only (PARAMETER_LOCAL).

        Local parameters are only visible within the current scope and child scopes
        created from it, but won't leak to parent or sibling scopes.

        Parameters
        ----------
        param : str
            Parameter name.
        value : Any
            Parameter value.
        """
        logger.debug(f"Adding local parameter '{param}' = {value} to local scope")
        self._local_params[param] = value

    def copy_with_child_scope(self) -> "ParameterSet":
        """Create a new ParameterSet with this as the parent scope.

        The child scope will be able to see parameters from this scope,
        but parameters added to the child won't leak back to this scope.

        Returns
        -------
        ParameterSet
            New ParameterSet with this as parent.
        """
        logger.debug("Creating child scope")
        return ParameterSet(parent=self)

    @contextlib.contextmanager
    def scope(self, segment: str) -> typing.Generator[None, None, None]:
        """Context manager to scope a URI segment during card reading.

        As the read stack descends (keyword -> card -> field), each layer
        pushes its segment and pops on exit. Parameter references are
        recorded at the current full URI path.

        Parameters
        ----------
        segment : str
            URI segment to push (e.g., keyword id, card index, field name).

        Yields
        ------
        None

        Examples
        --------
        >>> with parameter_set.scope("12345"):  # keyword id
        ...     with parameter_set.scope("card0"):
        ...         parameter_set.record_ref("secid", "&mysec")
        ...         # Records at URI: "12345/card0/secid"
        """
        self._uri_stack.append(segment)
        logger.debug(f"Pushed URI segment '{segment}', stack: {self._uri_stack}")
        try:
            yield
        finally:
            popped = self._uri_stack.pop()
            logger.debug(f"Popped URI segment '{popped}', stack: {self._uri_stack}")

    def record_ref(self, field: str, ref: str) -> None:
        """Record a parameter reference at the current URI path.

        Called during card reading when a parameter reference (e.g., &myvar)
        is resolved. Stores the original reference string so it can be
        written back instead of the resolved value.

        Parameters
        ----------
        field : str
            Field name or index to append to current URI path.
        ref : str
            The original parameter reference string (e.g., "&myvar", "-&density").
        """
        uri = "/".join(self._uri_stack + [field])
        self._refs[uri] = ref
        logger.debug(f"Recorded parameter ref at URI '{uri}': {ref}")

    def get_ref(self, *path_segments: str) -> typing.Optional[str]:
        """Get the parameter reference string for a URI path.

        Called during card writing to check if a field value came from
        a parameter reference that should be written instead of the value.

        Parameters
        ----------
        *path_segments : str
            URI path segments to join (e.g., keyword_id, card_index, field_name).

        Returns
        -------
        str or None
            The original parameter reference string if one was recorded,
            or None if the value was a literal.
        """
        uri = "/".join(path_segments)
        ref = self._refs.get(uri)
        if ref:
            logger.debug(f"Found parameter ref at URI '{uri}': {ref}")
        return ref

    def get_current_uri(self) -> str:
        """Get the current URI path as a string.

        Returns
        -------
        str
            The current URI path joined with '/'.
        """
        return "/".join(self._uri_stack)

    def get_global_params(self) -> typing.Dict[str, typing.Any]:
        """Get only the global parameters defined in this scope.

        Returns only parameters added via add() (PARAMETER keyword),
        not parameters added via add_local() (PARAMETER_LOCAL keyword).

        Returns
        -------
        dict
            Dictionary of global parameters defined in this scope only.
        """
        return self._params.copy()


def _unpack_param(param: "kwd.Parameter.Parameter") -> typing.Union[type, str, typing.Any]:  # noqa: F821
    """Converts parameter into type, name, and value of the given type."""
    name_field = param.name
    type_code = name_field[0]
    if type_code == "R":
        t = float
    elif type_code == "I":
        t = int
    elif type_code == "C":
        t = str
    else:
        raise Exception(f"Parameter name {name_field} does not name a type")
    val = t(param.val.strip())
    name = name_field[1:].strip()
    return t, name, val


def _load_parameters(deck, parameter: "kwd.Parameter", local: bool = False):  # noqa: F821
    """Load parameters from a PARAMETER or PARAMETER_LOCAL keyword into the deck.

    Parameters
    ----------
    deck : Deck
        Deck to add parameters to.
    parameter : Parameter or ParameterLocal
        Keyword containing parameters.
    local : bool, optional
        If True, parameters are added as local (PARAMETER_LOCAL).
        If False, parameters are added as global (PARAMETER).
    """
    deck_params = deck.parameters
    for p in parameter.parameters:
        t, name, val = _unpack_param(p)
        if local:
            logger.debug(f"Loading local parameter: {name} = {val}")
            deck_params.add_local(name, val)
        else:
            logger.debug(f"Loading global parameter: {name} = {val}")
            deck_params.add(name, val)


def _load_single_parameter_expression(deck, parameter_expression, local: bool = False):
    """Load and evaluate a single PARAMETER_EXPRESSION_NOECHO parameter.

    Note: Codegen currently uses Card (not TableCard) for NOECHO variant, limiting it
    to single parameters. This should be fixed in codegen to match LS-DYNA behavior.

    Parameters
    ----------
    deck : Deck
        Target deck.
    parameter_expression : ParameterExpressionNoecho
        Keyword with single expression.
    local : bool, optional
        Add as local (True) or global (False) parameter.
    """
    prmr = parameter_expression.prmr
    expression_text = parameter_expression.expression

    if not prmr or len(prmr) < 2:
        logger.warning(f"Skipping invalid parameter name: '{prmr}'")
        return

    param_type = prmr[0]  # 'R' or 'I'
    param_name = prmr[1:].strip()

    if param_type not in ("R", "I"):
        logger.warning(f"Unknown parameter type '{param_type}' for parameter '{prmr}'")
        return

    if not expression_text:
        logger.warning(f"Empty expression for parameter '{prmr}'")
        return

    logger.debug(f"Found single expression: {param_name} ({param_type}) = {expression_text}")

    # Evaluate the expression
    from ansys.dyna.core.lib.expression_evaluator import ExpressionEvaluator

    deck_params = deck.parameters
    evaluator = ExpressionEvaluator(deck_params)

    try:
        result = evaluator.evaluate(expression_text, param_type)

        # Add to deck parameters
        if local:
            logger.debug(f"Adding local parameter: {param_name} = {result}")
            deck_params.add_local(param_name, result)
        else:
            logger.debug(f"Adding global parameter: {param_name} = {result}")
            deck_params.add(param_name, result)

        logger.info(f"Evaluated parameter '{param_name}' = {result}")
    except Exception as e:
        logger.error(f"Failed to evaluate expression for '{param_name}': {e}")
        raise


def _load_parameter_expressions(deck, parameter_expression, local: bool = False):
    """Load and evaluate PARAMETER_EXPRESSION or PARAMETER_EXPRESSION_LOCAL.

    Parameters
    ----------
    deck : Deck
        Target deck.
    parameter_expression : ParameterExpression or ParameterExpressionLocal
        Keyword with expressions.
    local : bool, optional
        Add as local (True) or global (False) parameters.
    """
    # Extract expressions from the keyword's DataFrame
    df = parameter_expression.parameters

    if df is None or len(df) == 0:
        logger.debug("No expressions to evaluate")
        return

    # Extract expressions from the DataFrame
    expressions = []
    for idx in range(len(df)):
        prmr = str(df.iloc[idx]["prmr"]).strip()
        expression_text = str(df.iloc[idx]["expression"]).strip()

        # Extract type code and parameter name from prmr field
        if not prmr or len(prmr) < 2:
            logger.warning(f"Skipping invalid parameter name: '{prmr}'")
            continue

        param_type = prmr[0]  # 'R' or 'I'
        param_name = prmr[1:].strip()

        if param_type not in ("R", "I"):
            logger.warning(f"Unknown parameter type '{param_type}' for parameter '{prmr}'")
            continue

        logger.debug(f"Found expression: {param_name} ({param_type}) = {expression_text}")
        expressions.append((param_type, param_name, expression_text))

    if not expressions:
        logger.debug("No valid expressions to evaluate")
        return

    # Evaluate expressions using the expression evaluator
    deck_params = deck.parameters

    # Import here to avoid circular dependency
    from ansys.dyna.core.lib.expression_evaluator import DependencyResolver, ExpressionEvaluator

    # Resolve dependency order
    resolver = DependencyResolver()
    ordered_expressions = resolver.resolve(expressions)

    # Evaluate in order
    evaluator = ExpressionEvaluator(deck_params)
    for param_type, param_name, expression in ordered_expressions:
        logger.debug(f"Evaluating expression for '{param_name}': {expression}")
        result = evaluator.evaluate(expression, param_type)

        # Add to deck parameters immediately so later expressions can reference it
        if local:
            logger.debug(f"Adding local parameter: {param_name} = {result}")
            deck_params.add_local(param_name, result)
        else:
            logger.debug(f"Adding global parameter: {param_name} = {result}")
            deck_params.add(param_name, result)

        logger.info(f"Evaluated parameter '{param_name}' = {result}")


class ParameterHandler(ImportHandler):
    def __init__(self):
        pass

    def after_import(self, context: ImportContext, keyword: typing.Union["KeywordBase", str]) -> None:
        """Handle actions after importing a keyword."""
        from ansys.dyna.core import keywords as kwd

        if isinstance(keyword, kwd.Parameter):
            keyword: kwd.Parameter = keyword
            logger.debug(f"Processing PARAMETER keyword with {len(keyword.parameters.data)} parameters")
            _load_parameters(context.deck, keyword, local=False)
        elif isinstance(keyword, kwd.ParameterLocal):
            logger.debug(f"Processing PARAMETER_LOCAL keyword with {len(keyword.parameters.data)} parameters")
            _load_parameters(context.deck, keyword, local=True)
        elif isinstance(keyword, kwd.ParameterExpressionNoecho):
            # NOECHO variant uses a Card, not a TableCard, so handle it separately
            logger.debug("Processing PARAMETER_EXPRESSION_NOECHO keyword")
            _load_single_parameter_expression(context.deck, keyword, local=False)
        elif isinstance(keyword, (kwd.ParameterExpression, kwd.ParameterExpressionLocal)):
            # Handle PARAMETER_EXPRESSION and PARAMETER_EXPRESSION_LOCAL
            is_local = isinstance(keyword, kwd.ParameterExpressionLocal)
            logger.debug(f"Processing {'PARAMETER_EXPRESSION_LOCAL' if is_local else 'PARAMETER_EXPRESSION'} keyword")
            _load_parameter_expressions(context.deck, keyword, local=is_local)

    def on_error(self, error):
        """Emit a warning when parameter processing fails."""
        warnings.warn(f"Error processing parameter: {error}")
