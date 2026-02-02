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

"""Expression evaluator for LS-DYNA PARAMETER_EXPRESSION keywords.

Provides AST-based safe evaluation of arithmetic expressions with:
- Dependency resolution via topological sort (Kahn's algorithm)
- LS-DYNA function support (sin, cos, sqrt, abs, etc.)
- Parameter reference handling (param, &param, -&param)
"""

import ast
import logging
import math
import re
import typing

from ansys.dyna.core.lib.parameters import ParameterSet

logger = logging.getLogger(__name__)


# Mapping of LS-DYNA function names to Python equivalents
LSDYNA_FUNCTIONS = {
    # Trigonometric functions
    "sin": math.sin,
    "cos": math.cos,
    "tan": math.tan,
    "asin": math.asin,
    "acos": math.acos,
    "atan": math.atan,
    "atan2": math.atan2,
    # Hyperbolic functions
    "sinh": math.sinh,
    "cosh": math.cosh,
    "tanh": math.tanh,
    "asinh": math.asinh,
    "acosh": math.acosh,
    "atanh": math.atanh,
    # Other math functions
    "sqrt": math.sqrt,
    "abs": abs,
    "sign": lambda x: 1 if x > 0 else (-1 if x < 0 else 0),
    "exp": math.exp,
    "log": math.log,
    "log10": math.log10,
    "min": min,
    "max": max,
    "mod": lambda x, y: x % y,
    # Type conversion functions
    "LS_INTEGER": int,
    "LS_REAL": float,
    "aint": lambda x: float(int(x)),  # Truncate to integer, return as float
    "nint": lambda x: float(round(x)),  # Round to nearest integer, return as float
    "anint": lambda x: float(round(x)),  # Same as nint
}


class ExpressionEvaluator:
    """Safe evaluator for LS-DYNA parameter expressions using AST.

    Only allows whitelisted mathematical operations and functions.
    """

    def __init__(self, parameter_set: ParameterSet):
        """Initialize the expression evaluator.

        Parameters
        ----------
        parameter_set : ParameterSet
            Parameter set to use for resolving parameter references.
        """
        self.parameter_set = parameter_set
        logger.debug("Initialized ExpressionEvaluator")

    def evaluate(self, expression: str, param_type: str) -> typing.Union[int, float]:
        """Evaluate an expression and return the result with appropriate type.

        Parameters
        ----------
        expression : str
            Expression string to evaluate (e.g., "a+b*2").
        param_type : str
            Parameter type code ('R' for real/float, 'I' for integer).

        Returns
        -------
        int or float
            Evaluated result with type matching param_type.

        Raises
        ------
        ValueError
            If expression is invalid or contains unsupported operations.
        KeyError
            If expression references undefined parameters.
        """
        logger.debug(f"Evaluating expression: '{expression}' with type '{param_type}'")

        # Normalize whitespace
        expression = expression.strip()
        if not expression:
            raise ValueError("Empty expression")

        # Replace parameter references with their values
        expression_with_values = self._replace_parameter_references(expression)
        logger.debug(f"After parameter substitution: '{expression_with_values}'")

        # Parse and evaluate using AST
        try:
            tree = ast.parse(expression_with_values, mode="eval")
            result = self._eval_node(tree.body)
            logger.debug(f"Evaluation result: {result}")
        except Exception as e:
            logger.error(f"Failed to evaluate expression '{expression}': {e}", exc_info=True)
            raise ValueError(f"Invalid expression '{expression}': {e}") from e

        # Convert to appropriate type
        if param_type == "I":
            result = int(result)
            logger.debug(f"Converted to integer: {result}")
        elif param_type == "R":
            result = float(result)
            logger.debug(f"Converted to float: {result}")
        else:
            raise ValueError(f"Unknown parameter type: {param_type}")

        return result

    def _replace_parameter_references(self, expression: str) -> str:
        """Replace parameter references with numeric values.

        Handles: 'param', '&param', '-&param'. Preserves function names.

        Parameters
        ----------
        expression : str
            Expression with parameter references.

        Returns
        -------
        str
            Expression with values substituted.

        Raises
        ------
        KeyError
            If parameter not defined.
        """
        # Pattern to match parameter references:
        # - Optional minus sign: -?
        # - Optional ampersand: &?
        # - Parameter name: one or more word characters or underscores
        # Must not be preceded by alphanumeric (to avoid matching inside function names)
        param_pattern = r"(?<![a-zA-Z0-9_])(-?)(&?)([a-zA-Z_][a-zA-Z0-9_]*)"

        def replace_param(match):
            minus = match.group(1)
            ampersand = match.group(2)  # noqa: F841
            param_name = match.group(3)

            # Check if this is actually a function name
            if param_name in LSDYNA_FUNCTIONS:
                # Don't replace function names
                return match.group(0)

            # Try to get parameter value
            try:
                value = self.parameter_set.get(param_name)
                logger.debug(f"Replacing parameter '{param_name}' with value {value}")

                # Format the replacement
                if minus:
                    return f"(-{value})"
                else:
                    return str(value)
            except KeyError:
                # Parameter not found - re-raise with clear message
                logger.error(f"Undefined parameter reference: {param_name}")
                raise KeyError(f"Undefined parameter: {param_name}")

        return re.sub(param_pattern, replace_param, expression)

    def _eval_node(self, node: ast.AST) -> typing.Union[int, float]:
        """Recursively evaluate an AST node.

        Parameters
        ----------
        node : ast.AST
            AST node to evaluate.

        Returns
        -------
        int or float
            Evaluation result.

        Raises
        ------
        ValueError
            If node contains unsupported operations.
        """
        if isinstance(node, ast.Constant):
            return node.value
        elif isinstance(node, ast.UnaryOp):
            operand = self._eval_node(node.operand)
            if isinstance(node.op, ast.UAdd):
                return +operand
            elif isinstance(node.op, ast.USub):
                return -operand
            else:
                raise ValueError(f"Unsupported unary operator: {node.op.__class__.__name__}")
        elif isinstance(node, ast.BinOp):
            left = self._eval_node(node.left)
            right = self._eval_node(node.right)
            if isinstance(node.op, ast.Add):
                return left + right
            elif isinstance(node.op, ast.Sub):
                return left - right
            elif isinstance(node.op, ast.Mult):
                return left * right
            elif isinstance(node.op, ast.Div):
                return left / right
            elif isinstance(node.op, ast.Pow):
                return left**right
            elif isinstance(node.op, ast.Mod):
                return left % right
            else:
                raise ValueError(f"Unsupported binary operator: {node.op.__class__.__name__}")
        elif isinstance(node, ast.Call):
            # Function call
            if not isinstance(node.func, ast.Name):
                raise ValueError("Only simple function calls are supported")

            func_name = node.func.id
            if func_name not in LSDYNA_FUNCTIONS:
                raise ValueError(f"Unsupported function: {func_name}")

            # Evaluate arguments
            args = [self._eval_node(arg) for arg in node.args]

            # Call the function
            func = LSDYNA_FUNCTIONS[func_name]
            try:
                return func(*args)
            except Exception as e:
                raise ValueError(f"Error calling function {func_name}: {e}") from e
        else:
            raise ValueError(f"Unsupported expression type: {node.__class__.__name__}")


class DependencyResolver:
    """Resolves expression dependencies using topological sort (Kahn's algorithm)."""

    def __init__(self):
        logger.debug("Initialized DependencyResolver")

    def resolve(
        self, expressions: typing.List[typing.Tuple[str, str, str]]
    ) -> typing.List[typing.Tuple[str, str, str]]:
        """Order expressions by dependency using topological sort.

        Parameters
        ----------
        expressions : list of tuple
            (param_type, param_name, expression) tuples.

        Returns
        -------
        list of tuple
            Expressions ordered for evaluation (dependencies first).

        Raises
        ------
        ValueError
            If circular dependencies detected.
        """
        logger.debug(f"Resolving dependencies for {len(expressions)} expressions")

        # Build dependency graph
        deps = {}  # param_name -> set of param_names it depends on
        expr_map = {}  # param_name -> (param_type, param_name, expression)

        for param_type, param_name, expression in expressions:
            expr_map[param_name] = (param_type, param_name, expression)
            deps[param_name] = self._extract_parameter_names(expression)
            logger.debug(f"Parameter '{param_name}' depends on: {deps[param_name]}")

        # Topological sort using Kahn's algorithm
        # Calculate in-degrees
        in_degree = {name: 0 for name in expr_map.keys()}
        for name in expr_map.keys():
            for dep in deps[name]:
                if dep in in_degree:  # Only count dependencies within this expression set
                    in_degree[name] += 1

        logger.debug(f"In-degrees: {in_degree}")

        # Start with nodes that have no dependencies within this set
        queue = [name for name, degree in in_degree.items() if degree == 0]
        result = []

        while queue:
            # Sort queue for deterministic ordering
            queue.sort()
            current = queue.pop(0)
            result.append(expr_map[current])
            logger.debug(f"Added '{current}' to evaluation order")

            # Reduce in-degree for dependents
            for name, name_deps in deps.items():
                if current in name_deps and name in in_degree:
                    in_degree[name] -= 1
                    if in_degree[name] == 0:
                        queue.append(name)

        # Check for circular dependencies
        if len(result) != len(expressions):
            # Find which parameters are in the cycle
            unresolved = set(expr_map.keys()) - {name for _, name, _ in result}
            logger.error(f"Circular dependency detected involving: {unresolved}")
            raise ValueError(f"Circular dependency detected among parameters: {unresolved}")

        logger.debug(f"Resolved evaluation order: {[name for _, name, _ in result]}")
        return result

    def _extract_parameter_names(self, expression: str) -> typing.Set[str]:
        """Extract parameter names referenced in an expression.

        Parameters
        ----------
        expression : str
            Expression string.

        Returns
        -------
        set of str
            Set of parameter names referenced in the expression.
        """
        # Pattern to match parameter references (with or without &, with optional -)
        param_pattern = r"(?<![a-zA-Z0-9_])-?&?([a-zA-Z_][a-zA-Z0-9_]*)"

        matches = re.findall(param_pattern, expression)

        # Filter out function names
        params = {m for m in matches if m not in LSDYNA_FUNCTIONS}

        logger.debug(f"Extracted parameters from '{expression}': {params}")
        return params


def evaluate_parameter_expressions(
    parameter_set: ParameterSet, expressions: typing.List[typing.Tuple[str, str, str]]
) -> None:
    """Evaluate PARAMETER_EXPRESSION keywords and add results to parameter set.

    Resolves dependencies, evaluates in order, adds results to parameter_set.

    Parameters
    ----------
    parameter_set : ParameterSet
        Target parameter set.
    expressions : list of tuple
        (param_type, param_name, expression) tuples.

    Raises
    ------
    ValueError
        If circular dependencies or invalid expressions.
    KeyError
        If undefined parameter referenced.
    """
    logger.debug(f"Evaluating {len(expressions)} parameter expressions")

    if not expressions:
        return

    # Resolve dependencies to get evaluation order
    resolver = DependencyResolver()
    ordered_expressions = resolver.resolve(expressions)

    # Evaluate expressions in order
    evaluator = ExpressionEvaluator(parameter_set)
    for param_type, param_name, expression in ordered_expressions:
        logger.debug(f"Evaluating expression for '{param_name}': {expression}")

        result = evaluator.evaluate(expression, param_type)
        parameter_set.add(param_name, result)

        logger.info(f"Evaluated parameter '{param_name}' = {result}")
