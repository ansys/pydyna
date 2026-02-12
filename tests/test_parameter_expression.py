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

"""
Test PARAMETER_EXPRESSION functionality including expression evaluation,
dependency resolution, and parameter substitution.
"""

import math
import pytest

from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.lib.parameters import ParameterSet



class TestExpressionEvaluator:
    """Test the expression evaluator with various arithmetic operations and functions."""

    def test_simple_arithmetic_addition(self):
        """Test basic addition expression."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
$#    prmr                                                            expression
R result  1+2
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # After loading, parameter 'result' should equal 3.0
        assert deck.parameters.get("result") == 3.0

    def test_simple_arithmetic_subtraction(self):
        """Test basic subtraction expression."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  10-3
*END"""
        )
        assert deck.parameters.get("result") == 7.0

    def test_simple_arithmetic_multiplication(self):
        """Test basic multiplication expression."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  4*5
*END"""
        )
        assert deck.parameters.get("result") == 20.0

    def test_simple_arithmetic_division(self):
        """Test basic division expression."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  20/4
*END"""
        )
        assert deck.parameters.get("result") == 5.0

    def test_arithmetic_with_operator_precedence(self):
        """Test expression with operator precedence (multiplication before addition)."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  1+2*3
*END"""
        )
        # Should be 1 + (2*3) = 7, not (1+2)*3 = 9
        assert deck.parameters.get("result") == 7.0

    def test_arithmetic_with_parentheses(self):
        """Test expression with parentheses to override precedence."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  (1+2)*3
*END"""
        )
        # Should be (1+2)*3 = 9
        assert deck.parameters.get("result") == 9.0

    def test_arithmetic_with_nested_parentheses(self):
        """Test expression with nested parentheses."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  ((2+3)*4)-5
*END"""
        )
        # Should be ((2+3)*4)-5 = (5*4)-5 = 20-5 = 15
        assert deck.parameters.get("result") == 15.0

    def test_power_operator(self):
        """Test exponentiation using ** operator."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  2**3
*END"""
        )
        # 2^3 = 8
        assert deck.parameters.get("result") == 8.0

    def test_unary_minus(self):
        """Test unary minus operator."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  -5+3
*END"""
        )
        # -5+3 = -2
        assert deck.parameters.get("result") == -2.0

    def test_function_sqrt(self):
        """Test sqrt function."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  sqrt(16)
*END"""
        )
        assert deck.parameters.get("result") == 4.0

    def test_function_abs(self):
        """Test abs function."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  abs(-7.5)
*END"""
        )
        assert deck.parameters.get("result") == 7.5

    def test_function_sin(self):
        """Test sin function."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  sin(0.0)
*END"""
        )
        assert deck.parameters.get("result") == 0.0

    def test_function_cos(self):
        """Test cos function."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  cos(0.0)
*END"""
        )
        assert deck.parameters.get("result") == 1.0

    def test_function_exp(self):
        """Test exp function."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  exp(0.0)
*END"""
        )
        assert deck.parameters.get("result") == 1.0

    def test_function_log(self):
        """Test log (natural logarithm) function."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  log(2.718281828459045)
*END"""
        )
        # ln(e) = 1
        assert abs(deck.parameters.get("result") - 1.0) < 1e-10

    def test_function_min(self):
        """Test min function with two arguments."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  min(5, 3)
*END"""
        )
        assert deck.parameters.get("result") == 3.0

    def test_function_max(self):
        """Test max function with two arguments."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R result  max(5, 3)
*END"""
        )
        assert deck.parameters.get("result") == 5.0

    def test_integer_type_result(self):
        """Test integer type parameter (I prefix)."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
I result  5+3
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # Integer type should return int
        assert deck.parameters.get("result") == 8
        assert isinstance(deck.parameters.get("result"), int)

    def test_float_to_int_conversion(self):
        """Test that I prefix truncates/rounds float results to int."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
I result  7.9
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # Should truncate to int
        assert deck.parameters.get("result") == 7
        assert isinstance(deck.parameters.get("result"), int)



class TestParameterReferences:
    """Test expressions that reference other parameters."""

    def test_reference_regular_parameter(self):
        """Test expression referencing a regular PARAMETER."""
        deck_text = """*KEYWORD
*PARAMETER
R base    10.0
*PARAMETER_EXPRESSION
R result  base+5
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == 15.0

    def test_reference_parameter_with_ampersand(self):
        """Test expression referencing parameter with & prefix."""
        deck_text = """*KEYWORD
*PARAMETER
R base    10.0
*PARAMETER_EXPRESSION
R result  &base+5
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == 15.0

    def test_reference_negative_parameter(self):
        """Test expression with negative parameter reference."""
        deck_text = """*KEYWORD
*PARAMETER
R base    10.0
*PARAMETER_EXPRESSION
R result  -&base+5
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # -10 + 5 = -5
        assert deck.parameters.get("result") == -5.0

    def test_reference_another_expression_parameter(self):
        """Test expression referencing another expression-defined parameter."""
        deck_text = """*KEYWORD
*PARAMETER
R base    10.0
*PARAMETER_EXPRESSION
R first   base*2
R second  first+5
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # first = 10*2 = 20, second = 20+5 = 25
        assert deck.parameters.get("first") == 20.0
        assert deck.parameters.get("second") == 25.0

    def test_complex_expression_from_github_issue(self):
        """Test the exact example from GitHub issue #641."""
        deck_text = """*KEYWORD
*PARAMETER
R gravtime 0.3
R tramp    0.001
R diemv    145.45
R clsv     1000.0
*PARAMETER_EXPRESSION
R tramp1   tramp+gravtime
R endtime  tramp1+(abs(diemv)-0.5*clsv*tramp)/clsv
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # tramp1 = 0.001 + 0.3 = 0.301
        expected_tramp1 = 0.001 + 0.3
        assert abs(deck.parameters.get("tramp1") - expected_tramp1) < 1e-10

        # endtime = 0.301 + (abs(145.45) - 0.5*1000.0*0.001)/1000.0
        #         = 0.301 + (145.45 - 0.5)/1000.0
        #         = 0.301 + 144.95/1000.0
        #         = 0.301 + 0.14495
        #         = 0.44595
        expected_endtime = 0.301 + (abs(145.45) - 0.5 * 1000.0 * 0.001) / 1000.0
        assert abs(deck.parameters.get("endtime") - expected_endtime) < 1e-10

    def test_multiple_parameter_references(self):
        """Test expression with multiple parameter references."""
        deck_text = """*KEYWORD
*PARAMETER
R a        2.0
R b        3.0
R c        4.0
*PARAMETER_EXPRESSION
R result   a*b+c
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # 2*3 + 4 = 10
        assert deck.parameters.get("result") == 10.0



class TestDependencyResolution:
    """Test dependency resolution and evaluation ordering."""

    def test_simple_dependency_chain(self):
        """Test that expressions are evaluated in dependency order."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R a        1+1
R b        a+1
R c        b+1
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # a=2, b=3, c=4 (must evaluate in order)
        assert deck.parameters.get("a") == 2.0
        assert deck.parameters.get("b") == 3.0
        assert deck.parameters.get("c") == 4.0

    def test_out_of_order_dependencies(self):
        """Test that dependencies work even when defined out of order."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R c        b+1
R b        a+1
R a        1+1
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # Should still evaluate correctly: a=2, b=3, c=4
        assert deck.parameters.get("a") == 2.0
        assert deck.parameters.get("b") == 3.0
        assert deck.parameters.get("c") == 4.0

    def test_circular_dependency_error(self):
        """Test that circular dependencies are handled gracefully."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R a        b+1
R b        a+1
*END"""
        deck = Deck()
        with pytest.warns(UserWarning, match="Circular dependency detected"):
            deck.loads(deck_text)

        # Parameters should not be created due to circular dependency error
        with pytest.raises(KeyError):
            deck.parameters.get("a")
        with pytest.raises(KeyError):
            deck.parameters.get("b")

    def test_self_reference_error(self):
        """Test that self-referencing expressions are handled gracefully."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R a        a+1
*END"""
        deck = Deck()
        with pytest.warns(UserWarning, match="Circular dependency detected"):
            deck.loads(deck_text)

        # Parameter should not be created due to self-reference error
        with pytest.raises(KeyError):
            deck.parameters.get("a")

    def test_undefined_parameter_error(self):
        """Test that referencing undefined parameter is handled gracefully."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result   undefined_param+1
*END"""
        deck = Deck()
        with pytest.warns(UserWarning, match="Undefined parameter: undefined_param"):
            deck.loads(deck_text)

        # Parameter should not be created due to undefined reference
        with pytest.raises(KeyError):
            deck.parameters.get("result")

    def test_forward_reference_to_regular_parameter(self):
        """Test that expressions cannot reference parameters defined later."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result   base*2
*PARAMETER
R base     5.0
*END"""
        deck = Deck()
        with pytest.warns(UserWarning, match="Undefined parameter: base"):
            deck.loads(deck_text)

        # 'result' should not be created because 'base' is not defined yet
        with pytest.raises(KeyError):
            deck.parameters.get("result")
        # But 'base' should be created
        assert deck.parameters.get("base") == 5.0



class TestScoping:
    """Test PARAMETER_EXPRESSION scoping with includes."""

    def test_parameter_expression_global_scope(self):
        """Test that PARAMETER_EXPRESSION creates global parameters."""
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION
R global   10.0
*END"""
        )

        # Should be accessible
        assert deck.parameters.get("global") == 10.0

    def test_parameter_expression_local_scope(self):
        """Test that PARAMETER_EXPRESSION_LOCAL creates local parameters."""
        # This test requires file-based includes to properly test scoping
        # For now, just verify the keyword is recognized
        deck = Deck()
        deck.loads(
            """*KEYWORD
*PARAMETER_EXPRESSION_LOCAL
R local    10.0
*END"""
        )

        # Should be accessible in same file
        assert deck.parameters.get("local") == 10.0

    def test_expression_can_reference_parent_parameter(self):
        """Test that expressions can reference parameters from parent scope."""
        deck_text = """*KEYWORD
*PARAMETER
R parent   5.0
*PARAMETER_EXPRESSION
R child    parent*2
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("child") == 10.0



class TestMultipleExpressionKeywords:
    """Test multiple PARAMETER_EXPRESSION keywords in same deck."""

    def test_multiple_expression_blocks(self):
        """Test multiple PARAMETER_EXPRESSION keywords."""
        deck_text = """*KEYWORD
*PARAMETER
R base     10.0
*PARAMETER_EXPRESSION
R first    base+5
*PARAMETER_EXPRESSION
R second   first*2
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # first = 10+5 = 15
        assert deck.parameters.get("first") == 15.0
        # second = 15*2 = 30
        assert deck.parameters.get("second") == 30.0

    def test_expression_referencing_across_blocks(self):
        """Test that later expression blocks can reference earlier ones."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R a        5.0
*PARAMETER_EXPRESSION
R b        a*2
*PARAMETER_EXPRESSION
R c        b+a
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # a=5, b=10, c=15
        assert deck.parameters.get("a") == 5.0
        assert deck.parameters.get("b") == 10.0
        assert deck.parameters.get("c") == 15.0



class TestEdgeCases:
    """Test edge cases and special scenarios."""

    def test_expression_with_whitespace(self):
        """Test that expressions with extra whitespace are handled correctly."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result   1  +  2  *  3
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # Should still evaluate to 7
        assert deck.parameters.get("result") == 7.0

    def test_expression_with_float_literals(self):
        """Test expressions with floating-point literals."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result   1.5*2.0+0.5
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # 1.5*2.0 + 0.5 = 3.0 + 0.5 = 3.5
        assert deck.parameters.get("result") == 3.5

    def test_expression_zero_result(self):
        """Test expression that evaluates to zero."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result   5-5
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == 0.0

    def test_expression_negative_result(self):
        """Test expression with negative result."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result   3-10
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == -7.0

    def test_expression_very_small_result(self):
        """Test expression with very small floating point result."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result   1e-10*2
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert abs(deck.parameters.get("result") - 2e-10) < 1e-15

    def test_expression_very_large_result(self):
        """Test expression with very large result."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result   1e10*2
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == 2e10

    def test_empty_expression_string(self):
        """Test handling of empty expression."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R result
*END"""
        deck = Deck()
        with pytest.warns(UserWarning, match="Undefined parameter: nan"):
            deck.loads(deck_text)

        # Empty expression should not create the parameter
        with pytest.raises(KeyError):
            deck.parameters.get("result")

    def test_parameter_name_with_underscore(self):
        """Test parameter names containing underscores."""
        deck_text = """*KEYWORD
*PARAMETER
R my_val   10.0
*PARAMETER_EXPRESSION
R my_res   my_val*2
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("my_res") == 20.0



class TestNoEchoVariant:
    """Test PARAMETER_EXPRESSION_NOECHO variant."""

    def test_noecho_variant_basic(self):
        """Test that PARAMETER_EXPRESSION_NOECHO evaluates expressions correctly."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION_NOECHO
R result   5+5
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # Should evaluate and be usable just like regular PARAMETER_EXPRESSION
        assert deck.parameters.get("result") == 10.0



class TestParameterExpressionLocalScoping:
    """Test PARAMETER_EXPRESSION_LOCAL scoping with includes."""

    def test_local_expression_does_not_leak_to_parent(self, tmp_path):
        """Test that PARAMETER_EXPRESSION_LOCAL does not leak to parent scope."""
        # Create include file with local expression
        include_content = """*PARAMETER_EXPRESSION_LOCAL
Rlocalexp,10+5
*PARAMETER
Rlocalval,&localexp"""

        include_path = tmp_path / "include.k"
        include_path.write_text(include_content)

        # Main deck includes the file
        deck_text = f"""*KEYWORD
*INCLUDE
{include_path}
*END"""

        deck = Deck()
        deck.loads(deck_text)
        expanded = deck.expand(cwd=str(tmp_path))

        # The local expression parameter should NOT be accessible from parent
        # after include processing
        with pytest.raises(KeyError):
            expanded.parameters.get("localexp")

    def test_local_expression_visible_within_include(self, tmp_path):
        """Test that PARAMETER_EXPRESSION_LOCAL is visible within same include."""
        # Create include file that uses its own local expression
        # PARAMETER_EXPRESSION_LOCAL format: prmr (10 chars), expression (70 chars)
        include_content = (
            "*PARAMETER_EXPRESSION_LOCAL\n"
            "Rlocexp             100\n"
            "*SET_NODE_LIST\n"
            "         1       0.0       0.0       0.0       0.0    MECH         1\n"
            "   &locexp\n"
        )

        include_path = tmp_path / "include.k"
        include_path.write_text(include_content)

        deck_text = f"""*KEYWORD
*INCLUDE
{include_path}
*END"""

        deck = Deck()
        deck.loads(deck_text)
        expanded = deck.expand(cwd=str(tmp_path))

        # Find the SET_NODE_LIST keyword
        set_node = None
        for kw in expanded.keywords:
            if hasattr(kw, "nodes"):
                set_node = kw
                break

        # The local expression should have been substituted
        assert set_node is not None
        assert set_node.nodes[0] == 100  # &locexp substituted

    def test_sibling_includes_cannot_see_each_others_local_expressions(self, tmp_path):
        """Test that sibling includes cannot see each other's local expressions.

        When a keyword references an undefined parameter, it cannot be parsed into
        a keyword object. Instead, it is retained as a raw string in the deck with
        the unresolved parameter reference (e.g., &locexp1) still present. A warning
        is emitted.

        This test verifies LOCAL scoping works (locexp1 is not visible to include2)
        by checking that no parsed SET_NODE_LIST keyword object exists.

        Use strict=True to raise an error instead of retaining unparsed keywords.
        """
        # First include defines a local expression
        # PARAMETER_EXPRESSION_LOCAL format: prmr (10 chars), expression (70 chars)
        include1_content = "*PARAMETER_EXPRESSION_LOCAL\n" "Rlocexp1            50\n"

        include1_path = tmp_path / "include1.k"
        include1_path.write_text(include1_content)

        # Second include tries to use it (should fail because locexp1 is local to include1)
        include2_content = (
            "*SET_NODE_LIST\n" "         1       0.0       0.0       0.0       0.0    MECH         1\n" "  &locexp1\n"
        )

        include2_path = tmp_path / "include2.k"
        include2_path.write_text(include2_content)

        deck_text = f"""*KEYWORD
*INCLUDE
{include1_path}
*INCLUDE
{include2_path}
*END"""

        deck = Deck()
        deck.loads(deck_text)

        # Expand - keyword with undefined param is retained as string, not parsed
        # A warning is expected because 'locexp1' is a local parameter not visible to include2
        with pytest.warns(UserWarning, match="Error processing parameter.*locexp1"):
            expanded = deck.expand(cwd=str(tmp_path))

        # Verify the local parameter scoping worked - no parsed SET_NODE_LIST object
        # because &locexp1 is local to include1 and not visible to include2.
        # The keyword is retained as a raw string in expanded.all_keywords.
        set_node = None
        for kw in expanded.keywords:
            if hasattr(kw, "nodes"):
                set_node = kw
                break

        # No parsed SET_NODE_LIST object - it's retained as a string with &locexp1
        assert set_node is None, "SET_NODE_LIST should not be parsed because &locexp1 is undefined"

    def test_sibling_includes_strict_mode_raises_on_undefined_parameter(self, tmp_path):
        """Test that strict mode raises an error for undefined parameter references.

        When strict=True is passed to expand(), undefined parameter references
        raise a KeyError instead of retaining the keyword as an unparsed string.
        """
        # First include defines a local expression
        include1_content = "*PARAMETER_EXPRESSION_LOCAL\n" "Rlocexp1            50\n"

        include1_path = tmp_path / "include1.k"
        include1_path.write_text(include1_content)

        # Second include tries to use it (should fail because locexp1 is local to include1)
        include2_content = (
            "*SET_NODE_LIST\n" "         1       0.0       0.0       0.0       0.0    MECH         1\n" "  &locexp1\n"
        )

        include2_path = tmp_path / "include2.k"
        include2_path.write_text(include2_content)

        deck_text = f"""*KEYWORD
*INCLUDE
{include1_path}
*INCLUDE
{include2_path}
*END"""

        deck = Deck()
        deck.loads(deck_text)

        # With strict=True, expand should raise KeyError for undefined parameter
        with pytest.raises(KeyError, match="locexp1"):
            deck.expand(cwd=str(tmp_path), strict=True)

    def test_global_expression_visible_to_includes(self, tmp_path):
        """Test that global PARAMETER_EXPRESSION is visible to includes."""
        # Include file uses a parameter defined in parent
        include_content = """*SET_NODE_LIST
         1       0.0       0.0       0.0       0.0    MECH         1
&globalexp"""

        include_path = tmp_path / "include.k"
        include_path.write_text(include_content)

        deck_text = f"""*KEYWORD
*PARAMETER_EXPRESSION
Rglobalexp,200
*INCLUDE
{include_path}
*END"""

        deck = Deck()
        deck.loads(deck_text)
        expanded = deck.expand(cwd=str(tmp_path))

        # Find the SET_NODE_LIST keyword
        set_node = None
        for kw in expanded.keywords:
            if hasattr(kw, "nodes"):
                set_node = kw
                break

        assert set_node is not None
        assert set_node.nodes[0] == 200  # &globalexp substituted



class TestExpressionReferencingLocalParameter:
    """Test expressions referencing local parameters."""

    def test_expression_references_local_parameter(self):
        """Test that PARAMETER_EXPRESSION can reference PARAMETER_LOCAL.

        Verifies that an expression like:
            *PARAMETER_LOCAL
            Rlocalbase,10.0
            *PARAMETER_EXPRESSION
            Rresult,localbase*2

        Works correctly where the expression references a local parameter.
        """
        deck_text = """*KEYWORD
*PARAMETER_LOCAL
Rlocalbase,10.0
*PARAMETER_EXPRESSION
Rresult,localbase*2
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # Verify local parameter is accessible
        assert deck.parameters.get("localbase") == 10.0
        # Verify expression evaluated correctly using the local parameter
        assert deck.parameters.get("result") == 20.0

    def test_expression_references_local_parameter_in_include(self, tmp_path):
        """Test that PARAMETER_EXPRESSION can reference PARAMETER_LOCAL within an include file."""
        # Create include file with PARAMETER_LOCAL and PARAMETER_EXPRESSION that references it
        include_content = (
            "*PARAMETER_LOCAL\n"
            "Rlocalbase  10.0\n"
            "*PARAMETER_EXPRESSION\n"
            "Rresult     localbase*2\n"
            "*SET_NODE_LIST\n"
            "         1       0.0       0.0       0.0       0.0    MECH         1\n"
            "   &result\n"
        )
        include_path = tmp_path / "include.k"
        include_path.write_text(include_content)

        deck_text = f"""*KEYWORD
*INCLUDE
{include_path}
*END"""
        deck = Deck()
        deck.loads(deck_text)
        expanded = deck.expand(cwd=str(tmp_path))

        # Find the SET_NODE_LIST keyword
        set_node = None
        for kw in expanded.keywords:
            if hasattr(kw, "nodes"):
                set_node = kw
                break

        assert set_node is not None
        # &result should be 20.0 (localbase*2 = 10.0*2)
        assert set_node.nodes[0] == 20
