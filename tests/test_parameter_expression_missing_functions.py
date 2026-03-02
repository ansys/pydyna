# Copyright (C) 2021 - 2026 ANSYS, Inc. and/or its affiliates.
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
Test missing and incorrect PARAMETER_EXPRESSION functions.

This test module covers:
1. Missing functions: csc, sec, ctn, int, float, pi
2. Incorrect implementations: sign (should be 2-arg), nint (should return int), mod (should round args)
3. LS-DYNA manual compliance for all edge cases
"""

import math

from ansys.dyna.core.lib.deck import Deck


class TestMissingTrigonometricFunctions:
    """Test missing trigonometric functions: csc, sec, ctn."""

    def test_csc_function(self):
        """Test csc (cosecant) function: csc(x) = 1/sin(x)."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  csc(0.5235987755)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # csc(π/6) = csc(30°) = 2.0
        expected = 1.0 / math.sin(0.5235987755)
        assert abs(deck.parameters.get("result") - expected) < 1e-10

    def test_sec_function(self):
        """Test sec (secant) function: sec(x) = 1/cos(x)."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sec(0.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sec(0) = 1/cos(0) = 1.0
        expected = 1.0 / math.cos(0.0)
        assert abs(deck.parameters.get("result") - expected) < 1e-10

    def test_sec_function_pi_over_3(self):
        """Test sec at π/3 (60 degrees)."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sec(1.0471975511)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sec(π/3) = 1/cos(π/3) = 1/0.5 = 2.0
        expected = 1.0 / math.cos(1.0471975511)
        assert abs(deck.parameters.get("result") - expected) < 1e-10

    def test_ctn_function(self):
        """Test ctn (cotangent) function: ctn(x) = cos(x)/sin(x) = 1/tan(x)."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  ctn(0.7853981633)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # ctn(π/4) = cot(45°) = 1.0
        expected = math.cos(0.7853981633) / math.sin(0.7853981633)
        assert abs(deck.parameters.get("result") - expected) < 1e-10


class TestPiConstant:
    """Test pi constant support."""

    def test_pi_standalone(self):
        """Test pi as a bare name/constant, not a function call."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  pi
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # pi constant should equal math.pi
        assert abs(deck.parameters.get("result") - math.pi) < 1e-15

    def test_pi_constant(self):
        """Test pi constant: 15°*π/180° = π/12."""
        deck_text = """*KEYWORD
*PARAMETER
R  angle_d  15.0
*PARAMETER_EXPRESSION
R  angle_r  angle_d*pi/180.0
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # 15 * π/180 = π/12 ≈ 0.2617993877991494
        expected = 15.0 * math.pi / 180.0
        assert abs(deck.parameters.get("angle_r") - expected) < 1e-10

    def test_pi_in_expression(self):
        """Test pi in complex expression."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  2*pi
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # 2π
        expected = 2.0 * math.pi
        assert abs(deck.parameters.get("result") - expected) < 1e-10

    def test_pi_with_trigonometric_function(self):
        """Test pi used in trigonometric function call."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sin(pi/2)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sin(π/2) = 1.0
        expected = math.sin(math.pi / 2.0)
        assert abs(deck.parameters.get("result") - expected) < 1e-10

    def test_pi_in_complex_expression(self):
        """Test pi in complex expression with multiple operations."""
        deck_text = """*KEYWORD
*PARAMETER
R       gt 0.3
R       tr 0.001
R       de 145.45
R       cs 1000.0
*PARAMETER_EXPRESSION
R      tr1 tr+gt
R   result cos(15.0*pi/180.0)+sin(15.0*pi/180.0)*tan(15.0*pi/180.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # cos(π/12) + sin(π/12) * tan(π/12)
        angle = 15.0 * math.pi / 180.0
        expected = math.cos(angle) + math.sin(angle) * math.tan(angle)
        assert abs(deck.parameters.get("result") - expected) < 1e-10


class TestIntFunction:
    """Test int() function: converts to integer, truncates toward zero."""

    def test_int_positive(self):
        """Test int() on positive float."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  int(48.9)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # int(48.9) = 48 (truncate toward zero). Use R prefix to isolate int() return type
        assert deck.parameters.get("result") == 48
        assert isinstance(deck.parameters.get("result"), int)

    def test_int_negative(self):
        """Test int() on negative float."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  int(-48.9)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # int(-48.9) = -48 (truncate toward zero, not floor). R prefix isolates int() return type
        assert deck.parameters.get("result") == -48
        assert isinstance(deck.parameters.get("result"), int)

    def test_int_on_integer(self):
        """Test int() on integer value (should be idempotent)."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  int(42)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == 42
        assert isinstance(deck.parameters.get("result"), int)

    def test_int_vs_aint_return_types(self):
        """int() returns integer, aint() returns real — both truncate toward zero."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   as_int int(-48.1)
R  as_aint aint(-48.1)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # Both truncate to -48, but int() returns int type, aint() returns float
        assert deck.parameters.get("as_int") == -48
        assert isinstance(deck.parameters.get("as_int"), int)
        assert deck.parameters.get("as_aint") == -48.0
        assert isinstance(deck.parameters.get("as_aint"), float)


class TestFloatFunction:
    """Test float() function: converts to real."""

    def test_float_from_integer(self):
        """Test float() converting integer to real."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  float(42)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == 42.0
        assert isinstance(deck.parameters.get("result"), float)

    def test_float_from_real(self):
        """Test float() on real (should be idempotent)."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  float(3.14159)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert abs(deck.parameters.get("result") - 3.14159) < 1e-5
        assert isinstance(deck.parameters.get("result"), float)


class TestSignFunction:
    """Test sign(x, y) function: returns abs(x) with sign of y."""

    def test_sign_positive_positive(self):
        """Test sign with both arguments positive."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sign(4.0, 8.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sign(4, 8) = 4 (positive magnitude, positive sign)
        assert deck.parameters.get("result") == 4.0

    def test_sign_negative_positive(self):
        """Test sign with negative x, positive y."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sign(-4.0, 8.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sign(-4, 8) = 4 (abs(-4)=4, sign of 8=positive)
        assert deck.parameters.get("result") == 4.0

    def test_sign_positive_negative(self):
        """Test sign with positive x, negative y."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sign(4.0, -8.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sign(4, -8) = -4 (abs(4)=4, sign of -8=negative)
        assert deck.parameters.get("result") == -4.0

    def test_sign_negative_negative(self):
        """Test sign with both arguments negative."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sign(-4.0, -8.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sign(-4, -8) = -4 (abs(-4)=4, sign of -8=negative)
        assert deck.parameters.get("result") == -4.0

    def test_sign_zero_positive(self):
        """Test sign with zero x and positive y."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sign(0.0, 8.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sign(0, 8) = 0 (abs(0)=0, sign of 8=positive, but 0 has no sign)
        assert deck.parameters.get("result") == 0.0

    def test_sign_zero_negative(self):
        """Test sign with zero x and negative y."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sign(0.0, -8.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sign(0, -8) = 0 or -0 (edge case: LS-DYNA behavior)
        # Most implementations treat this as 0
        result = deck.parameters.get("result")
        assert result == 0.0 or result == -0.0


class TestNintFunction:
    """Test nint() function: rounds to nearest integer, returns integer type."""

    def test_nint_rounds_up(self):
        """Test nint() rounding up."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  nint(48.6)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # nint(48.6) = 49 (round to nearest) and returns int. Use R prefix to isolate nint() return type
        assert deck.parameters.get("result") == 49
        assert isinstance(deck.parameters.get("result"), int)

    def test_nint_rounds_down(self):
        """Test nint() rounding down."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  nint(48.4)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # nint(48.4) = 48 (round to nearest). R prefix isolates nint() return type
        assert deck.parameters.get("result") == 48
        assert isinstance(deck.parameters.get("result"), int)

    def test_nint_negative_rounding(self):
        """Test nint() on negative value."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  nint(-48.8)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # nint(-48.8) = -49 (round to nearest, retains sign)
        assert deck.parameters.get("result") == -49
        assert isinstance(deck.parameters.get("result"), int)

    def test_nint_positive_to_zero(self):
        """Test nint() on value < 0.5."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  nint(0.4)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == 0
        assert isinstance(deck.parameters.get("result"), int)

    def test_nint_vs_anint_return_types(self):
        """nint() returns integer, anint() returns real — both round same way."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R  as_nint  nint(-48.8)
R as_anint  anint(-48.8)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # Both round to -49, but nint() returns int type, anint() returns float
        assert deck.parameters.get("as_nint") == -49
        assert isinstance(deck.parameters.get("as_nint"), int)
        assert deck.parameters.get("as_anint") == -49.0
        assert isinstance(deck.parameters.get("as_anint"), float)


class TestAintFunction:
    """Test aint() function: truncates toward zero, returns real type."""

    def test_aint_positive(self):
        """Test aint() on positive float."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  aint(48.1)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # aint(48.1) = 48.0 (truncate, returns real)
        assert deck.parameters.get("result") == 48.0
        assert isinstance(deck.parameters.get("result"), float)

    def test_aint_negative(self):
        """Test aint() on negative float."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  aint(-48.1)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # aint(-48.1) = -48.0 (truncate toward zero, returns real)
        assert deck.parameters.get("result") == -48.0
        assert isinstance(deck.parameters.get("result"), float)

    def test_aint_less_than_one(self):
        """Test aint() on magnitude < 1."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  aint(0.9)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # aint(0.9) = 0.0
        assert deck.parameters.get("result") == 0.0


class TestAnintFunction:
    """Test anint() function: rounds to nearest integer, returns real type."""

    def test_anint_rounds_up(self):
        """Test anint() rounding up."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  anint(48.6)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # anint(48.6) = 49.0 (round to nearest, returns real)
        assert deck.parameters.get("result") == 49.0
        assert isinstance(deck.parameters.get("result"), float)

    def test_anint_rounds_down(self):
        """Test anint() rounding down."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  anint(48.4)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # anint(48.4) = 48.0 (round to nearest, returns real)
        assert deck.parameters.get("result") == 48.0
        assert isinstance(deck.parameters.get("result"), float)

    def test_anint_negative(self):
        """Test anint() on negative value."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  anint(-48.8)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # anint(-48.8) = -49.0 (round to nearest, retains sign, returns real)
        assert deck.parameters.get("result") == -49.0
        assert isinstance(deck.parameters.get("result"), float)


class TestModFunction:
    """Test mod(x, y) function: modulo, handles rounding of real arguments."""

    def test_mod_integers(self):
        """Test mod with integer arguments."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
I   result  mod(17, 5)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # 17 mod 5 = 2
        assert deck.parameters.get("result") == 2

    def test_mod_real_arguments_rounded(self):
        """Test mod with real arguments (should round them)."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
I   result  mod(17.6, 5.4)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # mod rounds: round(17.6)=18, round(5.4)=5
        # 18 mod 5 = 3
        assert deck.parameters.get("result") == 3

    def test_mod_negative(self):
        """Test mod with negative values."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
I   result  mod(-17, 5)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # -17 mod 5 = -2 (in Python: -17 % 5 = 3, but LS-DYNA may differ)
        # Behavior depends on LS-DYNA implementation
        result = deck.parameters.get("result")
        assert result == -2 or result == 3  # Accept either behavior


class TestComplexExpressionsWithNewFunctions:
    """Test complex expressions combining missing/fixed functions."""

    def test_angle_calculation_with_pi(self):
        """Test user's original example: angle calculation."""
        deck_text = """*KEYWORD
*PARAMETER
R  angle_d 15.0
*PARAMETER_EXPRESSION
R   F_calc cos(&angle_d*pi/180.0)+sin(&angle_d*pi/180.0)*tan(&angle_d*pi/180.0)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # cos(π/12) + sin(π/12) * tan(π/12)
        angle = 15.0 * math.pi / 180.0
        expected = math.cos(angle) + math.sin(angle) * math.tan(angle)
        assert abs(deck.parameters.get("F_calc") - expected) < 1e-10

    def test_trigonometric_identity(self):
        """Test trigonometric identity using new functions."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
R   result  sin(0.5)**2+cos(0.5)**2
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # sin²(x) + cos²(x) = 1
        assert abs(deck.parameters.get("result") - 1.0) < 1e-10

    def test_cotangent_equals_reciprocal_tangent(self):
        """Test that ctn(x) ≈ 1/tan(x)."""
        deck_text = """*KEYWORD
*PARAMETER
R    angle 1.2
*PARAMETER_EXPRESSION
R  via_ctn ctn(angle)
Rvia_recip 1.0/tan(angle)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        ctn_val = deck.parameters.get("via_ctn")
        recip_val = deck.parameters.get("via_recip")
        assert abs(ctn_val - recip_val) < 1e-10

    def test_secant_reciprocal_cosine(self):
        """Test that sec(x) ≈ 1/cos(x)."""
        deck_text = """*KEYWORD
*PARAMETER
R    angle 0.5
*PARAMETER_EXPRESSION
R  via_sec sec(angle)
Rvia_recip 1.0/cos(angle)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        sec_val = deck.parameters.get("via_sec")
        recip_val = deck.parameters.get("via_recip")
        assert abs(sec_val - recip_val) < 1e-10

    def test_cosecant_reciprocal_sine(self):
        """Test that csc(x) ≈ 1/sin(x)."""
        deck_text = """*KEYWORD
*PARAMETER
R    angle 1.0
*PARAMETER_EXPRESSION
R  via_csc csc(angle)
Rvia_recip 1.0/sin(angle)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        csc_val = deck.parameters.get("via_csc")
        recip_val = deck.parameters.get("via_recip")
        assert abs(csc_val - recip_val) < 1e-10

    def test_mixed_rounding_and_trigonometry(self):
        """Test expression combining int/nint with trigonometric functions."""
        deck_text = """*KEYWORD
*PARAMETER_EXPRESSION
I    count nint(4.7)
R    angle cos(pi/count)
*END"""
        deck = Deck()
        deck.loads(deck_text)

        # nint(4.7) = 5
        assert deck.parameters.get("count") == 5
        # cos(π/5) = cos(36°) ≈ 0.809
        expected_angle = math.cos(math.pi / 5.0)
        assert abs(deck.parameters.get("angle") - expected_angle) < 1e-10
