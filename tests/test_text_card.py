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

from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.text_card import TextCard

import pytest


def test_assign_text_card():
    """test assigning one-line value to text card"""
    t = TextCard("function", "f(a,b,c)=a*2+b*c+sqrt(a*c)")
    assert t.value == "f(a,b,c)=a*2+b*c+sqrt(a*c)"
    assert len(t._content_lines) == 1
    assert t._content_lines[0] == "f(a,b,c)=a*2+b*c+sqrt(a*c)"
    ref = "$#                                                                      function"
    assert t._get_comment(format_type.default) == ref



def test_read_text_card(string_utils):
    card_text = """1,x-velo
x(t)=1000*sin(100*t)"""
    t = TextCard("function")
    t.read(string_utils.as_buffer(card_text))
    assert t.value == card_text
    assert len(t._content_lines) == 2
    assert t._content_lines[1] == "x(t)=1000*sin(100*t)"



def test_text_card_write_long(ref_string):
    """test writing long format text card."""
    t = TextCard("function", "f(a,b,c)=a*2+b*c+sqrt(a*c)")
    assert t.value == "f(a,b,c)=a*2+b*c+sqrt(a*c)"
    assert len(t._content_lines) == 1
    assert t._content_lines[0] == "f(a,b,c)=a*2+b*c+sqrt(a*c)"
    assert t._get_comment(format_type.long) == ref_string.test_text_card_long



def test_text_card_read_long(string_utils):
    """test reading long format text card."""
    t = TextCard("function", format=format_type.long)
    # read one long line
    data = "-" * 160
    t.read(string_utils.as_buffer(data))
    assert len(t._content_lines) == 1
    assert t.value == data
