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

import pathlib
import typing


def handle_single_word_keyword(keyword: str) -> str:
    tokens = keyword.split("_")
    if len(tokens) == 2 and tokens[0] == tokens[1]:
        return tokens[0]
    return keyword


def fix_keyword(keyword: str) -> str:
    """Returns a "fixed" keyword in two ways:
    - a single word keyword will be defined from the kwdm as NAME_NAME,
      and the fixed keyword is just NAME
    - some keywords are not python and filesystem friendly, for example:
      MAT_BILKHU/DUBOIS_FOAM becomes MAT_BILKHU_DUBOIS_FOAM
    """
    keyword = handle_single_word_keyword(keyword)
    for bad_char in ["/", "-", " ", "(", ")"]:
        keyword = keyword.replace(bad_char, "_")
    return keyword


def get_classname(keyword: str):
    """Convert CLASS_NAME_FOO to ClassNameFoo"""
    tokens = keyword.split("_")
    return "".join([word.title() for word in tokens])


def get_this_folder():
    return pathlib.Path(__file__).parent.parent.parent


def get_license_header() -> str:
    with open(get_this_folder() / "license_header.txt", "r", encoding="utf-8") as f:
        return f.read()
