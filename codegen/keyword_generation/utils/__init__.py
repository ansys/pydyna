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


def fix_keyword(keyword: str, preserve_hyphen_distinction: bool = True) -> str:
    """Returns a "fixed" keyword for filenames.

    - A single word keyword will be defined from the kwdm as NAME_NAME,
      and the fixed keyword is just NAME
    - Some keywords are not python and filesystem friendly, for example:
      MAT_BILKHU/DUBOIS_FOAM becomes MAT_BILKHU_DUBOIS_FOAM

    Args:
        keyword: The keyword to fix
        preserve_hyphen_distinction: If True, hyphens become double underscores
            to distinguish from regular underscores (e.g., SPRING-DAMPER becomes
            SPRING__DAMPER, not SPRING_DAMPER). This prevents filename collisions
            when both hyphen and underscore variants exist.
    """
    keyword = handle_single_word_keyword(keyword)
    for bad_char in ["/", " ", "(", ")"]:
        keyword = keyword.replace(bad_char, "_")
    # Handle hyphens: use double underscore to preserve distinction
    if preserve_hyphen_distinction:
        keyword = keyword.replace("-", "__")
    else:
        keyword = keyword.replace("-", "_")
    return keyword


def get_classname(keyword: str):
    """convert CLASS_NAME_FOO to ClassNameFoo.
    Hyphens are converted to single underscores to preserve distinction.
    E.g., ALE_MULTI-MATERIAL_GROUP becomes AleMulti_MaterialGroup
    while ALE_MULTI_MATERIAL_GROUP becomes AleMultiMaterialGroup.
    Slashes and other special chars are removed."""
    keyword = handle_single_word_keyword(keyword)
    # Remove slashes, spaces, parens (these don't need distinction in class names)
    for bad_char in ["/", " ", "(", ")"]:
        keyword = keyword.replace(bad_char, "_")
    # Replace hyphens with a placeholder, then process
    # We use single underscore in the class name for hyphens
    parts = []
    for segment in keyword.split("-"):
        # Each segment between hyphens becomes TitleCase tokens joined
        tokens = segment.split("_")
        parts.append("".join([word.title() for word in tokens]))
    return "_".join(parts)


def get_this_folder():
    return pathlib.Path(__file__).parent.parent.parent


def get_license_header() -> str:
    with open(get_this_folder() / "license_header.txt", "r", encoding="utf-8") as f:
        return f.read()
