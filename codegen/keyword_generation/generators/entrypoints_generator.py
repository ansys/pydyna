# Copyright (C) 2021 - 2025 ANSYS, Inc. and/or its affiliates.
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

import os
import pathlib
import typing

from jinja2 import Environment
from keyword_generation.utils import get_license_header


def generate_entrypoints(env: Environment, lib_path: str, keywords_list: typing.List[typing.Dict]) -> None:
    """use templates to write keywords/type_mapping.py, keywords/__init__.py and touch keywords/auto/__init__.py"""
    license_header = get_license_header()
    keywords_lists = {"license": license_header, "keywords": keywords_list}
    with open(os.path.join(lib_path, "auto_keywords.py"), "w", encoding="utf-8") as f:
        f.write(env.get_template("importer.j2").render(**keywords_lists))

    with open(os.path.join(lib_path, "type_mapping.py"), "w", encoding="utf-8") as f:
        f.write(env.get_template("type-mapping.j2").render(**keywords_lists))

    with open(pathlib.Path(lib_path) / "auto" / "__init__.py", "w", encoding="utf-8") as f:
        f.write(license_header)
