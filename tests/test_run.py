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

import os
import pathlib

import pytest

from ansys.dyna.run import run_dyna
from .conftest import resolve_test_file


@pytest.mark.run
def test_run_from_input_file_001():
    input = resolve_test_file("i.k", "run/basic-eddy-current")
    example_folder = str(pathlib.Path(input).parent.resolve())
    input = os.path.join(example_folder, "i.k")
    try:
        wdir = run_dyna(input)
        assert wdir == example_folder
        assert os.path.isfile(os.path.join(example_folder, "d3plot"))
        if os.name == "nt":
            assert os.path.isfile(os.path.join(example_folder, "lsrun.out.txt"))
    except Exception as e:
        # TODO use a fixture for this?
        raise e
    finally:
        generated_files = [f for f in os.listdir(example_folder) if not f.endswith(".k")]
        for file in generated_files:
            os.remove(os.path.join(example_folder, file))