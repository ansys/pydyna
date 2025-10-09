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

import pytest
from ansys.dyna.core.run import run_dyna


@pytest.mark.run
def test_run_from_input_file_001(file_utils, runner):
    input_file = file_utils.testfiles_folder / "run"/ "basic-eddy-current" / "test.k"
    example_folder = str(input_file.parent.resolve())
    try:
        wdir = runner.run("i.k", working_directory=example_folder)
        assert wdir == example_folder
        assert os.path.isfile(os.path.join(example_folder, "d3plot"))
    except Exception as e:
        # TODO use a fixture for this?
        raise e
    finally:
        generated_files = [f for f in os.listdir(example_folder) if not f.endswith(".k")]
        for file in generated_files:
            os.remove(os.path.join(example_folder, file))
            
            
@pytest.mark.run
def test_case_option(file_utils, runner):
    input_file = file_utils.testfiles_folder / "run"/ "case-keywords" / "projectile.k"

    example_folder = str(input_file.parent.resolve())
    input_file = str(input_file)
    try:
        wdir = runner.run(input_file, working_directory=example_folder, activate_case=True)
        assert wdir == example_folder
        assert os.path.isfile(os.path.join(example_folder, "ZERO_VELOCITY.d3plot"))
        assert os.path.isfile(os.path.join(example_folder, "LOW_VELOCITY.d3plot"))
    except Exception as e:
        # TODO use a fixture for this?
        raise e
    finally:
        generated_files = [f for f in os.listdir(example_folder) if not f.endswith(".k")]
        for file in generated_files:
            os.remove(os.path.join(example_folder, file))