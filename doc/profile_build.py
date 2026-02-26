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

"""Profile the Sphinx documentation build."""

import os
from pathlib import Path
import sys

from pyinstrument import Profiler

# Set environment variables before importing Sphinx
os.environ["BUILD_EXAMPLES"] = "false"
os.environ["BUILD_AUTOKEYWORDS_API"] = "true"
os.environ["SPHINXJOBS"] = "1"

# Add source directory to path
doc_dir = Path(__file__).parent
source_dir = doc_dir / "source"
build_dir = doc_dir / "_build"

# Start profiling
profiler = Profiler()
profiler.start()

# Import and run Sphinx
from sphinx.cmd.build import main as sphinx_main

sys.argv = ["sphinx-build", "-M", "html", str(source_dir), str(build_dir), "-j", "1"]

try:
    sphinx_main(sys.argv[1:])
finally:
    # Stop profiling and save results
    profiler.stop()

    # Print to console
    print("\n" + "=" * 80)
    print("PROFILING RESULTS")
    print("=" * 80)
    profiler.print(show_all=False)

    # Save HTML report
    html_output = build_dir / "profile.html"
    with open(html_output, "w") as f:
        f.write(profiler.output_html())
    print(f"\nHTML profile saved to: {html_output}")
