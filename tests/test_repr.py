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

import numpy as np
import pandas as pd

from ansys.dyna.core import keywords as kwd

import pytest


def test_repr(ref_string):
    length = 60
    node_ids = np.arange(length) + 1
    xs = np.zeros(length) + 0.1
    ys = np.zeros(length) + 0.2
    zs = np.zeros(length) + 0.3
    df = pd.DataFrame({"nid": node_ids, "x": xs, "y": ys, "z": zs})
    n = kwd.Node()
    n.nodes = df
    value = repr(n)
    assert value == ref_string.test_repr_truncate


def test_repr_options(ref_string):
    c = kwd.ContactAutomaticSingleSurface()
    value = repr(c.options)
    assert value == ref_string.test_repr_options
