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

"""Transformation handler for *NODE."""

import warnings

import numpy as np
import pandas as pd

from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.lib.transforms.base_transform import Transform
from ansys.dyna.core.lib.transforms.utils.define_transformation import get_transform_matrix


def apply_rigid_transform(mtx: np.ndarray, nodes: pd.DataFrame) -> None:
    locations = nodes[["x", "y", "z"]]
    locations = locations.fillna(0.0)
    locations["w"] = 1.0
    locs = locations.to_numpy()
    for i, loc in enumerate(locs):
        res = np.dot(mtx, loc)  # transform the point
        locs[i] = res

    locations[["x", "y", "z", "w"]] = locs
    nodes[["x", "y", "z"]] = locations[["x", "y", "z"]]


class TransformNode(Transform):
    def transform(self, keyword: kwd.Node) -> None:
        self._apply_offset(keyword)
        try:
            self._apply_transform(keyword)
        except Exception as e:
            warnings.warn(f"Error applying transformation to *NODE: {e}")
            raise e

    def _apply_offset(self, keyword: kwd.Node) -> None:
        offset = self._xform.idnoff
        if offset is None or offset == 0:
            return
        keyword.nodes["nid"] = keyword.nodes["nid"] + offset

    def _apply_transform(self, keyword: kwd.Node) -> None:
        define_transform = self._xform.tranid_link
        mtx = get_transform_matrix(define_transform)
        if mtx is None:
            return
        apply_rigid_transform(mtx, keyword.nodes)
