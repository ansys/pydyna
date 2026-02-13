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

"""Algorithms to compute 4x4 transformation matrices from a ``*DEFINE_TRANSFORMATION`` keyword."""

import math
import typing
import warnings

import numpy as np
import pandas as pd
import transformations as tfm

from ansys.dyna.core.keywords.keyword_classes.auto.define.define_transformation import DefineTransformation


def _get_translation_matrix(a1: float, a2: float, a3: float) -> np.ndarray:
    return tfm.translation_matrix((a1, a2, a3))


def _get_scale_matrix(a1: float, a2: float, a3: float) -> np.ndarray:
    """Creates a 4x4 scaling matrix."""
    scale_x = tfm.scale_matrix(factor=a1, direction=[1.0, 0.0, 0.0])
    scale_y = tfm.scale_matrix(factor=a2, direction=[0.0, 1.0, 0.0])
    scale_z = tfm.scale_matrix(factor=a3, direction=[0.0, 0.0, 1.0])
    return tfm.concatenate_matrices(scale_x, scale_y, scale_z)


def _get_rotation_matrix(a1: float, a2: float, a3: float, a4: float, a5: float, a6: float, a7: float) -> np.ndarray:
    if (a4, a5, a6) == (0.0, 0.0, 0.0):
        if a7 != 0.0:
            # simple rotation about an axis going through the origin
            if np.isclose((a1, a2, a3), (0.0, 0.0, 0.0)).all():
                raise ValueError("Direction vector A1, A2, A3 cannot be all zero!")
            return tfm.rotation_matrix(math.radians(a7), [a1, a2, a3])
    parameters = (a1, a2, a3, a4, a5, a6, a7)
    warnings.warn(f"DEFINE_TRANFORMATION ROTATE option with parameters {parameters} not handled yet by pydyna!")
    return None


def _get_row_transform_matrix(transform: pd.Series) -> np.ndarray:
    """Transform is of a series of the form:

    option    ROTATE
    a1           0.0
    a2           0.0
    a3           1.0
    a4           0.0
    a5           0.0
    a6           0.0
    a7         -25.0

    whose behavior is according to the rules of *DEFINE_TRANSFORM
    """
    xf = transform.fillna(0.0)
    option, a1, a2, a3, a4, a5, a6, a7 = (
        xf["option"],
        xf["a1"],
        xf["a2"],
        xf["a3"],
        xf["a4"],
        xf["a5"],
        xf["a6"],
        xf["a7"],
    )
    if option == "TRANSL":
        return _get_translation_matrix(a1, a2, a3)
    elif option == "ROTATE":
        return _get_rotation_matrix(a1, a2, a3, a4, a5, a6, a7)
    elif option == "SCALE":
        return _get_scale_matrix(a1, a2, a3)
    else:
        warnings.warn(f"DEFINE_TRANFORMATION option {option} not handled yet by pydyna!")
        return None


def _get_transform_matrix(transforms: pd.DataFrame) -> np.ndarray:
    mtx = tfm.identity_matrix()
    for index, transform in transforms.iterrows():
        row_mtx = _get_row_transform_matrix(transform)
        if row_mtx is None:
            return None
        mtx = tfm.concatenate_matrices(mtx, row_mtx)
    return mtx


def get_transform_matrix(kwd: typing.Optional[DefineTransformation]) -> typing.Optional[np.ndarray]:
    r"""Get the 4x4 transformation matrix from a ``*DEFINE_TRANSFORMATION`` keyword."""
    if kwd is None:
        return None
    transforms = kwd.transforms
    if len(transforms) == 0:
        return None
    mtx = _get_transform_matrix(transforms)
    return mtx
