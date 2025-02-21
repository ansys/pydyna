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

"""Algorithms to compute 4x4 transformation matrices from a *DEFINE_TRANSFORMATION keyword."""

import typing
import warnings

from ansys.dyna.core.keywords.keyword_classes.auto.define_transformation import DefineTransformation

import transformations as tfm
import numpy as np
import pandas as pd

def _get_translation_matrix(a1: float, a2: float, a3: float) -> np.ndarray:
    return tfm.translation_matrix((a1, a2, a3))

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
    mtx = tfm.identity_matrix()
    option, a1, a2, a3, a4, a5, a6, a7 = transform['option'],  transform['a1'], transform['a2'], transform['a3'], transform['a4'], transform['a5'], transform['a6'], transform['a7']
    if option == 'TRANSL':
        return _get_translation_matrix(a1, a2, a3)
    else:
        warnings.warn(f"DEFINE_TRANFORMATION option {option} not handled yet by pydyna!")
        return None

def _get_transform_matrix(transforms: pd.DataFrame) -> np.ndarray:
    mtx = tfm.identity_matrix()
    for index, transform in transforms.iterrows():
        mtx = tfm.concatenate_matrices(mtx, _get_row_transform_matrix(transform))
    return mtx

def get_transform_matrix(kwd: typing.Optional[DefineTransformation]) -> typing.Optional[np.ndarray]:
    if kwd is None:
        return None
    transforms = kwd.transforms
    if len(transforms) == 0:
        return None
    mtx = _get_transform_matrix(transforms)
    return mtx
