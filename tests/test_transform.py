
import math
import re

from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.lib.transforms.utils.define_transformation import get_transform_matrix

import numpy as np
import pandas as pd
import pytest
import transformations as tfm


def test_transform_none():
    """Verify no transformation for no input."""
    mtx = get_transform_matrix(None)
    assert mtx is None


def test_transform_empty():
    """Verify no transfomration matrix for an empty DEFINE_TRANFORMATION."""
    define_transform_kwd = kwd.DefineTransformation()
    mtx = get_transform_matrix(None)
    assert mtx is None


def test_transform_rotation_1():
    """Verify the transfomration matrix for single ROTATE."""

    # The transformation of a rotation of 45 degrees about the X axis
    define_transform_kwd = kwd.DefineTransformation(option="ROTATE", a1=1, a2=0, a3=0, a7=45)
    mtx = get_transform_matrix(define_transform_kwd)
    ref_45x = tfm.rotation_matrix(math.radians(45),[1,0,0])
    assert np.allclose(mtx, ref_45x)

    # not yet supported if a4-a7 are zero
    define_transform_kwd = kwd.DefineTransformation(option="ROTATE", a1=1, a2=0, a3=0)
    expected_warning = "DEFINE_TRANFORMATION ROTATE option with parameters (1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) not handled yet by pydyna!"
    expected_warning_expression = re.escape(expected_warning)
    with pytest.warns(UserWarning, match=expected_warning_expression):
        mtx = get_transform_matrix(define_transform_kwd)
    assert mtx is None

    # not yet supported if a4-a7 are zero
    define_transform_kwd = kwd.DefineTransformation(option="ROTATE", a1=0, a2=0, a3=0, a7=0)
    expected_warning = "DEFINE_TRANFORMATION ROTATE option with parameters (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) not handled yet by pydyna!"
    expected_warning_expression = re.escape(expected_warning)
    with pytest.warns(UserWarning, match=expected_warning_expression):
        mtx = get_transform_matrix(define_transform_kwd)
    assert mtx is None

    # 360 degrees about any arbitrary axis is the identity matrix
    define_transform_kwd = kwd.DefineTransformation(option="ROTATE", a1=0.4, a2=3.2, a3=1.1, a7=360.0)
    mtx = get_transform_matrix(define_transform_kwd)
    ref_i = tfm.identity_matrix()
    assert np.allclose(mtx,ref_i)

    # a1, a2, a3 cannot be zero if the angle of rotation is nonzero
    define_transform_kwd = kwd.DefineTransformation(option="ROTATE", a1=0, a2=0, a3=0, a7=90)
    with pytest.raises(ValueError, match="Direction vector A1, A2, A3 cannot be all zero!"):
        get_transform_matrix(define_transform_kwd)


def test_transform_matrix_two_translations():
    """Verify the transformation matrix for multiple TRANSL"""
    define_transform_kwd = kwd.DefineTransformation()
    define_transform_kwd.transforms = pd.DataFrame(
        {
            "option": ["TRANSL", "TRANSL"],
            "a1": [0.0, -100.0],
            "a2": [0.0, 0.0],
            "a3": [200.0, 0.0],
        }
    )
    mtx = get_transform_matrix(define_transform_kwd)
    ref = tfm.translation_matrix((-100,0,200))
    assert np.allclose(mtx, ref)


def test_transform_matrix_one_translations():
    """Verify the transformation matrix for a single TRANSL."""
    define_transform_kwd = kwd.DefineTransformation(option="TRANSL", a1=-200, a2=0.0, a3=0.0)
    mtx = get_transform_matrix(define_transform_kwd)
    ref = tfm.translation_matrix((-200,0,0))
    assert np.allclose(mtx, ref)


def test_transform_matrix_translation_rotation():
    """Verify the transformation matrix for a single TRANSL."""
    define_transform_kwd = kwd.DefineTransformation()
    define_transform_kwd.transforms = pd.DataFrame(
        {
            "option": ["ROTATE", "TRANSL"],
            "a1": [0.0, -100.0],
            "a2": [0.0, 0.0],
            "a3": [1.0, 0.0],
            "a4": [0.0, 0.0],
            "a5": [0.0, 0.0],
            "a6": [0.0, 0.0],
            "a7": [25.0, 0.0],
        }
    )
    t_25z = tfm.rotation_matrix(math.radians(25),[0,0,1])
    t_trans = tfm.translation_matrix((-100,0,0))
    ref = tfm.concatenate_matrices(t_25z, t_trans)
    mtx = get_transform_matrix(define_transform_kwd)
    assert np.allclose(mtx, ref)


def test_transform_matrix_one_scale():
    """Verify the transformation matrix for a single SCALE."""
    define_transform_kwd = kwd.DefineTransformation(option="SCALE", a1=1.0, a2=-1.0, a3=1.0)
    mtx = get_transform_matrix(define_transform_kwd)
    ref = tfm.scale_matrix(factor=-1.0, direction=[0,1,0])
    assert np.allclose(mtx, ref)



def test_transform_unhandled():
    """Verify warning and no transformation when an unhandled DEFINE_TRANSFORMATION option is used."""
    option = "TRANSL2ND"
    define_transform_kwd = kwd.DefineTransformation(option=option)
    with pytest.warns(UserWarning, match=f"DEFINE_TRANFORMATION option {option} not handled yet by pydyna!"):
        mtx = get_transform_matrix(define_transform_kwd)
    assert mtx is None
