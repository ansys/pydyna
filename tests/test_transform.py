import pytest

from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.lib.transforms.utils.define_transformation import get_transform_matrix

import numpy as np
import transformations as tfm

@pytest.mark.keywords
def test_transform_matrix_translation():
    """Test using a custom transform handler as an override."""
    define_transform_kwd = kwd.DefineTransformation(option="TRANSL", a1=-200, a2=0.0, a3=0.0)
    mtx = get_transform_matrix(define_transform_kwd)
    ref = tfm.translation_matrix((-200,0,0))

    assert np.allclose(mtx, ref)
