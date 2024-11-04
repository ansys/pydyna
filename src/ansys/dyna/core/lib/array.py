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

import array as ar
import math


def array(element_type: type, reserved_size: int = 0, default_value=None):
    """A resizable array that supports optional values for any type
    Right now - array.array is used for single and double precision floating points
    for everything else - python list is used.  This is because no existing array
    type in numpy, pandas, and python meet the above requirements.  Specifically,
    numpy integer arrays do not have optional values and are not resizable, pandas
    integer arrays support optional values but are also not resizable, while python
    array arrays are resizable but do not support optional values.

    The problem with this approach is memory usage.  For 100k integers, a python list
    appears to take about 5300K, while a pandas array and numpy array take 488K and 584K
    respectively.  pandas arrays take more memory than numpy because of the masking used
    to support optional integer values.

    Given a python list of optional integer, where None is used to represent a missing value,
    - this is how you convert to either type:
    numpy:  np.array([item or 0 for item in the_list], dtype=np.int32)
    pandas: pd.array(the_list,dtype=pd.Int32Dtype())

    In the future - A dynamic array class based on some of the above types can be used for
    integer arrays.  For string arrays, pandas arrays don't offer any value over python
    lists.
    """
    if element_type == float:
        arr = ar.array("d")
        if reserved_size == 0:
            return arr
        if default_value == None:
            default_value = math.nan
        for i in range(reserved_size):
            arr.append(default_value)
        return arr
    if reserved_size == 0:
        return list()
    else:
        return [default_value] * reserved_size
