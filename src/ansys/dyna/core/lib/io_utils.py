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

"""Utils for i/o."""

import io
import typing


def write_or_return(buf: typing.Optional[typing.TextIO], func: typing.Callable) -> typing.Optional[str]:
    """Write to buffer or returns a string.

    Uses the callable `func` to write. If `buf` is None, then the function will create a string buffer
    before calling `func` and return the result as a string.
    """
    if buf == None:
        to_return = True
        buf = io.StringIO()
    else:
        to_return = False
    func(buf)
    if to_return:
        retval = buf.getvalue()
        return retval


def is_dataframe(obj) -> bool:
    """Check if object is a pandas DataFrame using duck typing.

    Uses duck typing instead of isinstance() for Python 3.13+ compatibility,
    where pd.DataFrame is no longer a valid type for isinstance checks.

    Parameters
    ----------
    obj : Any
        Object to check.

    Returns
    -------
    bool
        True if obj has DataFrame-like attributes.
    """
    return hasattr(obj, "iloc") and hasattr(obj, "columns") and hasattr(obj, "index")
