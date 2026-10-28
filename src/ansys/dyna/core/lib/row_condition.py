# Copyright (C) 2023 - 2026 Synopsys, Inc. and ANSYS, Inc. All rights reserved.
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

"""Per-row conditions for the sub-cards of a ``TableCardGroup``.

Each row of a ``TableCardGroup`` is written as one data line per sub-card. A row
condition makes a sub-card's line depend on the row's own values, for example the
THIC5-THIC8 line of ``*ELEMENT_SHELL_THICKNESS``, which only elements with mid-side
nodes have.
"""

import dataclasses
import math
import typing

import numpy as np
import pandas as pd


def is_missing(value: typing.Any) -> bool:
    """Return whether a field value is unset: ``None``, ``pd.NA`` or NaN."""
    return value is None or value is pd.NA or (isinstance(value, float) and math.isnan(value))


@dataclasses.dataclass(frozen=True)
class RowCondition:
    """Condition deciding, row by row, whether a sub-card of a ``TableCardGroup`` has a data line.

    Parameters
    ----------
    fields : tuple of str
        Names of the fields the condition reads. They must belong to earlier sub-cards of the group,
        because a reader only knows the values of the lines it has already read.
    evaluate_row : callable
        Called with the values of ``fields`` of a single row, in order. Values may be missing
        (see ``is_missing``). Returns whether the sub-card has a line in that row.
    evaluate_all : callable, optional
        Called with a table holding at least the ``fields`` columns. Returns one boolean per row
        and must agree with ``evaluate_row``. When omitted, ``evaluate_row`` is applied to each row.
    """

    fields: typing.Tuple[str, ...]
    evaluate_row: typing.Callable[..., bool]
    evaluate_all: typing.Optional[typing.Callable[[pd.DataFrame], typing.Any]] = None

    def __post_init__(self):
        if not self.fields:
            raise ValueError("A row condition must read at least one field.")

    def evaluate(self, table: pd.DataFrame) -> np.ndarray:
        """Return one boolean per row of ``table``, ``True`` where the sub-card has a line."""
        num_rows = len(table)
        if num_rows == 0:
            return np.zeros(0, dtype=bool)
        if self.evaluate_all is None:
            columns = [table[name].tolist() for name in self.fields]
            return np.fromiter((bool(self.evaluate_row(*values)) for values in zip(*columns)), bool, num_rows)
        mask = np.asarray(self.evaluate_all(table))
        if mask.dtype != np.bool_ or mask.shape != (num_rows,):
            raise ValueError(
                f"The row condition on {list(self.fields)} must give {num_rows} booleans, "
                f"got an array of dtype {mask.dtype} and shape {mask.shape}."
            )
        return mask
