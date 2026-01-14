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
"""Module for transforming element data."""

import typing
import warnings

import pandas as pd

from ansys.dyna.core.lib.io_utils import is_dataframe
from ansys.dyna.core.lib.transform import Transform


class TransformElement(Transform):
    def transform(self, keyword: typing.Any):
        elements = self._get_elements_dataframe(keyword)
        if elements is None:
            return
        self._transform_node_ids(elements)
        self._transform_element_ids(elements)
        self._transform_part_ids(elements)

    def _get_elements_dataframe(self, keyword) -> typing.Optional[pd.DataFrame]:
        warning = f"keyword {keyword.keyword}_{keyword.subkeyword} not transformed!"
        if not hasattr(keyword, "elements"):
            warnings.warn(warning)
            return None
        elements = keyword.elements
        if not is_dataframe(elements):
            warnings.warn(warning)
            return None
        return elements

    def _offset_column(self, df: pd.DataFrame, column: str, offset: int) -> None:
        if column in df:
            # TODO - check if the value is na, not just != 0
            df[column] = df[column].mask(df[column] != 0, df[column] + offset)

    def _transform_node_ids(self, elements: pd.DataFrame):
        offset = self._xform.idnoff
        if offset is None or offset == 0:
            return
        self._offset_column(elements, "n1", offset)
        self._offset_column(elements, "n2", offset)
        self._offset_column(elements, "n3", offset)
        self._offset_column(elements, "n4", offset)
        self._offset_column(elements, "n5", offset)
        self._offset_column(elements, "n6", offset)
        self._offset_column(elements, "n7", offset)
        self._offset_column(elements, "n8", offset)

    def _transform_element_ids(self, elements: pd.DataFrame):
        offset = self._xform.ideoff
        if offset is None or offset == 0:
            return
        self._offset_column(elements, "eid", offset)

    def _transform_part_ids(self, elements: pd.DataFrame):
        offset = self._xform.idpoff
        if offset is None or offset == 0:
            return
        self._offset_column(elements, "pid", offset)
