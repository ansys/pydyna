import typing
import warnings

import pandas as pd

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
        if not isinstance(elements, pd.DataFrame):
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
