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

"""Module for transforming *PART data.

NOTE: this only offsets the "pid" defined by the *PART card itself, matching what
TransformElement does for "pid" references on *ELEMENT_* cards. LS-DYNA's IDPOFF
actually applies to every part ID reference in the include file (e.g. *SET_PART,
*CONTACT_*, *INITIAL_VELOCITY_RIGID_BODY, ...). Handling those generically would
require walking each keyword's `_link_fields` for LinkType.PART, which is a larger
change left for a follow-up.
"""

import typing
import warnings

import pandas as pd

from ansys.dyna.core.lib.io_utils import is_dataframe
from ansys.dyna.core.lib.transform import Transform


class TransformPart(Transform):
    def transform(self, keyword: typing.Any):
        """Transform the part IDs defined by the given *PART keyword."""
        parts = self._get_parts_dataframe(keyword)
        if parts is None:
            return
        self._transform_part_ids(parts)

    def _get_parts_dataframe(self, keyword) -> typing.Optional[pd.DataFrame]:
        """Get the parts DataFrame from the keyword, if available."""
        warning = f"keyword {keyword.keyword}_{keyword.subkeyword} not transformed!"
        if not hasattr(keyword, "parts"):
            warnings.warn(warning)
            return None
        parts = keyword.parts
        if not is_dataframe(parts):
            warnings.warn(warning)
            return None
        return parts

    def _transform_part_ids(self, parts: pd.DataFrame):
        offset = self._xform.idpoff
        if offset is None or offset == 0:
            return
        if "pid" in parts:
            parts["pid"] = parts["pid"].mask(parts["pid"] != 0, parts["pid"] + offset)
