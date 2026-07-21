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

"""Module providing the Iga2DBasisTransformXyz class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGA2DBASISTRANSFORMXYZ_CARD0 = (
    FieldSchema("patchid", int, 0, 10, None),
    FieldSchema("filename", str, 10, 70, None),
)

class Iga2DBasisTransformXyz(KeywordBase):
    """DYNA IGA_2D_BASIS_TRANSFORM_XYZ keyword"""

    keyword = "IGA"
    subkeyword = "2D_BASIS_TRANSFORM_XYZ"

    def __init__(self, **kwargs):
        """Initialize the Iga2DBasisTransformXyz class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGA2DBASISTRANSFORMXYZ_CARD0,
                **kwargs,
            ),
        ]
    @property
    def patchid(self) -> typing.Optional[int]:
        """Get or set the Physical bivariate (rational) spline patch ID.
        """ # nopep8
        return self._cards[0].get_value("patchid")

    @patchid.setter
    def patchid(self, value: int) -> None:
        """Set the patchid property."""
        self._cards[0].set_value("patchid", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of file containing patch data.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

