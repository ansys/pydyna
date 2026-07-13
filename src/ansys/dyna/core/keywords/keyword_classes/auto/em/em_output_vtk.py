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

"""Module providing the EmOutputVtk class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMOUTPUTVTK_CARD0 = (
    FieldSchema("vtktype", int, 0, 10, 1),
    FieldSchema("vtkt", float, 10, 10, None),
)

class EmOutputVtk(KeywordBase):
    """DYNA EM_OUTPUT_VTK keyword"""

    keyword = "EM"
    subkeyword = "OUTPUT_VTK"

    def __init__(self, **kwargs):
        """Initialize the EmOutputVtk class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMOUTPUTVTK_CARD0,
                **kwargs,
            ),
        ]
    @property
    def vtktype(self) -> int:
        """Get or set the Type of the vtk files output.
        EQ.1: a single.vtk file is created.
        EQ.2: Parallel unstructured points data(.pvtu files), recommended in mpp executions.
        """ # nopep8
        return self._cards[0].get_value("vtktype")

    @vtktype.setter
    def vtktype(self, value: int) -> None:
        """Set the vtktype property."""
        if value not in [1, 2, None]:
            raise Exception("""vtktype must be `None` or one of {1,2}.""")
        self._cards[0].set_value("vtktype", value)

    @property
    def vtkt(self) -> typing.Optional[float]:
        """Get or set the Time period at which vtk files are exported.
        """ # nopep8
        return self._cards[0].get_value("vtkt")

    @vtkt.setter
    def vtkt(self, value: float) -> None:
        """Set the vtkt property."""
        self._cards[0].set_value("vtkt", value)

