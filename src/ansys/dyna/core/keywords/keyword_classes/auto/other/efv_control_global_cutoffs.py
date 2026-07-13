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

"""Module providing the EfvControlGlobalCutoffs class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVCONTROLGLOBALCUTOFFS_CARD0 = (
    FieldSchema("velcut", float, 0, 10, 1e-06),
    FieldSchema("vellim", float, 10, 10, 10000000000.0),
    FieldSchema("radcut", float, 20, 10, 0.001),
    FieldSchema("strcut", float, 30, 10, 1e-10),
)

class EfvControlGlobalCutoffs(KeywordBase):
    """DYNA EFV_CONTROL_GLOBAL_CUTOFFS keyword"""

    keyword = "EFV"
    subkeyword = "CONTROL_GLOBAL_CUTOFFS"

    def __init__(self, **kwargs):
        """Initialize the EfvControlGlobalCutoffs class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVCONTROLGLOBALCUTOFFS_CARD0,
                **kwargs,
            ),
        ]
    @property
    def velcut(self) -> float:
        """Get or set the Minimum velocity below   which the velocities are set to 0.0.
        """ # nopep8
        return self._cards[0].get_value("velcut")

    @velcut.setter
    def velcut(self, value: float) -> None:
        """Set the velcut property."""
        self._cards[0].set_value("velcut", value)

    @property
    def vellim(self) -> float:
        """Get or set the Maximum velocity beyond which the velocities are set to VELLIM
        """ # nopep8
        return self._cards[0].get_value("vellim")

    @vellim.setter
    def vellim(self, value: float) -> None:
        """Set the vellim property."""
        self._cards[0].set_value("vellim", value)

    @property
    def radcut(self) -> float:
        """Get or set the Minimum radius scaling factor in an axisymmetric model. RADCUT scales the smallest nodal non-zero radius. If the resulting radius is larger than some nodal radiuses, these nodes are considered on the symmetry axis.
        """ # nopep8
        return self._cards[0].get_value("radcut")

    @radcut.setter
    def radcut(self, value: float) -> None:
        """Set the radcut property."""
        self._cards[0].set_value("radcut", value)

    @property
    def strcut(self) -> float:
        """Get or set the Minimum rate of deformation below which the strain rates are set to 0.0
        """ # nopep8
        return self._cards[0].get_value("strcut")

    @strcut.setter
    def strcut(self, value: float) -> None:
        """Set the strcut property."""
        self._cards[0].set_value("strcut", value)

