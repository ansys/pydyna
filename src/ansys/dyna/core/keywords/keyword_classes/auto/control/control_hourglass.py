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

"""Module providing the ControlHourglass class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLHOURGLASS_CARD0 = (
    FieldSchema("ihq", int, 0, 10, None),
    FieldSchema("qh", float, 10, 10, 0.1),
)

class ControlHourglass(KeywordBase):
    """DYNA CONTROL_HOURGLASS keyword"""

    keyword = "CONTROL"
    subkeyword = "HOURGLASS"

    def __init__(self, **kwargs):
        """Initialize the ControlHourglass class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLHOURGLASS_CARD0,
                **kwargs,
            ),        ]
    @property
    def ihq(self) -> typing.Optional[int]:
        """Get or set the Default hourglass viscosity type:
        EQ.1: standard LS-DYNA,
        EQ.2: Flanagan-Belytschko integration,
        EQ.3: Flanagan-Belytschko with exact volume integration,
        EQ.4: stiffness form of type 2 (Flanagan-Belytschko),
        EQ.5: stiffness form of type 3 (Flanagan-Belytschko),
        EQ:6: Belytschko-Bindeman assumed strain co-rotational stiffness form for 2D and 3D solid elements only. Mandatory for implicit analysis.
        EQ.7: Linear total strain form of type 6 hourglass control.  This form is available for explicit and implicit solution method (See remark 6 below).
        EQ:8 Applicable to the type 16 fully integrated shell element.
        IHQ=8 Activates full projection warping stiffness for shell formulations 16 and -16, and is the default for these shell formulations.  A speed penalty of 25% is common for this option.
        EQ.9: Puso [2000] enhanced assumed strain stiffness form for 3D hexahedral elements.
        EQ.10: Cosserat Point Element (CPE) developed by Jabareen and Rubin [2008].
        """ # nopep8
        return self._cards[0].get_value("ihq")

    @ihq.setter
    def ihq(self, value: int) -> None:
        """Set the ihq property."""
        self._cards[0].set_value("ihq", value)

    @property
    def qh(self) -> float:
        """Get or set the Hourglass coefficient, QH (default = 0.1). Values of QH that exceed 0.15 may cause instabilities.
        """ # nopep8
        return self._cards[0].get_value("qh")

    @qh.setter
    def qh(self, value: float) -> None:
        """Set the qh property."""
        self._cards[0].set_value("qh", value)

