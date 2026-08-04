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

"""Module providing the IcfdModelViscoelastic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDMODELVISCOELASTIC_CARD0 = (
    FieldSchema("vid", int, 0, 10, None),
    FieldSchema("vtype", int, 10, 10, None),
)

_ICFDMODELVISCOELASTIC_CARD1 = (
    FieldSchema("rtime", float, 0, 10, None),
    FieldSchema("pvisc", float, 10, 10, None),
    FieldSchema("mobf", float, 20, 10, None),
)

class IcfdModelViscoelastic(KeywordBase):
    """DYNA ICFD_MODEL_VISCOELASTIC keyword"""

    keyword = "ICFD"
    subkeyword = "MODEL_VISCOELASTIC"

    def __init__(self, **kwargs):
        """Initialize the IcfdModelViscoelastic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDMODELVISCOELASTIC_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDMODELVISCOELASTIC_CARD1,
                **kwargs,
            ),
        ]
    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Viscoelastic model ID
        """ # nopep8
        return self._cards[0].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[0].set_value("vid", value)

    @property
    def vtype(self) -> typing.Optional[int]:
        """Get or set the Viscoelastic model type:
        EQ.1: Oldroyd - B model
        """ # nopep8
        return self._cards[0].get_value("vtype")

    @vtype.setter
    def vtype(self, value: int) -> None:
        """Set the vtype property."""
        self._cards[0].set_value("vtype", value)

    @property
    def rtime(self) -> typing.Optional[float]:
        """Get or set the Relaxation time
        """ # nopep8
        return self._cards[1].get_value("rtime")

    @rtime.setter
    def rtime(self, value: float) -> None:
        """Set the rtime property."""
        self._cards[1].set_value("rtime", value)

    @property
    def pvisc(self) -> typing.Optional[float]:
        """Get or set the Polymeric viscosity
        """ # nopep8
        return self._cards[1].get_value("pvisc")

    @pvisc.setter
    def pvisc(self, value: float) -> None:
        """Set the pvisc property."""
        self._cards[1].set_value("pvisc", value)

    @property
    def mobf(self) -> typing.Optional[float]:
        """Get or set the Mobility factor (for VTYPE=2 only)
        """ # nopep8
        return self._cards[1].get_value("mobf")

    @mobf.setter
    def mobf(self, value: float) -> None:
        """Set the mobf property."""
        self._cards[1].set_value("mobf", value)

