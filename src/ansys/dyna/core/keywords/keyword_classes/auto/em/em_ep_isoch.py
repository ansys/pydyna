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

"""Module providing the EmEpIsoch class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMEPISOCH_CARD0 = (
    FieldSchema("issoch", int, 0, 10, None),
    FieldSchema("idepol", int, 10, 10, 0),
    FieldSchema("dplthr", float, 20, 10, None),
    FieldSchema("irepol", int, 30, 10, 0),
    FieldSchema("rplthr", float, 40, 10, None),
    FieldSchema("cyclemin", float, 50, 10, 0.0),
    FieldSchema("apdmin", float, 60, 10, 0.0),
)

class EmEpIsoch(KeywordBase):
    """DYNA EM_EP_ISOCH keyword"""

    keyword = "EM"
    subkeyword = "EP_ISOCH"

    def __init__(self, **kwargs):
        """Initialize the EmEpIsoch class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPISOCH_CARD0,
                **kwargs,
            ),
        ]
    @property
    def issoch(self) -> typing.Optional[int]:
        """Get or set the ID  of the isochrone
        """ # nopep8
        return self._cards[0].get_value("issoch")

    @issoch.setter
    def issoch(self, value: int) -> None:
        """Set the issoch property."""
        self._cards[0].set_value("issoch", value)

    @property
    def idepol(self) -> int:
        """Get or set the Flag to activate the computation of depolarization:
        EQ.0: Off
        EQ.1: On
        """ # nopep8
        return self._cards[0].get_value("idepol")

    @idepol.setter
    def idepol(self, value: int) -> None:
        """Set the idepol property."""
        if value not in [0, 1, None]:
            raise Exception("""idepol must be `None` or one of {0,1}.""")
        self._cards[0].set_value("idepol", value)

    @property
    def dplthr(self) -> typing.Optional[float]:
        """Get or set the Amplitude threshold used for measuring depolarization
        """ # nopep8
        return self._cards[0].get_value("dplthr")

    @dplthr.setter
    def dplthr(self, value: float) -> None:
        """Set the dplthr property."""
        self._cards[0].set_value("dplthr", value)

    @property
    def irepol(self) -> int:
        """Get or set the Flag to activate the computation of repolarization times:
        EQ.0: Off
        EQ.1: On
        """ # nopep8
        return self._cards[0].get_value("irepol")

    @irepol.setter
    def irepol(self, value: int) -> None:
        """Set the irepol property."""
        if value not in [0, 1, None]:
            raise Exception("""irepol must be `None` or one of {0,1}.""")
        self._cards[0].set_value("irepol", value)

    @property
    def rplthr(self) -> typing.Optional[float]:
        """Get or set the Amplitude threshold used for measuring repolarization
        """ # nopep8
        return self._cards[0].get_value("rplthr")

    @rplthr.setter
    def rplthr(self, value: float) -> None:
        """Set the rplthr property."""
        self._cards[0].set_value("rplthr", value)

    @property
    def cyclemin(self) -> float:
        """Get or set the Minimum time between two consecutive depolarizations (used in the EP eikonal model with time stepping)
        """ # nopep8
        return self._cards[0].get_value("cyclemin")

    @cyclemin.setter
    def cyclemin(self, value: float) -> None:
        """Set the cyclemin property."""
        self._cards[0].set_value("cyclemin", value)

    @property
    def apdmin(self) -> float:
        """Get or set the Minimum time between depolarization and repolarization (used in the EP eikonal model with time stepping)
        """ # nopep8
        return self._cards[0].get_value("apdmin")

    @apdmin.setter
    def apdmin(self, value: float) -> None:
        """Set the apdmin property."""
        self._cards[0].set_value("apdmin", value)

