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

"""Module providing the DualceseSolverHybridMultiphase class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESESOLVERHYBRIDMULTIPHASE_CARD0 = (
    FieldSchema("eosinid", int, 0, 10, None),
    FieldSchema("eosrctid", int, 10, 10, None),
    FieldSchema("eosprdid", int, 20, 10, None),
    FieldSchema("reactid", int, 30, 10, None),
)

class DualceseSolverHybridMultiphase(KeywordBase):
    """DYNA DUALCESE_SOLVER_HYBRID_MULTIPHASE keyword"""

    keyword = "DUALCESE"
    subkeyword = "SOLVER_HYBRID_MULTIPHASE"

    def __init__(self, **kwargs):
        """Initialize the DualceseSolverHybridMultiphase class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESESOLVERHYBRIDMULTIPHASE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def eosinid(self) -> typing.Optional[int]:
        """Get or set the EOS ID of the inert component of the multiphase mixture
        """ # nopep8
        return self._cards[0].get_value("eosinid")

    @eosinid.setter
    def eosinid(self, value: int) -> None:
        """Set the eosinid property."""
        self._cards[0].set_value("eosinid", value)

    @property
    def eosrctid(self) -> typing.Optional[int]:
        """Get or set the EOS ID of the reactant phase of the multiphase mixture
        """ # nopep8
        return self._cards[0].get_value("eosrctid")

    @eosrctid.setter
    def eosrctid(self, value: int) -> None:
        """Set the eosrctid property."""
        self._cards[0].set_value("eosrctid", value)

    @property
    def eosprdid(self) -> typing.Optional[int]:
        """Get or set the EOS ID of the product phase of the multiphase mixture
        """ # nopep8
        return self._cards[0].get_value("eosprdid")

    @eosprdid.setter
    def eosprdid(self, value: int) -> None:
        """Set the eosprdid property."""
        self._cards[0].set_value("eosprdid", value)

    @property
    def reactid(self) -> typing.Optional[int]:
        """Get or set the ID of chemical reaction rate model
        """ # nopep8
        return self._cards[0].get_value("reactid")

    @reactid.setter
    def reactid(self, value: int) -> None:
        """Set the reactid property."""
        self._cards[0].set_value("reactid", value)

