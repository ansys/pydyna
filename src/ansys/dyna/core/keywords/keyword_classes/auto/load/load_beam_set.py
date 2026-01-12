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

"""Module providing the LoadBeamSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADBEAMSET_CARD0 = (
    FieldSchema("esid", int, 0, 10, None),
    FieldSchema("dal", int, 10, 10, 1),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("sf", float, 30, 10, 1.0),
)

class LoadBeamSet(KeywordBase):
    """DYNA LOAD_BEAM_SET keyword"""

    keyword = "LOAD"
    subkeyword = "BEAM_SET"

    def __init__(self, **kwargs):
        """Initialize the LoadBeamSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADBEAMSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def esid(self) -> typing.Optional[int]:
        """Get or set the Beam element set ID, see *SET_BEAM.
        """ # nopep8
        return self._cards[0].get_value("esid")

    @esid.setter
    def esid(self, value: int) -> None:
        """Set the esid property."""
        self._cards[0].set_value("esid", value)

    @property
    def dal(self) -> int:
        """Get or set the Direction of applied load:
        EQ.1: along r-axis of beam (default),
        EQ.2: along s-axis of beam,
        EQ.3: along t-axis of beam.
        """ # nopep8
        return self._cards[0].get_value("dal")

    @dal.setter
    def dal(self, value: int) -> None:
        """Set the dal property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""dal must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("dal", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE or *DEFINE_FUNCTION.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor. This is for a simple modification of the function values of the load curve.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

