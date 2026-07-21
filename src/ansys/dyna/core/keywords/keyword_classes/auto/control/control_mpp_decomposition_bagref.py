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

"""Module providing the ControlMppDecompositionBagref class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLMPPDECOMPOSITIONBAGREF_CARD0 = (
    FieldSchema("bagid1", int, 0, 10, None),
    FieldSchema("bagid2", int, 10, 10, None),
    FieldSchema("bagid3", int, 20, 10, None),
    FieldSchema("bagid4", int, 30, 10, None),
    FieldSchema("bagid5", int, 40, 10, None),
    FieldSchema("bagid6", int, 50, 10, None),
    FieldSchema("bagid7", int, 60, 10, None),
    FieldSchema("bagid8", int, 70, 10, None),
)

class ControlMppDecompositionBagref(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_BAGREF keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_BAGREF"

    def __init__(self, **kwargs):
        """Initialize the ControlMppDecompositionBagref class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLMPPDECOMPOSITIONBAGREF_CARD0,
                **kwargs,
            ),
        ]
    @property
    def bagid1(self) -> typing.Optional[int]:
        """Get or set the ID defined in *AIRBAG_?REFERENCE_?GEOMETRY_?ID or *AIRBAG_?SHELL_?REFERENCE_?GEOMETRY_?ID
        """ # nopep8
        return self._cards[0].get_value("bagid1")

    @bagid1.setter
    def bagid1(self, value: int) -> None:
        """Set the bagid1 property."""
        self._cards[0].set_value("bagid1", value)

    @property
    def bagid2(self) -> typing.Optional[int]:
        """Get or set the ID defined in *AIRBAG_?REFERENCE_?GEOMETRY_?ID or *AIRBAG_?SHELL_?REFERENCE_?GEOMETRY_?ID
        """ # nopep8
        return self._cards[0].get_value("bagid2")

    @bagid2.setter
    def bagid2(self, value: int) -> None:
        """Set the bagid2 property."""
        self._cards[0].set_value("bagid2", value)

    @property
    def bagid3(self) -> typing.Optional[int]:
        """Get or set the ID defined in *AIRBAG_?REFERENCE_?GEOMETRY_?ID or *AIRBAG_?SHELL_?REFERENCE_?GEOMETRY_?ID
        """ # nopep8
        return self._cards[0].get_value("bagid3")

    @bagid3.setter
    def bagid3(self, value: int) -> None:
        """Set the bagid3 property."""
        self._cards[0].set_value("bagid3", value)

    @property
    def bagid4(self) -> typing.Optional[int]:
        """Get or set the ID defined in *AIRBAG_?REFERENCE_?GEOMETRY_?ID or *AIRBAG_?SHELL_?REFERENCE_?GEOMETRY_?ID
        """ # nopep8
        return self._cards[0].get_value("bagid4")

    @bagid4.setter
    def bagid4(self, value: int) -> None:
        """Set the bagid4 property."""
        self._cards[0].set_value("bagid4", value)

    @property
    def bagid5(self) -> typing.Optional[int]:
        """Get or set the ID defined in *AIRBAG_?REFERENCE_?GEOMETRY_?ID or *AIRBAG_?SHELL_?REFERENCE_?GEOMETRY_?ID
        """ # nopep8
        return self._cards[0].get_value("bagid5")

    @bagid5.setter
    def bagid5(self, value: int) -> None:
        """Set the bagid5 property."""
        self._cards[0].set_value("bagid5", value)

    @property
    def bagid6(self) -> typing.Optional[int]:
        """Get or set the ID defined in *AIRBAG_?REFERENCE_?GEOMETRY_?ID or *AIRBAG_?SHELL_?REFERENCE_?GEOMETRY_?ID
        """ # nopep8
        return self._cards[0].get_value("bagid6")

    @bagid6.setter
    def bagid6(self, value: int) -> None:
        """Set the bagid6 property."""
        self._cards[0].set_value("bagid6", value)

    @property
    def bagid7(self) -> typing.Optional[int]:
        """Get or set the ID defined in *AIRBAG_?REFERENCE_?GEOMETRY_?ID or *AIRBAG_?SHELL_?REFERENCE_?GEOMETRY_?ID
        """ # nopep8
        return self._cards[0].get_value("bagid7")

    @bagid7.setter
    def bagid7(self, value: int) -> None:
        """Set the bagid7 property."""
        self._cards[0].set_value("bagid7", value)

    @property
    def bagid8(self) -> typing.Optional[int]:
        """Get or set the ID defined in *AIRBAG_?REFERENCE_?GEOMETRY_?ID or *AIRBAG_?SHELL_?REFERENCE_?GEOMETRY_?ID
        """ # nopep8
        return self._cards[0].get_value("bagid8")

    @bagid8.setter
    def bagid8(self, value: int) -> None:
        """Set the bagid8 property."""
        self._cards[0].set_value("bagid8", value)

