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

"""Module providing the Contact1D class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTACT1D_CARD0 = (
    FieldSchema("nsidr", int, 0, 10, None),
    FieldSchema("nsidc", int, 10, 10, None),
    FieldSchema("err", float, 20, 10, 0.0),
    FieldSchema("sigc", float, 30, 10, 0.0),
    FieldSchema("gb", float, 40, 10, 0.0),
    FieldSchema("smax", float, 50, 10, 0.0),
    FieldSchema("exp", float, 60, 10, 0.0),
)

class Contact1D(KeywordBase):
    """DYNA CONTACT_1D keyword"""

    keyword = "CONTACT"
    subkeyword = "1D"
    _link_fields = {
        "nsidr": LinkType.SET_NODE,
        "nsidc": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the Contact1D class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACT1D_CARD0,
                **kwargs,
            ),        ]
    @property
    def nsidr(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID for the rebar nodes that slide along the concrete; see* SET_NODE
        """ # nopep8
        return self._cards[0].get_value("nsidr")

    @nsidr.setter
    def nsidr(self, value: int) -> None:
        """Set the nsidr property."""
        self._cards[0].set_value("nsidr", value)

    @property
    def nsidc(self) -> typing.Optional[int]:
        """Get or set the Nodal set ID for the concrete nodes that the rebar nodes may slide along; see* SET_NODE
        """ # nopep8
        return self._cards[0].get_value("nsidc")

    @nsidc.setter
    def nsidc(self, value: int) -> None:
        """Set the nsidc property."""
        self._cards[0].set_value("nsidc", value)

    @property
    def err(self) -> float:
        """Get or set the External radius of rebar.
        """ # nopep8
        return self._cards[0].get_value("err")

    @err.setter
    def err(self, value: float) -> None:
        """Set the err property."""
        self._cards[0].set_value("err", value)

    @property
    def sigc(self) -> float:
        """Get or set the Compressive strength of concrete.
        """ # nopep8
        return self._cards[0].get_value("sigc")

    @sigc.setter
    def sigc(self, value: float) -> None:
        """Set the sigc property."""
        self._cards[0].set_value("sigc", value)

    @property
    def gb(self) -> float:
        """Get or set the Bond shear modulus.
        """ # nopep8
        return self._cards[0].get_value("gb")

    @gb.setter
    def gb(self, value: float) -> None:
        """Set the gb property."""
        self._cards[0].set_value("gb", value)

    @property
    def smax(self) -> float:
        """Get or set the Maximum shear strain displacement.
        """ # nopep8
        return self._cards[0].get_value("smax")

    @smax.setter
    def smax(self, value: float) -> None:
        """Set the smax property."""
        self._cards[0].set_value("smax", value)

    @property
    def exp(self) -> float:
        """Get or set the Exponent in damage curve.
        """ # nopep8
        return self._cards[0].get_value("exp")

    @exp.setter
    def exp(self, value: float) -> None:
        """Set the exp property."""
        self._cards[0].set_value("exp", value)

    @property
    def nsidr_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsidr."""
        return self._get_set_link("NODE", self.nsidr)

    @nsidr_link.setter
    def nsidr_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsidr."""
        self.nsidr = value.sid

    @property
    def nsidc_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsidc."""
        return self._get_set_link("NODE", self.nsidc)

    @nsidc_link.setter
    def nsidc_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsidc."""
        self.nsidc = value.sid

