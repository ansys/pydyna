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

"""Module providing the InitialContactWear class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_INITIALCONTACTWEAR_CARD0 = (
    FieldSchema("cid", int, 0, 10, None),
    FieldSchema("nid", int, 10, 10, None),
    FieldSchema("wdepth", float, 20, 10, None),
    FieldSchema("nx", float, 30, 10, None),
    FieldSchema("ny", float, 40, 10, None),
    FieldSchema("nz", float, 50, 10, None),
    FieldSchema("iseq", int, 60, 10, None),
    FieldSchema("ncyc", int, 70, 10, None),
)

class InitialContactWear(KeywordBase):
    """DYNA INITIAL_CONTACT_WEAR keyword"""

    keyword = "INITIAL"
    subkeyword = "CONTACT_WEAR"
    _link_fields = {
        "nid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialContactWear class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALCONTACTWEAR_CARD0,
                **kwargs,
            ),        ]
    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the contact Interface ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def wdepth(self) -> typing.Optional[float]:
        """Get or set the wear depth, in units of length.
        """ # nopep8
        return self._cards[0].get_value("wdepth")

    @wdepth.setter
    def wdepth(self, value: float) -> None:
        """Set the wdepth property."""
        self._cards[0].set_value("wdepth", value)

    @property
    def nx(self) -> typing.Optional[float]:
        """Get or set the Direction vector for wear, internally normalized.
        """ # nopep8
        return self._cards[0].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        """Set the nx property."""
        self._cards[0].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[float]:
        """Get or set the Direction vector for wear, internally normalized.
        """ # nopep8
        return self._cards[0].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        """Set the ny property."""
        self._cards[0].set_value("ny", value)

    @property
    def nz(self) -> typing.Optional[float]:
        """Get or set the Direction vector for wear, internally normalized.
        """ # nopep8
        return self._cards[0].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        """Set the nz property."""
        self._cards[0].set_value("nz", value)

    @property
    def iseq(self) -> typing.Optional[int]:
        """Get or set the Simulation sequence number for the entire process.
        """ # nopep8
        return self._cards[0].get_value("iseq")

    @iseq.setter
    def iseq(self, value: int) -> None:
        """Set the iseq property."""
        self._cards[0].set_value("iseq", value)

    @property
    def ncyc(self) -> typing.Optional[int]:
        """Get or set the Number of process cycles this particular simulation corresponds to, a negative number means that LS-DYNA will not apply this card.
        """ # nopep8
        return self._cards[0].get_value("ncyc")

    @ncyc.setter
    def ncyc(self, value: int) -> None:
        """Set the ncyc property."""
        self._cards[0].set_value("ncyc", value)

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

