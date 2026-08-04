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

"""Module providing the IcfdDefineResidencetimesource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ICFDDEFINERESIDENCETIMESOURCE_CARD0 = (
    FieldSchema("rtsid", int, 0, 10, None),
    FieldSchema("ishape", int, 10, 10, None),
    FieldSchema("r", float, 20, 10, None),
    FieldSchema("ptid1", int, 30, 10, None),
    FieldSchema("ptid2", int, 40, 10, None),
    FieldSchema("massdiff", float, 50, 10, 1e-06),
    FieldSchema("deatht", float, 60, 10, None),
    FieldSchema("irt0pbc", int, 70, 10, 10),
)

_ICFDDEFINERESIDENCETIMESOURCE_CARD1 = (
    FieldSchema("n_ptid", int, 0, 10, None, "n-ptid"),
    FieldSchema("n_norid", int, 10, 10, None, "n-norid"),
)

class IcfdDefineResidencetimesource(KeywordBase):
    """DYNA ICFD_DEFINE_RESIDENCETIMESOURCE keyword"""

    keyword = "ICFD"
    subkeyword = "DEFINE_RESIDENCETIMESOURCE"
    _link_fields = {
        "n_ptid": LinkType.NODE,
        "n_norid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdDefineResidencetimesource class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINERESIDENCETIMESOURCE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDDEFINERESIDENCETIMESOURCE_CARD1,
                **kwargs,
            ),
        ]
    @property
    def rtsid(self) -> typing.Optional[int]:
        """Get or set the RT source ID
        """ # nopep8
        return self._cards[0].get_value("rtsid")

    @rtsid.setter
    def rtsid(self, value: int) -> None:
        """Set the rtsid property."""
        self._cards[0].set_value("rtsid", value)

    @property
    def ishape(self) -> typing.Optional[int]:
        """Get or set the Shape of the volumetric RT source:
        EQ.1: Box
        EQ.2: Cylinder
        EQ.3: Sphere
        EQ.4: N-planes (See Remark 1)
        """ # nopep8
        return self._cards[0].get_value("ishape")

    @ishape.setter
    def ishape(self, value: int) -> None:
        """Set the ishape property."""
        self._cards[0].set_value("ishape", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the cylinder if ISHAPE = 2 or radius of the sphere if ISHAPE = 3
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[0].set_value("r", value)

    @property
    def ptid1(self) -> typing.Optional[int]:
        """Get or set the ID of a point (see *ICFD_DEFINE_POINT) giving the minimum coordinate of the box if ISHAPE = 1, the tail point for the cylinder if ISHAPE = 2, or the center of the sphere if ISHAPE = 3
        """ # nopep8
        return self._cards[0].get_value("ptid1")

    @ptid1.setter
    def ptid1(self, value: int) -> None:
        """Set the ptid1 property."""
        self._cards[0].set_value("ptid1", value)

    @property
    def ptid2(self) -> typing.Optional[int]:
        """Get or set the ID of a point giving the maximum coordinate of the box if ISHAPE = 1 or the head point of the cylinder if ISHAPE = 2.
        """ # nopep8
        return self._cards[0].get_value("ptid2")

    @ptid2.setter
    def ptid2(self, value: int) -> None:
        """Set the ptid2 property."""
        self._cards[0].set_value("ptid2", value)

    @property
    def massdiff(self) -> float:
        """Get or set the Mass diffusion for the transport equation
        """ # nopep8
        return self._cards[0].get_value("massdiff")

    @massdiff.setter
    def massdiff(self, value: float) -> None:
        """Set the massdiff property."""
        self._cards[0].set_value("massdiff", value)

    @property
    def deatht(self) -> typing.Optional[float]:
        """Get or set the End time for the source.
        EQ.0.0: End time of the simulation
        """ # nopep8
        return self._cards[0].get_value("deatht")

    @deatht.setter
    def deatht(self, value: float) -> None:
        """Set the deatht property."""
        self._cards[0].set_value("deatht", value)

    @property
    def irt0pbc(self) -> int:
        """Get or set the Flag for which prescribed boundaries RT = 0 is imposed:
        EQ.0: Imposed only on boundaries with prescribed velocity
        EQ.1: Imposed on boundaries with either prescribed velocity or prescribed pressure
        """ # nopep8
        return self._cards[0].get_value("irt0pbc")

    @irt0pbc.setter
    def irt0pbc(self, value: int) -> None:
        """Set the irt0pbc property."""
        if value not in [10, None]:
            raise Exception("""irt0pbc must be `None` or one of {10}.""")
        self._cards[0].set_value("irt0pbc", value)

    @property
    def n_ptid(self) -> typing.Optional[int]:
        """Get or set the ID of a point lying on the source plane for ISHAPE = 4
        """ # nopep8
        return self._cards[1].get_value("n_ptid")

    @n_ptid.setter
    def n_ptid(self, value: int) -> None:
        """Set the n_ptid property."""
        self._cards[1].set_value("n_ptid", value)

    @property
    def n_norid(self) -> typing.Optional[int]:
        """Get or set the ID of a point whose coordinate components specify the normal vector of the plane for ISHAPE = 4
        """ # nopep8
        return self._cards[1].get_value("n_norid")

    @n_norid.setter
    def n_norid(self, value: int) -> None:
        """Set the n_norid property."""
        self._cards[1].set_value("n_norid", value)

    @property
    def n_ptid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n_ptid."""
        return self._get_link_by_attr("NODE", "nid", self.n_ptid, "parts")

    @property
    def n_norid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n_norid."""
        return self._get_link_by_attr("NODE", "nid", self.n_norid, "parts")

