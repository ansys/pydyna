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

"""Module providing the ConstrainedGeneralizedWeldSpot class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDGENERALIZEDWELDSPOT_CARD0 = (
    FieldSchema("wid", int, 0, 10, None),
)

_CONSTRAINEDGENERALIZEDWELDSPOT_CARD1 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("cid", int, 10, 10, None),
    FieldSchema("filter", int, 20, 10, None),
    FieldSchema("window", float, 30, 10, 0.0),
    FieldSchema("npr", int, 40, 10, None),
    FieldSchema("nprt", int, 50, 10, 0),
)

_CONSTRAINEDGENERALIZEDWELDSPOT_CARD2 = (
    FieldSchema("tfail", float, 0, 10, 1e+20),
    FieldSchema("epsf", float, 10, 10, None),
    FieldSchema("sn", float, 20, 10, None),
    FieldSchema("ss", float, 30, 10, None),
    FieldSchema("n", float, 40, 10, None),
    FieldSchema("m", float, 50, 10, None),
)

class ConstrainedGeneralizedWeldSpot(KeywordBase):
    """DYNA CONSTRAINED_GENERALIZED_WELD_SPOT keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "GENERALIZED_WELD_SPOT"
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedGeneralizedWeldSpot class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDGENERALIZEDWELDSPOT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDGENERALIZEDWELDSPOT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDGENERALIZEDWELDSPOT_CARD2,
                **kwargs,
            ),        ]
    @property
    def wid(self) -> typing.Optional[int]:
        """Get or set the Optional weld ID
        """ # nopep8
        return self._cards[0].get_value("wid")

    @wid.setter
    def wid(self, value: int) -> None:
        """Set the wid property."""
        self._cards[0].set_value("wid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID, see *SET_NODE.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[1].set_value("nsid", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. CID is not required for spotwelds if the nodes are not coincident.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[1].set_value("cid", value)

    @property
    def filter(self) -> typing.Optional[int]:
        """Get or set the Number of force vectors saved for filtering. This option can eliminate spurious failures due to numerical force spikes; however, memory requirements are significant since 6 force components are stored with each vector.
        LE.1: no filtering,
        EQ.n: simple average of force components divided by n or the maximum number of force vectors that are stored for the time window option below.
        """ # nopep8
        return self._cards[1].get_value("filter")

    @filter.setter
    def filter(self, value: int) -> None:
        """Set the filter property."""
        self._cards[1].set_value("filter", value)

    @property
    def window(self) -> float:
        """Get or set the Time window for filtering. This option requires the specification of the maximum number of steps which can occur within the filtering time window. If the time step decreases too far, then the filtering time window will be ignored and the simple average is used.
        EQ.0: time window is not used.
        """ # nopep8
        return self._cards[1].get_value("window")

    @window.setter
    def window(self, value: float) -> None:
        """Set the window property."""
        self._cards[1].set_value("window", value)

    @property
    def npr(self) -> typing.Optional[int]:
        """Get or set the NFW, number of individual nodal pairs (only cross fillet or combined general weld).
        """ # nopep8
        return self._cards[1].get_value("npr")

    @npr.setter
    def npr(self, value: int) -> None:
        """Set the npr property."""
        self._cards[1].set_value("npr", value)

    @property
    def nprt(self) -> int:
        """Get or set the Print option in file RBDOUT.
        EQ.0: default from control card is used (default),
        EQ.1: data is printed,
        EQ.2: data is not printed.
        """ # nopep8
        return self._cards[1].get_value("nprt")

    @nprt.setter
    def nprt(self, value: int) -> None:
        """Set the nprt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nprt must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("nprt", value)

    @property
    def tfail(self) -> float:
        """Get or set the Failure time for constraint set, tf (default=1.0E+20).
        """ # nopep8
        return self._cards[2].get_value("tfail")

    @tfail.setter
    def tfail(self, value: float) -> None:
        """Set the tfail property."""
        self._cards[2].set_value("tfail", value)

    @property
    def epsf(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain at failure.
        """ # nopep8
        return self._cards[2].get_value("epsf")

    @epsf.setter
    def epsf(self, value: float) -> None:
        """Set the epsf property."""
        self._cards[2].set_value("epsf", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Sn, normal force at failure, only for the brittle failure of spotwelds.
        """ # nopep8
        return self._cards[2].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        """Set the sn property."""
        self._cards[2].set_value("sn", value)

    @property
    def ss(self) -> typing.Optional[float]:
        """Get or set the Ss, shear force at failure, only for the brittle failure of spotwelds.
        """ # nopep8
        return self._cards[2].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        """Set the ss property."""
        self._cards[2].set_value("ss", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the n, exponent for normal force, only for the brittle failure of spotwelds.
        """ # nopep8
        return self._cards[2].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[2].set_value("n", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the m, exponent for shear force, only for the brittle failure of spotwelds.
        """ # nopep8
        return self._cards[2].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[2].set_value("m", value)

    @property
    def cid_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

    @property
    def nsid_link(self) -> typing.Optional[KeywordBase]:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

