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

"""Module providing the InitialHydrostaticAle class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_INITIALHYDROSTATICALE_CARD0 = (
    FieldSchema("alesid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, 0),
    FieldSchema("vecid", int, 20, 10, None),
    FieldSchema("grav", float, 30, 10, None),
    FieldSchema("pbase", float, 40, 10, 0.0),
    FieldSchema("unused", int, 50, 10, None),
)

_INITIALHYDROSTATICALE_CARD1 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("mmgblo", int, 10, 10, None),
)

class InitialHydrostaticAle(KeywordBase):
    """DYNA INITIAL_HYDROSTATIC_ALE keyword"""

    keyword = "INITIAL"
    subkeyword = "HYDROSTATIC_ALE"
    _link_fields = {
        "nid": LinkType.NODE,
        "vecid": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialHydrostaticAle class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALHYDROSTATICALE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALHYDROSTATICALE_CARD1,
                **kwargs,
            ),        ]
    @property
    def alesid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("alesid")

    @alesid.setter
    def alesid(self, value: int) -> None:
        """Set the alesid property."""
        self._cards[0].set_value("alesid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type for the SID above:  EQ.0:  SID is a part set ID ; EQ.1:  SID is a part ID.
        EQ.2: Solid set ID (SSID).
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""stype must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("stype", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the A vector ID defining the direction of gravitational acceleration.
        """ # nopep8
        return self._cards[0].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        """Set the vecid property."""
        self._cards[0].set_value("vecid", value)

    @property
    def grav(self) -> typing.Optional[float]:
        """Get or set the Magnitude of the gravitational acceleration.
        """ # nopep8
        return self._cards[0].get_value("grav")

    @grav.setter
    def grav(self, value: float) -> None:
        """Set the grav property."""
        self._cards[0].set_value("grav", value)

    @property
    def pbase(self) -> float:
        """Get or set the The "base" pressure of each fluid layer.  This is the ambient pressure at the top of each ALE material (fluid) layer to be initialized.  Each layer must be represented by one ALE multi-material group ID (AMMG).
        """ # nopep8
        return self._cards[0].get_value("pbase")

    @pbase.setter
    def pbase(self, value: float) -> None:
        """Set the pbase property."""
        self._cards[0].set_value("pbase", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID defining the top location of a material/fluid layer.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def mmgblo(self) -> typing.Optional[int]:
        """Get or set the AMMG ID of the fluid layer immediately below this NID. Each node is defined in association with one AMMG layer below it. See Remark 3.  In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID.
        """ # nopep8
        return self._cards[1].get_value("mmgblo")

    @mmgblo.setter
    def mmgblo(self, value: int) -> None:
        """Set the mmgblo property."""
        self._cards[1].set_value("mmgblo", value)

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def vecid_link(self) -> DefineVector:
        """Get the DefineVector object for vecid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vecid:
                return kwd
        return None

    @vecid_link.setter
    def vecid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vecid."""
        self.vecid = value.vid

