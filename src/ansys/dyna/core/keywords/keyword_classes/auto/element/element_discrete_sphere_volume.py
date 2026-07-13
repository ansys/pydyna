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

"""Module providing the ElementDiscreteSphereVolume class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_ELEMENTDISCRETESPHEREVOLUME_CARD0 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("volume", float, 20, 10, 0.0),
    FieldSchema("inertia", float, 30, 10, 0.0),
    FieldSchema("radius", float, 40, 10, 0.0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("idist", int, 60, 10, 0),
    FieldSchema("nid2", int, 70, 10, None),
)

class ElementDiscreteSphereVolume(KeywordBase):
    """DYNA ELEMENT_DISCRETE_SPHERE_VOLUME keyword"""

    keyword = "ELEMENT"
    subkeyword = "DISCRETE_SPHERE_VOLUME"
    _link_fields = {
        "nid": LinkType.NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ElementDiscreteSphereVolume class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTDISCRETESPHEREVOLUME_CARD0,
                **kwargs,
            ),
        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the DES Node IDr
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the DES Part ID, see *PART
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def volume(self) -> float:
        """Get or set the Volume.  The mass is calculated from material density,M = VOLUMExp_mat    . For IDIST = -1 and 1, this value is the mean.For IDIST = -2 and 2 with MM != 0, this value is the scale parameter.
        """ # nopep8
        return self._cards[0].get_value("volume")

    @volume.setter
    def volume(self, value: float) -> None:
        """Set the volume property."""
        self._cards[0].set_value("volume", value)

    @property
    def inertia(self) -> float:
        """Get or set the Inertia per unit density.  The actual inertia is calculated from material density, I = INERTIAxp_mat .For IDIST = -1 and 1, this value is the mean.For IDIST = -2 and 2 with MI != 0, this value is the scale parameter.
        """ # nopep8
        return self._cards[0].get_value("inertia")

    @inertia.setter
    def inertia(self, value: float) -> None:
        """Set the inertia property."""
        self._cards[0].set_value("inertia", value)

    @property
    def radius(self) -> float:
        """Get or set the Particle radius. Determining contact between particles requires the particle radius. For IDIST = -1 and 1, this value is the mean. For IDIST = -2 and 2 with MR != 0, this value is the scale parameter.
        """ # nopep8
        return self._cards[0].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        """Set the radius property."""
        self._cards[0].set_value("radius", value)

    @property
    def idist(self) -> int:
        """Get or set the Distribution of DES properties (see Remarks 1 and 2)
        EQ. - 2: Weibull distribution(non - deterministic).
        EQ. - 1: Gaussian distribution(non - deterministic).
        EQ.0: Single property(default)
        EQ.1: Gaussian distribution(deterministic).
        EQ.2: Weibull distribution(deterministic):
        """ # nopep8
        return self._cards[0].get_value("idist")

    @idist.setter
    def idist(self, value: int) -> None:
        """Set the idist property."""
        if value not in [0, 1, 2, -1, -2, None]:
            raise Exception("""idist must be `None` or one of {0,1,2,-1,-2}.""")
        self._cards[0].set_value("idist", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Define more than one element with the same PID by setting this field without requiring additional cards. For IDIST = 0, these elements share equal VOLUME, INERTIA, and RADIUS. For IDIST != 0, these elements each have their VOLUME, INERTIA, and/or RADIUS randomly distributed according to the distribution and Card 2 parameters assigned. If set, NID2 is a node ID that must have a value greater than NID. Then, LS-DYNA associates a DES to each node with an ID between NID and NID2 (including NID and NID2). If 0 or left blank, then only NID has a DES associated with it.
        """ # nopep8
        return self._cards[0].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[0].set_value("nid2", value)

    @property
    def nid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

