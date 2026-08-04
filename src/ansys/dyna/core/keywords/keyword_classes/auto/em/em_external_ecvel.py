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

"""Module providing the EmExternalEcvel class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_EMEXTERNALECVEL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("nid1", int, 20, 10, None),
    FieldSchema("nid2", int, 30, 10, None),
    FieldSchema("omega", float, 40, 10, None),
    FieldSchema("vx", float, 50, 10, None),
    FieldSchema("vy", float, 60, 10, None),
    FieldSchema("vz", float, 70, 10, None),
)

class EmExternalEcvel(KeywordBase):
    """DYNA EM_EXTERNAL_ECVEL keyword"""

    keyword = "EM"
    subkeyword = "EXTERNAL_ECVEL"
    _link_fields = {
        "nid1": LinkType.NODE,
        "nid2": LinkType.NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmExternalEcvel class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEXTERNALECVEL_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID on which the velocity term will be added
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining the origin (NID1) and rotation axis (pointing from NID2 to NID1)
        """ # nopep8
        return self._cards[0].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        """Set the nid1 property."""
        self._cards[0].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node IDs defining the origin (NID1) and rotation axis (pointing from NID2 to NID1)
        """ # nopep8
        return self._cards[0].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[0].set_value("nid2", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Rotation velocity term (given in radians)
        GE.0.0: Constant value
        LT.0.0 : | OMEGA | points to a load curve ID for a load curve giving the rotation velocity as a function of time
        """ # nopep8
        return self._cards[0].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        """Set the omega property."""
        self._cards[0].set_value("omega", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the Translational velocity components:
        GE.0.0: Constant value
        LT.0.0 : The absolute value of V[X,Y,Z] refers to a load curve ID for the load curve giving the translational velocity component as a function of time.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the Translational velocity components:
        GE.0.0: Constant value
        LT.0.0 : The absolute value of V[X,Y,Z] refers to a load curve ID for the load curve giving the translational velocity component as a function of time.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the Translational velocity components:
        GE.0.0: Constant value
        LT.0.0 : The absolute value of V[X,Y,Z] refers to a load curve ID for the load curve giving the translational velocity component as a function of time.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def nid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid1."""
        return self._get_link_by_attr("NODE", "nid", self.nid1, "parts")

    @property
    def nid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid2."""
        return self._get_link_by_attr("NODE", "nid", self.nid2, "parts")

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

