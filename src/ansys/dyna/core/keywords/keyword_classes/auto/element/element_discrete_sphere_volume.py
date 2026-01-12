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

_ELEMENTDISCRETESPHEREVOLUME_CARD0 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("volume", float, 20, 10, 0.0),
    FieldSchema("inertia", float, 30, 10, 0.0),
    FieldSchema("radii", float, 40, 10, 0.0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("nid2", int, 70, 10, None),
)

class ElementDiscreteSphereVolume(KeywordBase):
    """DYNA ELEMENT_DISCRETE_SPHERE_VOLUME keyword"""

    keyword = "ELEMENT"
    subkeyword = "DISCRETE_SPHERE_VOLUME"

    def __init__(self, **kwargs):
        """Initialize the ElementDiscreteSphereVolume class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTDISCRETESPHEREVOLUME_CARD0,
                **kwargs,
            ),        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID and Element ID are the same for the discrete shpher
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def volume(self) -> float:
        """Get or set the volume value.
        """ # nopep8
        return self._cards[0].get_value("volume")

    @volume.setter
    def volume(self, value: float) -> None:
        """Set the volume property."""
        self._cards[0].set_value("volume", value)

    @property
    def inertia(self) -> float:
        """Get or set the inertia value.
        """ # nopep8
        return self._cards[0].get_value("inertia")

    @inertia.setter
    def inertia(self, value: float) -> None:
        """Set the inertia property."""
        self._cards[0].set_value("inertia", value)

    @property
    def radii(self) -> float:
        """Get or set the sphere radius.
        """ # nopep8
        return self._cards[0].get_value("radii")

    @radii.setter
    def radii(self, value: float) -> None:
        """Set the radii property."""
        self._cards[0].set_value("radii", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the More than one element with the same PID, MASS, INERTIA, and RADIUS can be defined by setting this field without requiring additional cards. If set, NID2 is a node ID that must have a value greater than NID. Then, DES are defined for each node with an ID between NID and NID2 (including NID and NID2). If 0 or left blank, then only a DES for NID is specified.
        """ # nopep8
        return self._cards[0].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        """Set the nid2 property."""
        self._cards[0].set_value("nid2", value)

