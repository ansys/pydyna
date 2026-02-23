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

"""Module providing the InitialFoamReferenceGeometryRamp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_INITIALFOAMREFERENCEGEOMETRYRAMP_CARD0 = (
    FieldSchema("ndtrrg", int, 0, 8, None),
)

_INITIALFOAMREFERENCEGEOMETRYRAMP_CARD1 = (
    FieldSchema("nid", int, 0, 8, None),
    FieldSchema("x", float, 8, 16, 0.0),
    FieldSchema("y", float, 24, 16, 0.0),
    FieldSchema("z", float, 40, 16, 0.0),
)

class InitialFoamReferenceGeometryRamp(KeywordBase):
    """DYNA INITIAL_FOAM_REFERENCE_GEOMETRY_RAMP keyword"""

    keyword = "INITIAL"
    subkeyword = "FOAM_REFERENCE_GEOMETRY_RAMP"
    _link_fields = {
        "nid": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialFoamReferenceGeometryRamp class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALFOAMREFERENCEGEOMETRYRAMP_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALFOAMREFERENCEGEOMETRYRAMP_CARD1,
                **kwargs,
            ),        ]
    @property
    def ndtrrg(self) -> typing.Optional[int]:
        """Get or set the Number of time steps taken for an element to restore its reference geometry.  Definition of NDTRRG allows an element to ramp up to its reference shape in NDTRRG time steps.  Currently ls-dynauses only one NDTRRG and applies it to all foam materials with reference geometries. If more than one NDTRRG is defined, the latter defined one will replace the previously define one.
        """ # nopep8
        return self._cards[0].get_value("ndtrrg")

    @ndtrrg.setter
    def ndtrrg(self, value: int) -> None:
        """Set the ndtrrg property."""
        self._cards[0].set_value("ndtrrg", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def x(self) -> float:
        """Get or set the x-coordinate.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y-coordinate.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the z-coordinate.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def nid_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

