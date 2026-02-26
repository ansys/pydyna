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

"""Module providing the AleStructuredMesh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_ALESTRUCTUREDMESH_CARD0 = (
    FieldSchema("mshid", int, 0, 10, 0),
    FieldSchema("dpid", int, 10, 10, None),
    FieldSchema("nbid", int, 20, 10, 0),
    FieldSchema("ebid", int, 30, 10, 0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("tdeath", float, 70, 10, 1e+16),
)

_ALESTRUCTUREDMESH_CARD1 = (
    FieldSchema("cpidx", int, 0, 10, None),
    FieldSchema("cpidy", int, 10, 10, None),
    FieldSchema("cpidz", int, 20, 10, None),
    FieldSchema("nid0", int, 30, 10, None),
    FieldSchema("lcsid", int, 40, 10, None),
)

class AleStructuredMesh(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH"
    _link_fields = {
        "nid0": LinkType.NODE,
        "lcsid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the AleStructuredMesh class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDMESH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALESTRUCTUREDMESH_CARD1,
                **kwargs,
            ),        ]
    @property
    def mshid(self) -> int:
        """Get or set the S-ALE Mesh ID. A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        """Set the mshid property."""
        self._cards[0].set_value("mshid", value)

    @property
    def dpid(self) -> typing.Optional[int]:
        """Get or set the Default Part ID. The elements generated will be with DPID.
        DPID refers to an empty part contains no material and used to
        reference the mesh only. This part definition is automatically
        generated during the input phase and contains no material and
        element formulation information. Please see remark 1.
        """ # nopep8
        return self._cards[0].get_value("dpid")

    @dpid.setter
    def dpid(self, value: int) -> None:
        """Set the dpid property."""
        self._cards[0].set_value("dpid", value)

    @property
    def nbid(self) -> int:
        """Get or set the Nodes are generated and assigned with node IDs starting from NBID.
        """ # nopep8
        return self._cards[0].get_value("nbid")

    @nbid.setter
    def nbid(self, value: int) -> None:
        """Set the nbid property."""
        self._cards[0].set_value("nbid", value)

    @property
    def ebid(self) -> int:
        """Get or set the Elements are generated and assigned with element IDs starting from EBID.
        """ # nopep8
        return self._cards[0].get_value("ebid")

    @ebid.setter
    def ebid(self, value: int) -> None:
        """Set the ebid property."""
        self._cards[0].set_value("ebid", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for this mesh.Please see Remark 3.
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[0].set_value("tdeath", value)

    @property
    def cpidx(self) -> typing.Optional[int]:
        """Get or set the Control point IDs defining node ID/value pairs along each local	axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
        """ # nopep8
        return self._cards[1].get_value("cpidx")

    @cpidx.setter
    def cpidx(self, value: int) -> None:
        """Set the cpidx property."""
        self._cards[1].set_value("cpidx", value)

    @property
    def cpidy(self) -> typing.Optional[int]:
        """Get or set the Control point IDs defining node ID/value pairs along each local axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
        """ # nopep8
        return self._cards[1].get_value("cpidy")

    @cpidy.setter
    def cpidy(self, value: int) -> None:
        """Set the cpidy property."""
        self._cards[1].set_value("cpidy", value)

    @property
    def cpidz(self) -> typing.Optional[int]:
        """Get or set the Control point IDs defining node ID/value pairs along each local	axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
        """ # nopep8
        return self._cards[1].get_value("cpidz")

    @cpidz.setter
    def cpidz(self, value: int) -> None:
        """Set the cpidz property."""
        self._cards[1].set_value("cpidz", value)

    @property
    def nid0(self) -> typing.Optional[int]:
        """Get or set the NID0 specifies the mesh origin node at the input phase. Later
        during the simulation, prescribed motion applied to this node
        gives the generated mesh the translational motion.
        """ # nopep8
        return self._cards[1].get_value("nid0")

    @nid0.setter
    def nid0(self, value: int) -> None:
        """Set the nid0 property."""
        self._cards[1].set_value("nid0", value)

    @property
    def lcsid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID. Please see Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcsid")

    @lcsid.setter
    def lcsid(self, value: int) -> None:
        """Set the lcsid property."""
        self._cards[1].set_value("lcsid", value)

    @property
    def nid0_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given nid0."""
        return self._get_link_by_attr("NODE", "nid", self.nid0, "parts")

    @property
    def lcsid_link(self) -> typing.Optional[DefineCoordinateSystem]:
        """Get the DefineCoordinateSystem object for lcsid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.lcsid:
                return kwd
        return None

    @lcsid_link.setter
    def lcsid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for lcsid."""
        self.lcsid = value.cid

