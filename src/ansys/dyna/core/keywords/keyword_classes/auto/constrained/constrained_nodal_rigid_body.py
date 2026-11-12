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

"""Module providing the ConstrainedNodalRigidBody class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_CONSTRAINEDNODALRIGIDBODY_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class ConstrainedNodalRigidBody(KeywordBase):
    """DYNA CONSTRAINED_NODAL_RIGID_BODY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODAL_RIGID_BODY"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "pnode": LinkType.NODE,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "nsid": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the ConstrainedNodalRigidBody class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            TableCard(
                [
                    Field("pid", int, 0, 10, None),
                    Field("cid", int, 10, 10, None),
                    Field("nsid", int, 20, 10, None),
                    Field("pnode", int, 30, 10, 0),
                    Field("iprt", int, 40, 10, 0),
                    Field("drflag", int, 50, 10, 0),
                    Field("rrflag", int, 60, 10, 0),
                ],
                None,
                name="constrained_nodal_rigid_bodies",
                **kwargs,
            ),
            OptionCardSet(
                option_spec = ConstrainedNodalRigidBody._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _CONSTRAINEDNODALRIGIDBODY_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def constrained_nodal_rigid_bodies(self) -> pd.DataFrame:
        """Get the table of constrained_nodal_rigid_bodies."""
        return self._cards[0].table

    @constrained_nodal_rigid_bodies.setter
    def constrained_nodal_rigid_bodies(self, df: pd.DataFrame):
        """Set constrained_nodal_rigid_bodies from the dataframe df"""
        self._cards[0].table = df

    @property
    def pid(self) -> pd.Series:
        """Get or set the Part ID of the nodal rigid body.
        """ # nopep8
        return self._cards[0].get_column("pid")

    @pid.setter
    def pid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pid", value)

    @property
    def cid(self) -> pd.Series:
        """Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. Only necessary if no local system is defined below.
        """ # nopep8
        return self._cards[0].get_column("cid")

    @cid.setter
    def cid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the cid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("cid", value)

    @property
    def nsid(self) -> pd.Series:
        """Get or set the Node set ID, see *SET_NODE. This nodal set defines the rigid body.If NSID=0, then NSID=PID, i.e., the node set ID and the part ID are assumed to be identical.
        """ # nopep8
        return self._cards[0].get_column("nsid")

    @nsid.setter
    def nsid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the nsid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("nsid", value)

    @property
    def pnode(self) -> pd.Series:
        """Get or set the An optional node (a massless node is allowed) used for post-processing rigid body data. If PNODE is not located at the rigid bodys center of mass, LS-DYNA resets the initial coordinates of PNODE to the center of mass.  If CID is defined, LS-DYNA outputs the velocities and accelerations of PNODE in the local system to the d3plot and d3thdt files unless you specify PNODE as a negative number, in which case the global system is used.
        """ # nopep8
        return self._cards[0].get_column("pnode")

    @pnode.setter
    def pnode(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pnode column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pnode", value)

    @property
    def iprt(self) -> pd.Series:
        """Get or set the Print flag.  For nodal rigid bodies, the following values apply:
        EQ.-2: Do not write data into rbdout. Output the forces and moments in bndout with respect to the nodal rigid body�s local coordinate system.
        EQ.-1: Write data into rbdout. Output the forces and moments in bndout with respect to the nodal rigid body�s local coordinate system.
        EQ.0: Defaults to the value of IPRTF in *CONTROL_OUTPUT for all nodal rigid bodies except two-noded rigid bodies.
        For two-noded rigid bodies, printing to rbdout is suppressed. This behavior exists to avoid excessively large rbdout files when the model
        contains many two-noded welds. Note that IPRTF does not change the coordinate system for bndout. Thus, for all nodal rigid bodies, the forces
        and moments in the bndout file are expressed in the global coordinate system.
        EQ.1: Write data into rbdout.  Output the forces and moments in bndout with respect to the global coordinate system.
        EQ.2: Do not write data into rbdout. Output the forces and moments in bndout with respect to the global coordinate system.
        """ # nopep8
        return self._cards[0].get_column("iprt")

    @iprt.setter
    def iprt(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the iprt column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("iprt", value)

    @property
    def drflag(self) -> pd.Series:
        """Get or set the Displacement release flag for all nodes except the first node in the definition.
        EQ.-7: release x, y, and z displacement in global system,
        EQ.-6: release z and x displacement in global system,
        EQ.-5: release y and z displacement in global system,
        EQ.-4: release x and y displacement in global system,
        EQ.-3: release z displacement in global system,
        EQ.-2: release y displacement in global system,
        EQ.-1: release x displacement in global system,
        EQ. 0: off for rigid body behavior,
        EQ. 1: release x displacement in rigid body local system,
        EQ. 2: release y displacement in rigid body local system,
        EQ. 3: release z displacement in rigid body local system,
        EQ. 4: release x and y displacement in rigid body local system,
        EQ. 5: release y and z displacement in rigid body local system,
        EQ. 6: release z and x displacement in rigid body local system,
        EQ. 7: release x, y, and z displacement in rigid body local system
        """ # nopep8
        return self._cards[0].get_column("drflag")

    @drflag.setter
    def drflag(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the drflag column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("drflag", value)

    @property
    def rrflag(self) -> pd.Series:
        """Get or set the Rotation release flag for all nodes except the first node in the definition.
        EQ.-7: release x, y, and z rotations in global system,
        EQ.-6: release z and x rotations in global system,
        EQ.-5: release y and z rotations in global system,
        EQ.-4: release x and y rotations in global system,
        EQ.-3: release z rotation in global system,
        EQ.-2: release y rotation in global system,
        EQ.-1: release x rotation in global system,
        EQ. 0: off for rigid body behavior,
        EQ. 1: release x rotation in rigid body local system,
        EQ. 2: release y rotation in rigid body local system,
        EQ. 3: release z rotation in rigid body local system,
        EQ. 4: release x and y rotations in rigid body local system,
        EQ. 5: release y and z rotations in rigid body local system,
        EQ. 6: release z and x rotations in rigid body local system,
        EQ. 7: release x, y, and z rotations in rigid body local system,
        """ # nopep8
        return self._cards[0].get_column("rrflag")

    @rrflag.setter
    def rrflag(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the rrflag column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("rrflag", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def pnode_links(self) -> typing.Dict[int, KeywordBase]:
        """Get all NODE keywords for pnode, keyed by pnode value."""
        return self._get_links_from_table("NODE", "nid", "constrained_nodal_rigid_bodies", "pnode", "")

    def get_pnode_link(self, pnode: int) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given pnode."""
        return self._get_link_by_attr("NODE", "nid", pnode, "")

    @property
    def cid_links(self) -> typing.Dict[typing.Any, KeywordBase]:
        """Get the keywords linked by the cid column, keyed by cid value."""
        return self._get_column_links(self.cid, lambda value: self._find_keyword("DEFINE", "COORDINATE_SYSTEM", "cid", value))

    @property
    def nsid_links(self) -> typing.Dict[typing.Any, KeywordBase]:
        """Get the keywords linked by the nsid column, keyed by nsid value."""
        return self._get_column_links(self.nsid, lambda value: self._get_set_link("NODE", value))

