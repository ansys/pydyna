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

"""Module providing the SetNodeGeneralCollect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SETNODEGENERALCOLLECT_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("da1", float, 10, 10, 0.0),
    FieldSchema("da2", float, 20, 10, 0.0),
    FieldSchema("da3", float, 30, 10, 0.0),
    FieldSchema("da4", float, 40, 10, 0.0),
    FieldSchema("solver", str, 50, 10, "MECH"),
    FieldSchema("its", int, 60, 10, 1),
    FieldSchema("unused", int, 70, 10, None),
)

_SETNODEGENERALCOLLECT_CARD1 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("e1", int, 10, 10, None),
    FieldSchema("e2", int, 20, 10, None),
    FieldSchema("e3", int, 30, 10, None),
    FieldSchema("e4", int, 40, 10, None),
    FieldSchema("e5", int, 50, 10, None),
    FieldSchema("e6", int, 60, 10, None),
    FieldSchema("e7", int, 70, 10, None),
)

_SETNODEGENERALCOLLECT_CARD2 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("mshid", int, 10, 10, None),
    FieldSchema("imin", int, 20, 10, None),
    FieldSchema("imax", int, 30, 10, None),
    FieldSchema("jmin", int, 40, 10, None),
    FieldSchema("jmax", int, 50, 10, None),
    FieldSchema("kmin", int, 60, 10, None),
    FieldSchema("kmax", int, 70, 10, None),
)

_SETNODEGENERALCOLLECT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetNodeGeneralCollect(KeywordBase):
    """DYNA SET_NODE_GENERAL_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "NODE_GENERAL_COLLECT"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SetNodeGeneralCollect class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETNODEGENERALCOLLECT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _SETNODEGENERALCOLLECT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _SETNODEGENERALCOLLECT_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = SetNodeGeneralCollect._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETNODEGENERALCOLLECT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Node set ID. All node sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        """Set the da4 property."""
        self._cards[0].set_value("da4", value)

    @property
    def solver(self) -> str:
        """Get or set the EQ.MECH: mechanics.
        EQ.CESE: CE/SE compressible fluid flow solver.
        EQ.ICFD: Incompressible fluid flow solver.
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: str) -> None:
        """Set the solver property."""
        if value not in ["MECH", "CESE", "ICFD", None]:
            raise Exception("""solver must be `None` or one of {"MECH","CESE","ICFD"}.""")
        self._cards[0].set_value("solver", value)

    @property
    def its(self) -> int:
        """Get or set the Specify coupling type across different scales in two-scale co-simulation. This flag should only be included for node sets that provide coupling information in the input file referred to by *INCLUDE_COSIM;
        EQ.1: Tied contact coupling
        EQ.2: Solid - in - shell immersed coupling
        """ # nopep8
        return self._cards[0].get_value("its")

    @its.setter
    def its(self, value: int) -> None:
        """Set the its property."""
        if value not in [1, 2, None]:
            raise Exception("""its must be `None` or one of {1,2}.""")
        self._cards[0].set_value("its", value)

    @property
    def option(self) -> str:
        """Get or set the EQ.ALL: All nodes will be included in the set.
        EQ.BRANCH: Nodes inside tree branches E1, E2, E3, � will be included.  (see *SET_PART_TREE)
        EQ.DBRANCH: Previously added nodes that are inside tree branches E1, E2, E3, � will be excluded.
        EQ.BOX: Nodes inside boxes E1, E2, E3, � will be included.  (see *DEFINE_BOX)
        EQ.DBOX: Previously added nodes that are inside boxes E1, E2, E3, � will be excluded.
        EQ.NODE: Nodes E1, E2, E3, � will be included.
        EQ.DNODE: Nodes E1, E2, E3, � if previously added will be excluded.
        EQ.PART: Nodes of parts E1, E2, E3, � will be included.
        EQ.DPART: Nodes that have been previously added and are of parts E1, E2, E3, � will be excluded.
        EQ.SALECPT: Nodes inside a box in a structured ALE (S-ALE) mesh. E1 here is the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, E7 correspond to IMIN, IMAX, JMIN, JMAX, KMIN, KMAX.  They are the minimum and the maximum nodal indices along each direction in S-ALE mesh. To include all nodes in the S-ALE mesh defined by E1, set values E2�E7 to zero or leave them blank. This option can only be used for S-ALE meshes. It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options. Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        EQ.SALEFAC: Nodes that are on the face of an S-ALE mesh.  E1 gives the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, E7 correspond to -x, +x, -y, +y, -z, +z faces.  Assigning 1, for instance, to these six values would include nodes belonging to all the surface segments at these faces in the node set.  This option is can only be used for S-ALE meshes. It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options. For trimmed S-ALE meshes (see *ALE_STRUCTURED_MESH_TRIM), a segment is treated as a surface segment as long as it has no neighboring element along the specified direction.  The set, thus, includes the nodes of surface segments belonging to exterior and interior boundaries.Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        EQ.SALEBOXL: Nodes inside a box in a structured ALE (S-ALE) mesh. E1 here is the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, and E7 correspond to XMIN, XMAX, YMIN, YMAX, ZMIN, and ZMAX.  They are the minimum and the maximum nodal local coordinates, as defined in *ALE_STRUCTURED_MESH_CONTROL_POINTS, along each direction in an S-ALE mesh.  This option can only be used for S-ALE meshes. It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options.Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        EQ.SET_NODE: Nodes from node sets with IDs E1, �, E7 will be included.
        EQ.DSET_NODE: Nodes that have been previously added and are from node sets with IDs E1, �, E7 will be excluded.
        EQ.SET_XXXX: Include nodal points of element sets defined by *SET_XXXX, where XXXX could be SHELL, SOLID, BEAM, TSHELL, or DISCRETE.
        EQ.SET_PART: Include nodal points in part sets E1, �, E7.
        EQ.VOL: Nodes inside contact volumes E1, E2, E3, � will be included.  See *DEFINE_CONTACT_VOLUME.
        EQ.DVOL: Previously added nodes that are inside contact volumes E1, E2, E3, � will be excluded.
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "BRANCH", "DBRANCH", "BOX", "DBOX", "NODE", "DNODE", "PART", "DPART", "SALECPT", "SALEFAC", "SALEBOXL", "SET_NODE", "DSET_NODE", "SET_SHELL", "SET_SOLID", "SET_BEAM", "SET_TSHELL", "SET_PART", "VOL", "DVOL", None]:
            raise Exception("""option must be `None` or one of {"ALL","BRANCH","DBRANCH","BOX","DBOX","NODE","DNODE","PART","DPART","SALECPT","SALEFAC","SALEBOXL","SET_NODE","DSET_NODE","SET_SHELL","SET_SOLID","SET_BEAM","SET_TSHELL","SET_PART","VOL","DVOL"}.""")
        self._cards[1].set_value("option", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E1 not used,
        OPTION.EQ.ELEM: Node E1 will be included in the current set,
        OPTION.EQ.DELEM: Node E1 will be excluded from the current set,
        OPTION.EQ.PART: Nodes from part E1 will be included in the current set,
        OPTION.EQ.DPART: Nodes from part E1 will be excluded from the current set,
        OPTION.EQ.BOX: Nodes inside box E1 will be included in the current set,
        OPTION.EQ.DBOX: Nodes inside box E1 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        """Set the e1 property."""
        self._cards[1].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E2 not used,
        OPTION.EQ.ELEM: Node E2 will be included in the current set,
        OPTION.EQ.DELEM: Node E2 will be excluded from the current set,
        OPTION.EQ.PART: Nodes from part E2 will be included in the current set,
        OPTION.EQ.DPART: Nodes from part E2 will be excluded from the current set,
        OPTION.EQ.BOX: Nodes inside box E2 will be included in the current set,
        OPTION.EQ.DBOX: Nodes inside box E2 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        """Set the e2 property."""
        self._cards[1].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E3 not used,
        OPTION.EQ.ELEM: Node E3 will be included in the current set,
        OPTION.EQ.DELEM: Node E3 will be excluded from the current set,
        OPTION.EQ.PART: Nodes from part E3 will be included in the current set,
        OPTION.EQ.DPART: Nodes from part E3 will be excluded from the current set,
        OPTION.EQ.BOX: Nodes inside box E3 will be included in the current set,
        OPTION.EQ.DBOX: Nodes inside box E3 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        """Set the e3 property."""
        self._cards[1].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E4 not used,
        OPTION.EQ.ELEM: Node E4 will be included in the current set,
        OPTION.EQ.DELEM: Node E4 will be excluded from the current set,
        OPTION.EQ.PART: Nodes from part E4 will be included in the current set,
        OPTION.EQ.DPART: Nodes from part E4 will be excluded from the current set,
        OPTION.EQ.BOX: Nodes inside box E4 will be included in the current set,
        OPTION.EQ.DBOX: Nodes inside box E4 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        """Set the e4 property."""
        self._cards[1].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E5 not used,
        OPTION.EQ.ELEM: Node E5 will be included in the current set,
        OPTION.EQ.DELEM: Node E5 will be excluded from the current set,
        OPTION.EQ.PART: Nodes from part E5 will be included in the current set,
        OPTION.EQ.DPART: Nodes from part E5 will be excluded from the current set,
        OPTION.EQ.BOX: Nodes inside box E5 will be included in the current set,
        OPTION.EQ.DBOX: Nodes inside box E5 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        """Set the e5 property."""
        self._cards[1].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E6 not used,
        OPTION.EQ.ELEM: Node E6 will be included in the current set,
        OPTION.EQ.DELEM: Node E6 will be excluded from the current set,
        OPTION.EQ.PART: Nodes from part E6 will be included in the current set,
        OPTION.EQ.DPART: Nodes from part E6 will be excluded from the current set,
        OPTION.EQ.BOX: Nodes inside box E6 will be included in the current set,
        OPTION.EQ.DBOX: Nodes inside box E6 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        """Set the e6 property."""
        self._cards[1].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E7 not used,
        OPTION.EQ.ELEM: Node E7 will be included in the current set,
        OPTION.EQ.DELEM: Node E7 will be excluded from the current set,
        OPTION.EQ.PART: Nodes from part E7 will be included in the current set,
        OPTION.EQ.DPART: Nodes from part E7 will be excluded from the current set,
        OPTION.EQ.BOX: Nodes inside box E7 will be included in the current set,
        OPTION.EQ.DBOX: Nodes inside box E7 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        """Set the e7 property."""
        self._cards[1].set_value("e7", value)

    @property
    def option(self) -> str:
        """Get or set the EQ.ALL: All nodes will be included in the set.
        EQ.BRANCH: Nodes inside tree branches E1, E2, E3, � will be included.  (see *SET_PART_TREE)
        EQ.DBRANCH: Previously added nodes that are inside tree branches E1, E2, E3, � will be excluded.
        EQ.BOX: Nodes inside boxes E1, E2, E3, � will be included.  (see *DEFINE_BOX)
        EQ.DBOX: Previously added nodes that are inside boxes E1, E2, E3, � will be excluded.
        EQ.NODE: Nodes E1, E2, E3, � will be included.
        EQ.DNODE: Nodes E1, E2, E3, � if previously added will be excluded.
        EQ.PART: Nodes of parts E1, E2, E3, � will be included.
        EQ.DPART: Nodes that have been previously added and are of parts E1, E2, E3, � will be excluded.
        EQ.SALECPT: Nodes inside a box in a structured ALE (S-ALE) mesh. E1 here is the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, E7 correspond to IMIN, IMAX, JMIN, JMAX, KMIN, KMAX.  They are the minimum and the maximum nodal indices along each direction in S-ALE mesh.  This option can only be used for S-ALE meshes. It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options. Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        EQ.SALEFAC: Nodes that are on the face of an S-ALE mesh.  E1 gives the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, E7 correspond to -x, +x, -y, +y, -z, +z faces.  Assigning 1, for instance, to these six values would include nodes belonging to all the surface segments at these faces in the node set.  This option is can only be used for S-ALE meshes. It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options. For trimmed S-ALE meshes (see *ALE_STRUCTURED_MESH_TRIM), a segment is treated as a surface segment as long as it has no neighboring element along the specified direction.  The set, thus, includes the nodes of surface segments belonging to exterior and interior boundaries.Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        EQ.SALEBOXL: Nodes inside a box in a structured ALE (S-ALE) mesh. E1 here is the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, and E7 correspond to XMIN, XMAX, YMIN, YMAX, ZMIN, and ZMAX.  They are the minimum and the maximum nodal local coordinates, as defined in *ALE_STRUCTURED_MESH_CONTROL_POINTS, along each direction in an S-ALE mesh.  This option can only be used for S-ALE meshes. It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options.Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        EQ.SET_NODE: Nodes from node sets with IDs E1, �, E7 will be included.
        EQ.DSET_NODE: Nodes that have been previously added and are from node sets with IDs E1, �, E7 will be excluded.
        EQ.SET_XXXX: Include nodal points of element sets defined by *SET_XXXX, where XXXX could be SHELL, SOLID, BEAM, TSHELL, or DISCRETE.
        EQ.SET_PART: Include nodal points in part sets E1, �, E7.
        EQ.VOL: Nodes inside contact volumes E1, E2, E3, � will be included.  See *DEFINE_CONTACT_VOLUME.
        EQ.DVOL: Previously added nodes that are inside contact volumes E1, E2, E3, � will be excluded.
        """ # nopep8
        return self._cards[2].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "BRANCH", "DBRANCH", "BOX", "DBOX", "NODE", "DNODE", "PART", "DPART", "SALECPT", "SALEFAC", "SALEBOXL", "SET_NODE", "DSET_NODE", "SET_SHELL", "SET_SOLID", "SET_BEAM", "SET_TSHELL", "SET_PART", "VOL", "DVOL", None]:
            raise Exception("""option must be `None` or one of {"ALL","BRANCH","DBRANCH","BOX","DBOX","NODE","DNODE","PART","DPART","SALECPT","SALEFAC","SALEBOXL","SET_NODE","DSET_NODE","SET_SHELL","SET_SOLID","SET_BEAM","SET_TSHELL","SET_PART","VOL","DVOL"}.""")
        self._cards[2].set_value("option", value)

    @property
    def mshid(self) -> typing.Optional[int]:
        """Get or set the S-ALE mesh ID.
        """ # nopep8
        return self._cards[2].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        """Set the mshid property."""
        self._cards[2].set_value("mshid", value)

    @property
    def imin(self) -> typing.Optional[int]:
        """Get or set the The minimum nodal indices along X in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("imin")

    @imin.setter
    def imin(self, value: int) -> None:
        """Set the imin property."""
        self._cards[2].set_value("imin", value)

    @property
    def imax(self) -> typing.Optional[int]:
        """Get or set the The maximum nodal indices along X in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("imax")

    @imax.setter
    def imax(self, value: int) -> None:
        """Set the imax property."""
        self._cards[2].set_value("imax", value)

    @property
    def jmin(self) -> typing.Optional[int]:
        """Get or set the The minimum nodal indices along Y in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("jmin")

    @jmin.setter
    def jmin(self, value: int) -> None:
        """Set the jmin property."""
        self._cards[2].set_value("jmin", value)

    @property
    def jmax(self) -> typing.Optional[int]:
        """Get or set the The maximum nodal indices along Y in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("jmax")

    @jmax.setter
    def jmax(self, value: int) -> None:
        """Set the jmax property."""
        self._cards[2].set_value("jmax", value)

    @property
    def kmin(self) -> typing.Optional[int]:
        """Get or set the The minimum nodal indices along Z in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("kmin")

    @kmin.setter
    def kmin(self, value: int) -> None:
        """Set the kmin property."""
        self._cards[2].set_value("kmin", value)

    @property
    def kmax(self) -> typing.Optional[int]:
        """Get or set the The maximum nodal indices along Z in S-ALE mesh.
        """ # nopep8
        return self._cards[2].get_value("kmax")

    @kmax.setter
    def kmax(self, value: int) -> None:
        """Set the kmax property."""
        self._cards[2].set_value("kmax", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

