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

"""Module providing the SetSegmentGeneral class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SETSEGMENTGENERAL_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("da1", float, 10, 10, 0.0),
    FieldSchema("da2", float, 20, 10, 0.0),
    FieldSchema("da3", float, 30, 10, 0.0),
    FieldSchema("da4", float, 40, 10, 0.0),
    FieldSchema("solver", str, 50, 10, "MECH"),
    FieldSchema("its", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_SETSEGMENTGENERAL_CARD1 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("e1", int, 10, 10, None),
    FieldSchema("e2", int, 20, 10, None),
    FieldSchema("e3", int, 30, 10, None),
    FieldSchema("e4", int, 40, 10, None),
    FieldSchema("e5", int, 50, 10, None),
    FieldSchema("e6", int, 60, 10, None),
    FieldSchema("e7", int, 70, 10, None),
)

_SETSEGMENTGENERAL_CARD2 = (
    FieldSchema("option", str, 0, 10, "ALL"),
    FieldSchema("mshid", int, 10, 10, None),
    FieldSchema("imin", int, 20, 10, None),
    FieldSchema("imax", int, 30, 10, None),
    FieldSchema("jmin", int, 40, 10, None),
    FieldSchema("jmax", int, 50, 10, None),
    FieldSchema("kmin", int, 60, 10, None),
    FieldSchema("kmax", int, 70, 10, None),
)

_SETSEGMENTGENERAL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetSegmentGeneral(KeywordBase):
    """DYNA SET_SEGMENT_GENERAL keyword"""

    keyword = "SET"
    subkeyword = "SEGMENT_GENERAL"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SetSegmentGeneral class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETSEGMENTGENERAL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _SETSEGMENTGENERAL_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _SETSEGMENTGENERAL_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = SetSegmentGeneral._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETSEGMENTGENERAL_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID. All segment sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First segment attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second segment attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third segment attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth segment attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        """Set the da4 property."""
        self._cards[0].set_value("da4", value)

    @property
    def solver(self) -> str:
        """Get or set the EQ.MECH: mechanict.
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
    def its(self) -> typing.Optional[int]:
        """Get or set the Define coupling type across different scales in two-scale co-simulation. See *INCLUDE_COSIM in Manual Volume IV: Multiscale Solvers.
        EQ.1: Tie - contact coupling.
        EQ.2: Solid - in - shell immersed coupling
        """ # nopep8
        return self._cards[0].get_value("its")

    @its.setter
    def its(self, value: int) -> None:
        """Set the its property."""
        self._cards[0].set_value("its", value)

    @property
    def option(self) -> str:
        """Get or set the 
        ALL: All exterior segments will be included in the set.
        BOX:Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  For shell elements, one segment per shell is generated. For solid elements, only those segments wrapping the solid part and pointing outward from the part will be generated.
        BOX_SHELL:Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  The segments are only generated for shell elements.  One segment per shell is generated.
        BOX_SLDIO:Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  Both exterior segments and inter-element segments are generated.
        BOX_SOLID:Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  The segments are only generated for exterior solid elements.
        BRANCH:Generate segments from tree branches E1, E2, and E3 with attributes E4, E5, E6, and E7.  For shell elementsSET one segment is generated per shell.  For solid elements, only those segments that wrap the solid part and point outward from the part will be generated.  See *SET_PART_TREE.
        BRANCH_IO:Generate segments from tree branches E1, E2, and E3 with attributes E4, E5, E6, and E7.  Same as the BRANCH option above, except that inter-element segments inside parts will be generated as well.  This option is sometimes useful for preventing negative volumes when using single surface contact between solid elements.
        BRSLDFi:Generate segments from the ith face of solid tree branches E1, E2, and E3 with attributes E4, E5, E6, and E7.  See Table 0-1 below for the face definition.
        DBOX: Segments inside boxes with IDs E1, �, E7 will be excluded.
        DBOX_SHELL: Shell-related segments inside boxes of IDs E1, �, and E7 will be excluded.
        DBOX_SOLID: Solid-related segments inside boxes of IDs E1, �, and E7 will be excluded.
        DPART: Segments of parts with IDs E1, �, and E7 will be excluded.
        DSEG: Segment with node IDs  E1, E2, E3, and E4 will be deleted.
        DSET: Segments of segment sets (*SET_SEGMENT) with IDs E1, E2, E3� will be excluded.
        DVOL: Segments inside contact volumes having IDs E1, �, and E7 will be excluded.
        DVOL_SHELL: Shell-related segments inside contact volumes having IDs E1, �, and E7 will be excluded.
        DVOL_SOLID: Solid-related segments inside contact volumes having IDs E1, �, and E7 will be excluded.
        PART: Generate segments from parts E1, E2, and E3 with attributes E4, E5, E6, and E7.  For shell elements, one segment per shell is generated.  For solid elements, only those segments wrapping the solid part and pointing outward from the part will be generated.  PART could refer to beam parts when defining 2D segments for traction application.
        PART_IO: Generate segments from parts E1, E2, and E3 with attributes E4, E5, E6, and E7.  Same as the PART option above except that inter-element segments inside parts will be generated as well.  This option is sometimes useful for single surface contact of solid elements to prevent negative volumes.
        PSLDFi: Generate segments from the ith face of solid parts E1, E2, and E3 with attributes E4, E5, E6, and E7.  See Table 0-1 below for face definition.
        SALEBOXL Segments inside a box in a structured ALE(S - ALE) mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, E5, E6, and E7 correspond to XMIN, XMAX, YMIN, YMAX, ZMIN, and ZMAX, respectively.They are the minimum and the maximum nodal local coordinates, as defined in* ALE_STRUCTURED_MESH_CONTROL_POINTS, along each direction in S - ALE mesh.This option is only to be used for S - ALE meshes..It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options.
        Refer to* ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        SALECPT Segments inside a box in a structured ALE(S - ALE) mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, E5, E6, and E7 correspond to IMIN, IMAX, JMIN, JMAX, KMIN, and KMAX.They are the minimum and the maximum nodal indices along each direction in the S - ALE mesh.To include all segments in the S-ALE mesh defined by E1, set values E2�E7 to zero or leave them blank.  This option is only to be used for S - ALE meshes..It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options.
        Refer to* ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        SALEFAC Segments on the face of an S - ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, E5, E6, E7 correspond to - X, +X, -Y, +Y, -Z, +Z faces.Assigning 1 to these 6 values would include all the surface segments at these faces in the segment set.This option is only to be used for S - ALE meshes..It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options.
        For trimmed S - ALE meshes(see * ALE_STRUCTURED_MESH_TRIM), a segment is treated as a surface segment as long as it has no neighboring element along the specified direction.The set thus includes all surface segments facing that direction at the exterior and interior boundaries.
        Refer to* ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        SEG : Create a segment with node IDs E1, E2, E3, and E4.
        SET : Segments of segment sets(*SET_SEGMENT) with IDs E1, E2, E3� will be included.
        SET_SHELL : Generate segments from shell elements in shell sets created with* SET_SHELL_LIST with IDs E1, E2, and E3.E4, E5, E6, and E7 set the attributes.
        SET_SOLID : Generate segments from solid elements in solid sets created with* SET_SOLID_LIST with IDs E1, E2, and E3.E4, E5, E6, and E7 set the attributes.
        SET_SLDIO : Generate segments for solid elements in solid sets created with* SET_SOLID_LIST with IDs E1, E2, and E3.E4, E5, E6, and E7 set the attributes.Both exterior and interior segments are generated.
        SET_SLDFi : Generate segments from the ith face of solid elements in the solid element sets created with* SET_SOLID_LIST with IDs E1, E2, and E3.E4, E5, E6, and E7 set the attributes.See Table 0 - 1 below for face definitions.
        SET_TSHELL : Generate segments from thick shell elements in the thick shell sets created with* SET_TSHELL_LIST with IDs of E1, E2, and E3.E4, E5, E6, and E7 set the attributes.Only exterior segments are generated.
        SET_TSHIO : Generate segments from thick shell elements in the thick shell sets created with* SET_TSHELL_LIST with IDs of E1, E2, and E3.E4, E5, E6, and E7 set the attributes.Both exterior and interior segments are generated.
        SHELL : Generate segments for shell elements with IDs of E1, E2, and E3 with attributes having values E4, E5, E6, and E7.
        VOL : Generate segments inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.See BOX option for other details.See* DEFINE_CONTACT_VOLUME.
        VOL_SHELL : Generate segments from shells inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7
        VOL_SLDIO : Generate segments from solid elements inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.See BOX_SLDIO for other details.
        VOL_SOLID : Generate segments from solid elements inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.See BOX_SOLID for other details.
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "BOX", "BOX_SHELL", "BOX_SLDIO", "BOX_SOLID", "BRANCH", "BRANCHI_IO", "BRSLDFi", "DBOX", "DBOX_SHELL", "DBOX_SOLID", "DPART", "DSET", "DVOL", "DVOL_SHELL", "DVOL_SOLID", "PART", "PART_IO", "PSLDFi", "SALEBOXL", "SALEFAC", "SALECPT", "SEG", "SET", "SET_SHELL", "SET_SOLID", "SET_SLDIO", "SET_SLDFi", "SET_TSHELL", "SET_TSHIO", "SHELL", "VOL", "VOL_SHELL", "VOL_SLDIO", "VOL_SOLID", None]:
            raise Exception("""option must be `None` or one of {"ALL","BOX","BOX_SHELL","BOX_SLDIO","BOX_SOLID","BRANCH","BRANCHI_IO","BRSLDFi","DBOX","DBOX_SHELL","DBOX_SOLID","DPART","DSET","DVOL","DVOL_SHELL","DVOL_SOLID","PART","PART_IO","PSLDFi","SALEBOXL","SALEFAC","SALECPT","SEG","SET","SET_SHELL","SET_SOLID","SET_SLDIO","SET_SLDFi","SET_TSHELL","SET_TSHIO","SHELL","VOL","VOL_SHELL","VOL_SLDIO","VOL_SOLID"}.""")
        self._cards[1].set_value("option", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.DBOX: Segment inside box E1 will be excluded from the current set,
        OPTION.EQ.DBOX_SHELL: Shell related segments inside box E1 will be excluded from the current set,
        OPTION.EQ.DBOX_SOLID: Solid related segments inside box E1 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E1 previously added will be excluded from the current set,
        OPTION.EQ.DSEG: Segments with node ID's E1, E2, E3, and E4 previously added will be deleted. The numbering sequence is irrelevant,
        OPTION.EQ.SEG: Create segment with node ID's E1, E2, E3, and E4.
        """ # nopep8
        return self._cards[1].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        """Set the e1 property."""
        self._cards[1].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.DBOX: Segment inside box E2 will be excluded from the current set,
        OPTION.EQ.DBOX_SHELL: Shell related segments inside box E2 will be excluded from the current set,
        OPTION.EQ.DBOX_SOLID: Solid related segments inside box E2 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E2 previously added will be excluded from the current set,
        OPTION.EQ.DSEG: Segments with node ID's E1, E2, E3, and E4 previously added will be deleted. The numbering sequence is irrelevant,
        OPTION.EQ.SEG: Create segment with node ID's E1, E2, E3, and E4.
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        """Set the e2 property."""
        self._cards[1].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.DBOX: Segment inside box E3 will be excluded from the current set,
        OPTION.EQ.DBOX_SHELL: Shell related segments inside box E3 will be excluded from the current set,
        OPTION.EQ.DBOX_SOLID: Solid related segments inside box E3 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E3 previously added will be excluded from the current set,
        OPTION.EQ.DSEG: Segments with node ID's E1, E2, E3, and E4 previously added will be deleted. The numbering sequence is irrelevant,
        OPTION.EQ.SEG: Create segment with node ID's E1, E2, E3, and E4.
        """ # nopep8
        return self._cards[1].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        """Set the e3 property."""
        self._cards[1].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.DBOX: Segment inside box E4 will be excluded from the current set,
        OPTION.EQ.DBOX_SHELL: Shell related segments inside box E4 will be excluded from the current set,
        OPTION.EQ.DBOX_SOLID: Solid related segments inside box E4 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E4 previously added will be excluded from the current set,
        OPTION.EQ.DSEG: Segments with node ID's E1, E2, E3, and E4 previously added will be deleted. The numbering sequence is irrelevant,
        OPTION.EQ.SEG: Create segment with node ID's E1, E2, E3, and E4.
        """ # nopep8
        return self._cards[1].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        """Set the e4 property."""
        self._cards[1].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.DBOX: Segment inside box E5 will be excluded from the current set,
        OPTION.EQ.DBOX_SHELL: Shell related segments inside box E5 will be excluded from the current set,
        OPTION.EQ.DBOX_SOLID: Solid related segments inside box E5 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E5 previously added will be excluded from the current set,
        OPTION.EQ.DSEG: Not used,
        OPTION.EQ.SEG: Not used.
        """ # nopep8
        return self._cards[1].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        """Set the e5 property."""
        self._cards[1].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.DBOX: Segment inside box E6 will be excluded from the current set,
        OPTION.EQ.DBOX_SHELL: Shell related segments inside box E6 will be excluded from the current set,
        OPTION.EQ.DBOX_SOLID: Solid related segments inside box E6 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E6 previously added will be excluded from the current set,
        OPTION.EQ.DSEG: Not used,
        OPTION.EQ.SEG: Not used.
        """ # nopep8
        return self._cards[1].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        """Set the e6 property."""
        self._cards[1].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.DBOX: Segment inside box E7 will be excluded from the current set,
        OPTION.EQ.DBOX_SHELL: Shell related segments inside box E7 will be excluded from the current set,
        OPTION.EQ.DBOX_SOLID: Solid related segments inside box E7 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E7 previously added will be excluded from the current set,
        OPTION.EQ.DSEG: Not used,
        OPTION.EQ.SEG: Not used.
        """ # nopep8
        return self._cards[1].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        """Set the e7 property."""
        self._cards[1].set_value("e7", value)

    @property
    def option(self) -> str:
        """Get or set the 
        ALL: All exterior segments will be included in the set.
        BOX:Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  For shell elements, one segment per shell is generated. For solid elements, only those segments wrapping the solid part and pointing outward from the part will be generated.
        BOX_SHELL:Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  The segments are only generated for shell elements.  One segment per shell is generated.
        BOX_SLDIO:Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  Both exterior segments and inter-element segments are generated.
        BOX_SOLID:Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  The segments are only generated for exterior solid elements.
        BRANCH:Generate segments from tree branches E1, E2, and E3 with attributes E4, E5, E6, and E7.  For shell elementsSET one segment is generated per shell.  For solid elements, only those segments that wrap the solid part and point outward from the part will be generated.  See *SET_PART_TREE.
        BRANCH_IO:Generate segments from tree branches E1, E2, and E3 with attributes E4, E5, E6, and E7.  Same as the BRANCH option above, except that inter-element segments inside parts will be generated as well.  This option is sometimes useful for preventing negative volumes when using single surface contact between solid elements.
        BRSLDFi:Generate segments from the ith face of solid tree branches E1, E2, and E3 with attributes E4, E5, E6, and E7.  See Table 0-1 below for the face definition.
        DBOX: Segments inside boxes with IDs E1, �, E7 will be excluded.
        DBOX_SHELL: Shell-related segments inside boxes of IDs E1, �, and E7 will be excluded.
        DBOX_SOLID: Solid-related segments inside boxes of IDs E1, �, and E7 will be excluded.
        DPART: Segments of parts with IDs E1, �, and E7 will be excluded.
        DSEG: Segment with node IDs  E1, E2, E3, and E4 will be deleted.
        DSET: Segments of segment sets (*SET_SEGMENT) with IDs E1, E2, E3� will be excluded.
        DVOL: Segments inside contact volumes having IDs E1, �, and E7 will be excluded.
        DVOL_SHELL: Shell-related segments inside contact volumes having IDs E1, �, and E7 will be excluded.
        DVOL_SOLID: Solid-related segments inside contact volumes having IDs E1, �, and E7 will be excluded.
        PART: Generate segments from parts E1, E2, and E3 with attributes E4, E5, E6, and E7.  For shell elements, one segment per shell is generated.  For solid elements, only those segments wrapping the solid part and pointing outward from the part will be generated.  PART could refer to beam parts when defining 2D segments for traction application.
        PART_IO: Generate segments from parts E1, E2, and E3 with attributes E4, E5, E6, and E7.  Same as the PART option above except that inter-element segments inside parts will be generated as well.  This option is sometimes useful for single surface contact of solid elements to prevent negative volumes.
        PSLDFi: Generate segments from the ith face of solid parts E1, E2, and E3 with attributes E4, E5, E6, and E7.  See Table 0-1 below for face definition.
        SALEBOXL Segments inside a box in a structured ALE(S - ALE) mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, E5, E6, and E7 correspond to XMIN, XMAX, YMIN, YMAX, ZMIN, and ZMAX, respectively.They are the minimum and the maximum nodal local coordinates, as defined in* ALE_STRUCTURED_MESH_CONTROL_POINTS, along each direction in S - ALE mesh.This option is only to be used for S - ALE meshes..It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options.
        Refer to* ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        SALECPT Segments inside a box in a structured ALE(S - ALE) mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, E5, E6, and E7 correspond to IMIN, IMAX, JMIN, JMAX, KMIN, and KMAX.They are the minimum and the maximum nodal indices along each direction in the S - ALE mesh.This option is only to be used for S - ALE meshes..It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options.
        Refer to* ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        SALEFAC Segments on the face of an S - ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, E5, E6, E7 correspond to - X, +X, -Y, +Y, -Z, +Z faces.Assigning 1 to these 6 values would include all the surface segments at these faces in the segment set.This option is only to be used for S - ALE meshes..It can be used with options SALECPT, SALEFAC, and SALEBOXL but should not be used with other GENERAL options.
        For trimmed S - ALE meshes(see * ALE_STRUCTURED_MESH_TRIM), a segment is treated as a surface segment as long as it has no neighboring element along the specified direction.The set thus includes all surface segments facing that direction at the exterior and interior boundaries.
        Refer to* ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH for more details.
        SEG : Create a segment with node IDs E1, E2, E3, and E4.
        SET : Segments of segment sets(*SET_SEGMENT) with IDs E1, E2, E3� will be included.
        SET_SHELL : Generate segments from shell elements in shell sets created with* SET_SHELL_LIST with IDs E1, E2, and E3.E4, E5, E6, and E7 set the attributes.
        SET_SOLID : Generate segments from solid elements in solid sets created with* SET_SOLID_LIST with IDs E1, E2, and E3.E4, E5, E6, and E7 set the attributes.
        SET_SLDIO : Generate segments for solid elements in solid sets created with* SET_SOLID_LIST with IDs E1, E2, and E3.E4, E5, E6, and E7 set the attributes.Both exterior and interior segments are generated.
        SET_SLDFi : Generate segments from the ith face of solid elements in the solid element sets created with* SET_SOLID_LIST with IDs E1, E2, and E3.E4, E5, E6, and E7 set the attributes.See Table 0 - 1 below for face definitions.
        SET_TSHELL : Generate segments from thick shell elements in the thick shell sets created with* SET_TSHELL_LIST with IDs of E1, E2, and E3.E4, E5, E6, and E7 set the attributes.Only exterior segments are generated.
        SET_TSHIO : Generate segments from thick shell elements in the thick shell sets created with* SET_TSHELL_LIST with IDs of E1, E2, and E3.E4, E5, E6, and E7 set the attributes.Both exterior and interior segments are generated.
        SHELL : Generate segments for shell elements with IDs of E1, E2, and E3 with attributes having values E4, E5, E6, and E7.
        VOL : Generate segments inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.See BOX option for other details.See* DEFINE_CONTACT_VOLUME.
        VOL_SHELL : Generate segments from shells inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7
        VOL_SLDIO : Generate segments from solid elements inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.See BOX_SLDIO for other details.
        VOL_SOLID : Generate segments from solid elements inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.See BOX_SOLID for other details.
        """ # nopep8
        return self._cards[2].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        """Set the option property."""
        if value not in ["ALL", "BOX", "BOX_SHELL", "BOX_SLDIO", "BOX_SOLID", "BRANCH", "BRANCHI_IO", "BRSLDFi", "DBOX", "DBOX_SHELL", "DBOX_SOLID", "DPART", "DSET", "DVOL", "DVOL_SHELL", "DVOL_SOLID", "PART", "PART_IO", "PSLDFi", "SALEBOXL", "SALEFAC", "SALECPT", "SEG", "SET", "SET_SHELL", "SET_SOLID", "SET_SLDIO", "SET_SLDFi", "SET_TSHELL", "SET_TSHIO", "SHELL", "VOL", "VOL_SHELL", "VOL_SLDIO", "VOL_SOLID", None]:
            raise Exception("""option must be `None` or one of {"ALL","BOX","BOX_SHELL","BOX_SLDIO","BOX_SOLID","BRANCH","BRANCHI_IO","BRSLDFi","DBOX","DBOX_SHELL","DBOX_SOLID","DPART","DSET","DVOL","DVOL_SHELL","DVOL_SOLID","PART","PART_IO","PSLDFi","SALEBOXL","SALEFAC","SALECPT","SEG","SET","SET_SHELL","SET_SOLID","SET_SLDIO","SET_SLDFi","SET_TSHELL","SET_TSHIO","SHELL","VOL","VOL_SHELL","VOL_SLDIO","VOL_SOLID"}.""")
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

