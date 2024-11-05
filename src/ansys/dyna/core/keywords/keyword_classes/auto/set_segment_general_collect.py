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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SetSegmentGeneralCollect(KeywordBase):
    """DYNA SET_SEGMENT_GENERAL_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "SEGMENT_GENERAL_COLLECT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "da1",
                        float,
                        10,
                        10,
                        kwargs.get("da1", 0.0)
                    ),
                    Field(
                        "da2",
                        float,
                        20,
                        10,
                        kwargs.get("da2", 0.0)
                    ),
                    Field(
                        "da3",
                        float,
                        30,
                        10,
                        kwargs.get("da3", 0.0)
                    ),
                    Field(
                        "da4",
                        float,
                        40,
                        10,
                        kwargs.get("da4", 0.0)
                    ),
                    Field(
                        "solver",
                        str,
                        50,
                        10,
                        kwargs.get("solver", "MECH")
                    ),
                    Field(
                        "its",
                        int,
                        60,
                        10,
                        kwargs.get("its")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "option",
                        str,
                        0,
                        10,
                        kwargs.get("option", "ALL")
                    ),
                    Field(
                        "e1",
                        int,
                        10,
                        10,
                        kwargs.get("e1")
                    ),
                    Field(
                        "e2",
                        int,
                        20,
                        10,
                        kwargs.get("e2")
                    ),
                    Field(
                        "e3",
                        int,
                        30,
                        10,
                        kwargs.get("e3")
                    ),
                    Field(
                        "e4",
                        int,
                        40,
                        10,
                        kwargs.get("e4")
                    ),
                    Field(
                        "e5",
                        int,
                        50,
                        10,
                        kwargs.get("e5")
                    ),
                    Field(
                        "e6",
                        int,
                        60,
                        10,
                        kwargs.get("e6")
                    ),
                    Field(
                        "e7",
                        int,
                        70,
                        10,
                        kwargs.get("e7")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetSegmentGeneralCollect.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
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
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First segment attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second segment attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third segment attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth segment attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
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
        if value not in ["MECH", "CESE", "ICFD"]:
            raise Exception("""solver must be one of {"MECH","CESE","ICFD"}""")
        self._cards[0].set_value("solver", value)

    @property
    def its(self) -> typing.Optional[int]:
        """Get or set the Define coupling type across different scales in two-scale co-simulation. See *INCLUDE_COSIM in Manual Volume IV: Multiscale Solvers.
        EQ.1:	Tie - contact coupling.
        EQ.2 : Solid - in - shell immersed coupling
        """ # nopep8
        return self._cards[0].get_value("its")

    @its.setter
    def its(self, value: int) -> None:
        self._cards[0].set_value("its", value)

    @property
    def option(self) -> str:
        """Get or set the ALL
        All exterior segments will be included in the set.
        BOX
        Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  For shell elements one segment per shell is generated. For solid elements only those segments wrapping the solid part and pointing outward from the part will be generated.
        BOX_SHELL
        Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  The segments are only generated for shell elements.  One segment per shell is generated.
        BOX_SLDIO
        Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  Both exterior segments and inter-element segments are generated.
        BOX_SOLID
        Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  The segments are only generated for exterior solid elements
        PART
        Generate segments of parts E1, E2, and E3 with attributes E4, E5, E6, and E7.  For shell elements one segment per shell is generated.  For solid elements only those segments wrapping the solid part and pointing outward from the part will be generated.  PART could refer to beam parts when defining 2D segments for traction application.
        PART_IO
        Generate segments from parts E1, E2, E3 with attributes E4, E5, E6, and E7.  Same as the PART option above except that inter-element segments inside parts will be generated as well.  This option is sometimes useful for single surface contact of solid elements to prevent negative volumes.
        PSLDFi	Generate segments from the i’th face of solid parts E1, E2, E3 with attributes E4, E5, E6, and E7.  See table below for face definition.
        SEG
        Create segment with node IDs E1, E2, E3, and E4.
        VOL	Generate segments inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  See BOX option for other details.
        VOL_SHELL
        Generate segments for shells inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7
        VOL_SLDIO
        Generate segments for solid elements inside contact volume IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.  See BOX_SLDIO for other details.
        VOL_SOLID
        Generate segments for solid elements inside contact volume IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.  See BOX_SOLID for other details.
        SET_SHELL
        Generate segments for shell elements in SET_SHELL_LIST with IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.
        SET_SOLID
        Generate segments for solid elements in SET_SOLID_LIST with IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.
        SET_SLDIO
        Generate segments for solid elements in SET_SOLID_LIST with IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.  Both exterior & interior segments are generated.
        SET_SLDFi
        Generate segments from the ith face of solid elements in SET_SOLID_LIST with IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.  See table below for face definition.
        SET_TSHELL
        Generate segments for thick shell elements in SET_TSHELL_LIST with IDs of E1, E2, and E3 with attributes E4, E5, E6, and E7.  Only exterior segments are generated.
        SET_TSHIO
        Generate segments for thick shell elements in SET_TSHELL_LIST with IDs of E1, E2, and E3 with attributes E5, E5, E6, and E7.  Both exterior & interior segments are generated.
        SHELL Generate segments for shell elements with IDs of E1, E2, and E3 with attributes E4, E5, E6, and E7.
        DBOX	Segments inside boxes with IDs E1, ? E7 will be excluded.
        DBOX_SHELL
        Shell related segments inside boxes of IDs E1, ? E7 will be excluded.
        DBOX_SOLID
        Solid related segments inside boxes of IDs E1, ? E7 will be excluded.
        DPART	Segments of parts with IDs E1, ? E7 will be excluded.
        DSEG	Segment with node IDs  E1, E2, E3, and E4 will be deleted.
        DVOL	Segments inside contact volumes having IDs E1, ? E7 will be excluded.
        DVOL_SHELL
        Shell related segments inside contact volumes having IDs E1, ? E7 will be excluded.
        DVOL_SOLID
        Solid related segments inside contact volumes having IDs E1, ? E7 will be excluded.
        SALECPT	Segments inside a box in Structured ALE mesh. E1 here is the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, E7 correspond to XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX.  They are the minimum and the maximum nodal indices along each direction in S-ALE mesh.  This option is only to be used for Structured ALE mesh and should not be used in a mixed manner with other “_GENERAL?options.
        Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH_CONTROL for more details.
        SALEFAC	Segments on the face of Structured ALE mesh. E1 here is the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, E7 correspond to -X, +X, -Y, +Y, -Z, +Z faces.  Assigning 1 to these 6 values would include all the surface segments at these faces in the segment set.  This option is only to be used for Structured ALE mesh and should not be used in a mixed manner with other “_GENERAL?options.
        Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH_CONTROL for more details
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        if value not in ["ALL", "BOX", "BOX_SHELL", "BOX_SLDIO", "BOX_SOLID", "PART", "PART_IO", "PSLDFi", "SEG", "VOL", "VOL_SHELL", "VOL_SLDIO", "VOL_SOLID", "SET_SHELL", "SET_SOLID", "SET_SLDIO", "SET_SLDFi", "SET_TSHELL", "SET_TSHIO", "SHELL", "DBOX", "DBOX_SHELL", "DBOX_SOLID", "DPART", "DSEG", "DVOL", "DVOL_SHELL", "DVOL_SOLID", "SALECPT", "SALEFAC"]:
            raise Exception("""option must be one of {"ALL","BOX","BOX_SHELL","BOX_SLDIO","BOX_SOLID","PART","PART_IO","PSLDFi","SEG","VOL","VOL_SHELL","VOL_SLDIO","VOL_SOLID","SET_SHELL","SET_SOLID","SET_SLDIO","SET_SLDFi","SET_TSHELL","SET_TSHIO","SHELL","DBOX","DBOX_SHELL","DBOX_SOLID","DPART","DSEG","DVOL","DVOL_SHELL","DVOL_SOLID","SALECPT","SALEFAC"}""")
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
        self._cards[1].set_value("e7", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

