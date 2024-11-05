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

class ContactEntity(KeywordBase):
    """DYNA CONTACT_ENTITY keyword"""

    keyword = "CONTACT"
    subkeyword = "ENTITY"
    option_specs = [
        OptionSpec("ID", -2, 1),
        OptionSpec("MPP", -1, 2),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "geotyp",
                        int,
                        10,
                        10,
                        kwargs.get("geotyp", 1)
                    ),
                    Field(
                        "ssid",
                        int,
                        20,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "sstyp",
                        int,
                        30,
                        10,
                        kwargs.get("sstyp", 0)
                    ),
                    Field(
                        "sf",
                        float,
                        40,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "df",
                        float,
                        50,
                        10,
                        kwargs.get("df", 0.0)
                    ),
                    Field(
                        "cf",
                        float,
                        60,
                        10,
                        kwargs.get("cf", 0.0)
                    ),
                    Field(
                        "intord",
                        int,
                        70,
                        10,
                        kwargs.get("intord", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "bt",
                        float,
                        0,
                        10,
                        kwargs.get("bt", 0.0)
                    ),
                    Field(
                        "dt",
                        float,
                        10,
                        10,
                        kwargs.get("dt", 1.0E+20)
                    ),
                    Field(
                        "so",
                        int,
                        20,
                        10,
                        kwargs.get("so", 0)
                    ),
                    Field(
                        "go",
                        int,
                        30,
                        10,
                        kwargs.get("go", 0)
                    ),
                    Field(
                        "ithk",
                        int,
                        40,
                        10,
                        kwargs.get("ithk", 0)
                    ),
                    Field(
                        "spr",
                        int,
                        50,
                        10,
                        kwargs.get("spr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xc",
                        float,
                        0,
                        10,
                        kwargs.get("xc", 0.0)
                    ),
                    Field(
                        "yc",
                        float,
                        10,
                        10,
                        kwargs.get("yc", 0.0)
                    ),
                    Field(
                        "zc",
                        float,
                        20,
                        10,
                        kwargs.get("zc", 0.0)
                    ),
                    Field(
                        "ax",
                        float,
                        30,
                        10,
                        kwargs.get("ax", 0.0)
                    ),
                    Field(
                        "ay",
                        float,
                        40,
                        10,
                        kwargs.get("ay", 0.0)
                    ),
                    Field(
                        "az",
                        float,
                        50,
                        10,
                        kwargs.get("az", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "bx",
                        float,
                        0,
                        10,
                        kwargs.get("bx", 0.0)
                    ),
                    Field(
                        "by",
                        float,
                        10,
                        10,
                        kwargs.get("by", 0.0)
                    ),
                    Field(
                        "bz",
                        float,
                        20,
                        10,
                        kwargs.get("bz", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "inout",
                        int,
                        0,
                        10,
                        kwargs.get("inout", 0)
                    ),
                    Field(
                        "g1",
                        float,
                        10,
                        10,
                        kwargs.get("g1", 0.0)
                    ),
                    Field(
                        "g2",
                        float,
                        20,
                        10,
                        kwargs.get("g2", 0.0)
                    ),
                    Field(
                        "g3",
                        float,
                        30,
                        10,
                        kwargs.get("g3", 0.0)
                    ),
                    Field(
                        "g4",
                        float,
                        40,
                        10,
                        kwargs.get("g4", 0.0)
                    ),
                    Field(
                        "g5",
                        float,
                        50,
                        10,
                        kwargs.get("g5", 0.0)
                    ),
                    Field(
                        "g6",
                        float,
                        60,
                        10,
                        kwargs.get("g6", 0.0)
                    ),
                    Field(
                        "g7",
                        float,
                        70,
                        10,
                        kwargs.get("g7", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = ContactEntity.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "cid",
                                int,
                                0,
                                10,
                                kwargs.get("cid")
                            ),
                            Field(
                                "heading",
                                str,
                                10,
                                70,
                                kwargs.get("heading")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = ContactEntity.option_specs[1],
                cards = [
                    Card(
                        [
                            Field(
                                "ignore",
                                int,
                                0,
                                10,
                                kwargs.get("ignore", 0)
                            ),
                            Field(
                                "bckt",
                                int,
                                10,
                                10,
                                kwargs.get("bckt", 200)
                            ),
                            Field(
                                "lcbckt",
                                int,
                                20,
                                10,
                                kwargs.get("lcbckt")
                            ),
                            Field(
                                "ns2trk",
                                int,
                                30,
                                10,
                                kwargs.get("ns2trk", 3)
                            ),
                            Field(
                                "inititr",
                                int,
                                40,
                                10,
                                kwargs.get("inititr", 2)
                            ),
                            Field(
                                "parmax",
                                float,
                                50,
                                10,
                                kwargs.get("parmax", 1.0005)
                            ),
                            Field(
                                "unused",
                                int,
                                60,
                                10,
                                kwargs.get("unused")
                            ),
                            Field(
                                "cparm8",
                                int,
                                70,
                                10,
                                kwargs.get("cparm8", 0)
                            ),
                        ],
                    ),
                    Card(
                        [
                            Field(
                                "mpp2",
                                bool,
                                0,
                                10,
                                Flag(
                                    kwargs.get("mpp2", True),
                                    "&",
                                    ""
                                )
                            ),
                            Field(
                                "chksegs",
                                int,
                                10,
                                10,
                                kwargs.get("chksegs", 0)
                            ),
                            Field(
                                "pensf",
                                float,
                                20,
                                10,
                                kwargs.get("pensf", 1.0)
                            ),
                            Field(
                                "grpable",
                                int,
                                30,
                                10,
                                kwargs.get("grpable", 0)
                            ),
                        ],
                        lambda: self.mpp2,
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the rigid body to which the geometric entity is attached, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def geotyp(self) -> int:
        """Get or set the Type of geometric entity:
        EQ.1: plane (default),
        EQ.2: sphere,
        EQ.3: cylinder,
        EQ.4: ellipsoid,
        EQ.5: torus,
        EQ.6: CAL3D/MADYMO plane, see Appendix F of USER MANUAL,
        EQ.7: CAL3D/MADYMO ellipsoid, see Appendix F of USER MANUAL,
        EQ.8: VDA surface, see Appendix I of USER MANUAL,
        EQ.9: rigid body finite element mesh (shells only),
        EQ.10: finite plane,
        EQ.11: load curve defining line as surface profile of axisymmetric rigid bodies.
        """ # nopep8
        return self._cards[0].get_value("geotyp")

    @geotyp.setter
    def geotyp(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]:
            raise Exception("""geotyp must be one of {1,2,3,4,5,6,7,8,9,10,11}""")
        self._cards[0].set_value("geotyp", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Slave set ID, see *SET_NODE_OPTION, *PART, or *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def sstyp(self) -> int:
        """Get or set the Slave set type:
        EQ.0: node set (default),
        EQ.1: part ID,
        EQ.2: part set ID.
        """ # nopep8
        return self._cards[0].get_value("sstyp")

    @sstyp.setter
    def sstyp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sstyp must be one of {0,1,2}""")
        self._cards[0].set_value("sstyp", value)

    @property
    def sf(self) -> float:
        """Get or set the Penalty scale factor. Useful to scale maximized penalty.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def df(self) -> float:
        """Get or set the Damping option, see description for *CONTACT_OPTION:
        EQ.0.0: no damping (default),
        GT.0.0: viscous damping in percent of critical, e.g., 20.0 for 20% damping,
        EQ.-n: |n| is the load curve ID giving the damping force versus relative normal velocity.
        """ # nopep8
        return self._cards[0].get_value("df")

    @df.setter
    def df(self, value: float) -> None:
        self._cards[0].set_value("df", value)

    @property
    def cf(self) -> float:
        """Get or set the Coulomb friction coefficient. Assumed to be constant (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("cf")

    @cf.setter
    def cf(self, value: float) -> None:
        self._cards[0].set_value("cf", value)

    @property
    def intord(self) -> int:
        """Get or set the Integration order (slaved materials only). This option is not available with entity types 8 and 9 where only nodes are checked:
        EQ.0: check nodes only (default),
        EQ.1: 1-point integration over segments,
        EQ.2: 2x2 integration,
        EQ.3: 3x3 integration,
        EQ.4: 4x4 integration,
        EQ.5: 5x5 integration.
        This option allows a check of the penetration of the rigid body into the deformable (slaved) material. Then virtual nodes at the location of the integration points are checked.
        """ # nopep8
        return self._cards[0].get_value("intord")

    @intord.setter
    def intord(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5]:
            raise Exception("""intord must be one of {0,1,2,3,4,5}""")
        self._cards[0].set_value("intord", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        self._cards[1].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def so(self) -> int:
        """Get or set the Flag to use penalty stiffness as in surface to surface contact:
        EQ.0: contact entity stiffness formulation (default),
        EQ.1: surface to surface contact method,
        EQ.-n: |n| is the load curve ID giving the force versus the normal penetration.
        """ # nopep8
        return self._cards[1].get_value("so")

    @so.setter
    def so(self, value: int) -> None:
        self._cards[1].set_value("so", value)

    @property
    def go(self) -> int:
        """Get or set the Flag for mesh generation of the contact entity for entity types 1-5 and 10-11. This is used for visualization in post-processing only:
        EQ.0: mesh is not generated (default),
        EQ.1: mesh is generated.
        """ # nopep8
        return self._cards[1].get_value("go")

    @go.setter
    def go(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""go must be one of {0,1}""")
        self._cards[1].set_value("go", value)

    @property
    def ithk(self) -> int:
        """Get or set the Flag for considering thickness for shell slave nodes (applies only
        to entity types 1, 2, 3; SSTYP must be set to zero).
        EQ.0: shell thickness is not considered,
        EQ.1: shell thickness is considered.
        """ # nopep8
        return self._cards[1].get_value("ithk")

    @ithk.setter
    def ithk(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ithk must be one of {0,1}""")
        self._cards[1].set_value("ithk", value)

    @property
    def spr(self) -> int:
        """Get or set the Include the slave side in *DATABASE_BINARY_INTFOR
        interface force files; valid only when SSTYP > 0:
        EQ.1: slave side forces included..
        """ # nopep8
        return self._cards[1].get_value("spr")

    @spr.setter
    def spr(self, value: int) -> None:
        self._cards[1].set_value("spr", value)

    @property
    def xc(self) -> float:
        """Get or set the xc, x-center.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[2].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the yc, y-center.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[2].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the zc, z-center.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[2].set_value("zc", value)

    @property
    def ax(self) -> float:
        """Get or set the Ax, x-direction for local axis A.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("ax")

    @ax.setter
    def ax(self, value: float) -> None:
        self._cards[2].set_value("ax", value)

    @property
    def ay(self) -> float:
        """Get or set the Ay, y-direction for local axis A.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("ay")

    @ay.setter
    def ay(self, value: float) -> None:
        self._cards[2].set_value("ay", value)

    @property
    def az(self) -> float:
        """Get or set the Az, z-direction for local axis A.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[2].get_value("az")

    @az.setter
    def az(self, value: float) -> None:
        self._cards[2].set_value("az", value)

    @property
    def bx(self) -> float:
        """Get or set the Bx, x-direction for local axis B.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[3].get_value("bx")

    @bx.setter
    def bx(self, value: float) -> None:
        self._cards[3].set_value("bx", value)

    @property
    def by(self) -> float:
        """Get or set the By, y-direction for local axis B.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[3].get_value("by")

    @by.setter
    def by(self, value: float) -> None:
        self._cards[3].set_value("by", value)

    @property
    def bz(self) -> float:
        """Get or set the Bz, z-direction for local axis B.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[3].get_value("bz")

    @bz.setter
    def bz(self, value: float) -> None:
        self._cards[3].set_value("bz", value)

    @property
    def inout(self) -> int:
        """Get or set the In-out flag. Allows contact from the inside or the outside of the entity:
        EQ.0: slave nodes exist outside of the entity (default),
        EQ.1: slave nodes exist inside the entity.
        """ # nopep8
        return self._cards[4].get_value("inout")

    @inout.setter
    def inout(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""inout must be one of {0,1}""")
        self._cards[4].set_value("inout", value)

    @property
    def g1(self) -> float:
        """Get or set the Entity coefficient g1 (CAL3D/MADYMO plane or ellipse number) for coupled analysis.
        For further information please see USER MANUAL section 6.33 and Appendix F.
        """ # nopep8
        return self._cards[4].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        self._cards[4].set_value("g1", value)

    @property
    def g2(self) -> float:
        """Get or set the Entity coefficient g2.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        self._cards[4].set_value("g2", value)

    @property
    def g3(self) -> float:
        """Get or set the Entity coefficient g3.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g3")

    @g3.setter
    def g3(self, value: float) -> None:
        self._cards[4].set_value("g3", value)

    @property
    def g4(self) -> float:
        """Get or set the Entity coefficient g4.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g4")

    @g4.setter
    def g4(self, value: float) -> None:
        self._cards[4].set_value("g4", value)

    @property
    def g5(self) -> float:
        """Get or set the Entity coefficient g5.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g5")

    @g5.setter
    def g5(self, value: float) -> None:
        self._cards[4].set_value("g5", value)

    @property
    def g6(self) -> float:
        """Get or set the Entity coefficient g6.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g6")

    @g6.setter
    def g6(self, value: float) -> None:
        self._cards[4].set_value("g6", value)

    @property
    def g7(self) -> float:
        """Get or set the Entity coefficient g7.
        For further information please see USER MANUAL section 6.33.
        """ # nopep8
        return self._cards[4].get_value("g7")

    @g7.setter
    def g7(self, value: float) -> None:
        self._cards[4].set_value("g7", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[5].cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[5].cards[0].set_value("cid", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Interface descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[5].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        self._cards[5].cards[0].set_value("heading", value)

    @property
    def ignore(self) -> int:
        """Get or set the By setting this variable to 1, the "ignore initial penetrations" option is turned on for this contact.  Alternatively, this option may be turned on by setting IGNORE = 1 on Card 4 of *CONTROL_CONTACT or on Optional Card C of *CONTACT.  In other words, if IGNORE is set to 1 in any of three places, initial penetrations are tracked.
        """ # nopep8
        return self._cards[6].cards[0].get_value("ignore")

    @ignore.setter
    def ignore(self, value: int) -> None:
        self._cards[6].cards[0].set_value("ignore", value)

    @property
    def bckt(self) -> int:
        """Get or set the Bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts. For these two exceptions, the BSORT option on Optional Card A applies instead.
        """ # nopep8
        return self._cards[6].cards[0].get_value("bckt")

    @bckt.setter
    def bckt(self, value: int) -> None:
        self._cards[6].cards[0].set_value("bckt", value)

    @property
    def lcbckt(self) -> typing.Optional[int]:
        """Get or set the Load curve for bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts.  For the two exceptions, the negative BSORT option on Optional Card A applies instead.
        """ # nopep8
        return self._cards[6].cards[0].get_value("lcbckt")

    @lcbckt.setter
    def lcbckt(self, value: int) -> None:
        self._cards[6].cards[0].set_value("lcbckt", value)

    @property
    def ns2trk(self) -> int:
        """Get or set the Number of potential contacts to track for each tracked node.  The normal input for this (DEPTH on Optional Card A) is ignored..
        """ # nopep8
        return self._cards[6].cards[0].get_value("ns2trk")

    @ns2trk.setter
    def ns2trk(self, value: int) -> None:
        self._cards[6].cards[0].set_value("ns2trk", value)

    @property
    def inititr(self) -> int:
        """Get or set the Number of iterations to perform when trying to eliminate initial penetrations.  Note that an input of 0 means 0, not the default value (which is 2).  Leaving this field blank will set INITITR to 2.
        """ # nopep8
        return self._cards[6].cards[0].get_value("inititr")

    @inititr.setter
    def inititr(self, value: int) -> None:
        self._cards[6].cards[0].set_value("inititr", value)

    @property
    def parmax(self) -> float:
        """Get or set the The parametric extension distance for contact segments.  The MAXPAR parameter on Optional Card A is not used for MPP.  For non-tied contacts, the default is 1.0005. For tied contacts the default is 1.035 and, the actual extension used is computed as follows: see the manual
        """ # nopep8
        return self._cards[6].cards[0].get_value("parmax")

    @parmax.setter
    def parmax(self, value: float) -> None:
        self._cards[6].cards[0].set_value("parmax", value)

    @property
    def cparm8(self) -> int:
        """Get or set the Flag for behavior of AUTOMATIC_GENERAL contacts.  CPARM8's value is interpreted as two separate flags: OPT1 and OPT2 according to the rule,
        "CPARM8" = "OPT1" + "OPT2".
        When OPT1 and OPT2 are both set, both options are active.

        OPT1.Flag to exclude beam - to - beam contact from the same PID.
        EQ.0:	Flag is not set(default).
        EQ.1 : Flag is set.
        EQ.2 : Flag is set.CPARM8 = 2 additionally permits contact treatment of spot weld(type 9) beams in AUTOMATIC_GENERAL contacts; spot weld beams are otherwise disregarded entirely by AUTOMATIC_GENERAL contacts.
        OPT2.Flag to shift generated beam affecting only shell - edge - to - shell - edge treatment.See also SRNDE in Optional Card E.
        EQ.10:	Beam generated on exterior shell edge will be shifted into the shell by half the shell thickness.Therefore, the shell - edge - to - shell - edge contact starts right at the shell edge and not at an extension of the shell edge.
        """ # nopep8
        return self._cards[6].cards[0].get_value("cparm8")

    @cparm8.setter
    def cparm8(self, value: int) -> None:
        if value not in [0, 1, 2, 10, 11, 12]:
            raise Exception("""cparm8 must be one of {0,1,2,10,11,12}""")
        self._cards[6].cards[0].set_value("cparm8", value)

    @property
    def mpp2(self) -> bool:
        """Get or set the Flag whether this is the MPP card.
        """ # nopep8
        return self._cards[6].cards[1].get_value("mpp2")

    @mpp2.setter
    def mpp2(self, value: bool) -> None:
        self._cards[6].cards[1].set_value("mpp2", value)

    @property
    def chksegs(self) -> int:
        """Get or set the If this value is non-zero, then for the node-to-surface and surface-to-surface contacts LS-DYNA performs a special check at time 0 for elements that are inverted (or nearly so), These elements are removed from contact.  These poorly formed elements have been known to occur on the tooling in metalforming problems, which allows these problems to run.  It should not normally be needed for reasonable meshes.
        """ # nopep8
        return self._cards[6].cards[1].get_value("chksegs")

    @chksegs.setter
    def chksegs(self, value: int) -> None:
        self._cards[6].cards[1].set_value("chksegs", value)

    @property
    def pensf(self) -> float:
        """Get or set the This option is used together with IGNORE for 3D forging problems.  If non-zero, the IGNORE penetration distance is multiplied by this value each cycle, effectively pushing the tracked node back out to the surface.  This is useful for nodes that might get generated below the reference surface during 3D remeshing.  Care should be exercised, as energy may be generated and stability may be effected for values lower than 0.95.  A value in the range of 0.98 to 0.99 or higher (but < 1.0) is recommended
        """ # nopep8
        return self._cards[6].cards[1].get_value("pensf")

    @pensf.setter
    def pensf(self, value: float) -> None:
        self._cards[6].cards[1].set_value("pensf", value)

    @property
    def grpable(self) -> int:
        """Get or set the Set to 1 to invoke an alternate MPP communication algorithm for various SINGLE_SURFACE (including AUTOMATIC_GEN-ERAL), NODES_TO_SURFACE, SURFACE_TO_SURFACE, ERODING and SOFT = 2 contacts.  This groupable algorithm does not support all contact options, including MORTAR. It is still under development.  It can be significantly faster and scale better than the normal algorithm when there are more than two or three applicable contact types defined in the model. It is intended for speeding up the contact processing without changing the behavior of the contact.  See also *CONTROL_MPP_-CONTACT_GROUPABLE.
        """ # nopep8
        return self._cards[6].cards[1].get_value("grpable")

    @grpable.setter
    def grpable(self, value: int) -> None:
        self._cards[6].cards[1].set_value("grpable", value)

