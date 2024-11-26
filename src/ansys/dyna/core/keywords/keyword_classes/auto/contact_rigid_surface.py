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

class ContactRigidSurface(KeywordBase):
    """DYNA CONTACT_RIGID_SURFACE keyword"""

    keyword = "CONTACT"
    subkeyword = "RIGID_SURFACE"
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
                        "cid",
                        int,
                        0,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "psid",
                        int,
                        10,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                    Field(
                        "segid",
                        int,
                        30,
                        10,
                        kwargs.get("segid")
                    ),
                    Field(
                        "fs",
                        float,
                        40,
                        10,
                        kwargs.get("fs", 0.0)
                    ),
                    Field(
                        "fd",
                        float,
                        50,
                        10,
                        kwargs.get("fd", 0.0)
                    ),
                    Field(
                        "dc",
                        float,
                        60,
                        10,
                        kwargs.get("dc", 0.0)
                    ),
                    Field(
                        "vc",
                        float,
                        70,
                        10,
                        kwargs.get("vc", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidx",
                        int,
                        0,
                        10,
                        kwargs.get("lcidx", 0)
                    ),
                    Field(
                        "lcidy",
                        int,
                        10,
                        10,
                        kwargs.get("lcidy", 0)
                    ),
                    Field(
                        "lcidz",
                        int,
                        20,
                        10,
                        kwargs.get("lcidz", 0)
                    ),
                    Field(
                        "fslcid",
                        int,
                        30,
                        10,
                        kwargs.get("fslcid", 0)
                    ),
                    Field(
                        "fdlcid",
                        int,
                        40,
                        10,
                        kwargs.get("fdlcid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sfs",
                        float,
                        0,
                        10,
                        kwargs.get("sfs", 1.0)
                    ),
                    Field(
                        "stthk",
                        float,
                        10,
                        10,
                        kwargs.get("stthk", 0.0)
                    ),
                    Field(
                        "sfthk",
                        float,
                        20,
                        10,
                        kwargs.get("sfthk", 1.0)
                    ),
                    Field(
                        "xpene",
                        float,
                        30,
                        10,
                        kwargs.get("xpene", 4.0)
                    ),
                    Field(
                        "bsort",
                        float,
                        40,
                        10,
                        kwargs.get("bsort", 10.0)
                    ),
                    Field(
                        "ctype",
                        int,
                        50,
                        10,
                        kwargs.get("ctype", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = ContactRigidSurface.option_specs[0],
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
                option_spec = ContactRigidSurface.option_specs[1],
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
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact interface ID. This must be a unique number.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID of all parts that may contact the rigid surface. See *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def boxid(self) -> int:
        """Get or set the Include only nodes of the part set that are within the specified box, see *DEFINE_BOX, in contact definition.
        EQ.0: all nodes from the part set, PSID, will be included in the contact (default).
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def segid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID defining the rigid surface. See *SET_SEGMENT.
        """ # nopep8
        return self._cards[0].get_value("segid")

    @segid.setter
    def segid(self, value: int) -> None:
        self._cards[0].set_value("segid", value)

    @property
    def fs(self) -> float:
        """Get or set the Static coefficient of friction.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[0].set_value("fs", value)

    @property
    def fd(self) -> float:
        """Get or set the Dynamic coefficient of friction.
        """ # nopep8
        return self._cards[0].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[0].set_value("fd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient.
        """ # nopep8
        return self._cards[0].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[0].set_value("dc", value)

    @property
    def vc(self) -> float:
        """Get or set the Coefficient for viscous friction.
        """ # nopep8
        return self._cards[0].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[0].set_value("vc", value)

    @property
    def lcidx(self) -> int:
        """Get or set the Load curve ID defining x-direction motion.
        EQ.0: There is no motion in the x-coordinate system.
        """ # nopep8
        return self._cards[1].get_value("lcidx")

    @lcidx.setter
    def lcidx(self, value: int) -> None:
        self._cards[1].set_value("lcidx", value)

    @property
    def lcidy(self) -> int:
        """Get or set the Load curve ID defining y-direction motion.
        EQ.0: There is no motion in the y-coordinate system.
        """ # nopep8
        return self._cards[1].get_value("lcidy")

    @lcidy.setter
    def lcidy(self, value: int) -> None:
        self._cards[1].set_value("lcidy", value)

    @property
    def lcidz(self) -> int:
        """Get or set the Load curve ID defining z-direction motion.
        EQ.0: There is no motion in the z-coordinate system.
        """ # nopep8
        return self._cards[1].get_value("lcidz")

    @lcidz.setter
    def lcidz(self, value: int) -> None:
        self._cards[1].set_value("lcidz", value)

    @property
    def fslcid(self) -> int:
        """Get or set the Load curve ID defining the static coefficient of friction as a function of interface pressure. This option applies to shell segments only.
        """ # nopep8
        return self._cards[1].get_value("fslcid")

    @fslcid.setter
    def fslcid(self, value: int) -> None:
        self._cards[1].set_value("fslcid", value)

    @property
    def fdlcid(self) -> int:
        """Get or set the Load curve ID defining the dynamic coefficient of friction as a function of interface pressure. This option applies to shell segments only.
        """ # nopep8
        return self._cards[1].get_value("fdlcid")

    @fdlcid.setter
    def fdlcid(self, value: int) -> None:
        self._cards[1].set_value("fdlcid", value)

    @property
    def sfs(self) -> float:
        """Get or set the Scale factor on default slave penalty stiffness, see also *CONTROL_ CONTACT.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[2].get_value("sfs")

    @sfs.setter
    def sfs(self, value: float) -> None:
        self._cards[2].set_value("sfs", value)

    @property
    def stthk(self) -> float:
        """Get or set the Optional thickness for slave surface (overrides true thickness). This option applies to contact with shell, solid, and beam elements. True thickness is the element thickness of the shell elements. Thickness offsets are not used for solid element unless this option is specified.
        Default is set to 0.0.
        """ # nopep8
        return self._cards[2].get_value("stthk")

    @stthk.setter
    def stthk(self, value: float) -> None:
        self._cards[2].set_value("stthk", value)

    @property
    def sfthk(self) -> float:
        """Get or set the Scale factor for slave surface thickness (scales true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements.
        Default is set to 1.0
        """ # nopep8
        return self._cards[2].get_value("sfthk")

    @sfthk.setter
    def sfthk(self, value: float) -> None:
        self._cards[2].set_value("sfthk", value)

    @property
    def xpene(self) -> float:
        """Get or set the Contact surface maximum penetration check multiplier. If the penetration of a node through the rigid surface exceeds the product of XPENE and the slave node thickness, the node is set free.
        Default is set to 4.0.
        """ # nopep8
        return self._cards[2].get_value("xpene")

    @xpene.setter
    def xpene(self, value: float) -> None:
        self._cards[2].set_value("xpene", value)

    @property
    def bsort(self) -> float:
        """Get or set the Number of cycles between bucket sorts. The default value is set to 10.0 but can be much larger, e.g., 50-100, for fully connected surfaces.
        """ # nopep8
        return self._cards[2].get_value("bsort")

    @bsort.setter
    def bsort(self, value: float) -> None:
        self._cards[2].set_value("bsort", value)

    @property
    def ctype(self) -> int:
        """Get or set the The contact formulation. The default, CTYPE=0, is equivalent to the ONE_WAY_SURFACE_TO_SURFACE formulation, and CTYPE=1 is a penalty formulation. If the slave surface belongs to a rigid body, CTYPE=1 must be used.
        """ # nopep8
        return self._cards[2].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ctype must be one of {0,1}""")
        self._cards[2].set_value("ctype", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[3].cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[3].cards[0].set_value("cid", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Interface descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[3].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        self._cards[3].cards[0].set_value("heading", value)

    @property
    def ignore(self) -> int:
        """Get or set the By setting this variable to 1, the "ignore initial penetrations" option is turned on for this contact.  Alternatively, this option may be turned on by setting IGNORE = 1 on Card 4 of *CONTROL_CONTACT or on Optional Card C of *CONTACT.  In other words, if IGNORE is set to 1 in any of three places, initial penetrations are tracked.
        """ # nopep8
        return self._cards[4].cards[0].get_value("ignore")

    @ignore.setter
    def ignore(self, value: int) -> None:
        self._cards[4].cards[0].set_value("ignore", value)

    @property
    def bckt(self) -> int:
        """Get or set the Bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts. For these two exceptions, the BSORT option on Optional Card A applies instead.
        """ # nopep8
        return self._cards[4].cards[0].get_value("bckt")

    @bckt.setter
    def bckt(self, value: int) -> None:
        self._cards[4].cards[0].set_value("bckt", value)

    @property
    def lcbckt(self) -> typing.Optional[int]:
        """Get or set the Load curve for bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts.  For the two exceptions, the negative BSORT option on Optional Card A applies instead.
        """ # nopep8
        return self._cards[4].cards[0].get_value("lcbckt")

    @lcbckt.setter
    def lcbckt(self, value: int) -> None:
        self._cards[4].cards[0].set_value("lcbckt", value)

    @property
    def ns2trk(self) -> int:
        """Get or set the Number of potential contacts to track for each tracked node.  The normal input for this (DEPTH on Optional Card A) is ignored..
        """ # nopep8
        return self._cards[4].cards[0].get_value("ns2trk")

    @ns2trk.setter
    def ns2trk(self, value: int) -> None:
        self._cards[4].cards[0].set_value("ns2trk", value)

    @property
    def inititr(self) -> int:
        """Get or set the Number of iterations to perform when trying to eliminate initial penetrations.  Note that an input of 0 means 0, not the default value (which is 2).  Leaving this field blank will set INITITR to 2.
        """ # nopep8
        return self._cards[4].cards[0].get_value("inititr")

    @inititr.setter
    def inititr(self, value: int) -> None:
        self._cards[4].cards[0].set_value("inititr", value)

    @property
    def parmax(self) -> float:
        """Get or set the The parametric extension distance for contact segments.  The MAXPAR parameter on Optional Card A is not used for MPP.  For non-tied contacts, the default is 1.0005. For tied contacts the default is 1.035 and, the actual extension used is computed as follows: see the manual
        """ # nopep8
        return self._cards[4].cards[0].get_value("parmax")

    @parmax.setter
    def parmax(self, value: float) -> None:
        self._cards[4].cards[0].set_value("parmax", value)

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
        return self._cards[4].cards[0].get_value("cparm8")

    @cparm8.setter
    def cparm8(self, value: int) -> None:
        if value not in [0, 1, 2, 10, 11, 12]:
            raise Exception("""cparm8 must be one of {0,1,2,10,11,12}""")
        self._cards[4].cards[0].set_value("cparm8", value)

    @property
    def mpp2(self) -> bool:
        """Get or set the Flag whether this is the MPP card.
        """ # nopep8
        return self._cards[4].cards[1].get_value("mpp2")

    @mpp2.setter
    def mpp2(self, value: bool) -> None:
        self._cards[4].cards[1].set_value("mpp2", value)

    @property
    def chksegs(self) -> int:
        """Get or set the If this value is non-zero, then for the node-to-surface and surface-to-surface contacts LS-DYNA performs a special check at time 0 for elements that are inverted (or nearly so), These elements are removed from contact.  These poorly formed elements have been known to occur on the tooling in metalforming problems, which allows these problems to run.  It should not normally be needed for reasonable meshes.
        """ # nopep8
        return self._cards[4].cards[1].get_value("chksegs")

    @chksegs.setter
    def chksegs(self, value: int) -> None:
        self._cards[4].cards[1].set_value("chksegs", value)

    @property
    def pensf(self) -> float:
        """Get or set the This option is used together with IGNORE for 3D forging problems.  If non-zero, the IGNORE penetration distance is multiplied by this value each cycle, effectively pushing the tracked node back out to the surface.  This is useful for nodes that might get generated below the reference surface during 3D remeshing.  Care should be exercised, as energy may be generated and stability may be effected for values lower than 0.95.  A value in the range of 0.98 to 0.99 or higher (but < 1.0) is recommended
        """ # nopep8
        return self._cards[4].cards[1].get_value("pensf")

    @pensf.setter
    def pensf(self, value: float) -> None:
        self._cards[4].cards[1].set_value("pensf", value)

    @property
    def grpable(self) -> int:
        """Get or set the Set to 1 to invoke an alternate MPP communication algorithm for various SINGLE_SURFACE (including AUTOMATIC_GEN-ERAL), NODES_TO_SURFACE, SURFACE_TO_SURFACE, ERODING and SOFT = 2 contacts.  This groupable algorithm does not support all contact options, including MORTAR. It is still under development.  It can be significantly faster and scale better than the normal algorithm when there are more than two or three applicable contact types defined in the model. It is intended for speeding up the contact processing without changing the behavior of the contact.  See also *CONTROL_MPP_-CONTACT_GROUPABLE.
        """ # nopep8
        return self._cards[4].cards[1].get_value("grpable")

    @grpable.setter
    def grpable(self, value: int) -> None:
        self._cards[4].cards[1].set_value("grpable", value)

