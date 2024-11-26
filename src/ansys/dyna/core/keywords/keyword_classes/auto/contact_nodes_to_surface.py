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

class ContactNodesToSurface(KeywordBase):
    """DYNA CONTACT_NODES_TO_SURFACE keyword"""

    keyword = "CONTACT"
    subkeyword = "NODES_TO_SURFACE"
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
                        "surfa",
                        int,
                        0,
                        10,
                        kwargs.get("surfa")
                    ),
                    Field(
                        "surfb",
                        int,
                        10,
                        10,
                        kwargs.get("surfb")
                    ),
                    Field(
                        "surfatyp",
                        int,
                        20,
                        10,
                        kwargs.get("surfatyp", 0)
                    ),
                    Field(
                        "surfbtyp",
                        int,
                        30,
                        10,
                        kwargs.get("surfbtyp", 0)
                    ),
                    Field(
                        "saboxid",
                        int,
                        40,
                        10,
                        kwargs.get("saboxid")
                    ),
                    Field(
                        "sbboxid",
                        int,
                        50,
                        10,
                        kwargs.get("sbboxid")
                    ),
                    Field(
                        "sapr",
                        int,
                        60,
                        10,
                        kwargs.get("sapr", 0)
                    ),
                    Field(
                        "sbpr",
                        int,
                        70,
                        10,
                        kwargs.get("sbpr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fs",
                        float,
                        0,
                        10,
                        kwargs.get("fs", 0.0)
                    ),
                    Field(
                        "fd",
                        float,
                        10,
                        10,
                        kwargs.get("fd", 0.0)
                    ),
                    Field(
                        "dc",
                        float,
                        20,
                        10,
                        kwargs.get("dc", 0.0)
                    ),
                    Field(
                        "vc",
                        float,
                        30,
                        10,
                        kwargs.get("vc", 0.0)
                    ),
                    Field(
                        "vdc",
                        float,
                        40,
                        10,
                        kwargs.get("vdc", 0.0)
                    ),
                    Field(
                        "penchk",
                        int,
                        50,
                        10,
                        kwargs.get("penchk")
                    ),
                    Field(
                        "bt",
                        float,
                        60,
                        10,
                        kwargs.get("bt", 0.0)
                    ),
                    Field(
                        "dt",
                        float,
                        70,
                        10,
                        kwargs.get("dt", 1.0E+20)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sfsa",
                        float,
                        0,
                        10,
                        kwargs.get("sfsa", 1.0)
                    ),
                    Field(
                        "sfsb",
                        float,
                        10,
                        10,
                        kwargs.get("sfsb", 1.0)
                    ),
                    Field(
                        "sast",
                        float,
                        20,
                        10,
                        kwargs.get("sast")
                    ),
                    Field(
                        "sbst",
                        float,
                        30,
                        10,
                        kwargs.get("sbst")
                    ),
                    Field(
                        "sfsat",
                        float,
                        40,
                        10,
                        kwargs.get("sfsat", 1.0)
                    ),
                    Field(
                        "sfsbt",
                        float,
                        50,
                        10,
                        kwargs.get("sfsbt", 1.0)
                    ),
                    Field(
                        "fsf",
                        float,
                        60,
                        10,
                        kwargs.get("fsf", 1.0)
                    ),
                    Field(
                        "vsf",
                        float,
                        70,
                        10,
                        kwargs.get("vsf", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = ContactNodesToSurface.option_specs[0],
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
                option_spec = ContactNodesToSurface.option_specs[1],
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
    def surfa(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface (see Setting the Contact Interface). See *SET_SEGMENT, *SET_NODE_OPTION, *PART, *SET_PART or *SET_SHELL_OPTION. For ERODING_SINGLE_SURFACE and ERODING_SURFACE_TO_SURFACE contact types, use either a part ID or a part set ID. For ERODING_NODES_TO_SURFACE contact, use a node set which includes all nodes that may be exposed to contact as element erosion occurs.
        EQ.0:	Includes all parts in the case of single surface contact types
        """ # nopep8
        return self._cards[0].get_value("surfa")

    @surfa.setter
    def surfa(self, value: int) -> None:
        self._cards[0].set_value("surfa", value)

    @property
    def surfb(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact (see Setting the Contact Interface).
        EQ.0:	SURFB side is not applicable for single surface contact types.
        """ # nopep8
        return self._cards[0].get_value("surfb")

    @surfb.setter
    def surfb(self, value: int) -> None:
        self._cards[0].set_value("surfb", value)

    @property
    def surfatyp(self) -> int:
        """Get or set the The ID type of SURFA:
        EQ.0: segment set ID for surface to surface contact,
        EQ.1: shell element set ID for surface to surface contact,
        EQ.2: part set ID,
        EQ.3: part ID,
        EQ.4: node set ID for node to surface contact,
        EQ.5: include all (SURFA field) is ignored,
        EQ.6: part set ID for exempted parts. All non-exempted parts are included in the contact.
        EQ.7:	Branch ID; see *SET_PART_TREE
        """ # nopep8
        return self._cards[0].get_value("surfatyp")

    @surfatyp.setter
    def surfatyp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""surfatyp must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("surfatyp", value)

    @property
    def surfbtyp(self) -> int:
        """Get or set the ID type of SURFB:
        EQ.0: segment set ID,
        EQ.1: shell element set ID,
        EQ.2: part set ID,
        EQ.3: part ID,
        EQ.5:Include all ( SURFB Field is ignored).
        EQ.6:	Part set ID for exempted parts.  All non-exempted parts are included in the contact.
        EQ.7:	Branch ID; see *SET_PART_TREE
        """ # nopep8
        return self._cards[0].get_value("surfbtyp")

    @surfbtyp.setter
    def surfbtyp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 5, 6, 7]:
            raise Exception("""surfbtyp must be one of {0,1,2,3,5,6,7}""")
        self._cards[0].set_value("surfbtyp", value)

    @property
    def saboxid(self) -> typing.Optional[int]:
        """Get or set the Include in contact definition only those SURFA nodes/segments within box SABOXID (corresponding to BOXID in *DEFINE_BOX), or if SABOXID is negative, only those SURFA nodes/segments within contact volume |SABOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SABOXID can be used only if SURFATYP is set to 2, 3, or 6, that is, SURFA is a part ID or part set ID. SABOXID is not available for ERODING contact types
        """ # nopep8
        return self._cards[0].get_value("saboxid")

    @saboxid.setter
    def saboxid(self, value: int) -> None:
        self._cards[0].set_value("saboxid", value)

    @property
    def sbboxid(self) -> typing.Optional[int]:
        """Get or set the Include in contact definition only those SURFB segments within box SBBOXID (corresponding to BOXID in *DEFINE_BOX), or if SBBOXID is negative, only those SURFB segments within contact volume |SBBOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SBBOXID can be used only if SURFBTYP is set to 2, 3, or 6, that is, SURFB is a part ID or part set ID.  SBBOXID is not available for ERODING contact types.
        """ # nopep8
        return self._cards[0].get_value("sbboxid")

    @sbboxid.setter
    def sbboxid(self, value: int) -> None:
        self._cards[0].set_value("sbboxid", value)

    @property
    def sapr(self) -> int:
        """Get or set the Include the SURFA side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
        EQ.0:	Do not include.
        EQ.1 : SURFA side forces included.
        EQ.2 : Same as 1 but also allows for SURFA nodes to be written as* INITIAL_CONTACT_WEAR to dynain; see NCYC on* INTERFACE_SPRINGBACK_LSDYNA.
        """ # nopep8
        return self._cards[0].get_value("sapr")

    @sapr.setter
    def sapr(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sapr must be one of {0,1,2}""")
        self._cards[0].set_value("sapr", value)

    @property
    def sbpr(self) -> int:
        """Get or set the Include the SURFB side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
        EQ.0:	Do not include.
        EQ.1 : SURFB side forces included.
        EQ.2 : Same as 1, but also allows for SURFB nodes to be written as* INITIAL_CONTACT_WEAR to dynain; see NCYC on* INTERFACE_SPRINGBACK_LSDYNA.
        """ # nopep8
        return self._cards[0].get_value("sbpr")

    @sbpr.setter
    def sbpr(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""sbpr must be one of {0,1,2}""")
        self._cards[0].set_value("sbpr", value)

    @property
    def fs(self) -> float:
        """Get or set the Static coefficient of friction if FS > 0 and not equal to 2.
        EQ.-1.0: If the frictional coefficients defined in the *PART section are to be used, set FS to a negative number.
        EQ. 2: For contact types SURFACE_TO_SURFACE and ONE_WAY_ SURFACE_TO_SURFACE, the dynamic coefficient of friction points to the table, see DEFINE_TABLE (The table ID is give by FD below.), giving the coefficient of friction as a function of the relative velocity and pressure. This option must be used in combination with the thickness offset option. See Figure 6.1.
        Note: For the special contact option TIED_SURFACE_TO_SURFACE_FAILURE only, the variables FS is the Normal tensile stress at failure.,
        """ # nopep8
        return self._cards[1].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[1].set_value("fs", value)

    @property
    def fd(self) -> float:
        """Get or set the Dynamic coefficient of friction. The frictional coefficient is assumed to be dependent on the relative velocity v-rel of the surfaces in contact. Give table ID if FS=2 (default=0.0).
        Note: For the special contact option TIED_SURFACE_TO_SURFACE_ FAILURE only, the variables FD is Shear stress at failure
        """ # nopep8
        return self._cards[1].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[1].set_value("fd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity v-rel of the surfaces in contact. (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[1].set_value("dc", value)

    @property
    def vc(self) -> float:
        """Get or set the Coefficient for viscous friction. This is necessary to limit the friction force to a maximum.
        """ # nopep8
        return self._cards[1].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[1].set_value("vc", value)

    @property
    def vdc(self) -> float:
        """Get or set the Viscous damping coefficient in percent of critical. In order to avoid undesirable oscillation in contact, e.g., for sheet forming simulation, a contact damping perpendicular to the contacting surfaces is applied.
        """ # nopep8
        return self._cards[1].get_value("vdc")

    @vdc.setter
    def vdc(self, value: float) -> None:
        self._cards[1].set_value("vdc", value)

    @property
    def penchk(self) -> typing.Optional[int]:
        """Get or set the Small penetration in contact search option.  If the tracked node penetrates more than the segment thickness times the factor XPENE (see *CONTROL_CONTACT), the penetration is ignored, and the tracked node is set free.  The thickness is taken as the shell thickness if the segment belongs to a shell element or it is taken as 1/20 of its shortest diagonal if the segment belongs to a solid element.  This option applies to the surface-to-surface contact algorithms.  See Table 0-17 for contact types and more details.
        """ # nopep8
        return self._cards[1].get_value("penchk")

    @penchk.setter
    def penchk(self, value: int) -> None:
        self._cards[1].set_value("penchk", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time (contact surface becomes active at this time):LT.0:	Birth time is set to | "BT" | .When negative, birth time is followed during the dynamic relaxation phase of the calculation.After dynamic relaxation has completed, contact is activated regardless of the value of BT.EQ.0 : Birth time is inactive, meaning contact is always activeGT.0 : If DT = -9999, BT is interpreted as the curve or table ID defining multiple pairs of birth - time / death - time; see Remark 2 below.Otherwise, if "DT" > 0, birth time applies both duringand after dynamic relaxation.
        """ # nopep8
        return self._cards[1].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        self._cards[1].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time (contact surface is deactivated at this time):LT.0:	If DT = -9999, BT is interpreted as the curve or table ID defining multiple pairs of birth - time / death - time.Otherwise, negative DT indicates that contact is inactive during dynamic relaxation.After dynamic relaxation the birth and death times are followed and set to | "BT" | and | "DT" | , respectively.EQ.0 : DT defaults to 10e20.GT.0 : DT sets the time at which the contact is deactivated.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def sfsa(self) -> float:
        """Get or set the Scale factor on default SURFA penalty stiffness when SOFT = 0 or SOFT = 2; see also *CONTROL_CONTACT.For MORTAR frictional contact this is the stiffness scale factor for the entire contact, and SFSB does not apply.
        """ # nopep8
        return self._cards[2].get_value("sfsa")

    @sfsa.setter
    def sfsa(self, value: float) -> None:
        self._cards[2].set_value("sfsa", value)

    @property
    def sfsb(self) -> float:
        """Get or set the Scale factor on default SURFA penalty stiffness when SOFT = 0 or SOFT = 2; see also *CONTROL_CONTACT.For MORTAR tied contact, this is an additional stiffness scale factor, resulting in a total stiffness scale of SFSA*SFSB.
        """ # nopep8
        return self._cards[2].get_value("sfsb")

    @sfsb.setter
    def sfsb(self, value: float) -> None:
        self._cards[2].set_value("sfsb", value)

    @property
    def sast(self) -> typing.Optional[float]:
        """Get or set the Optional thickness for SURFA surface (overrides true thickness). This option applies only to contact with shell elements. SAST has no bearing on the actual thickness of the elements; it only affects the location of the contact surface. For the *CONTACT_TIED_.. options, SAST and SBST below can be defined as negative values, which will cause the determination of whether or not a node is tied to depend only on the separation distance relative to the absolute value of these thicknesses. More information is given under General Remarks on *CONTACT following Optional Card C.
        """ # nopep8
        return self._cards[2].get_value("sast")

    @sast.setter
    def sast(self, value: float) -> None:
        self._cards[2].set_value("sast", value)

    @property
    def sbst(self) -> typing.Optional[float]:
        """Get or set the Optional thickness for SURFA surface (overrides true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements. For the TIED options see SAST above.
        """ # nopep8
        return self._cards[2].get_value("sbst")

    @sbst.setter
    def sbst(self, value: float) -> None:
        self._cards[2].set_value("sbst", value)

    @property
    def sfsat(self) -> float:
        """Get or set the Scale factor applied to contact thickness of SURFA surface.  This option applies to contact with shell and beam elements.
        SFSAT has no bearing on the actual thickness of the elements; it only affects the location of the contact surface.
        SFSAT is ignored if SAST is nonzero except in the case of MORTAR contact (see Remark 9 in the General Remarks: *Contact section).
        """ # nopep8
        return self._cards[2].get_value("sfsat")

    @sfsat.setter
    def sfsat(self, value: float) -> None:
        self._cards[2].set_value("sfsat", value)

    @property
    def sfsbt(self) -> float:
        """Get or set the Scale factor applied to contact thickness of SURFA surface.  This option applies only to contact with shell elements.
        SFSAT has no bearing on the actual thickness of the elements; it only affects the location of the contact surface.
        SFSAT is ignored if SBST is nonzero except in the case of MORTAR contact (see Remark 9 in the General Remarks: *Contact section).
        """ # nopep8
        return self._cards[2].get_value("sfsbt")

    @sfsbt.setter
    def sfsbt(self, value: float) -> None:
        self._cards[2].set_value("sfsbt", value)

    @property
    def fsf(self) -> float:
        """Get or set the Coulomb friction scale factor (default=1.0).The Coulomb friction value is scaled as μ_sc=FSF×μ_c; see Mandatory Card 2.
        """ # nopep8
        return self._cards[2].get_value("fsf")

    @fsf.setter
    def fsf(self, value: float) -> None:
        self._cards[2].set_value("fsf", value)

    @property
    def vsf(self) -> float:
        """Get or set the Viscous friction scale factor (default=1.0).If this factor is defined, then the limiting force becomes: F_lim =VSF×VC×A_cont ; see Mandatory Card 2.
        """ # nopep8
        return self._cards[2].get_value("vsf")

    @vsf.setter
    def vsf(self, value: float) -> None:
        self._cards[2].set_value("vsf", value)

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

