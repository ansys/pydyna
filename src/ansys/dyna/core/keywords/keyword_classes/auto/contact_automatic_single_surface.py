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

class ContactAutomaticSingleSurface(KeywordBase):
    """DYNA CONTACT_AUTOMATIC_SINGLE_SURFACE keyword"""

    keyword = "CONTACT"
    subkeyword = "AUTOMATIC_SINGLE_SURFACE"
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
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "msid",
                        int,
                        10,
                        10,
                        kwargs.get("msid")
                    ),
                    Field(
                        "sstyp",
                        int,
                        20,
                        10,
                        kwargs.get("sstyp")
                    ),
                    Field(
                        "mstyp",
                        int,
                        30,
                        10,
                        kwargs.get("mstyp")
                    ),
                    Field(
                        "sboxid",
                        int,
                        40,
                        10,
                        kwargs.get("sboxid")
                    ),
                    Field(
                        "mboxid",
                        int,
                        50,
                        10,
                        kwargs.get("mboxid")
                    ),
                    Field(
                        "spr",
                        int,
                        60,
                        10,
                        kwargs.get("spr")
                    ),
                    Field(
                        "mpr",
                        int,
                        70,
                        10,
                        kwargs.get("mpr")
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
                        kwargs.get("fs", 0)
                    ),
                    Field(
                        "fd",
                        float,
                        10,
                        10,
                        kwargs.get("fd", 0)
                    ),
                    Field(
                        "dc",
                        float,
                        20,
                        10,
                        kwargs.get("dc", 0)
                    ),
                    Field(
                        "vc",
                        float,
                        30,
                        10,
                        kwargs.get("vc", 0)
                    ),
                    Field(
                        "vdc",
                        float,
                        40,
                        10,
                        kwargs.get("vdc", 0)
                    ),
                    Field(
                        "penchk",
                        int,
                        50,
                        10,
                        kwargs.get("penchk", 0)
                    ),
                    Field(
                        "bt",
                        float,
                        60,
                        10,
                        kwargs.get("bt", 0)
                    ),
                    Field(
                        "dt",
                        float,
                        70,
                        10,
                        kwargs.get("dt", 0)
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
                        kwargs.get("sfs", 1)
                    ),
                    Field(
                        "sfm",
                        float,
                        10,
                        10,
                        kwargs.get("sfm", 1)
                    ),
                    Field(
                        "sst",
                        float,
                        20,
                        10,
                        kwargs.get("sst")
                    ),
                    Field(
                        "mst",
                        float,
                        30,
                        10,
                        kwargs.get("mst")
                    ),
                    Field(
                        "sfst",
                        float,
                        40,
                        10,
                        kwargs.get("sfst", 1)
                    ),
                    Field(
                        "sfmt",
                        float,
                        50,
                        10,
                        kwargs.get("sfmt", 1)
                    ),
                    Field(
                        "fsf",
                        float,
                        60,
                        10,
                        kwargs.get("fsf", 1)
                    ),
                    Field(
                        "vsf",
                        float,
                        70,
                        10,
                        kwargs.get("vsf", 1)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = ContactAutomaticSingleSurface.option_specs[0],
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
                option_spec = ContactAutomaticSingleSurface.option_specs[1],
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
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Slave segment set ID, node set ID, part set ID, part ID, or shell element set ID; see *SET_SEGMENT, *SET_NODE_OPTION, *PART, *SET_PART or *SET_SHELL_OPTION. For ERODING_SINGLE_SURFACE and ERODING_SURFACE_TO_SURFACE contact types, use either a part ID or a part set ID. For ERODING_NODES_TO_SURFACE contact, use a node set which includes all nodes that may be exposed to contact as element erosion occurs.
        EQ.0: Includes all parts in the case of single surface contact types.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def msid(self) -> typing.Optional[int]:
        """Get or set the Master segment set ID, node set ID, part set ID, part ID, or shell element set ID.
        EQ.0: Master side is not applicable for single surface contact types.
        """ # nopep8
        return self._cards[0].get_value("msid")

    @msid.setter
    def msid(self, value: int) -> None:
        self._cards[0].set_value("msid", value)

    @property
    def sstyp(self) -> typing.Optional[int]:
        """Get or set the ID type of SSID.
        EQ.0: Segment set ID for surface-to-surface contact.
        EQ.1: Shell element set ID for surface-to-surface contact
        EQ.2: Part set ID
        EQ.3: Part ID
        EQ.4: Node set ID for nodes-to-surface contact
        EQ.5: Include all (SSID is ignored)
        EQ.6: Part set ID for exempted parts. All non-exempted parts are included in the contact.
        EQ.7: Branch ID, see *SET_PART_TREE.
        For AUTOMATIC_BEAMS_TO_SURFACE contact, either a part set ID or a part ID can be specified.
        """ # nopep8
        return self._cards[0].get_value("sstyp")

    @sstyp.setter
    def sstyp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""sstyp must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[0].set_value("sstyp", value)

    @property
    def mstyp(self) -> typing.Optional[int]:
        """Get or set the ID type of MSID.
        EQ.0: Segment set ID
        EQ.1: Shell element set ID
        EQ.2: Part set ID
        EQ.3: Part ID
        EQ.5: Include all (MSID is ignored).
        EQ.6: Part set ID for exempted parts. All non-exempted parts are included in the contact.
        EQ.7: Branch ID; see *SET_PART_TREE.
        """ # nopep8
        return self._cards[0].get_value("mstyp")

    @mstyp.setter
    def mstyp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 5, 6, 7]:
            raise Exception("""mstyp must be one of {0,1,2,3,5,6,7}""")
        self._cards[0].set_value("mstyp", value)

    @property
    def sboxid(self) -> typing.Optional[int]:
        """Get or set the Include in contact definition only those slave nodes/segments within box SBOXID (corresponding to BOXID in *DEFINE_BOX), or if SBOXID is negative, only those slave nodes/segments within contact volume |SBOXID| (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SBOXID can be used only if SSTYP is set to 2, 3, or 6, that is, SSID is a part ID or part set ID. SBOXID is not available for ERODING contact options.
        """ # nopep8
        return self._cards[0].get_value("sboxid")

    @sboxid.setter
    def sboxid(self, value: int) -> None:
        self._cards[0].set_value("sboxid", value)

    @property
    def mboxid(self) -> typing.Optional[int]:
        """Get or set the Include in contact definition only those master segments within box MBOXID (corresponding to BOXID in *DEFINE_BOX), or if MBOXID is negative, only those master segments within contact volume |MBOXID| (corresponding to CVID in *DEFINE_CONTACT_VOLUME). MBOXID can be used only if MSTYP is set to 2, 3, or 6, that is, MSID is a part ID or part set ID. MBOXID is not available for ERODING contact options.
        """ # nopep8
        return self._cards[0].get_value("mboxid")

    @mboxid.setter
    def mboxid(self, value: int) -> None:
        self._cards[0].set_value("mboxid", value)

    @property
    def spr(self) -> typing.Optional[int]:
        """Get or set the Include the slave side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
        EQ.0: Do not include.
        EQ.1: Slave side forces included.
        EQ.2: Same as 1 but also allows for slave nodes to be written as *INITIAL_CONTACT_WEAR to dynain; see NCYC on *INTERFACE_SPRINGBACK_LSDYNA.
        """ # nopep8
        return self._cards[0].get_value("spr")

    @spr.setter
    def spr(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""spr must be one of {0,1,2}""")
        self._cards[0].set_value("spr", value)

    @property
    def mpr(self) -> typing.Optional[int]:
        """Get or set the Include the master side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
        EQ.0: Do not include.
        EQ.1: Master side forces included.
        EQ.2: Same as 1, but also allows for master nodes to be written as *INITIAL_CONTACT_WEAR to dynain; see NCYC on *INTERFACE_SPRINGBACK_LSDYNA.
        """ # nopep8
        return self._cards[0].get_value("mpr")

    @mpr.setter
    def mpr(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""mpr must be one of {0,1,2}""")
        self._cards[0].set_value("mpr", value)

    @property
    def fs(self) -> float:
        """Get or set the Static coefficient of friction. If FS is > 0 and not equal to 2. The frictional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact according to, `mu = FD + (FS - FD)e^(-DC|vrel|)`. The three other possibilities are:
        EQ.2: For a subset of SURFACE_TO_SURFACE type contacts, FD is a table ID (see *DEFINE_TABLE). That table specifies two or more values of contact pressure, with each pressure value in the table corresponding to a curve of friction coefficient as a function of relative velocity. Thus, the friction coefficient becomes a function of pressure and relative velocity.
        EQ.-2: If only one friction table is defined using *DEFINE_FRICTION, it will be used and there is no need to define parameter FD. If more than one friction table is defined, then the friction table ID is defined by FD below.
        EQ.-1: If the frictional coefficients defined in the *PART section are to be used, set FS to -1.0.
        """ # nopep8
        return self._cards[1].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[1].set_value("fs", value)

    @property
    def fd(self) -> float:
        """Get or set the Dynamic coefficient of friction. If FS > 0 and not equal to 2, the frictional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact according to, `mu = FD + (FS - FD)e^(-DC|vrel|)`. Otherwise: FS.EQ.-2: Friction table ID if more than one friction table is defined
        FS.EQ.2: Table ID for table that specifies two or more values of contact pressure, with each pressure value in the table corresponding to a curve of friction coefficient as a function of relative velocity.
        """ # nopep8
        return self._cards[1].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[1].set_value("fd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact `mu = FD + (FS - FD)e^(-DC|vrel|)`.
        """ # nopep8
        return self._cards[1].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[1].set_value("dc", value)

    @property
    def vc(self) -> float:
        """Get or set the Coefficient for viscous friction. This is necessary to limit the friction force to a maximum. A limiting force is computed as Flim = VC x Acont with Acont being the area of the segment contacted by the node in contact. The suggested value for VC is the yield stress in shear VC = sigma0/sqrt(3) where sigma0 is the yield stress of the contacted material.
        """ # nopep8
        return self._cards[1].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[1].set_value("vc", value)

    @property
    def vdc(self) -> float:
        """Get or set the Viscous damping coefficient in percent of critical or the coefficient of restitution expressed as percentage (see ICOR on Optional Card E). In order to avoid undesirable oscillation in contact, such as for sheet forming simulation, a contact damping perpendicular to the contacting surfaces is applied. When ICOR, the 6th column of Optional Card E, is not defined or 0, the applied damping coefficient is given by zeta=(VDC/100)*zedacrit, where VDC is an integer (in units of percent) between 0 and 100. The formula for critical damping is zetacrit = 2*m*omega, where m is determined by nodal masses as `m = min(mslave, mmaster)`, and omega is determined from k, the interface stiffness, according to `omega=sqrt(k*(mslave+mmaster)/(mmaster*mslave))`.
        """ # nopep8
        return self._cards[1].get_value("vdc")

    @vdc.setter
    def vdc(self, value: float) -> None:
        self._cards[1].set_value("vdc", value)

    @property
    def penchk(self) -> int:
        """Get or set the Small penetration in contact search option. If the slave node penetrates more than the segment thickness times the factor XPENE (see *CONTROL_CONTACT), the penetration is ignored, and the slave node is set free. The thickness is taken as the shell thickness if the segment belongs to a shell element or it is taken as 1/20 of its shortest diagonal if the segment belongs to a solid element. This option applies to the surface-to-surface contact algorithms.
        """ # nopep8
        return self._cards[1].get_value("penchk")

    @penchk.setter
    def penchk(self, value: int) -> None:
        self._cards[1].set_value("penchk", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time (contact surface becomes active at this time):
        LT.0: Birth time is set to |BT|. When negative, birth time is followed during the dynamic relaxation phase of the calculation. After dynamic relaxation has completed, contact is activated regardless of the value of BT.
        EQ.0: Birth time is inactive, meaning contact is always active
        GT.0: If DT = -9999, BT is interpreted as the curve or table ID defining multiple pairs of birth-time/death-time; see Remark 2 below. Otherwise, if DT > 0, birth time applies both during and after dynamic relaxation
        """ # nopep8
        return self._cards[1].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        self._cards[1].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time (contact surface is deactivated at this time):
        LT.0: If DT = -9999, BT is interpreted as the curve or table ID defining multiple pairs of birth-time/death-time. Otherwise, negative DT indicates that contact is inactive during dynamic relaxation. After dynamic relaxation the birth and death times are followed and set to |BT| and |DT|, respectively
        EQ.0: DT defaults to 1020
        GT.0: DT sets the time at which the contact is deactivated.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def sfs(self) -> float:
        """Get or set the Scale factor on default slave penalty stiffness when SOFT = 0 or SOFT = 2; see also *CONTROL_CONTACT.
        """ # nopep8
        return self._cards[2].get_value("sfs")

    @sfs.setter
    def sfs(self, value: float) -> None:
        self._cards[2].set_value("sfs", value)

    @property
    def sfm(self) -> float:
        """Get or set the Scale factor on default master penalty stiffness when SOFT = 0 or SOFT = 2; see also *CONTROL_CONTACT.
        """ # nopep8
        return self._cards[2].get_value("sfm")

    @sfm.setter
    def sfm(self, value: float) -> None:
        self._cards[2].set_value("sfm", value)

    @property
    def sst(self) -> typing.Optional[float]:
        """Get or set the Optional contact thickness for slave surface (overrides default contact thickness). This option applies to contact with shell and beam elements. SST has no bearing on the actual thickness of the elements; it only affects the location of the contact surface. For the *CONTACT_TIED_ options, SST and MST (below) can be defined as negative values, which will cause the determination of whether or not a node is tied to depend only on the separation distance relative to the absolute value of these thicknesses. More information is given under General Remarks: *CONTACT following Optional Card E.
        """ # nopep8
        return self._cards[2].get_value("sst")

    @sst.setter
    def sst(self, value: float) -> None:
        self._cards[2].set_value("sst", value)

    @property
    def mst(self) -> typing.Optional[float]:
        """Get or set the Optional contact thickness for master surface (overrides default contact thickness). This option applies only to contact with shell elements. For the TIED options, see SST above.
        """ # nopep8
        return self._cards[2].get_value("mst")

    @mst.setter
    def mst(self, value: float) -> None:
        self._cards[2].set_value("mst", value)

    @property
    def sfst(self) -> float:
        """Get or set the Scale factor applied to contact thickness of slave surface. This option applies to contact with shell and beam elements. SFST has no bearing on the actual thickness of the elements; it only affects the location of the contact surface. SFST is ignored if SST is nonzero except in the case of MORTAR contact (see Remark 9 in the General Remarks: *Contact section).
        """ # nopep8
        return self._cards[2].get_value("sfst")

    @sfst.setter
    def sfst(self, value: float) -> None:
        self._cards[2].set_value("sfst", value)

    @property
    def sfmt(self) -> float:
        """Get or set the Scale factor applied to contact thickness of master surface. This option applies only to contact with shell elements. SFMT has no bearing on the actual thickness of the elements; it only affects the location of the contact surface. SFMT is ignored if MST is nonzero except in the case of MORTAR contact (see Remark 9 in the General Remarks: *Contact section).
        """ # nopep8
        return self._cards[2].get_value("sfmt")

    @sfmt.setter
    def sfmt(self, value: float) -> None:
        self._cards[2].set_value("sfmt", value)

    @property
    def fsf(self) -> float:
        """Get or set the Coulomb friction scale factor. The Coulomb friction value is scaled as `musc = FSF x muc`; see Card 2.
        """ # nopep8
        return self._cards[2].get_value("fsf")

    @fsf.setter
    def fsf(self, value: float) -> None:
        self._cards[2].set_value("fsf", value)

    @property
    def vsf(self) -> float:
        """Get or set the Viscous friction scale factor. If this factor is defined, then the limiting force becomes: Flim = VSF x VC x Acont; see Card 2.
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

