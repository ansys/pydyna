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

"""Module providing the ContactGebodLeftShoulder class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACTGEBODLEFTSHOULDER_CARD0 = (
    FieldSchema("did", int, 0, 10, None),
    FieldSchema("ssid", int, 10, 10, None),
    FieldSchema("sstyp", int, 20, 10, 0),
    FieldSchema("sf", float, 30, 10, 1.0),
    FieldSchema("df", float, 40, 10, 20.0),
    FieldSchema("cf", float, 50, 10, 0.5),
    FieldSchema("intord", int, 60, 10, 0),
)

_CONTACTGEBODLEFTSHOULDER_CARD1 = (
    FieldSchema("bt", float, 0, 10, 0.0),
    FieldSchema("dt", float, 10, 10, 1e+20),
    FieldSchema("so", int, 20, 10, 0),
)

class ContactGebodLeftShoulder(KeywordBase):
    """DYNA CONTACT_GEBOD_LEFT_SHOULDER keyword"""

    keyword = "CONTACT"
    subkeyword = "GEBOD_LEFT_SHOULDER"
    option_specs = [
        OptionSpec("ID", -2, 1),
        OptionSpec("MPP", -1, 2),
        OptionSpec("A", 1, 0),
        OptionSpec("B", 2, 0),
        OptionSpec("C", 3, 0),
        OptionSpec("D", 4, 0),
        OptionSpec("E", 5, 0),
        OptionSpec("F", 6, 0),
        OptionSpec("G", 7, 0),
    ]

    def __init__(self, **kwargs):
        """Initialize the ContactGebodLeftShoulder class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACTGEBODLEFTSHOULDER_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTGEBODLEFTSHOULDER_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = ContactGebodLeftShoulder.option_specs[0],
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
                option_spec = ContactGebodLeftShoulder.option_specs[1],
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
            OptionCardSet(
                option_spec = ContactGebodLeftShoulder.option_specs[2],
                cards = [
                    Card(
                        [
                            Field(
                                "soft",
                                int,
                                0,
                                10,
                                kwargs.get("soft")
                            ),
                            Field(
                                "sofscl",
                                float,
                                10,
                                10,
                                kwargs.get("sofscl", 0.1)
                            ),
                            Field(
                                "lcidab",
                                int,
                                20,
                                10,
                                kwargs.get("lcidab", 0)
                            ),
                            Field(
                                "maxpar",
                                float,
                                30,
                                10,
                                kwargs.get("maxpar", 1.025)
                            ),
                            Field(
                                "sbopt",
                                int,
                                40,
                                10,
                                kwargs.get("sbopt", 2)
                            ),
                            Field(
                                "depth",
                                int,
                                50,
                                10,
                                kwargs.get("depth", 2)
                            ),
                            Field(
                                "bsort",
                                int,
                                60,
                                10,
                                kwargs.get("bsort")
                            ),
                            Field(
                                "frcfrq",
                                int,
                                70,
                                10,
                                kwargs.get("frcfrq", 1)
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = ContactGebodLeftShoulder.option_specs[3],
                cards = [
                    Card(
                        [
                            Field(
                                "penmax",
                                float,
                                0,
                                10,
                                kwargs.get("penmax", 0.0)
                            ),
                            Field(
                                "thkopt",
                                int,
                                10,
                                10,
                                kwargs.get("thkopt", 0)
                            ),
                            Field(
                                "shlthk",
                                int,
                                20,
                                10,
                                kwargs.get("shlthk", 0)
                            ),
                            Field(
                                "snlog",
                                int,
                                30,
                                10,
                                kwargs.get("snlog", 0)
                            ),
                            Field(
                                "isym",
                                int,
                                40,
                                10,
                                kwargs.get("isym", 0)
                            ),
                            Field(
                                "i2d3d",
                                int,
                                50,
                                10,
                                kwargs.get("i2d3d", 0)
                            ),
                            Field(
                                "sldthk",
                                float,
                                60,
                                10,
                                kwargs.get("sldthk", 0.0)
                            ),
                            Field(
                                "sldstf",
                                float,
                                70,
                                10,
                                kwargs.get("sldstf", 0.0)
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = ContactGebodLeftShoulder.option_specs[4],
                cards = [
                    Card(
                        [
                            Field(
                                "igap",
                                int,
                                0,
                                10,
                                kwargs.get("igap", 1)
                            ),
                            Field(
                                "ignore",
                                int,
                                10,
                                10,
                                kwargs.get("ignore", 0)
                            ),
                            Field(
                                "dprfac",
                                float,
                                20,
                                10,
                                kwargs.get("dprfac", 0.0)
                            ),
                            Field(
                                "dtstif",
                                float,
                                30,
                                10,
                                kwargs.get("dtstif", 0.0)
                            ),
                            Field(
                                "edgek",
                                float,
                                40,
                                10,
                                kwargs.get("edgek", 0.0)
                            ),
                            Field(
                                "flangl",
                                float,
                                60,
                                10,
                                kwargs.get("flangl", 0.0)
                            ),
                            Field(
                                "cid_rcf",
                                int,
                                70,
                                10,
                                kwargs.get("cid_rcf")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = ContactGebodLeftShoulder.option_specs[5],
                cards = [
                    Card(
                        [
                            Field(
                                "q2tri",
                                int,
                                0,
                                10,
                                kwargs.get("q2tri", 0)
                            ),
                            Field(
                                "dtpchk",
                                float,
                                10,
                                10,
                                kwargs.get("dtpchk", 0.0)
                            ),
                            Field(
                                "sfnbr",
                                float,
                                20,
                                10,
                                kwargs.get("sfnbr", 0.0)
                            ),
                            Field(
                                "fnlscl",
                                float,
                                30,
                                10,
                                kwargs.get("fnlscl", 0.0)
                            ),
                            Field(
                                "dnlscl",
                                float,
                                40,
                                10,
                                kwargs.get("dnlscl", 0.0)
                            ),
                            Field(
                                "tcso",
                                int,
                                50,
                                10,
                                kwargs.get("tcso", 0)
                            ),
                            Field(
                                "tiedid",
                                int,
                                60,
                                10,
                                kwargs.get("tiedid", 0)
                            ),
                            Field(
                                "shledg",
                                int,
                                70,
                                10,
                                kwargs.get("shledg", 0)
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = ContactGebodLeftShoulder.option_specs[6],
                cards = [
                    Card(
                        [
                            Field(
                                "sharec",
                                int,
                                0,
                                10,
                                kwargs.get("sharec", 0)
                            ),
                            Field(
                                "cparm8",
                                int,
                                10,
                                10,
                                kwargs.get("cparm8", 0)
                            ),
                            Field(
                                "ipback",
                                int,
                                20,
                                10,
                                kwargs.get("ipback", 0)
                            ),
                            Field(
                                "srnde",
                                int,
                                30,
                                10,
                                kwargs.get("srnde", 0)
                            ),
                            Field(
                                "fricsf",
                                float,
                                40,
                                10,
                                kwargs.get("fricsf", 1.0)
                            ),
                            Field(
                                "icor",
                                int,
                                50,
                                10,
                                kwargs.get("icor", 0)
                            ),
                            Field(
                                "ftorq",
                                int,
                                60,
                                10,
                                kwargs.get("ftorq", 0)
                            ),
                            Field(
                                "region",
                                int,
                                70,
                                10,
                                kwargs.get("region", 0)
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = ContactGebodLeftShoulder.option_specs[7],
                cards = [
                    Card(
                        [
                            Field(
                                "pstiff",
                                int,
                                0,
                                10,
                                kwargs.get("pstiff", 0)
                            ),
                            Field(
                                "ignroff",
                                int,
                                10,
                                10,
                                kwargs.get("ignroff", 0)
                            ),
                            Field(
                                "fstol",
                                float,
                                30,
                                10,
                                kwargs.get("fstol", 2.0)
                            ),
                            Field(
                                "2dbinr",
                                int,
                                40,
                                10,
                                kwargs.get("2dbinr", 0)
                            ),
                            Field(
                                "ssftyp",
                                int,
                                50,
                                10,
                                kwargs.get("ssftyp", 0)
                            ),
                            Field(
                                "swtpr",
                                int,
                                60,
                                10,
                                kwargs.get("swtpr", 0)
                            ),
                            Field(
                                "tetfac",
                                float,
                                70,
                                10,
                                kwargs.get("tetfac", 0.0)
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
            OptionCardSet(
                option_spec = ContactGebodLeftShoulder.option_specs[8],
                cards = [
                    Card(
                        [
                            Field(
                                "shloff",
                                float,
                                10,
                                10,
                                kwargs.get("shloff", 0.0)
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def did(self) -> typing.Optional[int]:
        """Get or set the Dummy ID, see *COMPONENT_GEBOD.
        """ # nopep8
        return self._cards[0].get_value("did")

    @did.setter
    def did(self, value: int) -> None:
        """Set the did property."""
        self._cards[0].set_value("did", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Slave set ID, see *SET_NODE_OPTION, *PART, or *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
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
        """Set the sstyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sstyp must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("sstyp", value)

    @property
    def sf(self) -> float:
        """Get or set the Penalty scale factor. Useful to scale maximized penalty (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def df(self) -> float:
        """Get or set the Damping option, see description for *CONTACT_OPTION:
        EQ.0.0: no damping,
        GT.0.0: viscous damping in percent of critical, e.g., 20 for 20% damping (default=20.0),
        EQ.-n: |n| is the load curve ID giving the damping force versus relative normal velocity.
        """ # nopep8
        return self._cards[0].get_value("df")

    @df.setter
    def df(self, value: float) -> None:
        """Set the df property."""
        self._cards[0].set_value("df", value)

    @property
    def cf(self) -> float:
        """Get or set the Coulomb friction coefficient. Assumed to be constant (default=0.5).
        """ # nopep8
        return self._cards[0].get_value("cf")

    @cf.setter
    def cf(self, value: float) -> None:
        """Set the cf property."""
        self._cards[0].set_value("cf", value)

    @property
    def intord(self) -> int:
        """Get or set the Integration order (slaved materials only).
        EQ.0: check nodes only (default),
        EQ.1: 1 point integration over segments,
        EQ.2: 2x2 integration,
        EQ.3: 3x3 integration,
        EQ.4: 4x4 integration,
        EQ.5: 5x5 integration.
        This option allows a check of the penetration of the dummy segment into the deformable (slaved) material. Then virtual nodes at the location of the integration points are checked.
        """ # nopep8
        return self._cards[0].get_value("intord")

    @intord.setter
    def intord(self, value: int) -> None:
        """Set the intord property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""intord must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[0].set_value("intord", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        """Set the bt property."""
        self._cards[1].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
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
        """Set the so property."""
        self._cards[1].set_value("so", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[2].cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[2].cards[0].set_value("cid", value)

        if value:
            self.activate_option("CID")

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Interface descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[2].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[2].cards[0].set_value("heading", value)

        if value:
            self.activate_option("HEADING")

    @property
    def ignore(self) -> int:
        """Get or set the By setting this variable to 1, the "ignore initial penetrations" option is turned on for this contact.  Alternatively, this option may be turned on by setting IGNORE = 1 on Card 4 of *CONTROL_CONTACT or on Optional Card C of *CONTACT.  In other words, if IGNORE is set to 1 in any of three places, initial penetrations are tracked.
        """ # nopep8
        return self._cards[3].cards[0].get_value("ignore")

    @ignore.setter
    def ignore(self, value: int) -> None:
        """Set the ignore property."""
        self._cards[3].cards[0].set_value("ignore", value)

        if value:
            self.activate_option("IGNORE")

    @property
    def bckt(self) -> int:
        """Get or set the Bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts. For these two exceptions, the BSORT option on Optional Card A applies instead.
        """ # nopep8
        return self._cards[3].cards[0].get_value("bckt")

    @bckt.setter
    def bckt(self, value: int) -> None:
        """Set the bckt property."""
        self._cards[3].cards[0].set_value("bckt", value)

        if value:
            self.activate_option("BCKT")

    @property
    def lcbckt(self) -> typing.Optional[int]:
        """Get or set the Load curve for bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts.  For the two exceptions, the negative BSORT option on Optional Card A applies instead.
        """ # nopep8
        return self._cards[3].cards[0].get_value("lcbckt")

    @lcbckt.setter
    def lcbckt(self, value: int) -> None:
        """Set the lcbckt property."""
        self._cards[3].cards[0].set_value("lcbckt", value)

        if value:
            self.activate_option("LCBCKT")

    @property
    def ns2trk(self) -> int:
        """Get or set the Number of potential contacts to track for each tracked node.  The normal input for this (DEPTH on Optional Card A) is ignored..
        """ # nopep8
        return self._cards[3].cards[0].get_value("ns2trk")

    @ns2trk.setter
    def ns2trk(self, value: int) -> None:
        """Set the ns2trk property."""
        self._cards[3].cards[0].set_value("ns2trk", value)

        if value:
            self.activate_option("NS2TRK")

    @property
    def inititr(self) -> int:
        """Get or set the Number of iterations to perform when trying to eliminate initial penetrations.  Note that an input of 0 means 0, not the default value (which is 2).  Leaving this field blank will set INITITR to 2.
        """ # nopep8
        return self._cards[3].cards[0].get_value("inititr")

    @inititr.setter
    def inititr(self, value: int) -> None:
        """Set the inititr property."""
        self._cards[3].cards[0].set_value("inititr", value)

        if value:
            self.activate_option("INITITR")

    @property
    def parmax(self) -> float:
        """Get or set the The parametric extension distance for contact segments.  The MAXPAR parameter on Optional Card A is not used for MPP.  For non-tied contacts, the default is 1.0005. For tied contacts the default is 1.035 and, the actual extension used is computed as follows: see the manual
        """ # nopep8
        return self._cards[3].cards[0].get_value("parmax")

    @parmax.setter
    def parmax(self, value: float) -> None:
        """Set the parmax property."""
        self._cards[3].cards[0].set_value("parmax", value)

        if value:
            self.activate_option("PARMAX")

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
        return self._cards[3].cards[0].get_value("cparm8")

    @cparm8.setter
    def cparm8(self, value: int) -> None:
        """Set the cparm8 property."""
        if value not in [0, 1, 2, 10, 11, 12]:
            raise Exception("""cparm8 must be one of {0,1,2,10,11,12}""")
        self._cards[3].cards[0].set_value("cparm8", value)

        if value:
            self.activate_option("CPARM8")

    @property
    def mpp2(self) -> bool:
        """Get or set the Flag whether this is the MPP card.
        """ # nopep8
        return self._cards[3].cards[1].get_value("mpp2")

    @mpp2.setter
    def mpp2(self, value: bool) -> None:
        """Set the mpp2 property."""
        self._cards[3].cards[1].set_value("mpp2", value)

        if value:
            self.activate_option("MPP2")

    @property
    def chksegs(self) -> int:
        """Get or set the If this value is non-zero, then for the node-to-surface and surface-to-surface contacts LS-DYNA performs a special check at time 0 for elements that are inverted (or nearly so), These elements are removed from contact.  These poorly formed elements have been known to occur on the tooling in metalforming problems, which allows these problems to run.  It should not normally be needed for reasonable meshes.
        """ # nopep8
        return self._cards[3].cards[1].get_value("chksegs")

    @chksegs.setter
    def chksegs(self, value: int) -> None:
        """Set the chksegs property."""
        self._cards[3].cards[1].set_value("chksegs", value)

        if value:
            self.activate_option("CHKSEGS")

    @property
    def pensf(self) -> float:
        """Get or set the This option is used together with IGNORE for 3D forging problems.  If non-zero, the IGNORE penetration distance is multiplied by this value each cycle, effectively pushing the tracked node back out to the surface.  This is useful for nodes that might get generated below the reference surface during 3D remeshing.  Care should be exercised, as energy may be generated and stability may be effected for values lower than 0.95.  A value in the range of 0.98 to 0.99 or higher (but < 1.0) is recommended
        """ # nopep8
        return self._cards[3].cards[1].get_value("pensf")

    @pensf.setter
    def pensf(self, value: float) -> None:
        """Set the pensf property."""
        self._cards[3].cards[1].set_value("pensf", value)

        if value:
            self.activate_option("PENSF")

    @property
    def grpable(self) -> int:
        """Get or set the Set to 1 to invoke an alternate MPP communication algorithm for various SINGLE_SURFACE (including AUTOMATIC_GEN-ERAL), NODES_TO_SURFACE, SURFACE_TO_SURFACE, ERODING and SOFT = 2 contacts.  This groupable algorithm does not support all contact options, including MORTAR. It is still under development.  It can be significantly faster and scale better than the normal algorithm when there are more than two or three applicable contact types defined in the model. It is intended for speeding up the contact processing without changing the behavior of the contact.  See also *CONTROL_MPP_-CONTACT_GROUPABLE.
        """ # nopep8
        return self._cards[3].cards[1].get_value("grpable")

    @grpable.setter
    def grpable(self, value: int) -> None:
        """Set the grpable property."""
        self._cards[3].cards[1].set_value("grpable", value)

        if value:
            self.activate_option("GRPABLE")

    @property
    def soft(self) -> typing.Optional[int]:
        """Get or set the Soft constraint option:
        EQ.0: Standard penalty formulation,
        EQ.1: soft constraint penalty formulation,
        EQ.2: pinball segment based contact penalty formulation.
        EQ.4: Constraint approach for FORMING contacts. This formulation only applies to one-way forming contacts. You should use it when the penalty formulations result in large penetrations. The results, however, are sensitive to damping.
        EQ.6:Special contact algorithm to handle sheet blank edge(deformable) to gage pin(rigid shell) contact during implicit gravity loading.This applies to * CONTACT_FORMING_NODES_TO_SURFACE only.See remarks under About SOFT = 6
        """ # nopep8
        return self._cards[4].cards[0].get_value("soft")

    @soft.setter
    def soft(self, value: int) -> None:
        """Set the soft property."""
        if value not in [0, 1, 2, 4, 6]:
            raise Exception("""soft must be one of {0,1,2,4,6}""")
        self._cards[4].cards[0].set_value("soft", value)

        if value:
            self.activate_option("SOFT")

    @property
    def sofscl(self) -> float:
        """Get or set the Scale factor for constraint forces of soft constraint option invoked with SOFT = 1(default=.10). Values greater than .5 for single surface contact and 1.0 for a one way treatment are inadmissible.
        """ # nopep8
        return self._cards[4].cards[0].get_value("sofscl")

    @sofscl.setter
    def sofscl(self, value: float) -> None:
        """Set the sofscl property."""
        self._cards[4].cards[0].set_value("sofscl", value)

        if value:
            self.activate_option("SOFSCL")

    @property
    def lcidab(self) -> int:
        """Get or set the Load curve ID defining airbag thickness as a function of time for type a13 contact (*CONTACT_AIRBAG_SINGLE_SURFACE).
        """ # nopep8
        return self._cards[4].cards[0].get_value("lcidab")

    @lcidab.setter
    def lcidab(self, value: int) -> None:
        """Set the lcidab property."""
        self._cards[4].cards[0].set_value("lcidab", value)

        if value:
            self.activate_option("LCIDAB")

    @property
    def maxpar(self) -> float:
        """Get or set the Maximum parametric coordinate in segment search (values 1.025 and 1.20 recommended). Larger values can increase cost. If zero, the default is set to 1.025. This factor allows an increase in the size of the segments . May be useful at sharp corners.
        """ # nopep8
        return self._cards[4].cards[0].get_value("maxpar")

    @maxpar.setter
    def maxpar(self, value: float) -> None:
        """Set the maxpar property."""
        self._cards[4].cards[0].set_value("maxpar", value)

        if value:
            self.activate_option("MAXPAR")

    @property
    def sbopt(self) -> int:
        """Get or set the Segment-based contact options (SOFT=2).
        EQ.0: defaults to 2.
        EQ.1: pinball edge-edge contact (not recommended).
        EQ.2: assume planer segments (default).
        EQ.3: warped segment checking.
        EQ.4: sliding option,
        EQ.5: do options 3 and 4.
        """ # nopep8
        return self._cards[4].cards[0].get_value("sbopt")

    @sbopt.setter
    def sbopt(self, value: int) -> None:
        """Set the sbopt property."""
        if value not in [2, 0, 1, 3, 4, 5]:
            raise Exception("""sbopt must be one of {2,0,1,3,4,5}""")
        self._cards[4].cards[0].set_value("sbopt", value)

        if value:
            self.activate_option("SBOPT")

    @property
    def depth(self) -> int:
        """Get or set the Search depth in automatic contact. Value of 1 is sufficiently accurate for most crash applications and is much less expensive. LS-DYNA for improved accuracy sets this value to 2. If zero, the default is set to 2.
        LT.0: |DEPTH| is the load curve ID defining searching depth versus time.
        """ # nopep8
        return self._cards[4].cards[0].get_value("depth")

    @depth.setter
    def depth(self, value: int) -> None:
        """Set the depth property."""
        self._cards[4].cards[0].set_value("depth", value)

        if value:
            self.activate_option("DEPTH")

    @property
    def bsort(self) -> typing.Optional[int]:
        """Get or set the Number of cycles between bucket sorts.  Values of 25 and 100 are recommended for contact types 4 (SINGLE_SURFACE) and 13 (AUTOMATIC_SINGLE_SURFACE), respectively.  Values of 10-15 are okay for surface-to-surface and node-to-surface contact.  If zero, LS-DYNA determines the interval.  BSORT applies only to SMP (see BCKT on MPP 1 for MPP) except in the case of SOFT = 2 or for Mortar contact, in which case BSORT applies to both SMP and MPP. For Mortar contact the default is the value associated with NSBCS on *CONTROL_CONTACT.
        LT.0: |BSORT| is the load curve ID defining bucket sorting frequency as a function of time.
        """ # nopep8
        return self._cards[4].cards[0].get_value("bsort")

    @bsort.setter
    def bsort(self, value: int) -> None:
        """Set the bsort property."""
        self._cards[4].cards[0].set_value("bsort", value)

        if value:
            self.activate_option("BSORT")

    @property
    def frcfrq(self) -> int:
        """Get or set the Number of cycles between contact force updates for penalty contact formulations. This option can provide a significant speed-up of the contact treatment. If used, values exceeding 3 or 4 are dangerous. Considerable care must be exercised when using this option, as this option assumes that contact does not change FRCFRG cycles.
        EQ.0: FRCFRG is set to 1 and force calculations are performed each cycle-strongly recommended.
        """ # nopep8
        return self._cards[4].cards[0].get_value("frcfrq")

    @frcfrq.setter
    def frcfrq(self, value: int) -> None:
        """Set the frcfrq property."""
        self._cards[4].cards[0].set_value("frcfrq", value)

        if value:
            self.activate_option("FRCFRQ")

    @property
    def penmax(self) -> float:
        """Get or set the For old types 3, 5, 8, 9, 10 (see Mapping of *CONTACT keyword option to contact type in d3hsp at the end of General Remarks) and Mortar contact, PENMAX is the maximum penetration distance. For contact types a3, a5, a10, 13, 15, and 26, the segment thickness multiplied by PENMAX defines the maximum penetration allowed (as a multiple of the segment thickness).  (See Table 0-2.):):
        EQ.0.0 for old type contacts 3, 5, and 10: Use small penetration search and value calculated from thickness and XPENE, see *CONTROL_ CONTACT.
        EQ.0.0 for contact types a 3, a 5, a10, 13, and 15: Default is 0.4, or 40 percent of the segment thickness
        EQ.0.0 for contact type26: Default is 200.0 times the segment thickness
        """ # nopep8
        return self._cards[5].cards[0].get_value("penmax")

    @penmax.setter
    def penmax(self, value: float) -> None:
        """Set the penmax property."""
        self._cards[5].cards[0].set_value("penmax", value)

        if value:
            self.activate_option("PENMAX")

    @property
    def thkopt(self) -> int:
        """Get or set the Thickness option for contact types 3, 5, and 10:
        EQ.0: default is taken from control card, *CONTROL_CONTACT,
        EQ.1: thickness offsets are included,
        EQ.2: thickness offsets are not included (old way).
        """ # nopep8
        return self._cards[5].cards[0].get_value("thkopt")

    @thkopt.setter
    def thkopt(self, value: int) -> None:
        """Set the thkopt property."""
        if value not in [0, 1, 2]:
            raise Exception("""thkopt must be one of {0,1,2}""")
        self._cards[5].cards[0].set_value("thkopt", value)

        if value:
            self.activate_option("THKOPT")

    @property
    def shlthk(self) -> int:
        """Get or set the Define if and only if THKOPT above equals 1. Shell thickness considered in type surface to surface and node to surface type contact options, where options 1 and 2 below activate the new contact algorithms. The thickness offsets are always included in single surface and constraint method contact types:
        EQ.0: thickness is not considered,
        EQ.1: thickness is considered but rigid bodies are excluded,
        EQ.2: thickness is considered including rigid bodies.
        """ # nopep8
        return self._cards[5].cards[0].get_value("shlthk")

    @shlthk.setter
    def shlthk(self, value: int) -> None:
        """Set the shlthk property."""
        if value not in [0, 1, 2]:
            raise Exception("""shlthk must be one of {0,1,2}""")
        self._cards[5].cards[0].set_value("shlthk", value)

        if value:
            self.activate_option("SHLTHK")

    @property
    def snlog(self) -> int:
        """Get or set the Disable shooting node logic in thickness offset contact. With the shooting node logic enabled, the first cycle that a tracked node penetrates a reference segment, that node is moved back to the reference surface without applying any contact force.
        EQ.0: logic is enabled (default),
        EQ.1: logic is skipped (sometimes recommended for metalforming calculations).
        """ # nopep8
        return self._cards[5].cards[0].get_value("snlog")

    @snlog.setter
    def snlog(self, value: int) -> None:
        """Set the snlog property."""
        if value not in [0, 1]:
            raise Exception("""snlog must be one of {0,1}""")
        self._cards[5].cards[0].set_value("snlog", value)

        if value:
            self.activate_option("SNLOG")

    @property
    def isym(self) -> int:
        """Get or set the Symmetry plane option:
        EQ.0: off,
        EQ.1: do not include faces with normal boundary constraints (e.g., segments of brick elements on a symmetry plane).
        This option is important to retain the correct boundary conditions in the model with symmetry. For the _ERODING_ contacts this option may also be defined on card 4.
        """ # nopep8
        return self._cards[5].cards[0].get_value("isym")

    @isym.setter
    def isym(self, value: int) -> None:
        """Set the isym property."""
        if value not in [0, 1]:
            raise Exception("""isym must be one of {0,1}""")
        self._cards[5].cards[0].set_value("isym", value)

        if value:
            self.activate_option("ISYM")

    @property
    def i2d3d(self) -> int:
        """Get or set the Segment searching option:
        EQ.0: search 2D elements (shells) before 3D elements (solids, thick shells) when locating segments.
        EQ.1: search 3D (solids, thick shells) elements before 2D elements (shells) when locating segments.
        """ # nopep8
        return self._cards[5].cards[0].get_value("i2d3d")

    @i2d3d.setter
    def i2d3d(self, value: int) -> None:
        """Set the i2d3d property."""
        if value not in [0, 1]:
            raise Exception("""i2d3d must be one of {0,1}""")
        self._cards[5].cards[0].set_value("i2d3d", value)

        if value:
            self.activate_option("I2D3D")

    @property
    def sldthk(self) -> float:
        """Get or set the Optional solid element thickness. A nonzero positive value will activate the contact thickness offsets in the contact algorithms where offsets apply. The contact treatment with then be equivalent to the case where null shell elements are used to cover the brick elements. The contact stiffness parameter below, SLDSTF, may also be used to override the default value.
        """ # nopep8
        return self._cards[5].cards[0].get_value("sldthk")

    @sldthk.setter
    def sldthk(self, value: float) -> None:
        """Set the sldthk property."""
        self._cards[5].cards[0].set_value("sldthk", value)

        if value:
            self.activate_option("SLDTHK")

    @property
    def sldstf(self) -> float:
        """Get or set the Optional solid element stiffness. A nonzero positive value overrides the bulk modulus taken from the material model referenced by the solid element.
        """ # nopep8
        return self._cards[5].cards[0].get_value("sldstf")

    @sldstf.setter
    def sldstf(self, value: float) -> None:
        """Set the sldstf property."""
        self._cards[5].cards[0].set_value("sldstf", value)

        if value:
            self.activate_option("SLDSTF")

    @property
    def igap(self) -> int:
        """Get or set the For mortar contact IGAP is used to progressively increase contact stiffness for large penetrations, or use a linear relationship between penetration and contact pressure; see remarks on mortar contact below.
        For other contacts it is a flag to improve implicit convergence behavior
        at the expense of (1) creating some sticking if parts attempt to separate
        and (2) possibly underreporting the contact force magnitude in the
        output files rcforc and ncforc. (IMPLICIT ONLY.).
        LT.0: Like IGAP = 1 except the maximum distance between contact surfaces at which stickiness is on is sacled by IGAP/10.
        EQ.1: Apply method to improve convergence (DEFAULT)
        EQ.2: Do not apply method
        GT.2: Set IGAP = 1 for first IGAP-2 converged equilibrium states,
        """ # nopep8
        return self._cards[6].cards[0].get_value("igap")

    @igap.setter
    def igap(self, value: int) -> None:
        """Set the igap property."""
        self._cards[6].cards[0].set_value("igap", value)

        if value:
            self.activate_option("IGAP")

    @property
    def ignore(self) -> int:
        """Get or set the Ignore initial penetrations for the *CONTACT_AUTOMATIC options:LT.0:Applies only to the Mortar contact.When less than zero, the behavior is the same as for | IGNORE| , but contact between segments belonging to the same part is ignored.
        The main purpose of this option is to avoid spurious contact detections that otherwise could result for complicated geometries in a single surface contact, typically, when eliminating initial penetrations by interference.See IGNORE = 3 and IGNORE = 4.
        EQ.0 : Take the default value from the fourth card of the* CONTROL_CONTACT input.
        EQ.1 : Allow initial penetrations to exist by tracking the initial penetrations.
        EQ.2 : Allow initial penetrations to exist by tracking the initial penetrations.However, penetration warning messages are printed with the original coordinates, and the recommended coordinates of each penetrating node are given.For Mortar contact, this is the default (see Remark 14 in the General Remarks section).
        EQ.3 : Applies only to the Mortar contact.With this option initial penetrations are eliminated between time zero and the time specified by MPAR1.Intended for small initial penetrations.See Remark 14 in the General Remarks section.
        EQ.4 : Applies only to the Mortar contact.With this option initial penetrations are eliminated between time zero and the time specified by MPAR1.In addition, a maximum penetration distance can be given as MPAR2, intended for large initial penetrations.See Remark 14 in the General Remarks section.
        """ # nopep8
        return self._cards[6].cards[0].get_value("ignore")

    @ignore.setter
    def ignore(self, value: int) -> None:
        """Set the ignore property."""
        self._cards[6].cards[0].set_value("ignore", value)

        if value:
            self.activate_option("IGNORE")

    @property
    def dprfac(self) -> float:
        """Get or set the Applies to the SOFT=2 and Mortar contacts. Depth of penetration reduction factor for SOFT=2 contact.
        EQ.0.0:Initial penetrations are always ignored.
        GT.0.0: Initial penetrations are penalized over time.
        LT.0.0:|DPRFAC| is the load curve ID defining DPRFAC versus time.
        For the mortar conatact MPAR1 corresponds to initial contact pressure in interfaces with initial penetrations if IGNORE=2, for IGNORE=3,4 it corresponds to the time of closure of initial penetrations.
        """ # nopep8
        return self._cards[6].cards[0].get_value("dprfac")

    @dprfac.setter
    def dprfac(self, value: float) -> None:
        """Set the dprfac property."""
        self._cards[6].cards[0].set_value("dprfac", value)

        if value:
            self.activate_option("DPRFAC")

    @property
    def dtstif(self) -> float:
        """Get or set the Applies to the SOFT=1 and SOFT=2 and Mortar contacts. Time step used in stiffness calculation for SOFT=1 and SOFT=2 contact.
        EQ.0.0:Use the initial value that is used for time integration.
        GT.0.0: Use the value specified.
        LT.-0.01 and GT.-1.0: use a moving average of the solution time step. (SOFT=2 only).
        LT.-1.0: |DTSTIF| is the load curve ID defining DTSTIF versus time.
        For the mortar contact and IGNORE=4, MPAR2 corresponds a penetration depth that must be at least the penetration occurring in the contact interface.
        """ # nopep8
        return self._cards[6].cards[0].get_value("dtstif")

    @dtstif.setter
    def dtstif(self, value: float) -> None:
        """Set the dtstif property."""
        self._cards[6].cards[0].set_value("dtstif", value)

        if value:
            self.activate_option("DTSTIF")

    @property
    def edgek(self) -> float:
        """Get or set the Scale factor for penalty stiffness of edge to edge contact when SOFT = 2 and DEPTH = 5, 15, 25, or 35:
        EQ.0.0: Use the default penalty stiffness.
        GT.0.0: Scale the stiffness by EDGEK.
        """ # nopep8
        return self._cards[6].cards[0].get_value("edgek")

    @edgek.setter
    def edgek(self, value: float) -> None:
        """Set the edgek property."""
        self._cards[6].cards[0].set_value("edgek", value)

        if value:
            self.activate_option("EDGEK")

    @property
    def flangl(self) -> float:
        """Get or set the Angle tolerance in radians for feature lines option in smooth contact.
        EQ.0.0:No feature line is considered for surface fitting in smooth contact.
        GT.0.0:Any edge with angle between two contact segments bigger than this angle will be treated as feature line during surface fitting in smooth contact.
        """ # nopep8
        return self._cards[6].cards[0].get_value("flangl")

    @flangl.setter
    def flangl(self, value: float) -> None:
        """Set the flangl property."""
        self._cards[6].cards[0].set_value("flangl", value)

        if value:
            self.activate_option("FLANGL")

    @property
    def cid_rcf(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID to output RCFORC force resultants in a local system.
        """ # nopep8
        return self._cards[6].cards[0].get_value("cid_rcf")

    @cid_rcf.setter
    def cid_rcf(self, value: int) -> None:
        """Set the cid_rcf property."""
        self._cards[6].cards[0].set_value("cid_rcf", value)

        if value:
            self.activate_option("CID_RCF")

    @property
    def q2tri(self) -> int:
        """Get or set the Option to split quadrilateral contact segments into two triangles (only available when SOFT=2).
        EQ.0:Off (default).
        EQ.1:On for all SURFA shell segments.
        EQ.2:On for all SURFB shell segments.
        EQ.3:On for all shell segments.
        EQ.4:On for all shell segments of material type 34.
        """ # nopep8
        return self._cards[7].cards[0].get_value("q2tri")

    @q2tri.setter
    def q2tri(self, value: int) -> None:
        """Set the q2tri property."""
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""q2tri must be one of {0,1,2,3,4}""")
        self._cards[7].cards[0].set_value("q2tri", value)

        if value:
            self.activate_option("Q2TRI")

    @property
    def dtpchk(self) -> float:
        """Get or set the Time interval between shell penetration reports (only available for segment based contact)
        EQ.0.0:Off (default).
        GT.0.0:  Check and report segment penetrations at time intervals equal to DTPCHK.
        LT.0.0:Check and report segment penetrations at time intervals equal to |DTPCHK|. In addition, calculation stops with an error at t=0 if any intersections are initially present
        """ # nopep8
        return self._cards[7].cards[0].get_value("dtpchk")

    @dtpchk.setter
    def dtpchk(self, value: float) -> None:
        """Set the dtpchk property."""
        self._cards[7].cards[0].set_value("dtpchk", value)

        if value:
            self.activate_option("DTPCHK")

    @property
    def sfnbr(self) -> float:
        """Get or set the Scale factor for neighbor segment contact (only available for segment based contact)
        EQ.0.0:Off (default).
        GT.0.0:  Check neighbor segments for contact
        """ # nopep8
        return self._cards[7].cards[0].get_value("sfnbr")

    @sfnbr.setter
    def sfnbr(self, value: float) -> None:
        """Set the sfnbr property."""
        self._cards[7].cards[0].set_value("sfnbr", value)

        if value:
            self.activate_option("SFNBR")

    @property
    def fnlscl(self) -> float:
        """Get or set the Scale factor for nonlinear force scaling
        """ # nopep8
        return self._cards[7].cards[0].get_value("fnlscl")

    @fnlscl.setter
    def fnlscl(self, value: float) -> None:
        """Set the fnlscl property."""
        self._cards[7].cards[0].set_value("fnlscl", value)

        if value:
            self.activate_option("FNLSCL")

    @property
    def dnlscl(self) -> float:
        """Get or set the Distance for nonlinear force scaling
        """ # nopep8
        return self._cards[7].cards[0].get_value("dnlscl")

    @dnlscl.setter
    def dnlscl(self, value: float) -> None:
        """Set the dnlscl property."""
        self._cards[7].cards[0].set_value("dnlscl", value)

        if value:
            self.activate_option("DNLSCL")

    @property
    def tcso(self) -> int:
        """Get or set the Option to consider only contact segments (not all attached elements) when
        computing the contact thickness for a node or segment (for SURFACE_TO_SURFACE contact and shell elements only)
        EQ.0: Off (default).
        EQ.1: Only consider segments in the contact definition
        """ # nopep8
        return self._cards[7].cards[0].get_value("tcso")

    @tcso.setter
    def tcso(self, value: int) -> None:
        """Set the tcso property."""
        if value not in [0, 1]:
            raise Exception("""tcso must be one of {0,1}""")
        self._cards[7].cards[0].set_value("tcso", value)

        if value:
            self.activate_option("TCSO")

    @property
    def tiedid(self) -> int:
        """Get or set the Incremental displacement update for tied contacts.EQ.0:  Off (default).
        EQ.1:  On.
        """ # nopep8
        return self._cards[7].cards[0].get_value("tiedid")

    @tiedid.setter
    def tiedid(self, value: int) -> None:
        """Set the tiedid property."""
        if value not in [0, 1]:
            raise Exception("""tiedid must be one of {0,1}""")
        self._cards[7].cards[0].set_value("tiedid", value)

        if value:
            self.activate_option("TIEDID")

    @property
    def shledg(self) -> int:
        """Get or set the Flag for assuming edge shape for shells when measuring penetration.This is available for segment - based contact(SOFT = 2).
        EQ.0:Default to SHELDG on * CONTROL_CONTACT
        EQ.1 : Shell edges are assumed to be square and are flush with the nodes.
        EQ.2 : Shell edges are assumed to be round with a radius equal to half the shell thickness.The edge centers lie on the lines between the segment nodes and extend outward by the radius.This option is not available for DEPTH values of 23, 33, or 35.
        """ # nopep8
        return self._cards[7].cards[0].get_value("shledg")

    @shledg.setter
    def shledg(self, value: int) -> None:
        """Set the shledg property."""
        if value not in [0, 1, 2]:
            raise Exception("""shledg must be one of {0,1,2}""")
        self._cards[7].cards[0].set_value("shledg", value)

        if value:
            self.activate_option("SHLEDG")

    @property
    def sharec(self) -> int:
        """Get or set the Shared constraint flag (only available for segment based contact)
        EQ.0: Segments that share constraints not checked for contact.
        EQ.1: Segments that share constraints are checked for contact.
        """ # nopep8
        return self._cards[8].cards[0].get_value("sharec")

    @sharec.setter
    def sharec(self, value: int) -> None:
        """Set the sharec property."""
        if value not in [0, 1]:
            raise Exception("""sharec must be one of {0,1}""")
        self._cards[8].cards[0].set_value("sharec", value)

        if value:
            self.activate_option("SHAREC")

    @property
    def cparm8(self) -> int:
        """Get or set the This variable is similar to CPARM8 in *CONTACT_..._MPP but applies to SMP and not to MPP.  CPARM8 for SMP only controls treatment of spot weld beams in *CONTACT_AUTOMATIC_GENERAL.
        EQ.0:Spot weld(type 9) beams are not considered in the contact even if included in SURFA
        EQ.2:Spot weld(type 9) beams are considered in the contact if included in SURFA
        """ # nopep8
        return self._cards[8].cards[0].get_value("cparm8")

    @cparm8.setter
    def cparm8(self, value: int) -> None:
        """Set the cparm8 property."""
        if value not in [0, 2]:
            raise Exception("""cparm8 must be one of {0,2}""")
        self._cards[8].cards[0].set_value("cparm8", value)

        if value:
            self.activate_option("CPARM8")

    @property
    def ipback(self) -> int:
        """Get or set the If set to a nonzero value, creates a  backup  penalty tied contact for this
        interface. This option applies to constrained tied contacts only. See Remark 2.
        """ # nopep8
        return self._cards[8].cards[0].get_value("ipback")

    @ipback.setter
    def ipback(self, value: int) -> None:
        """Set the ipback property."""
        self._cards[8].cards[0].set_value("ipback", value)

        if value:
            self.activate_option("IPBACK")

    @property
    def srnde(self) -> int:
        """Get or set the Segment Rounded Edges:
        EQ.0: free edges have their usual treatement
        EQ.1: free edges are rounded, but without extending them.
        """ # nopep8
        return self._cards[8].cards[0].get_value("srnde")

    @srnde.setter
    def srnde(self, value: int) -> None:
        """Set the srnde property."""
        self._cards[8].cards[0].set_value("srnde", value)

        if value:
            self.activate_option("SRNDE")

    @property
    def fricsf(self) -> float:
        """Get or set the Scale factor for frictional stiffness (available for SOFT = 2 only).
        """ # nopep8
        return self._cards[8].cards[0].get_value("fricsf")

    @fricsf.setter
    def fricsf(self, value: float) -> None:
        """Set the fricsf property."""
        self._cards[8].cards[0].set_value("fricsf", value)

        if value:
            self.activate_option("FRICSF")

    @property
    def icor(self) -> int:
        """Get or set the If set to a nonzero value, VDC is the coefficient of restitution
        expressed as a percentage. When SOFT = 0 or 1, this option applies
        to AUTOMATIC_NODES_TO_SURFACE, AUTOMATIC_SURFACE_TO_SURFACE and AUTOMATIC_SINGLE_SURFACE.
        When SOFT = 2, it applies to all available keywords.
        """ # nopep8
        return self._cards[8].cards[0].get_value("icor")

    @icor.setter
    def icor(self, value: int) -> None:
        """Set the icor property."""
        self._cards[8].cards[0].set_value("icor", value)

        if value:
            self.activate_option("ICOR")

    @property
    def ftorq(self) -> int:
        """Get or set the If set to 1, a torsional force is computed in the beam to beam portion
        of contact type AUTOMATIC_GENERAL, which balances the
        torque produced due to friction. This is currently only available in the MPP version.
        """ # nopep8
        return self._cards[8].cards[0].get_value("ftorq")

    @ftorq.setter
    def ftorq(self, value: int) -> None:
        """Set the ftorq property."""
        self._cards[8].cards[0].set_value("ftorq", value)

        if value:
            self.activate_option("FTORQ")

    @property
    def region(self) -> int:
        """Get or set the The ID of a *DEFINE_REGION which will delimit the volume of
        space where this contact is active. See Remark 4 below.
        """ # nopep8
        return self._cards[8].cards[0].get_value("region")

    @region.setter
    def region(self, value: int) -> None:
        """Set the region property."""
        self._cards[8].cards[0].set_value("region", value)

        if value:
            self.activate_option("REGION")

    @property
    def pstiff(self) -> int:
        """Get or set the Flag to choose the method for calculating the penalty stiffness. This is available for segment based contact (see SOFT on optional card A)
        EQ.0: Use the default as defined by PSTIFF on *CONTROL_CONTACT.
        EQ.1: Based on nodal masses
        EQ.2: Based on material density and segment dimensions.
        """ # nopep8
        return self._cards[9].cards[0].get_value("pstiff")

    @pstiff.setter
    def pstiff(self, value: int) -> None:
        """Set the pstiff property."""
        if value not in [0, 1, 2]:
            raise Exception("""pstiff must be one of {0,1,2}""")
        self._cards[9].cards[0].set_value("pstiff", value)

        if value:
            self.activate_option("PSTIFF")

    @property
    def ignroff(self) -> int:
        """Get or set the Flag to ignore the thickness offset for shells in the calculation of the shell contact penetration depth. This allows shells to be used for
        meshing rigid body dies without modifying the positions of the nodes to compensate for the shell thickness.
        EQ.0: Default
        EQ.1: Ignore the SURFB side thickness.
        EQ.2: Ignore the SURFA side thickness.
        EQ.3: Ignore the thickness of both sides..
        """ # nopep8
        return self._cards[9].cards[0].get_value("ignroff")

    @ignroff.setter
    def ignroff(self, value: int) -> None:
        """Set the ignroff property."""
        if value not in [0, 1, 2, 3]:
            raise Exception("""ignroff must be one of {0,1,2,3}""")
        self._cards[9].cards[0].set_value("ignroff", value)

        if value:
            self.activate_option("IGNROFF")

    @property
    def fstol(self) -> float:
        """Get or set the Tolerance used with the SMOOTH option for determining which segments are considered flat.  The value is in degrees and approximately represents half the angle between adjacent segments
        """ # nopep8
        return self._cards[9].cards[0].get_value("fstol")

    @fstol.setter
    def fstol(self, value: float) -> None:
        """Set the fstol property."""
        self._cards[9].cards[0].set_value("fstol", value)

        if value:
            self.activate_option("FSTOL")

    @property
    def _2dbinr(self) -> int:
        """Get or set the Flag to indicate that 2D belts initially inside retractors are involved in the contact.  This is only available for SURFACE_TO_SURFACE contact of segment-based contact (SOFT = 2).
        EQ.0:No 2D belt initially inside a retractor is involved.
        EQ.1 : 2D belts initially inside retractors are involved
        """ # nopep8
        return self._cards[9].cards[0].get_value("2dbinr")

    @_2dbinr.setter
    def _2dbinr(self, value: int) -> None:
        """Set the _2dbinr property."""
        if value not in [0, 1]:
            raise Exception("""_2dbinr must be one of {0,1}""")
        self._cards[9].cards[0].set_value("2dbinr", value)

        if value:
            self.activate_option("_2DBINR")

    @property
    def ssftyp(self) -> int:
        """Get or set the Flag to determine how the SSF option on *PART_CONTACT behaves when SOFT = 2 on optional card A:
        EQ.0:Use SSF from the tracked segment as determined by the SOFT = 2 algorithm (see Remark 2)
        EQ.1 : Use the larger of the SSF values.
        """ # nopep8
        return self._cards[9].cards[0].get_value("ssftyp")

    @ssftyp.setter
    def ssftyp(self, value: int) -> None:
        """Set the ssftyp property."""
        if value not in [0, 1]:
            raise Exception("""ssftyp must be one of {0,1}""")
        self._cards[9].cards[0].set_value("ssftyp", value)

        if value:
            self.activate_option("SSFTYP")

    @property
    def swtpr(self) -> int:
        """Get or set the Flag to use tapered shell contact segments adjacent to segments that are thinned by the SPOTHIN option on *CONTROL_CONTACT. This option is only available when SOFT=2 on optional card A.
        EQ.0:Use full thickness constant segments.
        EQ.1 : Use tapered segments.
        """ # nopep8
        return self._cards[9].cards[0].get_value("swtpr")

    @swtpr.setter
    def swtpr(self, value: int) -> None:
        """Set the swtpr property."""
        if value not in [0, 1]:
            raise Exception("""swtpr must be one of {0,1}""")
        self._cards[9].cards[0].set_value("swtpr", value)

        if value:
            self.activate_option("SWTPR")

    @property
    def tetfac(self) -> float:
        """Get or set the Scale factor for the computed volume of tetrahedral solid elements for the mass calculation in SOFT=2 contact. By default, half the mass of a solid element is considered for the contact segment, which is reasonable for hexahedrons. In contrast, for tetrahedrons, a larger value than 0.5 would be preferrable, because several tets fit into one hex. Therefore, a TETFAC value around 3.0 to 5.0 should make the contact stiffness more comparable with hex meshes.
        """ # nopep8
        return self._cards[9].cards[0].get_value("tetfac")

    @tetfac.setter
    def tetfac(self, value: float) -> None:
        """Set the tetfac property."""
        self._cards[9].cards[0].set_value("tetfac", value)

        if value:
            self.activate_option("TETFAC")

    @property
    def shloff(self) -> float:
        """Get or set the Flag affecting the location of the contact surfaces for shells when NLOC is nonzero in *SECTION_SHELL or *PART_COMPOSITE, or when OFFSET is specified using *ELEMENT_SHELL_OFFSET. Thus, set this field to 1 to enable the behavior locally for this contact and leave CNTCO as 0 to disable this behavior for all contacts without this field set to 1.
        EQ.0: The setting of CNTO on *CONTROL_SHELL determines the contact reference plane.
        EQ.1:The contact reference plance coincides with shell reference surface.
        """ # nopep8
        return self._cards[10].cards[0].get_value("shloff")

    @shloff.setter
    def shloff(self, value: float) -> None:
        """Set the shloff property."""
        self._cards[10].cards[0].set_value("shloff", value)

        if value:
            self.activate_option("SHLOFF")

