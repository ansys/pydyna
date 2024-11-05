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

class Contact2DAutomaticTiedOneWayThermal(KeywordBase):
    """DYNA CONTACT_2D_AUTOMATIC_TIED_ONE_WAY_THERMAL keyword"""

    keyword = "CONTACT"
    subkeyword = "2D_AUTOMATIC_TIED_ONE_WAY_THERMAL"
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
                        "sfact",
                        float,
                        20,
                        10,
                        kwargs.get("sfact", 1.0)
                    ),
                    Field(
                        "freq",
                        int,
                        30,
                        10,
                        kwargs.get("freq", 50)
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
                        "tbirth",
                        float,
                        0,
                        10,
                        kwargs.get("tbirth", 0.0)
                    ),
                    Field(
                        "tdeath",
                        float,
                        10,
                        10,
                        kwargs.get("tdeath", 1.0E+20)
                    ),
                    Field(
                        "soa",
                        float,
                        20,
                        10,
                        kwargs.get("soa", 1.0)
                    ),
                    Field(
                        "sob",
                        float,
                        30,
                        10,
                        kwargs.get("sob", 1.0)
                    ),
                    Field(
                        "nda",
                        int,
                        40,
                        10,
                        kwargs.get("nda", 0)
                    ),
                    Field(
                        "ndb",
                        int,
                        50,
                        10,
                        kwargs.get("ndb", 0)
                    ),
                    Field(
                        "cof",
                        int,
                        60,
                        10,
                        kwargs.get("cof", 0)
                    ),
                    Field(
                        "init",
                        int,
                        70,
                        10,
                        kwargs.get("init", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k",
                        float,
                        0,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "rad",
                        float,
                        10,
                        10,
                        kwargs.get("rad")
                    ),
                    Field(
                        "h",
                        float,
                        20,
                        10,
                        kwargs.get("h")
                    ),
                    Field(
                        "lmin",
                        float,
                        30,
                        10,
                        kwargs.get("lmin")
                    ),
                    Field(
                        "lmax",
                        float,
                        40,
                        10,
                        kwargs.get("lmax")
                    ),
                    Field(
                        "chlm",
                        float,
                        50,
                        10,
                        kwargs.get("chlm", 1.0)
                    ),
                    Field(
                        "bc_flag",
                        int,
                        60,
                        10,
                        kwargs.get("bc_flag", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vc",
                        float,
                        0,
                        10,
                        kwargs.get("vc", 0.0)
                    ),
                    Field(
                        "vdc",
                        float,
                        10,
                        10,
                        kwargs.get("vdc", 10.0)
                    ),
                    Field(
                        "ipf",
                        int,
                        20,
                        10,
                        kwargs.get("ipf", 0)
                    ),
                    Field(
                        "slide",
                        int,
                        30,
                        10,
                        kwargs.get("slide", 0)
                    ),
                    Field(
                        "istiff",
                        int,
                        40,
                        10,
                        kwargs.get("istiff", 0)
                    ),
                    Field(
                        "tiedgap",
                        float,
                        50,
                        10,
                        kwargs.get("tiedgap", 0.0)
                    ),
                    Field(
                        "igapcl",
                        int,
                        60,
                        10,
                        kwargs.get("igapcl", 0)
                    ),
                    Field(
                        "tietyp",
                        int,
                        70,
                        10,
                        kwargs.get("tietyp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sldsoa",
                        float,
                        0,
                        10,
                        kwargs.get("sldsoa", 0.0)
                    ),
                    Field(
                        "sldsob",
                        float,
                        10,
                        10,
                        kwargs.get("sldsob", 0.0)
                    ),
                    Field(
                        "tdpen",
                        float,
                        20,
                        10,
                        kwargs.get("tdpen", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Contact2DAutomaticTiedOneWayThermal.option_specs[0],
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
                option_spec = Contact2DAutomaticTiedOneWayThermal.option_specs[1],
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
        """Get or set the Set ID for SURFA.  If SURFA > 0, a part set is assumed; see *SET_‌PART.  If SURFA < 0, a node set with ID equal to the absolute value of SURFA is assumed; see *SET_‌NODE. For nonsymmetric contact, this surface is the tracked surface.
        """ # nopep8
        return self._cards[0].get_value("surfa")

    @surfa.setter
    def surfa(self, value: int) -> None:
        self._cards[0].set_value("surfa", value)

    @property
    def surfb(self) -> typing.Optional[int]:
        """Get or set the Set ID to define the SURFB surface.  If SURFB > 0, a part set is assumed; see *SET_‌PART.  If SURFB < 0, a node set with ID equal to the absolute value of SURFB is assumed; see *SET_‌NODE.  Do not define for single surface contact. For nonsymmetric contact, this surface is the reference surface.
        """ # nopep8
        return self._cards[0].get_value("surfb")

    @surfb.setter
    def surfb(self, value: int) -> None:
        self._cards[0].set_value("surfb", value)

    @property
    def sfact(self) -> float:
        """Get or set the Scale factor for the penalty force stiffness (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sfact")

    @sfact.setter
    def sfact(self, value: float) -> None:
        self._cards[0].set_value("sfact", value)

    @property
    def freq(self) -> int:
        """Get or set the Search frequency. The number of time steps between bucket sorts (default=50).
        """ # nopep8
        return self._cards[0].get_value("freq")

    @freq.setter
    def freq(self, value: int) -> None:
        self._cards[0].set_value("freq", value)

    @property
    def fs(self) -> float:
        """Get or set the Static coefficient of friction (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[0].set_value("fs", value)

    @property
    def fd(self) -> float:
        """Get or set the Dynamic coefficient of friction (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[0].set_value("fd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[0].set_value("dc", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time for contact (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[1].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for contact (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[1].set_value("tdeath", value)

    @property
    def soa(self) -> float:
        """Get or set the Surface offset from midline for 2D shells of SURFA surface:
        GT.0.0: scale factor applied to actual thickness,
        LT.0.0: absolute value is used as the offset.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("soa")

    @soa.setter
    def soa(self, value: float) -> None:
        self._cards[1].set_value("soa", value)

    @property
    def sob(self) -> float:
        """Get or set the Surface offset from midline for 2D shells of SURFB surface:
        GT.0.0: scale factor applied to actual thickness,
        LT.0.0: absolute value is used as the offset.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("sob")

    @sob.setter
    def sob(self, value: float) -> None:
        self._cards[1].set_value("sob", value)

    @property
    def nda(self) -> int:
        """Get or set the Normal direction flag for 2D shells of SURFA surface:
        EQ.0: Normal direction is determined automatically (default),
        EQ.1: Normal direction is in the positive direction,
        EQ.-1: Normal direction is in the negative direction.
        """ # nopep8
        return self._cards[1].get_value("nda")

    @nda.setter
    def nda(self, value: int) -> None:
        if value not in [0, 1, -1]:
            raise Exception("""nda must be one of {0,1,-1}""")
        self._cards[1].set_value("nda", value)

    @property
    def ndb(self) -> int:
        """Get or set the Normal direction flag for 2D shells of SURFB surface:
        EQ.0: Normal direction is determined automatically (default),
        EQ.1: Normal direction is in the positive direction,
        EQ.-1: Normal direction is in the negative direction.
        """ # nopep8
        return self._cards[1].get_value("ndb")

    @ndb.setter
    def ndb(self, value: int) -> None:
        if value not in [0, 1, -1]:
            raise Exception("""ndb must be one of {0,1,-1}""")
        self._cards[1].set_value("ndb", value)

    @property
    def cof(self) -> int:
        """Get or set the COF: Closing/opening flag for implicit analysis.
        EQ.0: Recommended for most problems where gaps are only closing (default),
        EQ.1: Recommended when gaps are opening to avoid sticking.
        """ # nopep8
        return self._cards[1].get_value("cof")

    @cof.setter
    def cof(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cof must be one of {0,1}""")
        self._cards[1].set_value("cof", value)

    @property
    def init(self) -> int:
        """Get or set the Special processing during initialization.
        EQ.0: No special processing,
        EQ.1: Forming option.
        """ # nopep8
        return self._cards[1].get_value("init")

    @init.setter
    def init(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""init must be one of {0,1}""")
        self._cards[1].set_value("init", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity (k) of fluid between the slide surfaces. If a gap with a thickness l-gap exists between the slide surfaces, then the conductance due to thermal conductivity between the slide surfaces is
        h-cond = k/l-gap
        Note: LS- DYNA calculates l-gap based on deformation.
        """ # nopep8
        return self._cards[2].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[2].set_value("k", value)

    @property
    def rad(self) -> typing.Optional[float]:
        """Get or set the Radiation factor (f) between the slide surfaces. A radient-heat-transfer coefficient (h-rad) is calculated (see *BOUNDARY_RADIATION). If a gap exists between the slide surfaces, then the contact conductance is calculated by
        h = h-cond + h-rad.
        """ # nopep8
        return self._cards[2].get_value("rad")

    @rad.setter
    def rad(self, value: float) -> None:
        self._cards[2].set_value("rad", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Heat transfer conductance (h-cont) for closed gaps. Use this heat transfer conductance for gaps in the range
        0 <= l-gap <= l-min
        where l-min is defined below.
        """ # nopep8
        return self._cards[2].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        self._cards[2].set_value("h", value)

    @property
    def lmin(self) -> typing.Optional[float]:
        """Get or set the Critical gap (l-min), use the heat transfer conductance defined (HTC) for gap thicknesses less than this value.
        """ # nopep8
        return self._cards[2].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        self._cards[2].set_value("lmin", value)

    @property
    def lmax(self) -> typing.Optional[float]:
        """Get or set the No thermal contact if gap is greater than this value (l-max).
        """ # nopep8
        return self._cards[2].get_value("lmax")

    @lmax.setter
    def lmax(self, value: float) -> None:
        self._cards[2].set_value("lmax", value)

    @property
    def chlm(self) -> float:
        """Get or set the Is a multiplier used on the element characteristic distance for the search routine. The characteristic length is the largest interface surface element diagonal.
        EQ.0.0: Default is set to 1.0.
        """ # nopep8
        return self._cards[2].get_value("chlm")

    @chlm.setter
    def chlm(self, value: float) -> None:
        self._cards[2].set_value("chlm", value)

    @property
    def bc_flag(self) -> int:
        """Get or set the Thermal boundary condition flag:
        EQ.0: thermal boundary conditions are ON when parts are in contact
        EQ.1: thermal boundary conditions are OFF when parts are in contact.
        """ # nopep8
        return self._cards[2].get_value("bc_flag")

    @bc_flag.setter
    def bc_flag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""bc_flag must be one of {0,1}""")
        self._cards[2].set_value("bc_flag", value)

    @property
    def vc(self) -> float:
        """Get or set the Coefficient for viscous friction. This is used to limit the friction force to a maximum.
        """ # nopep8
        return self._cards[3].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[3].set_value("vc", value)

    @property
    def vdc(self) -> float:
        """Get or set the Viscous damping coefficient in percent of critical for explicit contact.
        """ # nopep8
        return self._cards[3].get_value("vdc")

    @vdc.setter
    def vdc(self, value: float) -> None:
        self._cards[3].set_value("vdc", value)

    @property
    def ipf(self) -> int:
        """Get or set the Initial penetration flag for explicit contact.
        EQ.0: Allow initial penetrations to remain
        EQ.1: Push apart initially penetrated surfaces.
        """ # nopep8
        return self._cards[3].get_value("ipf")

    @ipf.setter
    def ipf(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ipf must be one of {0,1}""")
        self._cards[3].set_value("ipf", value)

    @property
    def slide(self) -> int:
        """Get or set the Sliding option.
        EQ:0. Off.
        EQ.1: On.
        """ # nopep8
        return self._cards[3].get_value("slide")

    @slide.setter
    def slide(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""slide must be one of {0,1}""")
        self._cards[3].set_value("slide", value)

    @property
    def istiff(self) -> int:
        """Get or set the Stiffness scaling option.
        EQ.0: Use default option.
        EQ.1: Scale stiffness using segment masses and explicit time step (default for explicit contact).
        EQ.2: Scale stiffness using segment stiffness and dimensions (default for implicit contact)
        """ # nopep8
        return self._cards[3].get_value("istiff")

    @istiff.setter
    def istiff(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""istiff must be one of {0,1,2}""")
        self._cards[3].set_value("istiff", value)

    @property
    def tiedgap(self) -> float:
        """Get or set the Search gap for tied contacts.
        EQ.0: Default, use 1% of the SURFB segment length
        GT.0: Use the input value
        LT.0: Use n% of the SURFB segment length where n=|TIEDGAP|.
        """ # nopep8
        return self._cards[3].get_value("tiedgap")

    @tiedgap.setter
    def tiedgap(self, value: float) -> None:
        self._cards[3].set_value("tiedgap", value)

    @property
    def igapcl(self) -> int:
        """Get or set the Flag to close gaps in tied contact:
        EQ.0: Default, allow gaps to remain
        EQ.1: Move SURFA nodes to SURFB segment to close gaps.
        """ # nopep8
        return self._cards[3].get_value("igapcl")

    @igapcl.setter
    def igapcl(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""igapcl must be one of {0,1}""")
        self._cards[3].set_value("igapcl", value)

    @property
    def tietyp(self) -> int:
        """Get or set the Flag to control constraint type of tied contact:
        EQ.0: Default, use kinematic constraints when possible
        EQ.1: Use only penalty type constraints.
        """ # nopep8
        return self._cards[3].get_value("tietyp")

    @tietyp.setter
    def tietyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tietyp must be one of {0,1}""")
        self._cards[3].set_value("tietyp", value)

    @property
    def sldsoa(self) -> float:
        """Get or set the Solid surface offset for the SURFA surface.
        """ # nopep8
        return self._cards[4].get_value("sldsoa")

    @sldsoa.setter
    def sldsoa(self, value: float) -> None:
        self._cards[4].set_value("sldsoa", value)

    @property
    def sldsob(self) -> float:
        """Get or set the Solid surface offset for the SURFB surface.
        """ # nopep8
        return self._cards[4].get_value("sldsob")

    @sldsob.setter
    def sldsob(self, value: float) -> None:
        self._cards[4].set_value("sldsob", value)

    @property
    def tdpen(self) -> float:
        """Get or set the Time span of penetration removal for 2D Mortar contacts.
        Each initial penetration will be gradually reduced linearly in time, so that it is removed by time TDPEN.
        This is the interference option analogue to MPAR1 for IGNORE = 3 in 3D automatic Mortar contacts.
        """ # nopep8
        return self._cards[4].get_value("tdpen")

    @tdpen.setter
    def tdpen(self, value: float) -> None:
        self._cards[4].set_value("tdpen", value)

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

