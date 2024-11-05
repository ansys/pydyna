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

class ContactSpg(KeywordBase):
    """DYNA CONTACT_SPG keyword"""

    keyword = "CONTACT"
    subkeyword = "SPG"
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
                        "pid1",
                        int,
                        0,
                        10,
                        kwargs.get("pid1")
                    ),
                    Field(
                        "pid2",
                        int,
                        10,
                        10,
                        kwargs.get("pid2")
                    ),
                    Field(
                        "pid3",
                        int,
                        20,
                        10,
                        kwargs.get("pid3")
                    ),
                    Field(
                        "pid4",
                        int,
                        30,
                        10,
                        kwargs.get("pid4")
                    ),
                    Field(
                        "pid5",
                        int,
                        40,
                        10,
                        kwargs.get("pid5")
                    ),
                    Field(
                        "pid6",
                        int,
                        50,
                        10,
                        kwargs.get("pid6")
                    ),
                    Field(
                        "pid7",
                        int,
                        60,
                        10,
                        kwargs.get("pid7")
                    ),
                    Field(
                        "pid8",
                        int,
                        70,
                        10,
                        kwargs.get("pid8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iself1",
                        int,
                        0,
                        10,
                        kwargs.get("iself1")
                    ),
                    Field(
                        "iself2",
                        int,
                        10,
                        10,
                        kwargs.get("iself2")
                    ),
                    Field(
                        "iself3",
                        int,
                        20,
                        10,
                        kwargs.get("iself3")
                    ),
                    Field(
                        "iself4",
                        int,
                        30,
                        10,
                        kwargs.get("iself4")
                    ),
                    Field(
                        "iself5",
                        int,
                        40,
                        10,
                        kwargs.get("iself5")
                    ),
                    Field(
                        "iself6",
                        int,
                        50,
                        10,
                        kwargs.get("iself6")
                    ),
                    Field(
                        "iself7",
                        int,
                        60,
                        10,
                        kwargs.get("iself7")
                    ),
                    Field(
                        "iself8",
                        int,
                        70,
                        10,
                        kwargs.get("iself8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pfac1",
                        float,
                        0,
                        10,
                        kwargs.get("pfac1")
                    ),
                    Field(
                        "pfac2",
                        float,
                        10,
                        10,
                        kwargs.get("pfac2")
                    ),
                    Field(
                        "pfac3",
                        float,
                        20,
                        10,
                        kwargs.get("pfac3")
                    ),
                    Field(
                        "pfac4",
                        float,
                        30,
                        10,
                        kwargs.get("pfac4")
                    ),
                    Field(
                        "pfac5",
                        float,
                        40,
                        10,
                        kwargs.get("pfac5")
                    ),
                    Field(
                        "pfac6",
                        float,
                        50,
                        10,
                        kwargs.get("pfac6")
                    ),
                    Field(
                        "pfac7",
                        float,
                        60,
                        10,
                        kwargs.get("pfac7")
                    ),
                    Field(
                        "pfac8",
                        float,
                        70,
                        10,
                        kwargs.get("pfac8")
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
                        kwargs.get("fs")
                    ),
                    Field(
                        "fd",
                        float,
                        10,
                        10,
                        kwargs.get("fd")
                    ),
                    Field(
                        "dc",
                        float,
                        20,
                        10,
                        kwargs.get("dc")
                    ),
                    Field(
                        "nfreq",
                        float,
                        30,
                        10,
                        kwargs.get("nfreq")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = ContactSpg.option_specs[0],
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
                option_spec = ContactSpg.option_specs[1],
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
    def pid1(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        self._cards[0].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        self._cards[0].set_value("pid2", value)

    @property
    def pid3(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid3")

    @pid3.setter
    def pid3(self, value: int) -> None:
        self._cards[0].set_value("pid3", value)

    @property
    def pid4(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid4")

    @pid4.setter
    def pid4(self, value: int) -> None:
        self._cards[0].set_value("pid4", value)

    @property
    def pid5(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid5")

    @pid5.setter
    def pid5(self, value: int) -> None:
        self._cards[0].set_value("pid5", value)

    @property
    def pid6(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid6")

    @pid6.setter
    def pid6(self, value: int) -> None:
        self._cards[0].set_value("pid6", value)

    @property
    def pid7(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid7")

    @pid7.setter
    def pid7(self, value: int) -> None:
        self._cards[0].set_value("pid7", value)

    @property
    def pid8(self) -> typing.Optional[int]:
        """Get or set the Part IDs of SPG parts involved in particle contact
        """ # nopep8
        return self._cards[0].get_value("pid8")

    @pid8.setter
    def pid8(self, value: int) -> None:
        self._cards[0].set_value("pid8", value)

    @property
    def iself1(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself1")

    @iself1.setter
    def iself1(self, value: int) -> None:
        self._cards[1].set_value("iself1", value)

    @property
    def iself2(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself2")

    @iself2.setter
    def iself2(self, value: int) -> None:
        self._cards[1].set_value("iself2", value)

    @property
    def iself3(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself3")

    @iself3.setter
    def iself3(self, value: int) -> None:
        self._cards[1].set_value("iself3", value)

    @property
    def iself4(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself4")

    @iself4.setter
    def iself4(self, value: int) -> None:
        self._cards[1].set_value("iself4", value)

    @property
    def iself5(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself5")

    @iself5.setter
    def iself5(self, value: int) -> None:
        self._cards[1].set_value("iself5", value)

    @property
    def iself6(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself6")

    @iself6.setter
    def iself6(self, value: int) -> None:
        self._cards[1].set_value("iself6", value)

    @property
    def iself7(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself7")

    @iself7.setter
    def iself7(self, value: int) -> None:
        self._cards[1].set_value("iself7", value)

    @property
    def iself8(self) -> typing.Optional[int]:
        """Get or set the Self-contact indicators
        = 0: no self - contact
        = 1 : self - contact is defined for the corresponding part
        """ # nopep8
        return self._cards[1].get_value("iself8")

    @iself8.setter
    def iself8(self, value: int) -> None:
        self._cards[1].set_value("iself8", value)

    @property
    def pfac1(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac1")

    @pfac1.setter
    def pfac1(self, value: float) -> None:
        self._cards[2].set_value("pfac1", value)

    @property
    def pfac2(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac2")

    @pfac2.setter
    def pfac2(self, value: float) -> None:
        self._cards[2].set_value("pfac2", value)

    @property
    def pfac3(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac3")

    @pfac3.setter
    def pfac3(self, value: float) -> None:
        self._cards[2].set_value("pfac3", value)

    @property
    def pfac4(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac4")

    @pfac4.setter
    def pfac4(self, value: float) -> None:
        self._cards[2].set_value("pfac4", value)

    @property
    def pfac5(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac5")

    @pfac5.setter
    def pfac5(self, value: float) -> None:
        self._cards[2].set_value("pfac5", value)

    @property
    def pfac6(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac6")

    @pfac6.setter
    def pfac6(self, value: float) -> None:
        self._cards[2].set_value("pfac6", value)

    @property
    def pfac7(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac7")

    @pfac7.setter
    def pfac7(self, value: float) -> None:
        self._cards[2].set_value("pfac7", value)

    @property
    def pfac8(self) -> typing.Optional[float]:
        """Get or set the Penalty factors
        """ # nopep8
        return self._cards[2].get_value("pfac8")

    @pfac8.setter
    def pfac8(self, value: float) -> None:
        self._cards[2].set_value("pfac8", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Static coefficient of friction
        """ # nopep8
        return self._cards[3].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[3].set_value("fs", value)

    @property
    def fd(self) -> typing.Optional[float]:
        """Get or set the Dynamic coefficient of friction
        """ # nopep8
        return self._cards[3].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[3].set_value("fd", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity, v_rel , of the surfaces in contact
        """ # nopep8
        return self._cards[3].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[3].set_value("dc", value)

    @property
    def nfreq(self) -> typing.Optional[float]:
        """Get or set the Contact searching frequency
        """ # nopep8
        return self._cards[3].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: float) -> None:
        self._cards[3].set_value("nfreq", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[4].cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[4].cards[0].set_value("cid", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Interface descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[4].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        self._cards[4].cards[0].set_value("heading", value)

    @property
    def ignore(self) -> int:
        """Get or set the By setting this variable to 1, the "ignore initial penetrations" option is turned on for this contact.  Alternatively, this option may be turned on by setting IGNORE = 1 on Card 4 of *CONTROL_CONTACT or on Optional Card C of *CONTACT.  In other words, if IGNORE is set to 1 in any of three places, initial penetrations are tracked.
        """ # nopep8
        return self._cards[5].cards[0].get_value("ignore")

    @ignore.setter
    def ignore(self, value: int) -> None:
        self._cards[5].cards[0].set_value("ignore", value)

    @property
    def bckt(self) -> int:
        """Get or set the Bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts. For these two exceptions, the BSORT option on Optional Card A applies instead.
        """ # nopep8
        return self._cards[5].cards[0].get_value("bckt")

    @bckt.setter
    def bckt(self, value: int) -> None:
        self._cards[5].cards[0].set_value("bckt", value)

    @property
    def lcbckt(self) -> typing.Optional[int]:
        """Get or set the Load curve for bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts.  For the two exceptions, the negative BSORT option on Optional Card A applies instead.
        """ # nopep8
        return self._cards[5].cards[0].get_value("lcbckt")

    @lcbckt.setter
    def lcbckt(self, value: int) -> None:
        self._cards[5].cards[0].set_value("lcbckt", value)

    @property
    def ns2trk(self) -> int:
        """Get or set the Number of potential contacts to track for each tracked node.  The normal input for this (DEPTH on Optional Card A) is ignored..
        """ # nopep8
        return self._cards[5].cards[0].get_value("ns2trk")

    @ns2trk.setter
    def ns2trk(self, value: int) -> None:
        self._cards[5].cards[0].set_value("ns2trk", value)

    @property
    def inititr(self) -> int:
        """Get or set the Number of iterations to perform when trying to eliminate initial penetrations.  Note that an input of 0 means 0, not the default value (which is 2).  Leaving this field blank will set INITITR to 2.
        """ # nopep8
        return self._cards[5].cards[0].get_value("inititr")

    @inititr.setter
    def inititr(self, value: int) -> None:
        self._cards[5].cards[0].set_value("inititr", value)

    @property
    def parmax(self) -> float:
        """Get or set the The parametric extension distance for contact segments.  The MAXPAR parameter on Optional Card A is not used for MPP.  For non-tied contacts, the default is 1.0005. For tied contacts the default is 1.035 and, the actual extension used is computed as follows: see the manual
        """ # nopep8
        return self._cards[5].cards[0].get_value("parmax")

    @parmax.setter
    def parmax(self, value: float) -> None:
        self._cards[5].cards[0].set_value("parmax", value)

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
        return self._cards[5].cards[0].get_value("cparm8")

    @cparm8.setter
    def cparm8(self, value: int) -> None:
        if value not in [0, 1, 2, 10, 11, 12]:
            raise Exception("""cparm8 must be one of {0,1,2,10,11,12}""")
        self._cards[5].cards[0].set_value("cparm8", value)

    @property
    def mpp2(self) -> bool:
        """Get or set the Flag whether this is the MPP card.
        """ # nopep8
        return self._cards[5].cards[1].get_value("mpp2")

    @mpp2.setter
    def mpp2(self, value: bool) -> None:
        self._cards[5].cards[1].set_value("mpp2", value)

    @property
    def chksegs(self) -> int:
        """Get or set the If this value is non-zero, then for the node-to-surface and surface-to-surface contacts LS-DYNA performs a special check at time 0 for elements that are inverted (or nearly so), These elements are removed from contact.  These poorly formed elements have been known to occur on the tooling in metalforming problems, which allows these problems to run.  It should not normally be needed for reasonable meshes.
        """ # nopep8
        return self._cards[5].cards[1].get_value("chksegs")

    @chksegs.setter
    def chksegs(self, value: int) -> None:
        self._cards[5].cards[1].set_value("chksegs", value)

    @property
    def pensf(self) -> float:
        """Get or set the This option is used together with IGNORE for 3D forging problems.  If non-zero, the IGNORE penetration distance is multiplied by this value each cycle, effectively pushing the tracked node back out to the surface.  This is useful for nodes that might get generated below the reference surface during 3D remeshing.  Care should be exercised, as energy may be generated and stability may be effected for values lower than 0.95.  A value in the range of 0.98 to 0.99 or higher (but < 1.0) is recommended
        """ # nopep8
        return self._cards[5].cards[1].get_value("pensf")

    @pensf.setter
    def pensf(self, value: float) -> None:
        self._cards[5].cards[1].set_value("pensf", value)

    @property
    def grpable(self) -> int:
        """Get or set the Set to 1 to invoke an alternate MPP communication algorithm for various SINGLE_SURFACE (including AUTOMATIC_GEN-ERAL), NODES_TO_SURFACE, SURFACE_TO_SURFACE, ERODING and SOFT = 2 contacts.  This groupable algorithm does not support all contact options, including MORTAR. It is still under development.  It can be significantly faster and scale better than the normal algorithm when there are more than two or three applicable contact types defined in the model. It is intended for speeding up the contact processing without changing the behavior of the contact.  See also *CONTROL_MPP_-CONTACT_GROUPABLE.
        """ # nopep8
        return self._cards[5].cards[1].get_value("grpable")

    @grpable.setter
    def grpable(self, value: int) -> None:
        self._cards[5].cards[1].set_value("grpable", value)

