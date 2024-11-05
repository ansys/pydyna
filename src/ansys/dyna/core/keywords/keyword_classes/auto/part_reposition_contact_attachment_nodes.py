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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class PartRepositionContactAttachmentNodes(KeywordBase):
    """DYNA PART_REPOSITION_CONTACT_ATTACHMENT_NODES keyword"""

    keyword = "PART"
    subkeyword = "REPOSITION_CONTACT_ATTACHMENT_NODES"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
                        "secid",
                        int,
                        10,
                        10,
                        kwargs.get("secid")
                    ),
                    Field(
                        "mid",
                        int,
                        20,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "eosid",
                        int,
                        30,
                        10,
                        kwargs.get("eosid", 0)
                    ),
                    Field(
                        "hgid",
                        int,
                        40,
                        10,
                        kwargs.get("hgid", 0)
                    ),
                    Field(
                        "grav",
                        int,
                        50,
                        10,
                        kwargs.get("grav", 0)
                    ),
                    Field(
                        "adpopt",
                        int,
                        60,
                        10,
                        kwargs.get("adpopt")
                    ),
                    Field(
                        "tmid",
                        int,
                        70,
                        10,
                        kwargs.get("tmid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cmsn",
                        int,
                        0,
                        10,
                        kwargs.get("cmsn")
                    ),
                    Field(
                        "mdep",
                        int,
                        10,
                        10,
                        kwargs.get("mdep", 0)
                    ),
                    Field(
                        "movopt",
                        int,
                        20,
                        10,
                        kwargs.get("movopt", 0)
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
                        "vc",
                        float,
                        30,
                        10,
                        kwargs.get("vc")
                    ),
                    Field(
                        "optt",
                        float,
                        40,
                        10,
                        kwargs.get("optt")
                    ),
                    Field(
                        "sft",
                        float,
                        50,
                        10,
                        kwargs.get("sft")
                    ),
                    Field(
                        "ssf",
                        float,
                        60,
                        10,
                        kwargs.get("ssf")
                    ),
                    Field(
                        "cparm8",
                        float,
                        70,
                        10,
                        kwargs.get("cparm8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ansid",
                        int,
                        0,
                        10,
                        kwargs.get("ansid", 0)
                    ),
                ],
            ),
        ]

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Heading for the part.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID defined in *SECTION section.
        """ # nopep8
        return self._cards[1].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        self._cards[1].set_value("secid", value)

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT section.
        """ # nopep8
        return self._cards[1].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[1].set_value("mid", value)

    @property
    def eosid(self) -> int:
        """Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
        """ # nopep8
        return self._cards[1].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[1].set_value("eosid", value)

    @property
    def hgid(self) -> int:
        """Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
        EQ.0: default values are used.
        """ # nopep8
        return self._cards[1].get_value("hgid")

    @hgid.setter
    def hgid(self, value: int) -> None:
        self._cards[1].set_value("hgid", value)

    @property
    def grav(self) -> int:
        """Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
        EQ.0: all parts initialized,
        EQ.1: only current material initialized.
        """ # nopep8
        return self._cards[1].get_value("grav")

    @grav.setter
    def grav(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""grav must be one of {0,1}""")
        self._cards[1].set_value("grav", value)

    @property
    def adpopt(self) -> typing.Optional[int]:
        """Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
        LT.0: R-adaptive remeshing for 2-D solids, |ADPOPT| gives the load curve ID that defines the element size as a function of time.
        EQ.0:Adaptive remeshing is inactive for this part ID.
        EQ.1:	h - adaptive for 3D shells and for shell / solid / shell sandwich composites.
        EQ.2 : r - adaptive remeshing for 2D solids, 3D tetrahedrons and 3D EFG.
        EQ.3 : Axisymmetric r - adaptive remeshing for 3D solid(see Remark 6).
        EQ.9 : Passive h - adaptive for 3D shells.The elements in this part will not be split unless their neighboring elements in other parts need to be split more than one level.
        """ # nopep8
        return self._cards[1].get_value("adpopt")

    @adpopt.setter
    def adpopt(self, value: int) -> None:
        self._cards[1].set_value("adpopt", value)

    @property
    def tmid(self) -> int:
        """Get or set the Thermal material property identication defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
        EQ.0: defaults to MID.
        """ # nopep8
        return self._cards[1].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        self._cards[1].set_value("tmid", value)

    @property
    def cmsn(self) -> typing.Optional[int]:
        """Get or set the CAL3D segment number/MADYMO system number. See the numbering in the corresponding program.
        """ # nopep8
        return self._cards[2].get_value("cmsn")

    @cmsn.setter
    def cmsn(self, value: int) -> None:
        self._cards[2].set_value("cmsn", value)

    @property
    def mdep(self) -> int:
        """Get or set the MADYMO ellipse/plane number:
        GT.0: ellipse number,
        EQ.0: default,
        LT.0: absolute value is plane number.
        """ # nopep8
        return self._cards[2].get_value("mdep")

    @mdep.setter
    def mdep(self, value: int) -> None:
        self._cards[2].set_value("mdep", value)

    @property
    def movopt(self) -> int:
        """Get or set the Flag to deactivate moving for merged rigid bodies, see *CONSTRAINED_RIGID_BODIES. This option allows a merged rigid body to be fixed in space while the nodes and elements of the generated CAL3D/MADYMO parts are repositioned:
        EQ.0: merged rigid body is repositioned (default),
        EQ.1: merged rigid body is not repositioned.
        """ # nopep8
        return self._cards[2].get_value("movopt")

    @movopt.setter
    def movopt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""movopt must be one of {0,1}""")
        self._cards[2].set_value("movopt", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Static coefficient of friction.
        """ # nopep8
        return self._cards[3].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        self._cards[3].set_value("fs", value)

    @property
    def fd(self) -> typing.Optional[float]:
        """Get or set the Dynamic coefficient of friction.
        """ # nopep8
        return self._cards[3].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        self._cards[3].set_value("fd", value)

    @property
    def dc(self) -> typing.Optional[float]:
        """Get or set the Exponential decay coefficient.
        """ # nopep8
        return self._cards[3].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        self._cards[3].set_value("dc", value)

    @property
    def vc(self) -> typing.Optional[float]:
        """Get or set the Viscous friction coefficient.
        """ # nopep8
        return self._cards[3].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[3].set_value("vc", value)

    @property
    def optt(self) -> typing.Optional[float]:
        """Get or set the Optional contact thickness. For SOFT = 2, it applies to solids, shells and beams. For SOFT = 0 and 1 and for Mortar contacts, it applies to shells and beams only. For SOFT = 0 and 1 with the MPP version, OPTT has a different meaning for solid elements. In this case, OPTT overrides the thickness of solid elements used for the calculation of the contact penetration release (see Table Error! Reference source not found.), but it does not affect the contact thickness
        """ # nopep8
        return self._cards[3].get_value("optt")

    @optt.setter
    def optt(self, value: float) -> None:
        self._cards[3].set_value("optt", value)

    @property
    def sft(self) -> typing.Optional[float]:
        """Get or set the Optional thickness scale factor for PART ID in automatic contact (scales true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements.
        """ # nopep8
        return self._cards[3].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        self._cards[3].set_value("sft", value)

    @property
    def ssf(self) -> typing.Optional[float]:
        """Get or set the Scale factor on default slave penalty stiffness for this PART ID whenever it appears in the contact definition. If zero, SSF is taken as unity.
        """ # nopep8
        return self._cards[3].get_value("ssf")

    @ssf.setter
    def ssf(self, value: float) -> None:
        self._cards[3].set_value("ssf", value)

    @property
    def cparm8(self) -> typing.Optional[float]:
        """Get or set the Flag to exclude beam-to-beam contact from the same PID for CONTACT_‌AUTOMATIC_‌GENERAL.  This applies only to MPP.  Global default may be set using CPARM8 on *CONTACT_‌…_MPP Optional Card.
        EQ.0:	Flag is not set(default).
        EQ.1 : Flag is set.
        EQ.2 : Flag is set.CPARM8 = 2 has the additional effect of permitting contact treatment of spot weld(type 9) beams in AUTOMATIC_‌GENERAL contacts; spot weld beams are otherwise disregarded entirely by AUTOMATIC_‌GENERAL contacts.
        """ # nopep8
        return self._cards[3].get_value("cparm8")

    @cparm8.setter
    def cparm8(self, value: float) -> None:
        self._cards[3].set_value("cparm8", value)

    @property
    def ansid(self) -> int:
        """Get or set the Attachment node set ID. This option should be used very cautiously and applies only to rigid bodies. The attachment point nodes are updated each cycle whereas other nodes in the rigid body are updated only in the output databases. All loads seen by the rigid body must be applied through this nodal subset or directly to the center of gravity of the rigid body. If the rigid body is in contact this set must include all interacting nodes.
        EQ.0: All nodal updates are skipped for this rigid body. The null option can be used if the rigid body is fixed in space or if the rigid body does not interact with other parts, e.g., the rigid body is only used for some visual purpose (default).
        """ # nopep8
        return self._cards[4].get_value("ansid")

    @ansid.setter
    def ansid(self, value: int) -> None:
        self._cards[4].set_value("ansid", value)

