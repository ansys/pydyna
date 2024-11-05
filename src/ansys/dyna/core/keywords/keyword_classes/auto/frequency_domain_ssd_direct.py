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

class FrequencyDomainSsdDirect(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_SSD_DIRECT keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_SSD_DIRECT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mdmin",
                        int,
                        0,
                        10,
                        kwargs.get("mdmin", 1)
                    ),
                    Field(
                        "mdmax",
                        int,
                        10,
                        10,
                        kwargs.get("mdmax")
                    ),
                    Field(
                        "fnmin",
                        float,
                        20,
                        10,
                        kwargs.get("fnmin", 0.0)
                    ),
                    Field(
                        "fnmax",
                        float,
                        30,
                        10,
                        kwargs.get("fnmax")
                    ),
                    Field(
                        "restmd",
                        int,
                        40,
                        10,
                        kwargs.get("restmd", 0)
                    ),
                    Field(
                        "restdp",
                        int,
                        50,
                        10,
                        kwargs.get("restdp", 0)
                    ),
                    Field(
                        "lcflag",
                        int,
                        60,
                        10,
                        kwargs.get("lcflag", 0)
                    ),
                    Field(
                        "relatv",
                        int,
                        70,
                        10,
                        kwargs.get("relatv", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dampf",
                        float,
                        0,
                        10,
                        kwargs.get("dampf", 0.0)
                    ),
                    Field(
                        "lcdam",
                        int,
                        10,
                        10,
                        kwargs.get("lcdam", 0)
                    ),
                    Field(
                        "lctyp",
                        int,
                        20,
                        10,
                        kwargs.get("lctyp", 0)
                    ),
                    Field(
                        "dmpmas",
                        float,
                        30,
                        10,
                        kwargs.get("dmpmas", 0.0)
                    ),
                    Field(
                        "dmpstf",
                        float,
                        40,
                        10,
                        kwargs.get("dmpstf", 0.0)
                    ),
                    Field(
                        "dmpflg",
                        int,
                        50,
                        10,
                        kwargs.get("dmpflg", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "istress",
                        int,
                        10,
                        10,
                        kwargs.get("istress")
                    ),
                    Field(
                        "memory",
                        int,
                        20,
                        10,
                        kwargs.get("memory", 0)
                    ),
                    Field(
                        "nerp",
                        int,
                        30,
                        10,
                        kwargs.get("nerp", 0)
                    ),
                    Field(
                        "strtyp",
                        int,
                        40,
                        10,
                        kwargs.get("strtyp", 0)
                    ),
                    Field(
                        "nout",
                        int,
                        50,
                        10,
                        kwargs.get("nout", 0)
                    ),
                    Field(
                        "notyp",
                        int,
                        60,
                        10,
                        kwargs.get("notyp", 0)
                    ),
                    Field(
                        "nova",
                        int,
                        70,
                        10,
                        kwargs.get("nova", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "ntyp",
                        int,
                        10,
                        10,
                        kwargs.get("ntyp", 0)
                    ),
                    Field(
                        "dof",
                        int,
                        20,
                        10,
                        kwargs.get("dof", 1)
                    ),
                    Field(
                        "vad",
                        int,
                        30,
                        10,
                        kwargs.get("vad", 0)
                    ),
                    Field(
                        "lc1",
                        int,
                        40,
                        10,
                        kwargs.get("lc1")
                    ),
                    Field(
                        "lc2",
                        int,
                        50,
                        10,
                        kwargs.get("lc2")
                    ),
                    Field(
                        "sf",
                        float,
                        60,
                        10,
                        kwargs.get("sf", 0)
                    ),
                    Field(
                        "vid",
                        int,
                        70,
                        10,
                        kwargs.get("vid", 0)
                    ),
                ],
            ),
        ]

    @property
    def mdmin(self) -> int:
        """Get or set the The first mode in modal superposition method (optional).
        """ # nopep8
        return self._cards[0].get_value("mdmin")

    @mdmin.setter
    def mdmin(self, value: int) -> None:
        self._cards[0].set_value("mdmin", value)

    @property
    def mdmax(self) -> typing.Optional[int]:
        """Get or set the The last mode in modal superposition method (optional).
        """ # nopep8
        return self._cards[0].get_value("mdmax")

    @mdmax.setter
    def mdmax(self, value: int) -> None:
        self._cards[0].set_value("mdmax", value)

    @property
    def fnmin(self) -> float:
        """Get or set the The minimum natural frequency in modal superposition method(optional).
        """ # nopep8
        return self._cards[0].get_value("fnmin")

    @fnmin.setter
    def fnmin(self, value: float) -> None:
        self._cards[0].set_value("fnmin", value)

    @property
    def fnmax(self) -> typing.Optional[float]:
        """Get or set the The maximum natural frequency in modal superposition method(optional).
        """ # nopep8
        return self._cards[0].get_value("fnmax")

    @fnmax.setter
    def fnmax(self, value: float) -> None:
        self._cards[0].set_value("fnmax", value)

    @property
    def restmd(self) -> int:
        """Get or set the Restart option.
        EQ.0: A new modal analysis is performed,
        EQ.1: Restart with d3eigv,
        EQ.2: Restart with "modeshp" binary scratch file.
        """ # nopep8
        return self._cards[0].get_value("restmd")

    @restmd.setter
    def restmd(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""restmd must be one of {0,1,2}""")
        self._cards[0].set_value("restmd", value)

    @property
    def restdp(self) -> int:
        """Get or set the Restart option.
        EQ.0: A new run without dumpssd,
        EQ.1: Restart with dumpssd.
        """ # nopep8
        return self._cards[0].get_value("restdp")

    @restdp.setter
    def restdp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""restdp must be one of {0,1}""")
        self._cards[0].set_value("restdp", value)

    @property
    def lcflag(self) -> int:
        """Get or set the Load Curve definition flag.
        EQ.0: load curves are given as amplitude / phase angle,
        EQ.1: load curves are given as real / imaginary components.
        """ # nopep8
        return self._cards[0].get_value("lcflag")

    @lcflag.setter
    def lcflag(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""lcflag must be one of {0,1}""")
        self._cards[0].set_value("lcflag", value)

    @property
    def relatv(self) -> int:
        """Get or set the Flag for displacement, velocity and acceleration results:
        EQ.0: absolute values are requested,
        EQ.1: relative values are requested (for VAD = 2, 3 and 4 only).
        """ # nopep8
        return self._cards[0].get_value("relatv")

    @relatv.setter
    def relatv(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""relatv must be one of {0,1}""")
        self._cards[0].set_value("relatv", value)

    @property
    def dampf(self) -> float:
        """Get or set the Modal damping coefficient, ζ.
        """ # nopep8
        return self._cards[1].get_value("dampf")

    @dampf.setter
    def dampf(self, value: float) -> None:
        self._cards[1].set_value("dampf", value)

    @property
    def lcdam(self) -> int:
        """Get or set the Load Curve ID defining mode dependent modal damping coefficient ζ.
        """ # nopep8
        return self._cards[1].get_value("lcdam")

    @lcdam.setter
    def lcdam(self, value: int) -> None:
        self._cards[1].set_value("lcdam", value)

    @property
    def lctyp(self) -> int:
        """Get or set the Type of load curve defining modal damping coefficient
        EQ.0: Abscissa value defines frequency,
        EQ.1: Abscissa value defines mode number.
        """ # nopep8
        return self._cards[1].get_value("lctyp")

    @lctyp.setter
    def lctyp(self, value: int) -> None:
        self._cards[1].set_value("lctyp", value)

    @property
    def dmpmas(self) -> float:
        """Get or set the Mass proportional damping constant α, in Rayleigh damping..
        """ # nopep8
        return self._cards[1].get_value("dmpmas")

    @dmpmas.setter
    def dmpmas(self, value: float) -> None:
        self._cards[1].set_value("dmpmas", value)

    @property
    def dmpstf(self) -> float:
        """Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
        """ # nopep8
        return self._cards[1].get_value("dmpstf")

    @dmpstf.setter
    def dmpstf(self, value: float) -> None:
        self._cards[1].set_value("dmpstf", value)

    @property
    def dmpflg(self) -> int:
        """Get or set the Damping flag:
        EQ.0: use modal damping coefficient ζ,defined by DAMPF, or LCDAM, or Rayleigh damping defined by DMPMAS and DMPSTF in this card.
        EQ.1: use damping defined by *DAMPING_PART_MASS and *DAMPING_PART_STIFFNESS.
        """ # nopep8
        return self._cards[1].get_value("dmpflg")

    @dmpflg.setter
    def dmpflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dmpflg must be one of {0,1}""")
        self._cards[1].set_value("dmpflg", value)

    @property
    def istress(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("istress")

    @istress.setter
    def istress(self, value: int) -> None:
        self._cards[2].set_value("istress", value)

    @property
    def memory(self) -> int:
        """Get or set the Memory flag:
        EQ.0: modal superposition will be performed in-core. This is helpful to speed up the simulation.
        EQ.1: modal superposition will be performed out-of-core. This is needed for some large scale problems which require huge memory (beyond the memory available).
        """ # nopep8
        return self._cards[2].get_value("memory")

    @memory.setter
    def memory(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""memory must be one of {0,1}""")
        self._cards[2].set_value("memory", value)

    @property
    def nerp(self) -> int:
        """Get or set the Number of ERP panels
        """ # nopep8
        return self._cards[2].get_value("nerp")

    @nerp.setter
    def nerp(self, value: int) -> None:
        self._cards[2].set_value("nerp", value)

    @property
    def strtyp(self) -> int:
        """Get or set the Stress used in fatigue analysis:
        EQ.0: Von Mises stress,
        EQ.1: Maximum principal stress,
        EQ.2: Maximum shear stress.
        """ # nopep8
        return self._cards[2].get_value("strtyp")

    @strtyp.setter
    def strtyp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""strtyp must be one of {0,1,2}""")
        self._cards[2].set_value("strtyp", value)

    @property
    def nout(self) -> int:
        """Get or set the Part, part set, segment set, or node set ID for response output (use with acoustic computation). See NOTYP below.
        """ # nopep8
        return self._cards[2].get_value("nout")

    @nout.setter
    def nout(self, value: int) -> None:
        self._cards[2].set_value("nout", value)

    @property
    def notyp(self) -> int:
        """Get or set the Type of NOUT:
        EQ.0: part set ID (not implemented),
        EQ.1: part ID (not implemented),
        EQ.2: segment set ID,
        EQ.3: node set ID
        EQ.-2: segment set ID which mismatches with acoustic boundary nodes. Mapping of velocity or acceleration to the acoustic boundary nodes is performed.
        """ # nopep8
        return self._cards[2].get_value("notyp")

    @notyp.setter
    def notyp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, -2]:
            raise Exception("""notyp must be one of {0,1,2,3,-2}""")
        self._cards[2].set_value("notyp", value)

    @property
    def nova(self) -> int:
        """Get or set the Response output type:
        EQ.0: velocity,
        EQ.1: acceleration.
        """ # nopep8
        return self._cards[2].get_value("nova")

    @nova.setter
    def nova(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""nova must be one of {0,1}""")
        self._cards[2].set_value("nova", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node, Node set,Segment set ID for excitation input.See NTYP below.
        """ # nopep8
        return self._cards[3].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[3].set_value("nid", value)

    @property
    def ntyp(self) -> int:
        """Get or set the Type of NID:
        EQ.0: node ID,
        EQ.1: node set ID,
        EQ.2: segment set ID.
        """ # nopep8
        return self._cards[3].get_value("ntyp")

    @ntyp.setter
    def ntyp(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ntyp must be one of {0,1,2}""")
        self._cards[3].set_value("ntyp", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom for excitation input(ignored if VAD=1).
        EQ. 1: x-translational degree-of-freedom x-rotational degree-of-freedom (for torque excitation, VAD=8),
        EQ. 2: y-translational degree-of-freedom or y-rotational degree-of-freedom (for torque excitation, VAD=8),
        EQ. 3: z-translational degree-of-freedom or z-rotational degree-of-freedom (for torque excitation, VAD=8),
        EQ. 4: translational movement in direction given by vector VID or rotational movement with axis given by vector VID (for torque excitation, VAD=8).
        """ # nopep8
        return self._cards[3].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""dof must be one of {1,2,3,4}""")
        self._cards[3].set_value("dof", value)

    @property
    def vad(self) -> int:
        """Get or set the Excitation input type:
        EQ.0: nodal force,
        EQ.1: pressure
        EQ.2: base velocity,
        EQ.3: base acceleration,
        EQ.4: base displacement,
        EQ.5: enforced velocity by large mass method (see remark 10),
        EQ.6: enforced acceleration by large mass method (see remark 10),
        EQ.7: enforced displacement by large mass method (see remark 10).
        EQ.8: torque.
        EQ.9: base angular velocity,
        EQ.10: base angular acceleration,
        EQ.11: base angular displacement.
        EQ.12: enforced velocity (for DIRECT type keyword options only)
        EQ.13: enforced acceleration (for DIRECT type keyword options only)
        EQ.14: enforced displacement (for DIRECT type keyword options only)
        """ # nopep8
        return self._cards[3].get_value("vad")

    @vad.setter
    def vad(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]:
            raise Exception("""vad must be one of {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14}""")
        self._cards[3].set_value("vad", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID defining amplitude (LCFLAG=0) or real (in-phase) part (LCFLAG=1) of load as a function of frequency
        """ # nopep8
        return self._cards[3].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        self._cards[3].set_value("lc1", value)

    @property
    def lc2(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID defining phase angle (LCFLAG=0) or imaginary (out-phase) part (LCFLAG=1) of load as a function of frequency.
        """ # nopep8
        return self._cards[3].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        self._cards[3].set_value("lc2", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor for the load.
        """ # nopep8
        return self._cards[3].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[3].set_value("sf", value)

    @property
    def vid(self) -> int:
        """Get or set the Vector ID for DOF=4 for excitation input, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[3].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[3].set_value("vid", value)

