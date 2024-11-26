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

class FrequencyDomainResponseSpectrum(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_RESPONSE_SPECTRUM keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_RESPONSE_SPECTRUM"

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
                        "restrt",
                        int,
                        40,
                        10,
                        kwargs.get("restrt", 0)
                    ),
                    Field(
                        "mcomb",
                        int,
                        50,
                        10,
                        kwargs.get("mcomb", 0)
                    ),
                    Field(
                        "relatv",
                        int,
                        60,
                        10,
                        kwargs.get("relatv", 0)
                    ),
                    Field(
                        "mprs",
                        int,
                        70,
                        10,
                        kwargs.get("mprs", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mcomb1",
                        int,
                        0,
                        10,
                        kwargs.get("mcomb1", 0)
                    ),
                    Field(
                        "mcomb2",
                        int,
                        10,
                        10,
                        kwargs.get("mcomb2", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "w1",
                        float,
                        0,
                        10,
                        kwargs.get("w1", 0.5)
                    ),
                    Field(
                        "w1",
                        float,
                        10,
                        10,
                        kwargs.get("w1", 0.5)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r40",
                        float,
                        0,
                        10,
                        kwargs.get("r40", 0.4)
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
                        kwargs.get("dampf")
                    ),
                    Field(
                        "lcdamp",
                        int,
                        10,
                        10,
                        kwargs.get("lcdamp")
                    ),
                    Field(
                        "ldtyp",
                        int,
                        20,
                        10,
                        kwargs.get("ldtyp", 0)
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
                ],
            ),
            Card(
                [
                    Field(
                        "lctyp",
                        int,
                        0,
                        10,
                        kwargs.get("lctyp", 0)
                    ),
                    Field(
                        "dof",
                        int,
                        10,
                        10,
                        kwargs.get("dof", 1)
                    ),
                    Field(
                        "lc/tbid",
                        int,
                        20,
                        10,
                        kwargs.get("lc/tbid")
                    ),
                    Field(
                        "sf",
                        float,
                        30,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "vid",
                        int,
                        40,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "lnid",
                        int,
                        50,
                        10,
                        kwargs.get("lnid")
                    ),
                    Field(
                        "lntyp",
                        int,
                        60,
                        10,
                        kwargs.get("lntyp", 1)
                    ),
                    Field(
                        "inflag",
                        int,
                        70,
                        10,
                        kwargs.get("inflag", 0)
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
        """Get or set the The minimum natural frequency in modal superposition method (optional).
        """ # nopep8
        return self._cards[0].get_value("fnmin")

    @fnmin.setter
    def fnmin(self, value: float) -> None:
        self._cards[0].set_value("fnmin", value)

    @property
    def fnmax(self) -> typing.Optional[float]:
        """Get or set the The maximum natural frequency in modal superposition method (optional).
        """ # nopep8
        return self._cards[0].get_value("fnmax")

    @fnmax.setter
    def fnmax(self, value: float) -> None:
        self._cards[0].set_value("fnmax", value)

    @property
    def restrt(self) -> int:
        """Get or set the Restart option
        EQ.0: A new run including modal analysis,
        EQ.1: Restart with d3eigv family files created elsewhere.
        """ # nopep8
        return self._cards[0].get_value("restrt")

    @restrt.setter
    def restrt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""restrt must be one of {0,1}""")
        self._cards[0].set_value("restrt", value)

    @property
    def mcomb(self) -> int:
        """Get or set the Method for combination of modes:
        EQ.0: SRSS method,
        EQ.1: NRC Grouping method,
        EQ.2: Complete Quadratic Combination method (CQC),
        EQ.3: Double Sum method,
        EQ.4: NRL-SUM method,
        EQ.-4:	NRL-SUM method with CSM (Closely Spaced Modes) treatment.  The CSM pairs are automatically identified.
        EQ. - 14:	NRL - SUM method with CSM(Closely Spaced Modes) treatment, where the CSM pairs are defined by SID(Mode set ID, see * SET_MODE) in Card 5
        EQ.5: Double Sum method based on Gupta-Cordero coefficient,
        EQ.6: Double Sum method based on modified Gupta-Cordero coefficient,
        EQ.7: Rosenblueth method.
        EQ.8: 	Absolute value method (ABS)
        EQ.99:combining results provided by two mode combination methods defined in Card 1.1 with corresponding weights defined in Card 1.2
        """ # nopep8
        return self._cards[0].get_value("mcomb")

    @mcomb.setter
    def mcomb(self, value: int) -> None:
        self._cards[0].set_value("mcomb", value)

    @property
    def relatv(self) -> int:
        """Get or set the Type of nodal displacement, velocity and acceleration results:/n EQ.0: Relative values (with respect to the ground) are provided,/n EQ.1: Absolute values are provided.
        """ # nopep8
        return self._cards[0].get_value("relatv")

    @relatv.setter
    def relatv(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""relatv must be one of {0,1}""")
        self._cards[0].set_value("relatv", value)

    @property
    def mprs(self) -> int:
        """Get or set the Multi-point or multidirectional response combination method:
        EQ.0:	SRSS.
        EQ.1 : 100 - 40 - 40 rule(Newmark method).
        EQ.2:	100-40-40 rule (Newmark method) with coefficient 0.4 replaced by R40 in Card 1a
        """ # nopep8
        return self._cards[0].get_value("mprs")

    @mprs.setter
    def mprs(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""mprs must be one of {0,1,2}""")
        self._cards[0].set_value("mprs", value)

    @property
    def mcomb1(self) -> int:
        """Get or set the mode combination method for which results will be combined to the other combination method. It can have any value from the MCOMB description other than 99
        """ # nopep8
        return self._cards[1].get_value("mcomb1")

    @mcomb1.setter
    def mcomb1(self, value: int) -> None:
        self._cards[1].set_value("mcomb1", value)

    @property
    def mcomb2(self) -> int:
        """Get or set the mode combination method for which results will be combined to the other combination method. It can have any value from the MCOMB description other than 99
        """ # nopep8
        return self._cards[1].get_value("mcomb2")

    @mcomb2.setter
    def mcomb2(self, value: int) -> None:
        self._cards[1].set_value("mcomb2", value)

    @property
    def w1(self) -> float:
        """Get or set the Weight for the results given bythe MCOMB combination
        """ # nopep8
        return self._cards[2].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        self._cards[2].set_value("w1", value)

    @property
    def w1(self) -> float:
        """Get or set the Weight for the results given bythe MCOMB combination
        """ # nopep8
        return self._cards[2].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        self._cards[2].set_value("w1", value)

    @property
    def r40(self) -> float:
        """Get or set the Coefficient to replace 0.4 in 100-40-40 rule
        """ # nopep8
        return self._cards[3].get_value("r40")

    @r40.setter
    def r40(self, value: float) -> None:
        self._cards[3].set_value("r40", value)

    @property
    def dampf(self) -> typing.Optional[float]:
        """Get or set the Modal damping ratio, ζ.
        """ # nopep8
        return self._cards[4].get_value("dampf")

    @dampf.setter
    def dampf(self, value: float) -> None:
        self._cards[4].set_value("dampf", value)

    @property
    def lcdamp(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID for defining frequency dependent modal damping ratio ζ.
        """ # nopep8
        return self._cards[4].get_value("lcdamp")

    @lcdamp.setter
    def lcdamp(self, value: int) -> None:
        self._cards[4].set_value("lcdamp", value)

    @property
    def ldtyp(self) -> int:
        """Get or set the Type of load curve for LCDAMP
        EQ.0: Abscissa value defines frequency,
        EQ.1: Abscissa value defines mode number.
        """ # nopep8
        return self._cards[4].get_value("ldtyp")

    @ldtyp.setter
    def ldtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ldtyp must be one of {0,1}""")
        self._cards[4].set_value("ldtyp", value)

    @property
    def dmpmas(self) -> float:
        """Get or set the Mass proportional damping constant α, in Rayleigh damping.
        """ # nopep8
        return self._cards[4].get_value("dmpmas")

    @dmpmas.setter
    def dmpmas(self, value: float) -> None:
        self._cards[4].set_value("dmpmas", value)

    @property
    def dmpstf(self) -> float:
        """Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
        """ # nopep8
        return self._cards[4].get_value("dmpstf")

    @dmpstf.setter
    def dmpstf(self, value: float) -> None:
        self._cards[4].set_value("dmpstf", value)

    @property
    def lctyp(self) -> int:
        """Get or set the Load curve type for defining the input spectrum.
        EQ.0: base velocity (vs. natural frequency),
        EQ.1: base acceleration (vs. natural frequency),
        EQ.2: base displacement (vs. natural frequency),
        EQ.3: nodal force (vs. natural frequency),
        EQ.4: pressure (vs. natural frequency),
        EQ.5: base velocity (vs. natural period),
        EQ.6: base acceleration (vs. natural period),
        EQ.7: base displacement (vs. natural period),
        EQ.8: nodal force (vs. natural period),
        EQ.9: pressure (vs. natural period),
        EQ.10: base velocity time history,
        EQ.11: base acceleration time history,
        EQ.12: base displacement time history.
        """ # nopep8
        return self._cards[5].get_value("lctyp")

    @lctyp.setter
    def lctyp(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]:
            raise Exception("""lctyp must be one of {0,1,2,3,4,5,6,7,8,9,10,11,12}""")
        self._cards[5].set_value("lctyp", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom for excitation input:
        EQ. 1: x-translational degree-of-freedom,
        EQ. 2: y-translational degree-of-freedom,
        EQ. 3: z-translational degree-of-freedom,
        EQ. 4: translational movement in direction given by vector VID.
        """ # nopep8
        return self._cards[5].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""dof must be one of {1,2,3,4}""")
        self._cards[5].set_value("dof", value)

    @property
    def lc_tbid(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID, see *DEFINE_TABLE, defining the response spectrum for frequencies. If the table definition is used a family of curves are defined for discrete critical damping ratios.
        """ # nopep8
        return self._cards[5].get_value("lc/tbid")

    @lc_tbid.setter
    def lc_tbid(self, value: int) -> None:
        self._cards[5].set_value("lc/tbid", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor for the input load spectrum.
        """ # nopep8
        return self._cards[5].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[5].set_value("sf", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID for DOF values of 4.
        """ # nopep8
        return self._cards[5].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[5].set_value("vid", value)

    @property
    def lnid(self) -> typing.Optional[int]:
        """Get or set the Node ID, or node set ID, or segment set ID where the excitation is applied. If the input load is given as base excitation spectrum, LNID=0
        """ # nopep8
        return self._cards[5].get_value("lnid")

    @lnid.setter
    def lnid(self, value: int) -> None:
        self._cards[5].set_value("lnid", value)

    @property
    def lntyp(self) -> int:
        """Get or set the Set type for LNID:
        EQ.1: Node, see *NODE,
        EQ.2: Node set, see *SET_NODE,
        EQ.3: Segment set, see *SET_SEGMENT,
        EQ.4: Part, see *PART,
        EQ.5: Part set, see *SET_PART.
        """ # nopep8
        return self._cards[5].get_value("lntyp")

    @lntyp.setter
    def lntyp(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5]:
            raise Exception("""lntyp must be one of {1,2,3,4,5}""")
        self._cards[5].set_value("lntyp", value)

    @property
    def inflag(self) -> int:
        """Get or set the Frequency interpolation option
        EQ.0: Logarithmic interpolation,
        EQ.1: Semi-logarithmic interpolation.
        EQ.2: Linear interpolation.
        """ # nopep8
        return self._cards[5].get_value("inflag")

    @inflag.setter
    def inflag(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""inflag must be one of {0,1,2}""")
        self._cards[5].set_value("inflag", value)

