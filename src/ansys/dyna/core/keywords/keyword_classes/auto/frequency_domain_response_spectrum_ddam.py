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

class FrequencyDomainResponseSpectrumDdam(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_RESPONSE_SPECTRUM_DDAM keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_RESPONSE_SPECTRUM_DDAM"

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
                        "std",
                        int,
                        0,
                        10,
                        kwargs.get("std", 1)
                    ),
                    Field(
                        "unit",
                        int,
                        10,
                        10,
                        kwargs.get("unit", 1)
                    ),
                    Field(
                        "amin",
                        float,
                        20,
                        10,
                        kwargs.get("amin", 6.0)
                    ),
                    Field(
                        "vid",
                        int,
                        30,
                        10,
                        kwargs.get("vid", 0)
                    ),
                    Field(
                        "xc",
                        float,
                        40,
                        10,
                        kwargs.get("xc", 0.0)
                    ),
                    Field(
                        "yc",
                        float,
                        50,
                        10,
                        kwargs.get("yc", 0.0)
                    ),
                    Field(
                        "zc",
                        float,
                        60,
                        10,
                        kwargs.get("zc", 0.0)
                    ),
                    Field(
                        "effmas",
                        float,
                        70,
                        10,
                        kwargs.get("effmas", 80.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "shptyp",
                        int,
                        0,
                        10,
                        kwargs.get("shptyp", 1)
                    ),
                    Field(
                        "mount",
                        int,
                        10,
                        10,
                        kwargs.get("mount", 1)
                    ),
                    Field(
                        "movemt",
                        int,
                        20,
                        10,
                        kwargs.get("movemt", 1)
                    ),
                    Field(
                        "mattyp",
                        int,
                        30,
                        10,
                        kwargs.get("mattyp", 1)
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
    def std(self) -> int:
        """Get or set the Design spectrum standard for shock load
        EQ.1:	NRL-1396,
        EQ.-1:Spectrum constants defined by user in Card 5.
        """ # nopep8
        return self._cards[5].get_value("std")

    @std.setter
    def std(self, value: int) -> None:
        if value not in [1, -1]:
            raise Exception("""std must be one of {1,-1}""")
        self._cards[5].set_value("std", value)

    @property
    def unit(self) -> int:
        """Get or set the Unit system
        EQ.1:	MKS (kg, m, s, N, Pa)
        EQ.2:	GPA (kg, mm, ms, kN, GPa)
        EQ.3:	MPA (ton, mm, s, N, MPa)
        EQ.4:	BIN (lb, in, s, lbf, psi)
        EQ.5	miu_MKS (gm, mm, ms, N, N/mm2)
        EQ.6:	CGS (gm, cm, s, dyne, dyne/cm2).
        """ # nopep8
        return self._cards[5].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6]:
            raise Exception("""unit must be one of {1,2,3,4,5,6}""")
        self._cards[5].set_value("unit", value)

    @property
    def amin(self) -> float:
        """Get or set the Minimum acceleration (in g - gravity acceleration).
        """ # nopep8
        return self._cards[5].get_value("amin")

    @amin.setter
    def amin(self, value: float) -> None:
        self._cards[5].set_value("amin", value)

    @property
    def vid(self) -> int:
        """Get or set the Direction of shock load
        EQ.1:	x-direction
        EQ.2:	y-direction
        EQ.3:	z-direction
        < 0:	direction is given by vector |VID|.
        """ # nopep8
        return self._cards[5].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[5].set_value("vid", value)

    @property
    def xc(self) -> float:
        """Get or set the X-directional cosine of shock load (if VID is undefined).
        """ # nopep8
        return self._cards[5].get_value("xc")

    @xc.setter
    def xc(self, value: float) -> None:
        self._cards[5].set_value("xc", value)

    @property
    def yc(self) -> float:
        """Get or set the Y-directional cosine of shock load (if VID is undefined).
        """ # nopep8
        return self._cards[5].get_value("yc")

    @yc.setter
    def yc(self, value: float) -> None:
        self._cards[5].set_value("yc", value)

    @property
    def zc(self) -> float:
        """Get or set the Z-directional cosine of shock load (if VID is undefined).
        """ # nopep8
        return self._cards[5].get_value("zc")

    @zc.setter
    def zc(self, value: float) -> None:
        self._cards[5].set_value("zc", value)

    @property
    def effmas(self) -> float:
        """Get or set the Minimum percentage requirement of total modal mass.
        """ # nopep8
        return self._cards[5].get_value("effmas")

    @effmas.setter
    def effmas(self, value: float) -> None:
        self._cards[5].set_value("effmas", value)

    @property
    def shptyp(self) -> int:
        """Get or set the Ship type
        EQ.1:	Submarine
        EQ.2:	Surface ship.
        """ # nopep8
        return self._cards[6].get_value("shptyp")

    @shptyp.setter
    def shptyp(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""shptyp must be one of {1,2}""")
        self._cards[6].set_value("shptyp", value)

    @property
    def mount(self) -> int:
        """Get or set the Mount type
        EQ.1:	Hull Mounted System
        EQ.2:	Deck Mounted System
        EQ.3:	Shell Plating Mounted System.
        """ # nopep8
        return self._cards[6].get_value("mount")

    @mount.setter
    def mount(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""mount must be one of {1,2,3}""")
        self._cards[6].set_value("mount", value)

    @property
    def movemt(self) -> int:
        """Get or set the Movement type
        EQ.1:	Vertical
        EQ.2:	Athwartship
        EQ.3:	Fore and Aft.
        """ # nopep8
        return self._cards[6].get_value("movemt")

    @movemt.setter
    def movemt(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""movemt must be one of {1,2,3}""")
        self._cards[6].set_value("movemt", value)

    @property
    def mattyp(self) -> int:
        """Get or set the Material type
        EQ.1:	Elastic
        EQ.2:	Elasto-plastic.
        """ # nopep8
        return self._cards[6].get_value("mattyp")

    @mattyp.setter
    def mattyp(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""mattyp must be one of {1,2}""")
        self._cards[6].set_value("mattyp", value)

