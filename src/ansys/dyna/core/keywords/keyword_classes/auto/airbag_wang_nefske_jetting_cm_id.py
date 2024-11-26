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

class AirbagWangNefskeJettingCmId(KeywordBase):
    """DYNA AIRBAG_WANG_NEFSKE_JETTING_CM_ID keyword"""

    keyword = "AIRBAG"
    subkeyword = "WANG_NEFSKE_JETTING_CM_ID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "sidtyp",
                        int,
                        10,
                        10,
                        kwargs.get("sidtyp", 0)
                    ),
                    Field(
                        "rbid",
                        int,
                        20,
                        10,
                        kwargs.get("rbid", 0)
                    ),
                    Field(
                        "vsca",
                        float,
                        30,
                        10,
                        kwargs.get("vsca", 1.0)
                    ),
                    Field(
                        "psca",
                        float,
                        40,
                        10,
                        kwargs.get("psca", 1.0)
                    ),
                    Field(
                        "vini",
                        float,
                        50,
                        10,
                        kwargs.get("vini", 0.0)
                    ),
                    Field(
                        "mwd",
                        float,
                        60,
                        10,
                        kwargs.get("mwd", 0.0)
                    ),
                    Field(
                        "spsf",
                        float,
                        70,
                        10,
                        kwargs.get("spsf", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cv",
                        float,
                        0,
                        10,
                        kwargs.get("cv")
                    ),
                    Field(
                        "cp",
                        float,
                        10,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "t",
                        float,
                        20,
                        10,
                        kwargs.get("t", 0.0)
                    ),
                    Field(
                        "lct",
                        int,
                        30,
                        10,
                        kwargs.get("lct", 0)
                    ),
                    Field(
                        "lcmt",
                        int,
                        40,
                        10,
                        kwargs.get("lcmt")
                    ),
                    Field(
                        "tvol",
                        float,
                        50,
                        10,
                        kwargs.get("tvol", 0.0)
                    ),
                    Field(
                        "lcdt",
                        int,
                        60,
                        10,
                        kwargs.get("lcdt", 0)
                    ),
                    Field(
                        "iabt",
                        float,
                        70,
                        10,
                        kwargs.get("iabt", not used)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c23",
                        float,
                        0,
                        10,
                        kwargs.get("c23")
                    ),
                    Field(
                        "lcc23",
                        int,
                        10,
                        10,
                        kwargs.get("lcc23", 0)
                    ),
                    Field(
                        "a23",
                        float,
                        20,
                        10,
                        kwargs.get("a23")
                    ),
                    Field(
                        "lca23",
                        int,
                        30,
                        10,
                        kwargs.get("lca23", 0)
                    ),
                    Field(
                        "cp23",
                        float,
                        40,
                        10,
                        kwargs.get("cp23")
                    ),
                    Field(
                        "lccp23",
                        int,
                        50,
                        10,
                        kwargs.get("lccp23", 0)
                    ),
                    Field(
                        "ap23",
                        float,
                        60,
                        10,
                        kwargs.get("ap23", 0.0)
                    ),
                    Field(
                        "lcap23",
                        int,
                        70,
                        10,
                        kwargs.get("lcap23", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pe",
                        float,
                        0,
                        10,
                        kwargs.get("pe")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "gc",
                        float,
                        20,
                        10,
                        kwargs.get("gc")
                    ),
                    Field(
                        "lcefr",
                        int,
                        30,
                        10,
                        kwargs.get("lcefr", 0)
                    ),
                    Field(
                        "pover",
                        float,
                        40,
                        10,
                        kwargs.get("pover", 0.0)
                    ),
                    Field(
                        "ppop",
                        float,
                        50,
                        10,
                        kwargs.get("ppop", 0.0)
                    ),
                    Field(
                        "opt",
                        int,
                        60,
                        10,
                        kwargs.get("opt", 1)
                    ),
                    Field(
                        "knkdn",
                        int,
                        70,
                        10,
                        kwargs.get("knkdn", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ioc",
                        float,
                        0,
                        10,
                        kwargs.get("ioc")
                    ),
                    Field(
                        "ioa",
                        float,
                        10,
                        10,
                        kwargs.get("ioa")
                    ),
                    Field(
                        "ivol",
                        float,
                        20,
                        10,
                        kwargs.get("ivol")
                    ),
                    Field(
                        "iro",
                        float,
                        30,
                        10,
                        kwargs.get("iro")
                    ),
                    Field(
                        "it",
                        float,
                        40,
                        10,
                        kwargs.get("it")
                    ),
                    Field(
                        "lcbf",
                        int,
                        50,
                        10,
                        kwargs.get("lcbf")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "text",
                        float,
                        0,
                        10,
                        kwargs.get("text")
                    ),
                    Field(
                        "a",
                        float,
                        10,
                        10,
                        kwargs.get("a")
                    ),
                    Field(
                        "b",
                        float,
                        20,
                        10,
                        kwargs.get("b")
                    ),
                    Field(
                        "mw",
                        float,
                        30,
                        10,
                        kwargs.get("mw")
                    ),
                    Field(
                        "gasc",
                        float,
                        40,
                        10,
                        kwargs.get("gasc")
                    ),
                    Field(
                        "hconv",
                        float,
                        50,
                        10,
                        kwargs.get("hconv", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xjfp",
                        float,
                        0,
                        10,
                        kwargs.get("xjfp")
                    ),
                    Field(
                        "yjfp",
                        float,
                        10,
                        10,
                        kwargs.get("yjfp")
                    ),
                    Field(
                        "zjfp",
                        float,
                        20,
                        10,
                        kwargs.get("zjfp")
                    ),
                    Field(
                        "xjvh",
                        float,
                        30,
                        10,
                        kwargs.get("xjvh")
                    ),
                    Field(
                        "yjvh",
                        float,
                        40,
                        10,
                        kwargs.get("yjvh")
                    ),
                    Field(
                        "zjvh",
                        float,
                        50,
                        10,
                        kwargs.get("zjvh")
                    ),
                    Field(
                        "ca",
                        float,
                        60,
                        10,
                        kwargs.get("ca")
                    ),
                    Field(
                        "beta",
                        float,
                        70,
                        10,
                        kwargs.get("beta", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xsjfp",
                        float,
                        0,
                        10,
                        kwargs.get("xsjfp")
                    ),
                    Field(
                        "ysjfp",
                        float,
                        10,
                        10,
                        kwargs.get("ysjfp")
                    ),
                    Field(
                        "zsjfp",
                        float,
                        20,
                        10,
                        kwargs.get("zsjfp")
                    ),
                    Field(
                        "psid",
                        int,
                        30,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "angle",
                        float,
                        40,
                        10,
                        kwargs.get("angle")
                    ),
                    Field(
                        "node1",
                        int,
                        50,
                        10,
                        kwargs.get("node1", 0)
                    ),
                    Field(
                        "node2",
                        int,
                        60,
                        10,
                        kwargs.get("node2", 0)
                    ),
                    Field(
                        "node3",
                        int,
                        70,
                        10,
                        kwargs.get("node3", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nreact",
                        int,
                        0,
                        10,
                        kwargs.get("nreact", 0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Optional Airbag ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[1].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[1].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set type:
        EQ.0: segment,
        EQ.1: part IDs.
        """ # nopep8
        return self._cards[1].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sidtyp must be one of {0,1}""")
        self._cards[1].set_value("sidtyp", value)

    @property
    def rbid(self) -> int:
        """Get or set the Rigid body part ID for user defined activation subroutine:
        EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
        EQ.0: the control volume is active from time zero,
        EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
        """ # nopep8
        return self._cards[1].get_value("rbid")

    @rbid.setter
    def rbid(self, value: int) -> None:
        self._cards[1].set_value("rbid", value)

    @property
    def vsca(self) -> float:
        """Get or set the Volume scale factor, V-sca (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("vsca")

    @vsca.setter
    def vsca(self, value: float) -> None:
        self._cards[1].set_value("vsca", value)

    @property
    def psca(self) -> float:
        """Get or set the Pressure scale factor, P-sca (default=1.0).
        """ # nopep8
        return self._cards[1].get_value("psca")

    @psca.setter
    def psca(self, value: float) -> None:
        self._cards[1].set_value("psca", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial filled volume, V-ini (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        self._cards[1].set_value("vini", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        self._cards[1].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
        """ # nopep8
        return self._cards[1].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        self._cards[1].set_value("spsf", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at constant volume.
        """ # nopep8
        return self._cards[2].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        self._cards[2].set_value("cv", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at constant pressure.
        """ # nopep8
        return self._cards[2].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[2].set_value("cp", value)

    @property
    def t(self) -> float:
        """Get or set the Temperature of input gas (default =0.0).
        For temperature variations a load curve, LCT, may be defined.
        """ # nopep8
        return self._cards[2].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[2].set_value("t", value)

    @property
    def lct(self) -> int:
        """Get or set the Optional load curve number defining temperature of input gas versus time.  This overides columns T.
        """ # nopep8
        return self._cards[2].get_value("lct")

    @lct.setter
    def lct(self, value: int) -> None:
        self._cards[2].set_value("lct", value)

    @property
    def lcmt(self) -> typing.Optional[int]:
        """Get or set the Load curve specifying input mass flow rate or tank pressure versus time. If the tank volume, TVOL, is nonzero the curve ID is assumed to be tank pressure versus time. If LCMT=0, then the inflator has to be modeled, see Card 4. During the dynamic relaxation phase the airbag is ignored unless the curve is flagged to act during dynamic relaxation.
        """ # nopep8
        return self._cards[2].get_value("lcmt")

    @lcmt.setter
    def lcmt(self, value: int) -> None:
        self._cards[2].set_value("lcmt", value)

    @property
    def tvol(self) -> float:
        """Get or set the Tank volume which is required only for the tank pressure versus time curve, LCMT.
        """ # nopep8
        return self._cards[2].get_value("tvol")

    @tvol.setter
    def tvol(self, value: float) -> None:
        self._cards[2].set_value("tvol", value)

    @property
    def lcdt(self) -> int:
        """Get or set the Load curve for time rate of change of temperature (dT/dt) versus time.
        """ # nopep8
        return self._cards[2].get_value("lcdt")

    @lcdt.setter
    def lcdt(self, value: int) -> None:
        self._cards[2].set_value("lcdt", value)

    @property
    def iabt(self) -> float:
        """Get or set the Initial airbag temperature. (Optional, generally not defined).
        """ # nopep8
        return self._cards[2].get_value("iabt")

    @iabt.setter
    def iabt(self, value: float) -> None:
        self._cards[2].set_value("iabt", value)

    @property
    def c23(self) -> typing.Optional[float]:
        """Get or set the Vent orifice coefficient which applies to exit hole. Set to zero if LCC23 is defined below.
        """ # nopep8
        return self._cards[3].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        self._cards[3].set_value("c23", value)

    @property
    def lcc23(self) -> int:
        """Get or set the Load curve number defining the vent orifice coefficient which applies to exit hole as a function of time. A nonzero value for C23 overrides LCC23.
        """ # nopep8
        return self._cards[3].get_value("lcc23")

    @lcc23.setter
    def lcc23(self, value: int) -> None:
        self._cards[3].set_value("lcc23", value)

    @property
    def a23(self) -> typing.Optional[float]:
        """Get or set the Vent orifice area which applies to exit hole. Set to zero if LCA23 is defined below.
        """ # nopep8
        return self._cards[3].get_value("a23")

    @a23.setter
    def a23(self, value: float) -> None:
        self._cards[3].set_value("a23", value)

    @property
    def lca23(self) -> int:
        """Get or set the Load curve number defining the vent orifice area which applies to exit hole as a function of absolute pressure. A nonzero value for A23 overrides LCA23.
        """ # nopep8
        return self._cards[3].get_value("lca23")

    @lca23.setter
    def lca23(self, value: int) -> None:
        self._cards[3].set_value("lca23", value)

    @property
    def cp23(self) -> typing.Optional[float]:
        """Get or set the Orifice coefficient for leakage (fabric porosity). Set to zero if LCCP23 is defined below.
        """ # nopep8
        return self._cards[3].get_value("cp23")

    @cp23.setter
    def cp23(self, value: float) -> None:
        self._cards[3].set_value("cp23", value)

    @property
    def lccp23(self) -> int:
        """Get or set the Load curve number defining the orifice coefficient for leakage (fabric porosity) as a function of time. A nonzero value for CP23 overrides LCCP23.
        """ # nopep8
        return self._cards[3].get_value("lccp23")

    @lccp23.setter
    def lccp23(self, value: int) -> None:
        self._cards[3].set_value("lccp23", value)

    @property
    def ap23(self) -> float:
        """Get or set the Area for leakage (fabric porosity).
        """ # nopep8
        return self._cards[3].get_value("ap23")

    @ap23.setter
    def ap23(self, value: float) -> None:
        self._cards[3].set_value("ap23", value)

    @property
    def lcap23(self) -> int:
        """Get or set the Load curve number defining the area for leakage (fabric porosity) as a function of (absolute) pressure. A nonzero value for AP23 overrides LCAP23.
        """ # nopep8
        return self._cards[3].get_value("lcap23")

    @lcap23.setter
    def lcap23(self, value: int) -> None:
        self._cards[3].set_value("lcap23", value)

    @property
    def pe(self) -> typing.Optional[float]:
        """Get or set the Ambient pressure.
        """ # nopep8
        return self._cards[4].get_value("pe")

    @pe.setter
    def pe(self, value: float) -> None:
        self._cards[4].set_value("pe", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Ambient density.
        """ # nopep8
        return self._cards[4].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[4].set_value("ro", value)

    @property
    def gc(self) -> typing.Optional[float]:
        """Get or set the Gravitational conversion constant (mandatory - no default). If consistent units are being used for all parameters in the airbag definition then unity should be input.
        """ # nopep8
        return self._cards[4].get_value("gc")

    @gc.setter
    def gc(self, value: float) -> None:
        self._cards[4].set_value("gc", value)

    @property
    def lcefr(self) -> int:
        """Get or set the Optional curve for exit flow rate versus (gauge) pressure.
        """ # nopep8
        return self._cards[4].get_value("lcefr")

    @lcefr.setter
    def lcefr(self, value: int) -> None:
        self._cards[4].set_value("lcefr", value)

    @property
    def pover(self) -> float:
        """Get or set the Initial relative overpressure (gauge), P-over in control volume.
        """ # nopep8
        return self._cards[4].get_value("pover")

    @pover.setter
    def pover(self, value: float) -> None:
        self._cards[4].set_value("pover", value)

    @property
    def ppop(self) -> float:
        """Get or set the Pop pressure: relative pressure (gauge) for initiating exit flow, P-pop.
        """ # nopep8
        return self._cards[4].get_value("ppop")

    @ppop.setter
    def ppop(self, value: float) -> None:
        self._cards[4].set_value("ppop", value)

    @property
    def opt(self) -> int:
        """Get or set the Fabric venting option, if nonzero CP23, LCCP23, AP23, and LCAP23 are set to zero.
        EQ.1: Wang-Nefske formulas for venting through an orifice are used. Blockage is not considered (default).
        EQ.2: Wang-Nefske formulas for venting through an orifice are used. Blockage of venting area due to contact is considered.
        EQ.3: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage is not considered.
        EQ.4: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage of venting area due to contact is considered.
        EQ.5: Leakage formulas based on flow through a porous media are used. Blockage is not considered.
        EQ.6: Leakage formulas based on flow through a porous media are used. Blockage of venting area due to contact is considered.
        EQ.7: Simple porosity model. Blockage is not considered.
        EQ.8: Simple porosity model. Blockage of venting area due to contact is considered.
        """ # nopep8
        return self._cards[4].get_value("opt")

    @opt.setter
    def opt(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""opt must be one of {1,2,3,4,5,6,7,8}""")
        self._cards[4].set_value("opt", value)

    @property
    def knkdn(self) -> int:
        """Get or set the Optional load curve ID defining the knock down pressure scale factor versus time. This option only applies to jetting. The scale factor defined by this load curve scales the pressure applied to airbag segments which do not have a clear line-of-sight to the jet. Typically, at very early times this scale factor will be less than unity and equal to unity at later times. The full pressure is always applied to segments which can see the jets.
        """ # nopep8
        return self._cards[4].get_value("knkdn")

    @knkdn.setter
    def knkdn(self, value: int) -> None:
        self._cards[4].set_value("knkdn", value)

    @property
    def ioc(self) -> typing.Optional[float]:
        """Get or set the Inflator orifice coefficient.
        """ # nopep8
        return self._cards[5].get_value("ioc")

    @ioc.setter
    def ioc(self, value: float) -> None:
        self._cards[5].set_value("ioc", value)

    @property
    def ioa(self) -> typing.Optional[float]:
        """Get or set the Inflator orifice area.
        """ # nopep8
        return self._cards[5].get_value("ioa")

    @ioa.setter
    def ioa(self, value: float) -> None:
        self._cards[5].set_value("ioa", value)

    @property
    def ivol(self) -> typing.Optional[float]:
        """Get or set the Inflator volume.
        """ # nopep8
        return self._cards[5].get_value("ivol")

    @ivol.setter
    def ivol(self, value: float) -> None:
        self._cards[5].set_value("ivol", value)

    @property
    def iro(self) -> typing.Optional[float]:
        """Get or set the Inflator density.
        """ # nopep8
        return self._cards[5].get_value("iro")

    @iro.setter
    def iro(self, value: float) -> None:
        self._cards[5].set_value("iro", value)

    @property
    def it(self) -> typing.Optional[float]:
        """Get or set the Inflator temperature.
        """ # nopep8
        return self._cards[5].get_value("it")

    @it.setter
    def it(self, value: float) -> None:
        self._cards[5].set_value("it", value)

    @property
    def lcbf(self) -> typing.Optional[int]:
        """Get or set the Load curve defining burn fraction versus time.
        """ # nopep8
        return self._cards[5].get_value("lcbf")

    @lcbf.setter
    def lcbf(self, value: int) -> None:
        self._cards[5].set_value("lcbf", value)

    @property
    def text(self) -> typing.Optional[float]:
        """Get or set the Ambient temperature.
        """ # nopep8
        return self._cards[6].get_value("text")

    @text.setter
    def text(self, value: float) -> None:
        self._cards[6].set_value("text", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the First heat capacity coefficient of inflator gas. (e.g., Joules/mole/oK)
        """ # nopep8
        return self._cards[6].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[6].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Second heat capacity coefficient of inflator gas. (e.g., Joules/mole/oK2)
        """ # nopep8
        return self._cards[6].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[6].set_value("b", value)

    @property
    def mw(self) -> typing.Optional[float]:
        """Get or set the Molecular weight of inflator gas. (e.g., Kg/mole)
        """ # nopep8
        return self._cards[6].get_value("mw")

    @mw.setter
    def mw(self, value: float) -> None:
        self._cards[6].set_value("mw", value)

    @property
    def gasc(self) -> typing.Optional[float]:
        """Get or set the Universal gas constant of inflator gas. (e.g., 8.314 Joules/mole/oK)
        """ # nopep8
        return self._cards[6].get_value("gasc")

    @gasc.setter
    def gasc(self, value: float) -> None:
        self._cards[6].set_value("gasc", value)

    @property
    def hconv(self) -> float:
        """Get or set the Convection heat transfer coefficient
        """ # nopep8
        return self._cards[6].get_value("hconv")

    @hconv.setter
    def hconv(self, value: float) -> None:
        self._cards[6].set_value("hconv", value)

    @property
    def xjfp(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of jet focal point.
        """ # nopep8
        return self._cards[7].get_value("xjfp")

    @xjfp.setter
    def xjfp(self, value: float) -> None:
        self._cards[7].set_value("xjfp", value)

    @property
    def yjfp(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of jet focal point.
        """ # nopep8
        return self._cards[7].get_value("yjfp")

    @yjfp.setter
    def yjfp(self, value: float) -> None:
        self._cards[7].set_value("yjfp", value)

    @property
    def zjfp(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of jet focal point.
        """ # nopep8
        return self._cards[7].get_value("zjfp")

    @zjfp.setter
    def zjfp(self, value: float) -> None:
        self._cards[7].set_value("zjfp", value)

    @property
    def xjvh(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of jet vector head to defined code centerline.
        """ # nopep8
        return self._cards[7].get_value("xjvh")

    @xjvh.setter
    def xjvh(self, value: float) -> None:
        self._cards[7].set_value("xjvh", value)

    @property
    def yjvh(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of jet vector head to defined code centerline.
        """ # nopep8
        return self._cards[7].get_value("yjvh")

    @yjvh.setter
    def yjvh(self, value: float) -> None:
        self._cards[7].set_value("yjvh", value)

    @property
    def zjvh(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of jet vector head to defined code centerline.
        """ # nopep8
        return self._cards[7].get_value("zjvh")

    @zjvh.setter
    def zjvh(self, value: float) -> None:
        self._cards[7].set_value("zjvh", value)

    @property
    def ca(self) -> typing.Optional[float]:
        """Get or set the Cone angle, alpha, defined in radians.
        LT.0.0: |alpha| is the load curve ID defining cone angle as a function of time.
        """ # nopep8
        return self._cards[7].get_value("ca")

    @ca.setter
    def ca(self, value: float) -> None:
        self._cards[7].set_value("ca", value)

    @property
    def beta(self) -> float:
        """Get or set the Efficiency factor, beta, which scales the final value of pressure obtained from Bernoulli's equation (default=1.0).
        LT.0.0: |beta| is the load curve ID defining the efficiency factor as a function of time.
        """ # nopep8
        return self._cards[7].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[7].set_value("beta", value)

    @property
    def xsjfp(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of secondary jet focal point, passenger side bag. If the coordinates of the secondary point are (0,0,0) then a conical jet (driver's side airbag) is assumed.
        """ # nopep8
        return self._cards[8].get_value("xsjfp")

    @xsjfp.setter
    def xsjfp(self, value: float) -> None:
        self._cards[8].set_value("xsjfp", value)

    @property
    def ysjfp(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of secondary jet focal point.
        """ # nopep8
        return self._cards[8].get_value("ysjfp")

    @ysjfp.setter
    def ysjfp(self, value: float) -> None:
        self._cards[8].set_value("ysjfp", value)

    @property
    def zsjfp(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of secondary jet focal point.
        """ # nopep8
        return self._cards[8].get_value("zsjfp")

    @zsjfp.setter
    def zsjfp(self, value: float) -> None:
        self._cards[8].set_value("zsjfp", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Optional part set ID, see *SET_PART.
        EQ.0: all elements are included in the airbag.
        """ # nopep8
        return self._cards[8].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[8].set_value("psid", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Not to be defined.
        """ # nopep8
        return self._cards[8].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        self._cards[8].set_value("angle", value)

    @property
    def node1(self) -> int:
        """Get or set the Node ID located at the jet focal point.
        """ # nopep8
        return self._cards[8].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        self._cards[8].set_value("node1", value)

    @property
    def node2(self) -> int:
        """Get or set the Node ID for node along the axis of the jet.
        """ # nopep8
        return self._cards[8].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        self._cards[8].set_value("node2", value)

    @property
    def node3(self) -> int:
        """Get or set the Optional node ID located at secondary jet focal point.
        """ # nopep8
        return self._cards[8].get_value("node3")

    @node3.setter
    def node3(self, value: int) -> None:
        self._cards[8].set_value("node3", value)

    @property
    def nreact(self) -> int:
        """Get or set the Node for reacting jet force.
        EQ.0: No jet force will be applied.
        """ # nopep8
        return self._cards[9].get_value("nreact")

    @nreact.setter
    def nreact(self, value: int) -> None:
        self._cards[9].set_value("nreact", value)

