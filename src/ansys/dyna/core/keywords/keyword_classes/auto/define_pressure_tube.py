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

class DefinePressureTube(KeywordBase):
    """DYNA DEFINE_PRESSURE_TUBE keyword"""

    keyword = "DEFINE"
    subkeyword = "PRESSURE_TUBE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "ws",
                        float,
                        10,
                        10,
                        kwargs.get("ws", 0.0)
                    ),
                    Field(
                        "pr",
                        float,
                        20,
                        10,
                        kwargs.get("pr", 0.0)
                    ),
                    Field(
                        "mtd",
                        int,
                        30,
                        10,
                        kwargs.get("mtd", 0)
                    ),
                    Field(
                        "type",
                        int,
                        40,
                        10,
                        kwargs.get("type", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "visc",
                        float,
                        0,
                        10,
                        kwargs.get("visc", 1.0)
                    ),
                    Field(
                        "cfl",
                        float,
                        10,
                        10,
                        kwargs.get("cfl", 0.9)
                    ),
                    Field(
                        "damp",
                        float,
                        20,
                        10,
                        kwargs.get("damp", 0.0)
                    ),
                    Field(
                        "bndl",
                        float,
                        30,
                        10,
                        kwargs.get("bndl", 0.0)
                    ),
                    Field(
                        "bndr",
                        float,
                        40,
                        10,
                        kwargs.get("bndr", 0.0)
                    ),
                    Field(
                        "cavl",
                        float,
                        50,
                        10,
                        kwargs.get("cavl", 0.0)
                    ),
                    Field(
                        "cavr",
                        float,
                        60,
                        10,
                        kwargs.get("cavr", 0.0)
                    ),
                    Field(
                        "snode",
                        int,
                        70,
                        10,
                        kwargs.get("snode", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nshl",
                        int,
                        0,
                        10,
                        kwargs.get("nshl", 12)
                    ),
                    Field(
                        "elform",
                        int,
                        10,
                        10,
                        kwargs.get("elform", 16)
                    ),
                    Field(
                        "nip",
                        int,
                        20,
                        10,
                        kwargs.get("nip", 3)
                    ),
                    Field(
                        "shrf",
                        float,
                        30,
                        10,
                        kwargs.get("shrf", 1.0)
                    ),
                    Field(
                        "bpid",
                        int,
                        40,
                        10,
                        kwargs.get("bpid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsld",
                        int,
                        0,
                        10,
                        kwargs.get("nsld", 12)
                    ),
                    Field(
                        "elform",
                        int,
                        10,
                        10,
                        kwargs.get("elform", 1)
                    ),
                    Field(
                        "nthk",
                        int,
                        20,
                        10,
                        kwargs.get("nthk", 3)
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "bpid",
                        int,
                        40,
                        10,
                        kwargs.get("bpid")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefinePressureTube.option_specs[0],
                cards = [
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
                ],
                **kwargs
            ),
        ]

    @property
    def pid(self) -> int:
        """Get or set the Part ID of tube. The tube(s) consists of all the beam elements in the
        part. Only ELFORM = 1,4,5,11 are allowed. Each set of joint beam
        elements in the part will model a tube and the beam elements may
        not contain junctions. Also, two different parts where
        *DEFINE_PRESSURE_TUBE is applied may not share beam nodes.
        For MPP all elements in the part will be on a single processor, so it is
        recommended that the part should only contain beam elements.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def ws(self) -> float:
        """Get or set the Wave propagation speed.
        """ # nopep8
        return self._cards[0].get_value("ws")

    @ws.setter
    def ws(self, value: float) -> None:
        self._cards[0].set_value("ws", value)

    @property
    def pr(self) -> float:
        """Get or set the Initial tube pressure.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def mtd(self) -> int:
        """Get or set the Solution method:
        EQ.0:	Standard Galerkin FEM.
        EQ.1: Discontinuous Galerkin
        EQ.2: Discontinuous Galerkin on isothermal Euler equations
        """ # nopep8
        return self._cards[0].get_value("mtd")

    @mtd.setter
    def mtd(self, value: int) -> None:
        self._cards[0].set_value("mtd", value)

    @property
    def type(self) -> int:
        """Get or set the Tube elements:
        EQ.0:	The tube is entirely simulated with beam elements. Cross section area is given from contact penetration of the beam elements. The mechanical response in radial direction of the beam elements is governed by contact stiffness. Only mortar contacts are supported.
        EQ.1:	The tube is simulated by automatic generation of shell elements, which are assigned the beam part ID and the beam material model. A new part ID is given to the beam elements, and those are no longer part of the mechanical solution. Contacts and other properties associated with the old beam part ID will now apply to the new shell part. Cross section area is given from the shell element nodes, and the mechanical response is governed entirely by the shells. Supports all contact definitions.
        EQ.2:	the tube is simulated by automatic generation of solid elements, similarly to TYPE = 1 above.
        LT.0:	automatic generation of elements as above, but the beam nodes are given new nodal IDs.
        The old beam NIDs are moved to the automatically generated tube (one row of nodes along the length).
        Any nodal constraints will thus apply to the new tube instead of the beam element tube.
        See Figure 0-1 for an example of different values of TYPE and how they affect nodal constraints
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        self._cards[0].set_value("type", value)

    @property
    def visc(self) -> float:
        """Get or set the MTD.EQ.0: Artificial viscosity multiplier (VISC > 0.0); see Remark 2. A smaller value gives a more resolved pulse at shorter wavelengths but may lead to instabilities. For typical automotive crash applications (tube length ~2m, diameter ~5mm, pressure pulse width ~5ms) the default value is recommended.
        MTD.GT.0: Slope limiter smoothing factor; see Remark 2. Smaller value gives a more resolved pulse at shorter wavelengths but may lead to instabilities.Larger value leads to a smeared pulse.
        """ # nopep8
        return self._cards[1].get_value("visc")

    @visc.setter
    def visc(self, value: float) -> None:
        self._cards[1].set_value("visc", value)

    @property
    def cfl(self) -> float:
        """Get or set the Stability factor (CFL > 0.0); see Remark 1. A smaller value leads to increased stability at the expense of increased computational cost.
        For typical automotive crash applications, the default value is recommended.
        """ # nopep8
        return self._cards[1].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        self._cards[1].set_value("cfl", value)

    @property
    def damp(self) -> float:
        """Get or set the Linear damping (DAMP ≥ 0.0); see Remark 1.
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[1].set_value("damp", value)

    @property
    def bndl(self) -> float:
        """Get or set the Left boundary condition (0 ≤ BNDi ≤ 1); see Remark 2. Special cases are:
        EQ.0.0:	closed tube end, that is, zero velocity boundary condition
        EQ:0.5:	non-reflecting boundary condition
        EQ:1.0:	open tube end, that is, constant pressure boundary condition
        Left tube end is automatically assigned to the lowest/highest beam node number on the tube, respectively.
        """ # nopep8
        return self._cards[1].get_value("bndl")

    @bndl.setter
    def bndl(self, value: float) -> None:
        self._cards[1].set_value("bndl", value)

    @property
    def bndr(self) -> float:
        """Get or set the Right boundary condition (0 ≤ BNDi ≤ 1); see Remark 2. Special cases are:
        EQ.0.0:	closed tube end, that is, zero velocity boundary condition
        EQ:0.5:	non-reflecting boundary condition
        EQ:1.0:	open tube end, that is, constant pressure boundary condition
        Right tube end is automatically assigned to the lowest/highest beam node number on the tube, respectively.
        """ # nopep8
        return self._cards[1].get_value("bndr")

    @bndr.setter
    def bndr(self, value: float) -> None:
        self._cards[1].set_value("bndr", value)

    @property
    def cavl(self) -> float:
        """Get or set the Left cavity; see Remark 3.
        GT.0.0: elements near the end of the tube are replaced with a cavity.
        The integer part of CAVi determines the number of beam elements that belong to the cavity.
        The remainder of CAVi determines the boundary condition on the interface between the tube and the cavity.
        LT:0.0:	the tube is extended with a cavity by adding new beam elements.
        The length of the added cavity is given by  where  truncates the decimal portion of  (leaving an integer).
        The remainder of  determines the boundary condition on the interface between the tube and the cavity.
        """ # nopep8
        return self._cards[1].get_value("cavl")

    @cavl.setter
    def cavl(self, value: float) -> None:
        self._cards[1].set_value("cavl", value)

    @property
    def cavr(self) -> float:
        """Get or set the Right cavity; see Remark 3.
        GT.0.0: elements near the end of the tube are replaced with a cavity.
        The integer part of CAVi determines the number of beam elements that belong to the cavity.
        The remainder of CAVi determines the boundary condition on the interface between the tube and the cavity.
        LT:0.0:	the tube is extended with a cavity by adding new beam elements.
        The length of the added cavity is given by  where  truncates the decimal portion of  (leaving an integer).
        The remainder of  determines the boundary condition on the interface between the tube and the cavity.
        """ # nopep8
        return self._cards[1].get_value("cavr")

    @cavr.setter
    def cavr(self, value: float) -> None:
        self._cards[1].set_value("cavr", value)

    @property
    def snode(self) -> int:
        """Get or set the Optional starting node. This node determines the left end of the tube. If not set, the tube starts at the lowest numbered beam node
        """ # nopep8
        return self._cards[1].get_value("snode")

    @snode.setter
    def snode(self, value: int) -> None:
        self._cards[1].set_value("snode", value)

    @property
    def nshl(self) -> int:
        """Get or set the Number of automatically generated shells/solids on circumference of tube
        """ # nopep8
        return self._cards[2].get_value("nshl")

    @nshl.setter
    def nshl(self, value: int) -> None:
        self._cards[2].set_value("nshl", value)

    @property
    def elform(self) -> int:
        """Get or set the ELFORM for automatically generated shells/solids; see *SECTION_‌SHELL/SOLID.
        """ # nopep8
        return self._cards[2].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        self._cards[2].set_value("elform", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of through thickness integration points for automatically generated shells; see NIP in *SECTION_‌SHELL.
        """ # nopep8
        return self._cards[2].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        self._cards[2].set_value("nip", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear correction factor for automatically generated shells; see SHRF in *SECTION_‌SHELL
        """ # nopep8
        return self._cards[2].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        self._cards[2].set_value("shrf", value)

    @property
    def bpid(self) -> typing.Optional[int]:
        """Get or set the Optional PID given to beam elements when automatically generating shells/solids.
        """ # nopep8
        return self._cards[2].get_value("bpid")

    @bpid.setter
    def bpid(self, value: int) -> None:
        self._cards[2].set_value("bpid", value)

    @property
    def nsld(self) -> int:
        """Get or set the Number of automatically generated shells/solids on circumference of tube
        """ # nopep8
        return self._cards[3].get_value("nsld")

    @nsld.setter
    def nsld(self, value: int) -> None:
        self._cards[3].set_value("nsld", value)

    @property
    def elform(self) -> int:
        """Get or set the ELFORM for automatically generated shells/solids; see *SECTION_‌SHELL/SOLID.
        """ # nopep8
        return self._cards[3].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        self._cards[3].set_value("elform", value)

    @property
    def nthk(self) -> int:
        """Get or set the Number of solid elements in thickness of tube for automatically generated solids.
        """ # nopep8
        return self._cards[3].get_value("nthk")

    @nthk.setter
    def nthk(self, value: int) -> None:
        self._cards[3].set_value("nthk", value)

    @property
    def bpid(self) -> typing.Optional[int]:
        """Get or set the Optional PID given to beam elements when automatically generating shells/solids.
        """ # nopep8
        return self._cards[3].get_value("bpid")

    @bpid.setter
    def bpid(self, value: int) -> None:
        self._cards[3].set_value("bpid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

