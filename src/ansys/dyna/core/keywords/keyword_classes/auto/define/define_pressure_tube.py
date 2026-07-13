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

"""Module providing the DefinePressureTube class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEPRESSURETUBE_CARD0 = (
    FieldSchema("pid", int, 0, 10, 0),
    FieldSchema("ws", float, 10, 10, 0.0),
    FieldSchema("pr", float, 20, 10, 0.0),
    FieldSchema("mtd", int, 30, 10, 0),
    FieldSchema("type", int, 40, 10, 0),
    FieldSchema("gamma", float, 50, 10, 1.0),
    FieldSchema("cfac", float, 60, 10, 0.0),
    FieldSchema("cutoff", int, 70, 10, 0),
)

_DEFINEPRESSURETUBE_CARD1 = (
    FieldSchema("visc", float, 0, 10, 1.0),
    FieldSchema("cfl", float, 10, 10, 0.9),
    FieldSchema("damp", float, 20, 10, 0.0),
    FieldSchema("bndl", float, 30, 10, 0.0),
    FieldSchema("bndr", float, 40, 10, 0.0),
    FieldSchema("cavl", float, 50, 10, 0.0),
    FieldSchema("cavr", float, 60, 10, 0.0),
    FieldSchema("snode", int, 70, 10, 0),
)

_DEFINEPRESSURETUBE_CARD2 = (
    FieldSchema("nshl", int, 0, 10, 12),
    FieldSchema("elform", int, 10, 10, 16),
    FieldSchema("nip", int, 20, 10, 3),
    FieldSchema("shrf", float, 30, 10, 1.0),
    FieldSchema("bpid", int, 40, 10, None),
    FieldSchema("isave", int, 50, 10, 0),
    FieldSchema("iorien", int, 60, 10, 0),
)

_DEFINEPRESSURETUBE_CARD3 = (
    FieldSchema("nsld", int, 0, 10, 12),
    FieldSchema("elform", int, 10, 10, 1),
    FieldSchema("nthk", int, 20, 10, 3),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("bpid", int, 40, 10, None),
    FieldSchema("isave", int, 50, 10, 0),
    FieldSchema("iorien", int, 60, 10, 0),
)

_DEFINEPRESSURETUBE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefinePressureTube(KeywordBase):
    """DYNA DEFINE_PRESSURE_TUBE keyword"""

    keyword = "DEFINE"
    subkeyword = "PRESSURE_TUBE"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefinePressureTube class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEPRESSURETUBE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEPRESSURETUBE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEPRESSURETUBE_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINEPRESSURETUBE_CARD3,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefinePressureTube._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEPRESSURETUBE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def pid(self) -> int:
        """Get or set the Part ID of the tube. All connected beam elements in the part will model a tube. Only tubular beam elements are allowed, that is, ELFORM = 1, 4, 5, and 11 with CST = 1 on *SECTION_BEAM.  The initial tube cross-sectional area is calculated using the beam inner diameter TT1/TT2 fields in the *SECTION_BEAM keyword. The outer diameter TS1/TS2 fields in the *SECTION_BEAM keyword are used if no inner diameter is given.The beam elements may not contain junctions.Two different parts on which *DEFINE_PRESSURE_TUBE is defined may not share nodes.For MPP, all elements in the part will be on a single processor.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ws(self) -> float:
        """Get or set the Wave propagation speed.
        """ # nopep8
        return self._cards[0].get_value("ws")

    @ws.setter
    def ws(self, value: float) -> None:
        """Set the ws property."""
        self._cards[0].set_value("ws", value)

    @property
    def pr(self) -> float:
        """Get or set the Initial tube pressure.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def mtd(self) -> int:
        """Get or set the Solution method:
        EQ.0: Standard Galerkin FEM.
        EQ.1: Discontinuous Galerkin
        EQ.2: Discontinuous Galerkin on isothermal Euler equations
        """ # nopep8
        return self._cards[0].get_value("mtd")

    @mtd.setter
    def mtd(self, value: int) -> None:
        """Set the mtd property."""
        self._cards[0].set_value("mtd", value)

    @property
    def type(self) -> int:
        """Get or set the Tube elements:
        Q.0:	The tube is entirely simulated with beam elements. The contact penetration of the beam elements gives the cross-sectional area. Contact stiffness governs the mechanical response in the radial direction of the beam elements. Only mortar contacts are supported.
        EQ.1:	The tube is simulated by the automatic generation of shell elements, which are assigned the beam part ID and the beam material model.A new part ID is given to the beam elements, which are no longer part of the mechanical solution.Contacts and other properties associated with the old beam part ID apply to the new shell part.The shell element nodes give the cross - sectional area.The shells entirely govern the mechanical response.All contact definitions are supported.Constraints defined by * BOUNDARY_SPC,* BOUNDARY_PRESCRIBED_MOTION,* CONSTRAINED_EXTRA_NODES, and *CONSTRAINED_NODAL_RIGID_BODY and nodes that are shared with a rigid body are moved to the new shell tube.
        EQ.2 : The tube is simulated by the automatic generation of solid elements, similarly to TYPE = 1 above.
        LT.0 : Automatic generation of elements as above, but the beam nodes are given new nodal IDs.The old beam NIDs are moved to the automatically generated tube(one row of nodes along the length).Any nodal constraints thus apply to the new tube instead of the beam element tube.See Figure 0 - 1 for an example of different values of TYPE and how they affect nodal constraints.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[0].set_value("type", value)

    @property
    def gamma(self) -> float:
        """Get or set the Adiabatic index 1, only used for MTD.EQ.2
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def cfac(self) -> float:
        """Get or set the Cavity correction factor; see Remark 5. This post-processing correction overwrites the pressure on cavity interface nodes (for CAVi != 0 on Card 2).
        """ # nopep8
        return self._cards[0].get_value("cfac")

    @cfac.setter
    def cfac(self, value: float) -> None:
        """Set the cfac property."""
        self._cards[0].set_value("cfac", value)

    @property
    def cutoff(self) -> int:
        """Get or set the Flag for enabling tube cutoff:
        EQ.0:	Inactive
        EQ.1 : Active
        When a part of the tube is fully compressed, air can no longer pass through, and the equations become invalid.When CUTOFF = 0, this issue is avoided by assuming that the minimum tube cross - sectional area under compression is a small fraction(1 %) of the initial area for stability reasons.This assumption makes the equations valid but can lead to small pressure waves passing through a closed - off tube.With CUTOFF = 1, nodes are temporarily removed from the simulation if the area is less than 1 % of the initial area and if the flow velocity is sufficiently slow(0.1 % of WS).Thus, the solver is not used in the fully compressed area at all.Waves cannot pass through that part of the tube and instead are reflected with a reflective boundary condition.
        """ # nopep8
        return self._cards[0].get_value("cutoff")

    @cutoff.setter
    def cutoff(self, value: int) -> None:
        """Set the cutoff property."""
        self._cards[0].set_value("cutoff", value)

    @property
    def visc(self) -> float:
        """Get or set the MTD.EQ.0: Artificial viscosity multiplier (VISC > 0.0); see Remark 2. A smaller value gives a more resolved pulse at shorter wavelengths but may lead to instabilities. We recommend the default value for typical automotive crash applications (tube length ~2m, diameter ~5mm, pressure pulse width ~5ms).
        MTD.GT.0: Slope limiter smoothing factor; see Remark 2. Smaller values give a more resolved pulse at shorter wavelengths but may lead to instabilities.Larger values lead to a smeared pulse.
        """ # nopep8
        return self._cards[1].get_value("visc")

    @visc.setter
    def visc(self, value: float) -> None:
        """Set the visc property."""
        self._cards[1].set_value("visc", value)

    @property
    def cfl(self) -> float:
        """Get or set the Stability factor (CFL>0.0); see Remark 2. A smaller value increases stability at the expense of increased computational cost.
        For typical automotive crash applications, the default value is recommended.
        """ # nopep8
        return self._cards[1].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        """Set the cfl property."""
        self._cards[1].set_value("cfl", value)

    @property
    def damp(self) -> float:
        """Get or set the Linear damping (DAMP0.0); see Remark 1.
        """ # nopep8
        return self._cards[1].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[1].set_value("damp", value)

    @property
    def bndl(self) -> float:
        """Get or set the Left boundary condition (0  BNDi  1); see Remark 2. Special cases are:
        EQ.0.0: closed tube end, that is, zero velocity boundary condition
        EQ:0.5: non-reflecting boundary condition
        EQ:1.0: open tube end, that is, constant pressure boundary condition
        Left tube end is automatically assigned to the lowest/highest beam node number on the tube, respectively.
        """ # nopep8
        return self._cards[1].get_value("bndl")

    @bndl.setter
    def bndl(self, value: float) -> None:
        """Set the bndl property."""
        self._cards[1].set_value("bndl", value)

    @property
    def bndr(self) -> float:
        """Get or set the Right boundary condition (0  BNDi  1); see Remark 2. Special cases are:
        EQ.0.0: closed tube end, that is, zero velocity boundary condition
        EQ:0.5: non-reflecting boundary condition
        EQ:1.0: open tube end, that is, constant pressure boundary condition
        Right tube end is automatically assigned to the lowest/highest beam node number on the tube, respectively.
        """ # nopep8
        return self._cards[1].get_value("bndr")

    @bndr.setter
    def bndr(self, value: float) -> None:
        """Set the bndr property."""
        self._cards[1].set_value("bndr", value)

    @property
    def cavl(self) -> float:
        """Get or set the Left cavity; see Remark 4.
        GT.0.0: A cavity replaces the elements near the end of the tube.
        The integer part of CAVi determines the number of beam elements that belong to the cavity.The remainder of CAVi determines the boundary condition on the interface between the tube and the cavity.
        LT:0.0: A cavity extends the tube by adding new beam elements.
        The length of the added cavity is given by  where  truncates the decimal portion of  (leaving an integer).
        The remainder of  determines the boundary condition on the interface between the tube and the cavity.
        """ # nopep8
        return self._cards[1].get_value("cavl")

    @cavl.setter
    def cavl(self, value: float) -> None:
        """Set the cavl property."""
        self._cards[1].set_value("cavl", value)

    @property
    def cavr(self) -> float:
        """Get or set the Right cavity; see Remark 3.
        GT.0.0: elements near the end of the tube are replaced with a cavity.
        The integer part of CAVi determines the number of beam elements that belong to the cavity.
        The remainder of CAVi determines the boundary condition on the interface between the tube and the cavity.
        LT:0.0: the tube is extended with a cavity by adding new beam elements.
        The length of the added cavity is given by  where  truncates the decimal portion of  (leaving an integer).
        The remainder of  determines the boundary condition on the interface between the tube and the cavity.
        """ # nopep8
        return self._cards[1].get_value("cavr")

    @cavr.setter
    def cavr(self, value: float) -> None:
        """Set the cavr property."""
        self._cards[1].set_value("cavr", value)

    @property
    def snode(self) -> int:
        """Get or set the Optional starting node. This node determines the left end of the tube. If not set, the tube starts at the lowest numbered beam node
        """ # nopep8
        return self._cards[1].get_value("snode")

    @snode.setter
    def snode(self, value: int) -> None:
        """Set the snode property."""
        self._cards[1].set_value("snode", value)

    @property
    def nshl(self) -> int:
        """Get or set the Number of automatically generated shells/solids on the circumference of the tube
        """ # nopep8
        return self._cards[2].get_value("nshl")

    @nshl.setter
    def nshl(self, value: int) -> None:
        """Set the nshl property."""
        self._cards[2].set_value("nshl", value)

    @property
    def elform(self) -> int:
        """Get or set the ELFORM for automatically generated shells/solids; see *SECTION_SHELL/SOLID.
        """ # nopep8
        return self._cards[2].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        self._cards[2].set_value("elform", value)

    @property
    def nip(self) -> int:
        """Get or set the Number of through thickness integration points for automatically generated shells; see NIP in *SECTION_SHELL.
        """ # nopep8
        return self._cards[2].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        """Set the nip property."""
        self._cards[2].set_value("nip", value)

    @property
    def shrf(self) -> float:
        """Get or set the Shear correction factor for automatically generated shells; see SHRF in *SECTION_SHELL
        """ # nopep8
        return self._cards[2].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[2].set_value("shrf", value)

    @property
    def bpid(self) -> typing.Optional[int]:
        """Get or set the Optional PID given to beam elements when automatically generating shells/solids.
        """ # nopep8
        return self._cards[2].get_value("bpid")

    @bpid.setter
    def bpid(self, value: int) -> None:
        """Set the bpid property."""
        self._cards[2].set_value("bpid", value)

    @property
    def isave(self) -> int:
        """Get or set the Save shell/solid geometry and connectivity to a keyword file:
        EQ.0: No saving(default)
        EQ.1: Save to keyword file prtube.k.
        Subsequent runs can include the generated file, in which case TYPEand Card 3 are ignored for saved tubes.Boundary conditionsand contacts are not saved.Thus, beam boundary conditionsand contacts are not transferred to the saved shell / solid tubes in subsequent runs.Therefore, boundary conditions, contacts, etc., must still be supplied to the saved shell / solid geometry.
        """ # nopep8
        return self._cards[2].get_value("isave")

    @isave.setter
    def isave(self, value: int) -> None:
        """Set the isave property."""
        if value not in [0, 1, None]:
            raise Exception("""isave must be `None` or one of {0,1}.""")
        self._cards[2].set_value("isave", value)

    @property
    def iorien(self) -> int:
        """Get or set the Control circumferential orientation of automatically generated shells/solids:
        EQ.0: The global coordinate system is used for orientation of the first end segment.Subsequent segments will be created to minimize twisting along the length of the tube. (default)
        EQ.1: Each segment will be oriented using the  third beam node, if it exists.Otherwise, each segment will be oriented using the vector from* ELEMENT_BEAM_ORIENTATION.
        """ # nopep8
        return self._cards[2].get_value("iorien")

    @iorien.setter
    def iorien(self, value: int) -> None:
        """Set the iorien property."""
        if value not in [0, 1, None]:
            raise Exception("""iorien must be `None` or one of {0,1}.""")
        self._cards[2].set_value("iorien", value)

    @property
    def nsld(self) -> int:
        """Get or set the Number of automatically generated shells/solids on the circumference of the tube
        """ # nopep8
        return self._cards[3].get_value("nsld")

    @nsld.setter
    def nsld(self, value: int) -> None:
        """Set the nsld property."""
        self._cards[3].set_value("nsld", value)

    @property
    def elform(self) -> int:
        """Get or set the ELFORM for automatically generated shells/solids; see *SECTION_SHELL/SOLID.
        """ # nopep8
        return self._cards[3].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        """Set the elform property."""
        self._cards[3].set_value("elform", value)

    @property
    def nthk(self) -> int:
        """Get or set the Number of solid elements in the thickness of the tube for automatically generated solids.
        """ # nopep8
        return self._cards[3].get_value("nthk")

    @nthk.setter
    def nthk(self, value: int) -> None:
        """Set the nthk property."""
        self._cards[3].set_value("nthk", value)

    @property
    def bpid(self) -> typing.Optional[int]:
        """Get or set the Optional PID given to the beam elements when automatically generating shells/solids.
        """ # nopep8
        return self._cards[3].get_value("bpid")

    @bpid.setter
    def bpid(self, value: int) -> None:
        """Set the bpid property."""
        self._cards[3].set_value("bpid", value)

    @property
    def isave(self) -> int:
        """Get or set the Save shell/solid geometry and connectivity to a keyword file:
        EQ.0: No saving(default)
        EQ.1: Save to keyword file prtube.k.
        Subsequent runs can include the generated file, in which case TYPEand Card 3 are ignored for saved tubes.Boundary conditionsand contacts are not saved.Thus, beam boundary conditionsand contacts are not transferred to the saved shell / solid tubes in subsequent runs.Therefore, boundary conditions, contacts, etc., must still be supplied to the saved shell / solid geometry.
        """ # nopep8
        return self._cards[3].get_value("isave")

    @isave.setter
    def isave(self, value: int) -> None:
        """Set the isave property."""
        if value not in [0, 1, None]:
            raise Exception("""isave must be `None` or one of {0,1}.""")
        self._cards[3].set_value("isave", value)

    @property
    def iorien(self) -> int:
        """Get or set the Control circumferential orientation of automatically generated shells/solids:
        EQ.0: The global coordinate system is used for orientation of the first end segment.Subsequent segments will be created to minimize twisting along the length of the tube. (default)
        EQ.1: Each segment will be oriented using the  third beam node, if it exists.Otherwise, each segment will be oriented using the vector from* ELEMENT_BEAM_ORIENTATION.
        """ # nopep8
        return self._cards[3].get_value("iorien")

    @iorien.setter
    def iorien(self, value: int) -> None:
        """Set the iorien property."""
        if value not in [0, 1, None]:
            raise Exception("""iorien must be `None` or one of {0,1}.""")
        self._cards[3].set_value("iorien", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

