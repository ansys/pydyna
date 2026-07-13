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

"""Module providing the ControlContact class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLCONTACT_CARD0 = (
    FieldSchema("slsfac", float, 0, 10, 0.1),
    FieldSchema("rwpnal", float, 10, 10, None),
    FieldSchema("islchk", int, 20, 10, 1),
    FieldSchema("shlthk", int, 30, 10, 0),
    FieldSchema("penopt", int, 40, 10, 1),
    FieldSchema("thkchg", int, 50, 10, 0),
    FieldSchema("orien", int, 60, 10, 1),
    FieldSchema("enmass", int, 70, 10, 0),
)

_CONTROLCONTACT_CARD1 = (
    FieldSchema("usrstr", int, 0, 10, 0),
    FieldSchema("usrfrc", int, 10, 10, 0),
    FieldSchema("nsbcs", int, 20, 10, 0),
    FieldSchema("interm", int, 30, 10, 0),
    FieldSchema("xpene", float, 40, 10, 4.0),
    FieldSchema("ssthk", int, 50, 10, 0),
    FieldSchema("ecdt", int, 60, 10, 0),
    FieldSchema("tiedprj", int, 70, 10, 0),
)

_CONTROLCONTACT_CARD2 = (
    FieldSchema("sfric", float, 0, 10, 0.0),
    FieldSchema("dfric", float, 10, 10, 0.0),
    FieldSchema("edc", float, 20, 10, 0.0),
    FieldSchema("vfc", float, 30, 10, 0.0),
    FieldSchema("th", float, 40, 10, 0.0),
    FieldSchema("th_sf", float, 50, 10, 0.0),
    FieldSchema("pen_sf", float, 60, 10, 0.0),
    FieldSchema("ptscl", float, 70, 10, 1.0),
)

_CONTROLCONTACT_CARD3 = (
    FieldSchema("ignore", int, 0, 10, 0),
    FieldSchema("frceng", int, 10, 10, 0),
    FieldSchema("skiprwg", int, 20, 10, 0),
    FieldSchema("outseg", int, 30, 10, 0),
    FieldSchema("spotstp", int, 40, 10, 0),
    FieldSchema("spotdel", int, 50, 10, 0),
    FieldSchema("spothin", float, 60, 10, None),
    FieldSchema("dir_tie", int, 70, 10, None),
)

_CONTROLCONTACT_CARD4 = (
    FieldSchema("isym", int, 0, 10, 0),
    FieldSchema("nserod", int, 10, 10, 0),
    FieldSchema("rwgaps", int, 20, 10, 1),
    FieldSchema("rwgdth", float, 30, 10, 0.0),
    FieldSchema("rwksf", float, 40, 10, 1.0),
    FieldSchema("icov", int, 50, 10, 0),
    FieldSchema("swradf", float, 60, 10, 0.0),
    FieldSchema("ithoff", int, 70, 10, 0),
)

_CONTROLCONTACT_CARD5 = (
    FieldSchema("shledg", int, 0, 10, 0),
    FieldSchema("pstiff", int, 10, 10, 0),
    FieldSchema("ithcnt", int, 20, 10, 0),
    FieldSchema("tdcnof", int, 30, 10, 0),
    FieldSchema("ftall", int, 40, 10, 0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("shltrw", float, 60, 10, 0.0),
    FieldSchema("igactc", int, 70, 10, 0),
)

_CONTROLCONTACT_CARD6 = (
    FieldSchema("irevspt", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("cohtiem", int, 20, 10, 0),
    FieldSchema("tieopt", int, 30, 10, 0),
    FieldSchema("strobj", int, 40, 10, 0),
    FieldSchema("befblk", float, 50, 10, 0.0),
)

class ControlContact(KeywordBase):
    """DYNA CONTROL_CONTACT keyword"""

    keyword = "CONTROL"
    subkeyword = "CONTACT"

    def __init__(self, **kwargs):
        """Initialize the ControlContact class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLCONTACT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLCONTACT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLCONTACT_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLCONTACT_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLCONTACT_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLCONTACT_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CONTROLCONTACT_CARD6,
                **kwargs,
            ),
        ]
    @property
    def slsfac(self) -> float:
        """Get or set the Scale factor for sliding interface penalties (default = 0.1)
        """ # nopep8
        return self._cards[0].get_value("slsfac")

    @slsfac.setter
    def slsfac(self, value: float) -> None:
        """Set the slsfac property."""
        self._cards[0].set_value("slsfac", value)

    @property
    def rwpnal(self) -> typing.Optional[float]:
        """Get or set the Scale factor for rigid wall penalties (see *RIGIDWALL) that treats nodal points interacting with rigid walls.  The penalties are set so that an absolute value of unity should be optimal; however, this penalty value may be very problem-dependent.  If rigid/deformable materials switching is used, this option should be used if the switched materials interact with rigid walls.
        If you have IGA parts in your model, see Remark 10.
        LT.0.0:	All nodes are treated by the penalty method.This is set to - 1.0 for implicit calculations.Since seven(7) variables are stored for each possible tracked node(see NSID on * RIGIDWALL_?PLANAR / GEOMETRIC), only the nodes that may interact with the wall should be included in the node list.
        EQ.0.0 : The constraint method is used, and nodal points that belong to rigid bodies are not considered.
        GT.0.0 : Rigid body nodes are treated by the penalty method, and all other nodes are treated by the constraint method.
        """ # nopep8
        return self._cards[0].get_value("rwpnal")

    @rwpnal.setter
    def rwpnal(self, value: float) -> None:
        """Set the rwpnal property."""
        self._cards[0].set_value("rwpnal", value)

    @property
    def islchk(self) -> int:
        """Get or set the Initial penetration check in contact surfaces.
        EQ.1: no checking,
        EQ.2: full check of initial penetration is performed.
        """ # nopep8
        return self._cards[0].get_value("islchk")

    @islchk.setter
    def islchk(self, value: int) -> None:
        """Set the islchk property."""
        if value not in [1, 0, 2, None]:
            raise Exception("""islchk must be `None` or one of {1,0,2}.""")
        self._cards[0].set_value("islchk", value)

    @property
    def shlthk(self) -> int:
        """Get or set the Shell thickness considered in type surface to surface and node to surface type contact options, where options 1 and 2 below activate the new contact algorithms.
        EQ.0: thickness is not considered,
        EQ.1: thickness is considered but rigid bodies are excluded,
        EQ.2: thickness is considered including rigid bodies.
        """ # nopep8
        return self._cards[0].get_value("shlthk")

    @shlthk.setter
    def shlthk(self, value: int) -> None:
        """Set the shlthk property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""shlthk must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("shlthk", value)

    @property
    def penopt(self) -> int:
        """Get or set the Penalty stiffness value option (applies to the Standard Penalty Formulation and the Soft Constraint Penalty Formulation, that is, SOFT = 0 and 1 on *CONTACT_?OPTION).  For the default calculation of the penalty value, please refer to the LS-DYNA Theory Manual.
        EQ.1:	Minimum of reference segment and tracked node(default for most contact types)
        EQ.2 : Use the reference segment stiffness(old way).
        EQ.3 : Use the tracked node value.
        EQ.4 : Use the tracked node value, area or mass weighted.
        EQ.5 : Same as 4 but inversely proportional to the shell thickness.This may require special scaling and is not generally recommended.
        PENOPT = 4 and 5 can be used for metal forming calculations.In general, PENOPT = 2 - 5 should be avoided if both the tracked nodes and the reference segments belong to deformable parts.
        """ # nopep8
        return self._cards[0].get_value("penopt")

    @penopt.setter
    def penopt(self, value: int) -> None:
        """Set the penopt property."""
        if value not in [1, 0, 2, 3, 4, 5, None]:
            raise Exception("""penopt must be `None` or one of {1,0,2,3,4,5}.""")
        self._cards[0].set_value("penopt", value)

    @property
    def thkchg(self) -> int:
        """Get or set the Shell thickness changes considered in single surface contact:
        EQ.0: no consideration (default),
        EQ.1: shell thickness changes are included.
        EQ.2: Applies to MPP only. Shell thickness changes are included, but a different algorithm is used than for THKCHG = 1. This method is more consistent with the way the initial contact thickness is computed.
        """ # nopep8
        return self._cards[0].get_value("thkchg")

    @thkchg.setter
    def thkchg(self, value: int) -> None:
        """Set the thkchg property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""thkchg must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("thkchg", value)

    @property
    def orien(self) -> int:
        """Get or set the Optional automatic reorientation of contact interface segments during initialization:
        EQ.0: default is set to 1.
        EQ.1: active for automated (part) input only. Contact surfaces are given by *PART definitions.
        EQ.2: active for manual (segment) and automated (part) input.
        EQ.3: inactive for non-forming contact.
        EQ.4: inactive for froming contact.
        """ # nopep8
        return self._cards[0].get_value("orien")

    @orien.setter
    def orien(self, value: int) -> None:
        """Set the orien property."""
        if value not in [1, 0, 2, 3, 4, None]:
            raise Exception("""orien must be `None` or one of {1,0,2,3,4}.""")
        self._cards[0].set_value("orien", value)

    @property
    def enmass(self) -> int:
        """Get or set the Flag for treatment of eroded nodes in contact.  An eroded node is a node that is no longer attached to any element after an element deletion.  ENMASS is not supported by all contact types; it is suggested that the user toggle on �Show Deleted Nodes� in LS-PrePost when postprocessing to display eroded nodes as particles, and in so doing, determine if ENMASS affects the contact behavior.  ENMASS is not supported when SOFT = 2 on Optional Card A of *CONTACT.
        EQ.0:	Eroded nodes are not considered in the contact algorithm.
        EQ.1 : Eroded nodes of solid elements remain active in the contact algorithm.
        EQ.2 : Eroded nodes of solid and shell elements remain active in the contact algorithm.
        """ # nopep8
        return self._cards[0].get_value("enmass")

    @enmass.setter
    def enmass(self, value: int) -> None:
        """Set the enmass property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""enmass must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("enmass", value)

    @property
    def usrstr(self) -> int:
        """Get or set the Storage per contact interface for user supplied interface control subroutine.  If zero, no input data is read.
        """ # nopep8
        return self._cards[1].get_value("usrstr")

    @usrstr.setter
    def usrstr(self, value: int) -> None:
        """Set the usrstr property."""
        self._cards[1].set_value("usrstr", value)

    @property
    def usrfrc(self) -> int:
        """Get or set the Storage per contact interface for user supplied interface friction subroutine. If zero, no input data is read.
        """ # nopep8
        return self._cards[1].get_value("usrfrc")

    @usrfrc.setter
    def usrfrc(self, value: int) -> None:
        """Set the usrfrc property."""
        self._cards[1].set_value("usrfrc", value)

    @property
    def nsbcs(self) -> int:
        """Get or set the Number of cycles between contact searching using three-dimensional bucket searches.  Using the default value for this field is strongly recommended.  For mortar contact (option MORTAR on the CONTACT card), the default is 100. For MPP, this field is ignored when SOFT = 0 and 1, and only BCKT on MPP 1 of *CONTACT_OPTION_... applies in those cases.
        """ # nopep8
        return self._cards[1].get_value("nsbcs")

    @nsbcs.setter
    def nsbcs(self, value: int) -> None:
        """Set the nsbcs property."""
        self._cards[1].set_value("nsbcs", value)

    @property
    def interm(self) -> int:
        """Get or set the Flag for intermittent searching in old surface to surface contact using the interval specified as NSBCS above:
        EQ.0: off,
        EQ.1: on.
        """ # nopep8
        return self._cards[1].get_value("interm")

    @interm.setter
    def interm(self, value: int) -> None:
        """Set the interm property."""
        if value not in [0, 1, None]:
            raise Exception("""interm must be `None` or one of {0,1}.""")
        self._cards[1].set_value("interm", value)

    @property
    def xpene(self) -> float:
        """Get or set the Contact surface maximum penetration check multiplier
        """ # nopep8
        return self._cards[1].get_value("xpene")

    @xpene.setter
    def xpene(self, value: float) -> None:
        """Set the xpene property."""
        self._cards[1].set_value("xpene", value)

    @property
    def ssthk(self) -> int:
        """Get or set the Flag for using actual shell thickness in single surface contact logic-types 4,13,15 and 26.
        EQ.0: Actual shell thickness is not used in the contacts (default),
        EQ.1: Actual shell thickness is used in the contacts.
        """ # nopep8
        return self._cards[1].get_value("ssthk")

    @ssthk.setter
    def ssthk(self, value: int) -> None:
        """Set the ssthk property."""
        if value not in [0, 1, None]:
            raise Exception("""ssthk must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ssthk", value)

    @property
    def ecdt(self) -> int:
        """Get or set the Time step size override for eroding contact:
        EQ.0: contact time size may control Dt.
        EQ.1: contact is not considered in Dt determination.
        """ # nopep8
        return self._cards[1].get_value("ecdt")

    @ecdt.setter
    def ecdt(self, value: int) -> None:
        """Set the ecdt property."""
        if value not in [0, 1, None]:
            raise Exception("""ecdt must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ecdt", value)

    @property
    def tiedprj(self) -> int:
        """Get or set the Bypass projection of slave nodes to master surface in types:
        *CONTACT_TIED_NODES_TO_SURFACE, *CONTACT_TIED_SHELL_EDGE_TO_SURFACE, and, *CONTACT_TIED_SURFACE_TO_SURFACE tied interface options:
        EQ.0: eliminate gaps by projection nodes,
        EQ.1: bypass projection.
        """ # nopep8
        return self._cards[1].get_value("tiedprj")

    @tiedprj.setter
    def tiedprj(self, value: int) -> None:
        """Set the tiedprj property."""
        if value not in [0, 1, None]:
            raise Exception("""tiedprj must be `None` or one of {0,1}.""")
        self._cards[1].set_value("tiedprj", value)

    @property
    def sfric(self) -> float:
        """Get or set the Default static coefficient of friction (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("sfric")

    @sfric.setter
    def sfric(self, value: float) -> None:
        """Set the sfric property."""
        self._cards[2].set_value("sfric", value)

    @property
    def dfric(self) -> float:
        """Get or set the Default dynamic coefficient of friction (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("dfric")

    @dfric.setter
    def dfric(self, value: float) -> None:
        """Set the dfric property."""
        self._cards[2].set_value("dfric", value)

    @property
    def edc(self) -> float:
        """Get or set the Default exponential decay coefficient (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("edc")

    @edc.setter
    def edc(self, value: float) -> None:
        """Set the edc property."""
        self._cards[2].set_value("edc", value)

    @property
    def vfc(self) -> float:
        """Get or set the Default viscous friction coefficient (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("vfc")

    @vfc.setter
    def vfc(self, value: float) -> None:
        """Set the vfc property."""
        self._cards[2].set_value("vfc", value)

    @property
    def th(self) -> float:
        """Get or set the Default contact thickness (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("th")

    @th.setter
    def th(self, value: float) -> None:
        """Set the th property."""
        self._cards[2].set_value("th", value)

    @property
    def th_sf(self) -> float:
        """Get or set the Default thickness scale factor (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("th_sf")

    @th_sf.setter
    def th_sf(self, value: float) -> None:
        """Set the th_sf property."""
        self._cards[2].set_value("th_sf", value)

    @property
    def pen_sf(self) -> float:
        """Get or set the Default local penalty scale factor (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("pen_sf")

    @pen_sf.setter
    def pen_sf(self, value: float) -> None:
        """Set the pen_sf property."""
        self._cards[2].set_value("pen_sf", value)

    @property
    def ptscl(self) -> float:
        """Get or set the Scale factor on the contact stress exerted onto shells formulations 25, 26, and 27.  When DOF = 3 the scale factor also applies to shell formulations 2, 4,and 16.
        """ # nopep8
        return self._cards[2].get_value("ptscl")

    @ptscl.setter
    def ptscl(self, value: float) -> None:
        """Set the ptscl property."""
        self._cards[2].set_value("ptscl", value)

    @property
    def ignore(self) -> int:
        """Get or set the Ignore initial penetrations for the *CONTACT_?AUTOMATIC options.  In the SMP contact, this flag is not implemented for the AUTOMATIC_?GENERAL option.  �Initial� in this context refers to the first time step that a penetration is encountered.  This option can also be specified for each interface on Optional Card C of *CONTACT_....  The value defined here will be the default.
        EQ.0:	Move nodes to eliminate initial penetrations in the model definition.
        EQ.1 : Allow initial penetrations to exist by tracking the initial penetrations.
        EQ.2 : Allow initial penetrations to exist by tracking the initial penetrations.However, penetration warning messages are printed with the original coordinates and the recommended coordinates for each penetrating node given.
        """ # nopep8
        return self._cards[3].get_value("ignore")

    @ignore.setter
    def ignore(self, value: int) -> None:
        """Set the ignore property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ignore must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("ignore", value)

    @property
    def frceng(self) -> int:
        """Get or set the Flag to activate the calculation of frictional sliding energy:
        EQ.0: do not calculate,
        EQ.1: calculation frictional energy in contact.
        """ # nopep8
        return self._cards[3].get_value("frceng")

    @frceng.setter
    def frceng(self, value: int) -> None:
        """Set the frceng property."""
        if value not in [0, 1, None]:
            raise Exception("""frceng must be `None` or one of {0,1}.""")
        self._cards[3].set_value("frceng", value)

    @property
    def skiprwg(self) -> int:
        """Get or set the Flag not to a display stationary rigid wall by default.
        EQ.0: generate four extra nodes and one shell element to visulize stationary planar rigid wall.
        EQ.1: do not generate stationary rigid wall.
        """ # nopep8
        return self._cards[3].get_value("skiprwg")

    @skiprwg.setter
    def skiprwg(self, value: int) -> None:
        """Set the skiprwg property."""
        if value not in [0, 1, None]:
            raise Exception("""skiprwg must be `None` or one of {0,1}.""")
        self._cards[3].set_value("skiprwg", value)

    @property
    def outseg(self) -> int:
        """Get or set the Flag to output each spot weld slave node and its master segment for contact type: *CONTACT_SPOTWELD into the D3HSP file.
        EQ.0: no, do not write out this information.
        EQ.1: yes, write out this information.
        """ # nopep8
        return self._cards[3].get_value("outseg")

    @outseg.setter
    def outseg(self, value: int) -> None:
        """Set the outseg property."""
        if value not in [0, 1, None]:
            raise Exception("""outseg must be `None` or one of {0,1}.""")
        self._cards[3].set_value("outseg", value)

    @property
    def spotstp(self) -> int:
        """Get or set the If a spot weld node (related to a *MAT_SPOTWELD beam) cannot be fouind on a master segment, should an error termination occur
        EQ.0: no, print warning message and continue calculation.
        EQ.1: yes, print error message and terminate.
        EQ.2: no, delete the weld, print a message, and continue,
        EQ.3: no, keep the weld: (This is not recommended as it can lead to instabilities.)
        """ # nopep8
        return self._cards[3].get_value("spotstp")

    @spotstp.setter
    def spotstp(self, value: int) -> None:
        """Set the spotstp property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""spotstp must be `None` or one of {0,1,2,3}.""")
        self._cards[3].set_value("spotstp", value)

    @property
    def spotdel(self) -> int:
        """Get or set the This option controls the behavior of spot welds when the parent element erodes.  When SPOTDEL is set to 1, the beam or solid spot weld is deleted, and the tied constraint is removed when the parent element erodes.  The parent element is the element to which the SURFA node is attached using the TIED interface.  This option also works for SPRs, namely, they automatically fail if at least one of the parent elements fails.  To avoid instabilities, this option should be set to 1 whenever the parent element is expected to erode.
        EQ.0:	Do not delete the spot weld beam, solid element, or SPR.
        EQ.1 : Delete the spot weld elements or SPRs when the attached shells on one side of the element fail.
        GT.1 : Delete the SPR when SPOTDEL nodes are attached to failed elements in the search radius.
        On vector processors, this option can significantly slow down the calculation if many weld elements fail since the vector lengths are reduced.On non - vector processors, the cost penalty is minimal.
        """ # nopep8
        return self._cards[3].get_value("spotdel")

    @spotdel.setter
    def spotdel(self, value: int) -> None:
        """Set the spotdel property."""
        self._cards[3].set_value("spotdel", value)

    @property
    def spothin(self) -> typing.Optional[float]:
        """Get or set the Optional thickness scale factor. If active, define a factor greater than zero, but less than one. Premature failure of spot welds can occur
        due to contact of the spot welded parts in the vicinity of the spot weld. This contact creates tensile forces in the spot weld.
        Although this may seem physical, the compressive forces generated in the contact are large enough to fail the weld in tension before failure is observed in an
        experimental test. With this option, the thickness of the parts in the vicinity of the weld is automatically scaled, the contact forces do not develop, and the problem is avoided.
        We recommend setting the IGNORE option to 1 or 2 if SPOTHIN is active. In MPP, this option applies to all non-Mortar contacts that have SINGLE_SURFACE, AUTOMATIC_GENERAL,
        SURFACE_TO_SURFACE, or NODES_TO_SURFACE in the name. In SMP it only applies to the AUTOMATIC_SINGLE_SURFACE option. See Remark 5.
        """ # nopep8
        return self._cards[3].get_value("spothin")

    @spothin.setter
    def spothin(self, value: float) -> None:
        """Set the spothin property."""
        self._cards[3].set_value("spothin", value)

    @property
    def dir_tie(self) -> typing.Optional[int]:
        """Get or set the Directional tie for MPP non-groupable tied contacts. If this flag is set to 1, then each node in the SURFA side of a tied contact is associated with outward normal vectors. A node belonging to the surface of a shell, solid, or thick shell surface is associated with one or more vectors depending on the angle between adjacent segments. Typically the node of a flat surface will have one normal vector, the node of an edge will have two normal vectors, and the node of a corner will have three normal vectors. Nodes on a cylinder may have one or two normal vectors, depending on the mesh resolution of the faceted surface. When deciding which SURFB segment to tie to, the algorithm gives preference to those segments for which the direction from the SURFA node to the SURFB segment has a positive dot product with at least one of the aforementioned normal vectors. Therefore, a SURFA node will not necessarily tie to the closest SURFB segment, but to the correct segment. This feature avoids nonphysical tie situations or even zero solid element volumes as a result.
        """ # nopep8
        return self._cards[3].get_value("dir_tie")

    @dir_tie.setter
    def dir_tie(self, value: int) -> None:
        """Set the dir_tie property."""
        self._cards[3].set_value("dir_tie", value)

    @property
    def isym(self) -> int:
        """Get or set the Symmetry plane default for automatic segment generation when contact is defined by part IDs:
        LT.0: is a node set on the symmetry boundary, supported and recommended for Mortar contact.
        This will allow for a correct treatment of segments close to the symmetry face/edge. See Remark 8
        EQ.0: Off.
        EQ.1: do not include faces whith normal boundary constraints ( e.g. segments of brick elements on a symmetry plane.
        """ # nopep8
        return self._cards[4].get_value("isym")

    @isym.setter
    def isym(self, value: int) -> None:
        """Set the isym property."""
        self._cards[4].set_value("isym", value)

    @property
    def nserod(self) -> int:
        """Get or set the Flag to use one way node to surface erosion:
        EQ.0: use two-way algorithm
        EQ.1: use one-way algorithm.
        """ # nopep8
        return self._cards[4].get_value("nserod")

    @nserod.setter
    def nserod(self, value: int) -> None:
        """Set the nserod property."""
        if value not in [0, 1, None]:
            raise Exception("""nserod must be `None` or one of {0,1}.""")
        self._cards[4].set_value("nserod", value)

    @property
    def rwgaps(self) -> int:
        """Get or set the Flag to add rigid wall gap stiffness, see parameter RWGDTH below.
        EQ.1: add gap stiffness (default).
        EQ.2: do not add gap stiffness
        """ # nopep8
        return self._cards[4].get_value("rwgaps")

    @rwgaps.setter
    def rwgaps(self, value: int) -> None:
        """Set the rwgaps property."""
        if value not in [1, 2, None]:
            raise Exception("""rwgaps must be `None` or one of {1,2}.""")
        self._cards[4].set_value("rwgaps", value)

    @property
    def rwgdth(self) -> float:
        """Get or set the Death time for gap stiffness. After this time the gap stiffness is no longer added.
        """ # nopep8
        return self._cards[4].get_value("rwgdth")

    @rwgdth.setter
    def rwgdth(self, value: float) -> None:
        """Set the rwgdth property."""
        self._cards[4].set_value("rwgdth", value)

    @property
    def rwksf(self) -> float:
        """Get or set the Rigid wall penalty scale factor for contact with deformable parts during implicit calculations.  This value is independent of SLSFAC and RWPNAL. If RWKSF is also specified in *RIGIDWALL_PLANAR, the stiffness is scaled by the product of the two values..
        """ # nopep8
        return self._cards[4].get_value("rwksf")

    @rwksf.setter
    def rwksf(self, value: float) -> None:
        """Set the rwksf property."""
        self._cards[4].set_value("rwksf", value)

    @property
    def icov(self) -> int:
        """Get or set the Invokes the covariant formulation of Konyukhov and Schweizerhof in the FORMING contact option.
        """ # nopep8
        return self._cards[4].get_value("icov")

    @icov.setter
    def icov(self, value: int) -> None:
        """Set the icov property."""
        if value not in [0, 1, None]:
            raise Exception("""icov must be `None` or one of {0,1}.""")
        self._cards[4].set_value("icov", value)

    @property
    def swradf(self) -> float:
        """Get or set the Spot weld radius scale factor for neighbor segment thinning:EQ.0: Neighbor segments are not thinned(default).GT.0: The radius of a spot weld is scaled by SWRADF when searching for close neighbor segments to thin.
        """ # nopep8
        return self._cards[4].get_value("swradf")

    @swradf.setter
    def swradf(self, value: float) -> None:
        """Set the swradf property."""
        self._cards[4].set_value("swradf", value)

    @property
    def ithoff(self) -> int:
        """Get or set the Thermal contact heat transfer position.
        EQ.0 Heat transferred to mid plane in thick thermal shell elements.
        EQ.1 Heat transferred to outer surface on thick thermal shell elements
        """ # nopep8
        return self._cards[4].get_value("ithoff")

    @ithoff.setter
    def ithoff(self, value: int) -> None:
        """Set the ithoff property."""
        if value not in [0, 1, None]:
            raise Exception("""ithoff must be `None` or one of {0,1}.""")
        self._cards[4].set_value("ithoff", value)

    @property
    def shledg(self) -> int:
        """Get or set the Flag for assuming edge shape for shells when measuring penetration. This is available for segment-to-segment contact (see SOFT on *CONTACT)
        EQ.0: Shell edges are assumed round (default),
        EQ.1: Shell edges are assumed square and are flush with the nodes.
        """ # nopep8
        return self._cards[5].get_value("shledg")

    @shledg.setter
    def shledg(self, value: int) -> None:
        """Set the shledg property."""
        if value not in [0, 1, None]:
            raise Exception("""shledg must be `None` or one of {0,1}.""")
        self._cards[5].set_value("shledg", value)

    @property
    def pstiff(self) -> int:
        """Get or set the Flag to choose the method for calculating the penalty stiffness. This is available for segment-to-segment contact (see SOFT on *CONTACT)
        EQ.0: Based on material density and segment dimensions (default),
        EQ.1: Based on nodal masses or on material density and segment dimensions. The segment mass is taken as the larger of the two values calculated..
        """ # nopep8
        return self._cards[5].get_value("pstiff")

    @pstiff.setter
    def pstiff(self, value: int) -> None:
        """Set the pstiff property."""
        if value not in [0, 1, None]:
            raise Exception("""pstiff must be `None` or one of {0,1}.""")
        self._cards[5].set_value("pstiff", value)

    @property
    def ithcnt(self) -> int:
        """Get or set the Thermal contact heat transfer methodology
        LT.0: conduction evevenly distributed (pre R4)
        EQ.0: default set to 1
        EQ.1: conduction weighted by shape functions, reduced intergration
        EQ.2: conduction weighted by shape functions, full integration
        """ # nopep8
        return self._cards[5].get_value("ithcnt")

    @ithcnt.setter
    def ithcnt(self, value: int) -> None:
        """Set the ithcnt property."""
        self._cards[5].set_value("ithcnt", value)

    @property
    def tdcnof(self) -> int:
        """Get or set the Tied constraint offset contact update option.
        EQ.0: Update velocities and displacements from accelerations
        EQ.1: Update velocities and acclelerations from displacements. This noption is recommended only when there are large angle changes nwhere the default does not maintain a constant offset to a small ntolerance. This latter option is not as stable as the default and may require additional damping for stability. See *CONTROL_BULK_VISCOSITY and *DAMPING_PART_STIFFNESS.
        """ # nopep8
        return self._cards[5].get_value("tdcnof")

    @tdcnof.setter
    def tdcnof(self, value: int) -> None:
        """Set the tdcnof property."""
        if value not in [0, 1, None]:
            raise Exception("""tdcnof must be `None` or one of {0,1}.""")
        self._cards[5].set_value("tdcnof", value)

    @property
    def ftall(self) -> int:
        """Get or set the Option to output contact forces to RCFORC for all 2 surface force
        transducers when the force transducer surfaces overlap.
        EQ.0: Output to the first force transducer that matches (default)
        EQ.1: Output to all force transducers that match
        """ # nopep8
        return self._cards[5].get_value("ftall")

    @ftall.setter
    def ftall(self, value: int) -> None:
        """Set the ftall property."""
        if value not in [0, 1, None]:
            raise Exception("""ftall must be `None` or one of {0,1}.""")
        self._cards[5].set_value("ftall", value)

    @property
    def shltrw(self) -> float:
        """Get or set the Optional shell thickness scale factor for contact with rigid walls. Shell thickness is not considered when SHLTRW=0 (default). SHLTRW=0.5
        will result in an offset of half of shell thickness in contact with rigid walls..
        """ # nopep8
        return self._cards[5].get_value("shltrw")

    @shltrw.setter
    def shltrw(self, value: float) -> None:
        """Set the shltrw property."""
        self._cards[5].set_value("shltrw", value)

    @property
    def igactc(self) -> int:
        """Get or set the Options to use isogeometric shells for contact detection when contact involves isogeometric shells:
        EQ.0: contact between interpolated nodes and interpolated shells
        EQ.1: contact between interpolated nodes and isogeometric shells.
        """ # nopep8
        return self._cards[5].get_value("igactc")

    @igactc.setter
    def igactc(self, value: int) -> None:
        """Set the igactc property."""
        if value not in [0, 1, None]:
            raise Exception("""igactc must be `None` or one of {0,1}.""")
        self._cards[5].set_value("igactc", value)

    @property
    def irevspt(self) -> int:
        """Get or set the Flag to revert the spot weld thinning behavior where beam and brick spot welds share nodes with shell parts instead of being tied to the shells:
        EQ.0: Thinning at shared nodes will be done as it has been in all versions after R9.3.1.
        EQ.1: Behavior reverts to that of R9.3.1.In this version and previous versions, spot weld thinning was not done.
        """ # nopep8
        return self._cards[6].get_value("irevspt")

    @irevspt.setter
    def irevspt(self, value: int) -> None:
        """Set the irevspt property."""
        if value not in [0, 1, None]:
            raise Exception("""irevspt must be `None` or one of {0,1}.""")
        self._cards[6].set_value("irevspt", value)

    @property
    def cohtiem(self) -> int:
        """Get or set the Flag to treat how the mass from SURFB of a tied contact affects the time step estimation of cohesive elements:
        EQ.0: No treatment
        EQ.1: Assuming the cohesive element's nodes are on SURFA of a tied contact, LS - DYNA includes the mass from SURFB when estimating the cohesive element's time step. Note that groupable tied contacts are not currently supported with this option.
        """ # nopep8
        return self._cards[6].get_value("cohtiem")

    @cohtiem.setter
    def cohtiem(self, value: int) -> None:
        """Set the cohtiem property."""
        if value not in [0, 1, None]:
            raise Exception("""cohtiem must be `None` or one of {0,1}.""")
        self._cards[6].set_value("cohtiem", value)

    @property
    def tieopt(self) -> int:
        """Get or set the Option for constrained tied contact formulations to circumvent some of the shortcomings present in the standard implementations:
        EQ.0:	Not active
        EQ.1 : Active, a), b) and c) in Remark 11 apply.
        EQ.2:	Active, a), b), c) and d) in Remark 11 apply.
        """ # nopep8
        return self._cards[6].get_value("tieopt")

    @tieopt.setter
    def tieopt(self, value: int) -> None:
        """Set the tieopt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""tieopt must be `None` or one of {0,1,2}.""")
        self._cards[6].set_value("tieopt", value)

    @property
    def strobj(self) -> int:
        """Get or set the Flag for strong objectivity (frame invariance) in non-groupable single surface contacts with SOFT = 0 or 1.
        Strong objectivity incurs an extra cost but may improve results.
        EQ.0: Do not turn on strong objectivity.
        EQ.1: Turn on strong objectivity.
        """ # nopep8
        return self._cards[6].get_value("strobj")

    @strobj.setter
    def strobj(self, value: int) -> None:
        """Set the strobj property."""
        self._cards[6].set_value("strobj", value)

    @property
    def befblk(self) -> float:
        """Get or set the Default bulk modulus used for the contact stiffness calculation. This field applies to all penalty-based contacts that use the bulk modulus to calculate the contact stiffness. Thus, it applies when using the standard penalty formulation (SOFT = 0), the soft constraint penalty formulation (SOFT = 1), and Mortar segment-to-segment contact. See Remark 12.
        """ # nopep8
        return self._cards[6].get_value("befblk")

    @befblk.setter
    def befblk(self, value: float) -> None:
        """Set the befblk property."""
        self._cards[6].set_value("befblk", value)

