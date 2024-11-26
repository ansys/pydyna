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

class ControlContact(KeywordBase):
    """DYNA CONTROL_CONTACT keyword"""

    keyword = "CONTROL"
    subkeyword = "CONTACT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "slsfac",
                        float,
                        0,
                        10,
                        kwargs.get("slsfac", 0.1)
                    ),
                    Field(
                        "rwpnal",
                        float,
                        10,
                        10,
                        kwargs.get("rwpnal")
                    ),
                    Field(
                        "islchk",
                        int,
                        20,
                        10,
                        kwargs.get("islchk", 1)
                    ),
                    Field(
                        "shlthk",
                        int,
                        30,
                        10,
                        kwargs.get("shlthk", 0)
                    ),
                    Field(
                        "penopt",
                        int,
                        40,
                        10,
                        kwargs.get("penopt", 1)
                    ),
                    Field(
                        "thkchg",
                        int,
                        50,
                        10,
                        kwargs.get("thkchg", 0)
                    ),
                    Field(
                        "orien",
                        int,
                        60,
                        10,
                        kwargs.get("orien", 1)
                    ),
                    Field(
                        "enmass",
                        int,
                        70,
                        10,
                        kwargs.get("enmass", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "usrstr",
                        int,
                        0,
                        10,
                        kwargs.get("usrstr", 0)
                    ),
                    Field(
                        "usrfrc",
                        int,
                        10,
                        10,
                        kwargs.get("usrfrc", 0)
                    ),
                    Field(
                        "nsbcs",
                        int,
                        20,
                        10,
                        kwargs.get("nsbcs", 0)
                    ),
                    Field(
                        "interm",
                        int,
                        30,
                        10,
                        kwargs.get("interm", 0)
                    ),
                    Field(
                        "xpene",
                        float,
                        40,
                        10,
                        kwargs.get("xpene", 4.0)
                    ),
                    Field(
                        "ssthk",
                        int,
                        50,
                        10,
                        kwargs.get("ssthk", 0)
                    ),
                    Field(
                        "ecdt",
                        int,
                        60,
                        10,
                        kwargs.get("ecdt", 0)
                    ),
                    Field(
                        "tiedprj",
                        int,
                        70,
                        10,
                        kwargs.get("tiedprj", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sfric",
                        float,
                        0,
                        10,
                        kwargs.get("sfric", 0.0)
                    ),
                    Field(
                        "dfric",
                        float,
                        10,
                        10,
                        kwargs.get("dfric", 0.0)
                    ),
                    Field(
                        "edc",
                        float,
                        20,
                        10,
                        kwargs.get("edc", 0.0)
                    ),
                    Field(
                        "vfc",
                        float,
                        30,
                        10,
                        kwargs.get("vfc", 0.0)
                    ),
                    Field(
                        "th",
                        float,
                        40,
                        10,
                        kwargs.get("th", 0.0)
                    ),
                    Field(
                        "th_sf",
                        float,
                        50,
                        10,
                        kwargs.get("th_sf", 0.0)
                    ),
                    Field(
                        "pen_sf",
                        float,
                        60,
                        10,
                        kwargs.get("pen_sf", 0.0)
                    ),
                ],
            ),
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
                        "frceng",
                        int,
                        10,
                        10,
                        kwargs.get("frceng", 0)
                    ),
                    Field(
                        "skiprwg",
                        int,
                        20,
                        10,
                        kwargs.get("skiprwg", 0)
                    ),
                    Field(
                        "outseg",
                        int,
                        30,
                        10,
                        kwargs.get("outseg", 0)
                    ),
                    Field(
                        "spotstp",
                        int,
                        40,
                        10,
                        kwargs.get("spotstp", 0)
                    ),
                    Field(
                        "spotdel",
                        int,
                        50,
                        10,
                        kwargs.get("spotdel", 0)
                    ),
                    Field(
                        "spothin",
                        float,
                        60,
                        10,
                        kwargs.get("spothin")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "isym",
                        int,
                        0,
                        10,
                        kwargs.get("isym", 0)
                    ),
                    Field(
                        "nserod",
                        int,
                        10,
                        10,
                        kwargs.get("nserod", 0)
                    ),
                    Field(
                        "rwgaps",
                        int,
                        20,
                        10,
                        kwargs.get("rwgaps", 1)
                    ),
                    Field(
                        "rwgdth",
                        float,
                        30,
                        10,
                        kwargs.get("rwgdth", 0.0)
                    ),
                    Field(
                        "rwksf",
                        float,
                        40,
                        10,
                        kwargs.get("rwksf", 1.0)
                    ),
                    Field(
                        "icov",
                        int,
                        50,
                        10,
                        kwargs.get("icov", 0)
                    ),
                    Field(
                        "swradf",
                        float,
                        60,
                        10,
                        kwargs.get("swradf", 0.0)
                    ),
                    Field(
                        "ithoff",
                        int,
                        70,
                        10,
                        kwargs.get("ithoff", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "shledg",
                        int,
                        0,
                        10,
                        kwargs.get("shledg", 0)
                    ),
                    Field(
                        "pstiff",
                        int,
                        10,
                        10,
                        kwargs.get("pstiff", 0)
                    ),
                    Field(
                        "ithcnt",
                        int,
                        20,
                        10,
                        kwargs.get("ithcnt", 0)
                    ),
                    Field(
                        "tdcnof",
                        int,
                        30,
                        10,
                        kwargs.get("tdcnof", 0)
                    ),
                    Field(
                        "ftall",
                        int,
                        40,
                        10,
                        kwargs.get("ftall", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "shltrw",
                        float,
                        60,
                        10,
                        kwargs.get("shltrw", 0.0)
                    ),
                    Field(
                        "igactc",
                        int,
                        70,
                        10,
                        kwargs.get("igactc", 0)
                    ),
                ],
            ),
        ]

    @property
    def slsfac(self) -> float:
        """Get or set the Scale factor for sliding interface penalties (default = 0.1)
        """ # nopep8
        return self._cards[0].get_value("slsfac")

    @slsfac.setter
    def slsfac(self, value: float) -> None:
        self._cards[0].set_value("slsfac", value)

    @property
    def rwpnal(self) -> typing.Optional[float]:
        """Get or set the Scale factor for rigid wall penalties, which treat nodal points interacting with rigid walls, RWPNAL.  The penalties are set so that an absolute value of unity should be optimal; however, this penalty value may be very problem dependent.  If rigid/deformable materials switching is used, this option should be used if the switched materials are interacting with rigid walls.
        In case you have IGA parts in your model, please see Remark 10.
        LT.0.0:	All nodes are treated by the penalty method.This is set to - 1.0 for implicit calculations.Since seven(7) variables are stored for each slave node, only the nodes that may interact with the wall should be included in the node list.
        EQ.0.0 : The constraint method is used and nodal points which belong to rigid bodies are not considered.
        GT.0.0 : Rigid bodies nodes are treated by the penalty method and all other nodes are treated by the constraint method.
        """ # nopep8
        return self._cards[0].get_value("rwpnal")

    @rwpnal.setter
    def rwpnal(self, value: float) -> None:
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
        if value not in [1, 0, 2]:
            raise Exception("""islchk must be one of {1,0,2}""")
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
        if value not in [0, 1, 2]:
            raise Exception("""shlthk must be one of {0,1,2}""")
        self._cards[0].set_value("shlthk", value)

    @property
    def penopt(self) -> int:
        """Get or set the Penalty stiffness value option.
        EQ.0: the default is set to 1,
        EQ.1: minimum of master segment and slave node (default for most contact types),
        EQ.2: use master segment stiffness (old way),
        EQ.3: use slave node value,
        EQ.4: use slave node value, area or mass weighted,
        EQ.5: same as 4 but inversely proportional to the shell thickness.
        Options 4 and 5 are recommended for metalforming calculations..
        """ # nopep8
        return self._cards[0].get_value("penopt")

    @penopt.setter
    def penopt(self, value: int) -> None:
        if value not in [1, 0, 2, 3, 4, 5]:
            raise Exception("""penopt must be one of {1,0,2,3,4,5}""")
        self._cards[0].set_value("penopt", value)

    @property
    def thkchg(self) -> int:
        """Get or set the Shell thickness changes considered in single surface contact:
        EQ.0: no consideration (default),
        EQ.1: shell thickness changes are included.
        """ # nopep8
        return self._cards[0].get_value("thkchg")

    @thkchg.setter
    def thkchg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""thkchg must be one of {0,1}""")
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
        if value not in [1, 0, 2, 3, 4]:
            raise Exception("""orien must be one of {1,0,2,3,4}""")
        self._cards[0].set_value("orien", value)

    @property
    def enmass(self) -> int:
        """Get or set the Treatment of the mass of eroded nodes in contact. This option effects all contact types where nodes are removed after surrounding elements fail. Generally, the removal of eroded nodes makes the calculation more stable; however, in problems where erosion is important the reduction of mass will lead to incorrect results.
        EQ.0: eroding nodes are removed from the calculation.
        EQ.1: eroding nodes of solid elements are retained and continue to be active in contact.
        EQ.2: the eroding nodes of solid and shell elements are retained and continue to be active in contact.
        """ # nopep8
        return self._cards[0].get_value("enmass")

    @enmass.setter
    def enmass(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""enmass must be one of {0,1,2}""")
        self._cards[0].set_value("enmass", value)

    @property
    def usrstr(self) -> int:
        """Get or set the Storage per contact interface for user supplied interface control subroutine.  If zero, no input data is read.
        """ # nopep8
        return self._cards[1].get_value("usrstr")

    @usrstr.setter
    def usrstr(self, value: int) -> None:
        self._cards[1].set_value("usrstr", value)

    @property
    def usrfrc(self) -> int:
        """Get or set the Storage per contact interface for user supplied interface friction subroutine. If zero, no input data is read.
        """ # nopep8
        return self._cards[1].get_value("usrfrc")

    @usrfrc.setter
    def usrfrc(self, value: int) -> None:
        self._cards[1].set_value("usrfrc", value)

    @property
    def nsbcs(self) -> int:
        """Get or set the Number of cycles between contact searching. Values between 10-100 recommended.
        """ # nopep8
        return self._cards[1].get_value("nsbcs")

    @nsbcs.setter
    def nsbcs(self, value: int) -> None:
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
        if value not in [0, 1]:
            raise Exception("""interm must be one of {0,1}""")
        self._cards[1].set_value("interm", value)

    @property
    def xpene(self) -> float:
        """Get or set the Contact surface maximum penetration check multiplier
        """ # nopep8
        return self._cards[1].get_value("xpene")

    @xpene.setter
    def xpene(self, value: float) -> None:
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
        if value not in [0, 1]:
            raise Exception("""ssthk must be one of {0,1}""")
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
        if value not in [0, 1]:
            raise Exception("""ecdt must be one of {0,1}""")
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
        if value not in [0, 1]:
            raise Exception("""tiedprj must be one of {0,1}""")
        self._cards[1].set_value("tiedprj", value)

    @property
    def sfric(self) -> float:
        """Get or set the Default static coefficient of friction (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("sfric")

    @sfric.setter
    def sfric(self, value: float) -> None:
        self._cards[2].set_value("sfric", value)

    @property
    def dfric(self) -> float:
        """Get or set the Default dynamic coefficient of friction (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("dfric")

    @dfric.setter
    def dfric(self, value: float) -> None:
        self._cards[2].set_value("dfric", value)

    @property
    def edc(self) -> float:
        """Get or set the Default exponential decay coefficient (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("edc")

    @edc.setter
    def edc(self, value: float) -> None:
        self._cards[2].set_value("edc", value)

    @property
    def vfc(self) -> float:
        """Get or set the Default viscous friction coefficient (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("vfc")

    @vfc.setter
    def vfc(self, value: float) -> None:
        self._cards[2].set_value("vfc", value)

    @property
    def th(self) -> float:
        """Get or set the Default contact thickness (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("th")

    @th.setter
    def th(self, value: float) -> None:
        self._cards[2].set_value("th", value)

    @property
    def th_sf(self) -> float:
        """Get or set the Default thickness scale factor (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("th_sf")

    @th_sf.setter
    def th_sf(self, value: float) -> None:
        self._cards[2].set_value("th_sf", value)

    @property
    def pen_sf(self) -> float:
        """Get or set the Default local penalty scale factor (see *PART_CONTACT).
        """ # nopep8
        return self._cards[2].get_value("pen_sf")

    @pen_sf.setter
    def pen_sf(self, value: float) -> None:
        self._cards[2].set_value("pen_sf", value)

    @property
    def ignore(self) -> int:
        """Get or set the Ignore initial penetrations in the *CONTACT_AUTOMATIC options. This option can also be specified for each interface on the third optional card under the keyword, *CONTACT. The value defined here will be the default.
        EQ.0: Move nodes to eliminate initial penetrations in the model definition.
        EQ.1: Allow initial penetrations to exist by tracking the initial penetrations.
        EQ.2: Allow initial penetrations to exist by tracking the initial penetrations. However, penetration warning messages are printed with the original coordinates and the recommended coordinates of each slave node given.
        """ # nopep8
        return self._cards[3].get_value("ignore")

    @ignore.setter
    def ignore(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ignore must be one of {0,1,2}""")
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
        if value not in [0, 1]:
            raise Exception("""frceng must be one of {0,1}""")
        self._cards[3].set_value("frceng", value)

    @property
    def skiprwg(self) -> int:
        """Get or set the Flag not to display stationary rigid wall by default.
        EQ.0:  generate 4 extra nodes and 1 shell element to visulize stationary planar rigid wall.
        EQ.1:  do not generate stationary rigid wall.
        """ # nopep8
        return self._cards[3].get_value("skiprwg")

    @skiprwg.setter
    def skiprwg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""skiprwg must be one of {0,1}""")
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
        if value not in [0, 1]:
            raise Exception("""outseg must be one of {0,1}""")
        self._cards[3].set_value("outseg", value)

    @property
    def spotstp(self) -> int:
        """Get or set the If a spot weld node (related to a *MAT_SPOTWELD beam) cannot be fouind on a master segment, should an error termination occur?
        EQ.0: no, print warning message and continue calculation.
        EQ.1: yes, print error message and terminate.
        EQ.2: no, delete the weld, print a message, and continue,
        EQ.3: no, keep the weld: (This is not recommended as it can lead to instabilities.)
        """ # nopep8
        return self._cards[3].get_value("spotstp")

    @spotstp.setter
    def spotstp(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""spotstp must be one of {0,1,2,3}""")
        self._cards[3].set_value("spotstp", value)

    @property
    def spotdel(self) -> int:
        """Get or set the If a spot weld node of a spot weld beam is attached to a shell element, which fails and is deleted, then the attached spot weld beam element is deleted if this flag is on. There is a small cost penalty realted to this option on non-vector processors. On vector processors, however, this option can significantly slow down the calculation if many weld elements fail since the vector lengths are reduced.
        EQ.0: no, do not delete the beam element,
        EQ.1: yes, delete the beam elements when the attached shell fails.
        GT.1: delete the SPR when SPOTDEL nodes are attached to failed elements in the search radius.
        """ # nopep8
        return self._cards[3].get_value("spotdel")

    @spotdel.setter
    def spotdel(self, value: int) -> None:
        self._cards[3].set_value("spotdel", value)

    @property
    def spothin(self) -> typing.Optional[float]:
        """Get or set the Optional thickness scale factor. If active, define a factor greater than zero, but less than one.
        """ # nopep8
        return self._cards[3].get_value("spothin")

    @spothin.setter
    def spothin(self, value: float) -> None:
        self._cards[3].set_value("spothin", value)

    @property
    def isym(self) -> int:
        """Get or set the Symmetry plane default for automatic segment generation when contact is defined by part IDs:
        LT.0:	 is a node set on the symmetry boundary, supported and recommended for Mortar contact.
        This will allow for a correct treatment of segments close to the symmetry face/edge. See Remark 8
        EQ.0: Off.
        EQ.1: do not include faces whith normal boundary constraints ( e.g. segments of brick elements on a symmetry plane.
        """ # nopep8
        return self._cards[4].get_value("isym")

    @isym.setter
    def isym(self, value: int) -> None:
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
        if value not in [0, 1]:
            raise Exception("""nserod must be one of {0,1}""")
        self._cards[4].set_value("nserod", value)

    @property
    def rwgaps(self) -> int:
        """Get or set the Flag to add rigid wall gap stiffness, see parameter RWGDTH below.
        EQ.1:  add gap stiffness.
        EQ.2:  do not add gap stiffness
        """ # nopep8
        return self._cards[4].get_value("rwgaps")

    @rwgaps.setter
    def rwgaps(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""rwgaps must be one of {1,2}""")
        self._cards[4].set_value("rwgaps", value)

    @property
    def rwgdth(self) -> float:
        """Get or set the Death time for gap stiffness. After this time the gap stiffness is no longer added.
        """ # nopep8
        return self._cards[4].get_value("rwgdth")

    @rwgdth.setter
    def rwgdth(self, value: float) -> None:
        self._cards[4].set_value("rwgdth", value)

    @property
    def rwksf(self) -> float:
        """Get or set the Rigid wall penalty scale factor for contact with deformable parts during implicit calculations.  This value is independent of SLSFAC and RWPNAL. If RWKSF is also specified in *RIGIDWALL_PLANAR, the stiffness is scaled by the product of the two values..
        """ # nopep8
        return self._cards[4].get_value("rwksf")

    @rwksf.setter
    def rwksf(self, value: float) -> None:
        self._cards[4].set_value("rwksf", value)

    @property
    def icov(self) -> int:
        """Get or set the Invokes the covariant formulation of Konyukhov and Schweizerhof in the FORMING contact option.
        """ # nopep8
        return self._cards[4].get_value("icov")

    @icov.setter
    def icov(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""icov must be one of {0,1}""")
        self._cards[4].set_value("icov", value)

    @property
    def swradf(self) -> float:
        """Get or set the Spot weld radius scale factor for neighbor segment thinning:EQ.0:	Neighbor segments are not thinned(default).GT.0 : The radius of a spot weld is scaled by SWRADF when searching for close neighbor segments to thin.
        """ # nopep8
        return self._cards[4].get_value("swradf")

    @swradf.setter
    def swradf(self, value: float) -> None:
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
        if value not in [0, 1]:
            raise Exception("""ithoff must be one of {0,1}""")
        self._cards[4].set_value("ithoff", value)

    @property
    def shledg(self) -> int:
        """Get or set the Flag for assuming edge shape for shells when measuring penetration.
        This is available for segment based contact (see SOFT on *CONTACT)
        EQ.0: Shell edges are assumed round (default),
        EQ.1: Shell edges are assumed square and are flush with the nodes.
        """ # nopep8
        return self._cards[5].get_value("shledg")

    @shledg.setter
    def shledg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""shledg must be one of {0,1}""")
        self._cards[5].set_value("shledg", value)

    @property
    def pstiff(self) -> int:
        """Get or set the Flag to choose the method for calculating the penalty stiffness. This is
        available for segment based contact (see SOFT on *CONTACT)
        EQ.0: Based on material density and segment dimensions (default),
        EQ.1: Based on nodal masses.
        """ # nopep8
        return self._cards[5].get_value("pstiff")

    @pstiff.setter
    def pstiff(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""pstiff must be one of {0,1}""")
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
        self._cards[5].set_value("ithcnt", value)

    @property
    def tdcnof(self) -> int:
        """Get or set the Tied constraint offset contact update option.
        EQ.0: Update velocities and displacements from accelerations
        EQ.1: Update velocities and acclelerations from displacements. This
        option is recommended only when there are large angle changes
        where the default does not maintain a constant offset to a small
        tolerance. This latter option is not as stable as the default and may
        require additional damping for stability. See *CONTROL_BULK_VISCOSITY and *DAMPING_PART_STIFFNESS.
        """ # nopep8
        return self._cards[5].get_value("tdcnof")

    @tdcnof.setter
    def tdcnof(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tdcnof must be one of {0,1}""")
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
        if value not in [0, 1]:
            raise Exception("""ftall must be one of {0,1}""")
        self._cards[5].set_value("ftall", value)

    @property
    def shltrw(self) -> float:
        """Get or set the Optional shell thickness scale factor for contact with rigid walls. Shell thickness is not considered when SHLTRW=0 (default). SHLTRW=0.5
        will result in an offset of half of shell thickness in contact with rigid walls..
        """ # nopep8
        return self._cards[5].get_value("shltrw")

    @shltrw.setter
    def shltrw(self, value: float) -> None:
        self._cards[5].set_value("shltrw", value)

    @property
    def igactc(self) -> int:
        """Get or set the Options to use isogeometric shells for contact detection when
        contact involves isogeometric shells:
        EQ.0: contact between interpolated nodes and interpolated shells
        EQ.1: contact between interpolated nodes and isogeometric shells.
        """ # nopep8
        return self._cards[5].get_value("igactc")

    @igactc.setter
    def igactc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""igactc must be one of {0,1}""")
        self._cards[5].set_value("igactc", value)

