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

"""Module providing the FrequencyDomainSsdErp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_vector import DefineVector

_FREQUENCYDOMAINSSDERP_CARD0 = (
    FieldSchema("mdmin", int, 0, 10, 1),
    FieldSchema("mdmax", int, 10, 10, None),
    FieldSchema("fnmin", float, 20, 10, 0.0),
    FieldSchema("fnmax", float, 30, 10, None),
    FieldSchema("restmd", int, 40, 10, 0),
    FieldSchema("restdp", int, 50, 10, 0),
    FieldSchema("lcflag", int, 60, 10, 0),
    FieldSchema("relatv", int, 70, 10, 0),
)

_FREQUENCYDOMAINSSDERP_CARD1 = (
    FieldSchema("dampf", float, 0, 10, 0.0),
    FieldSchema("lcdam", int, 10, 10, 0),
    FieldSchema("lctyp", int, 20, 10, 0),
    FieldSchema("dmpmas", float, 30, 10, 0.0),
    FieldSchema("dmpstf", float, 40, 10, 0.0),
    FieldSchema("dmpflg", int, 50, 10, 0),
)

_FREQUENCYDOMAINSSDERP_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("istress", int, 10, 10, None),
    FieldSchema("memory", int, 20, 10, 0),
    FieldSchema("nerp", int, 30, 10, 0),
    FieldSchema("strtyp", int, 40, 10, 0),
    FieldSchema("nout", int, 50, 10, 0),
    FieldSchema("notyp", int, 60, 10, 0),
    FieldSchema("nova", int, 70, 10, 0),
)

_FREQUENCYDOMAINSSDERP_CARD3 = (
    FieldSchema("ro", float, 0, 10, None),
    FieldSchema("c", float, 10, 10, None),
    FieldSchema("erprlf", float, 20, 10, 1.0),
    FieldSchema("erpref", float, 30, 10, 0.0),
    FieldSchema("radeff", int, 30, 10, 0),
)

_FREQUENCYDOMAINSSDERP_CARD4 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("ptyp", int, 10, 10, 0),
)

_FREQUENCYDOMAINSSDERP_CARD5 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("ntyp", int, 10, 10, 0),
    FieldSchema("dof", int, 20, 10, 1),
    FieldSchema("vad", int, 30, 10, 0),
    FieldSchema("lc1", int, 40, 10, None),
    FieldSchema("lc2", int, 50, 10, None),
    FieldSchema("sf", float, 60, 10, 0.0),
    FieldSchema("vid", int, 70, 10, 0),
)

class FrequencyDomainSsdErp(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_SSD_ERP keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_SSD_ERP"
    _link_fields = {
        "lcdam": LinkType.DEFINE_CURVE,
        "lc1": LinkType.DEFINE_CURVE,
        "lc2": LinkType.DEFINE_CURVE,
        "vid": LinkType.DEFINE_VECTOR,
    }

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainSsdErp class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSSDERP_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSSDERP_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSSDERP_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSSDERP_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSSDERP_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSSDERP_CARD5,
                **kwargs,
            ),        ]
    @property
    def mdmin(self) -> int:
        """Get or set the The first mode in modal superposition method (optional).
        """ # nopep8
        return self._cards[0].get_value("mdmin")

    @mdmin.setter
    def mdmin(self, value: int) -> None:
        """Set the mdmin property."""
        self._cards[0].set_value("mdmin", value)

    @property
    def mdmax(self) -> typing.Optional[int]:
        """Get or set the The last mode in modal superposition method (optional).
        """ # nopep8
        return self._cards[0].get_value("mdmax")

    @mdmax.setter
    def mdmax(self, value: int) -> None:
        """Set the mdmax property."""
        self._cards[0].set_value("mdmax", value)

    @property
    def fnmin(self) -> float:
        """Get or set the The minimum natural frequency in modal superposition method(optional).
        """ # nopep8
        return self._cards[0].get_value("fnmin")

    @fnmin.setter
    def fnmin(self, value: float) -> None:
        """Set the fnmin property."""
        self._cards[0].set_value("fnmin", value)

    @property
    def fnmax(self) -> typing.Optional[float]:
        """Get or set the The maximum natural frequency in modal superposition method(optional).
        """ # nopep8
        return self._cards[0].get_value("fnmax")

    @fnmax.setter
    def fnmax(self, value: float) -> None:
        """Set the fnmax property."""
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
        """Set the restmd property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""restmd must be `None` or one of {0,1,2}.""")
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
        """Set the restdp property."""
        if value not in [0, 1, None]:
            raise Exception("""restdp must be `None` or one of {0,1}.""")
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
        """Set the lcflag property."""
        if value not in [0, 1, None]:
            raise Exception("""lcflag must be `None` or one of {0,1}.""")
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
        """Set the relatv property."""
        if value not in [0, 1, None]:
            raise Exception("""relatv must be `None` or one of {0,1}.""")
        self._cards[0].set_value("relatv", value)

    @property
    def dampf(self) -> float:
        """Get or set the Modal damping coefficient, ζ.
        """ # nopep8
        return self._cards[1].get_value("dampf")

    @dampf.setter
    def dampf(self, value: float) -> None:
        """Set the dampf property."""
        self._cards[1].set_value("dampf", value)

    @property
    def lcdam(self) -> int:
        """Get or set the Load Curve ID defining mode dependent modal damping coefficient ζ.
        """ # nopep8
        return self._cards[1].get_value("lcdam")

    @lcdam.setter
    def lcdam(self, value: int) -> None:
        """Set the lcdam property."""
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
        """Set the lctyp property."""
        self._cards[1].set_value("lctyp", value)

    @property
    def dmpmas(self) -> float:
        """Get or set the Mass proportional damping constant α, in Rayleigh damping..
        """ # nopep8
        return self._cards[1].get_value("dmpmas")

    @dmpmas.setter
    def dmpmas(self, value: float) -> None:
        """Set the dmpmas property."""
        self._cards[1].set_value("dmpmas", value)

    @property
    def dmpstf(self) -> float:
        """Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
        """ # nopep8
        return self._cards[1].get_value("dmpstf")

    @dmpstf.setter
    def dmpstf(self, value: float) -> None:
        """Set the dmpstf property."""
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
        """Set the dmpflg property."""
        if value not in [0, 1, None]:
            raise Exception("""dmpflg must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dmpflg", value)

    @property
    def istress(self) -> typing.Optional[int]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("istress")

    @istress.setter
    def istress(self, value: int) -> None:
        """Set the istress property."""
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
        """Set the memory property."""
        if value not in [0, 1, None]:
            raise Exception("""memory must be `None` or one of {0,1}.""")
        self._cards[2].set_value("memory", value)

    @property
    def nerp(self) -> int:
        """Get or set the Number of ERP panels
        """ # nopep8
        return self._cards[2].get_value("nerp")

    @nerp.setter
    def nerp(self, value: int) -> None:
        """Set the nerp property."""
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
        """Set the strtyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""strtyp must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("strtyp", value)

    @property
    def nout(self) -> int:
        """Get or set the Part, part set, segment set, or node set ID for response output (use with acoustic computation). See NOTYP below.
        """ # nopep8
        return self._cards[2].get_value("nout")

    @nout.setter
    def nout(self, value: int) -> None:
        """Set the nout property."""
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
        """Set the notyp property."""
        if value not in [0, 1, 2, 3, -2, None]:
            raise Exception("""notyp must be `None` or one of {0,1,2,3,-2}.""")
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
        """Set the nova property."""
        if value not in [0, 1, None]:
            raise Exception("""nova must be `None` or one of {0,1}.""")
        self._cards[2].set_value("nova", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid density.
        """ # nopep8
        return self._cards[3].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[3].set_value("ro", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Sound speed of the fluid.
        """ # nopep8
        return self._cards[3].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[3].set_value("c", value)

    @property
    def erprlf(self) -> float:
        """Get or set the ERP radiation loss factor.
        """ # nopep8
        return self._cards[3].get_value("erprlf")

    @erprlf.setter
    def erprlf(self, value: float) -> None:
        """Set the erprlf property."""
        self._cards[3].set_value("erprlf", value)

    @property
    def erpref(self) -> float:
        """Get or set the ERP reference value. This is used to convert the absolute ERP value to ERP in decibels (dB).
        """ # nopep8
        return self._cards[3].get_value("erpref")

    @erpref.setter
    def erpref(self, value: float) -> None:
        """Set the erpref property."""
        self._cards[3].set_value("erpref", value)

    @property
    def radeff(self) -> int:
        """Get or set the Radiation efficiency computation flag:
        EQ.0:	radiation efficiency computation is not requested.
        EQ.1:	radiation efficiency computation is requested.
        """ # nopep8
        return self._cards[3].get_value("radeff")

    @radeff.setter
    def radeff(self, value: int) -> None:
        """Set the radeff property."""
        if value not in [0, 1, None]:
            raise Exception("""radeff must be `None` or one of {0,1}.""")
        self._cards[3].set_value("radeff", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part, part set, or segment set ID for ERP computation.
        """ # nopep8
        return self._cards[4].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[4].set_value("pid", value)

    @property
    def ptyp(self) -> int:
        """Get or set the Type of PID:
        EQ.0:	part ID,
        EQ.1:	part set ID,
        EQ.2:	segment set ID.
        """ # nopep8
        return self._cards[4].get_value("ptyp")

    @ptyp.setter
    def ptyp(self, value: int) -> None:
        """Set the ptyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ptyp must be `None` or one of {0,1,2}.""")
        self._cards[4].set_value("ptyp", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node, Node set,Segment set ID for excitation input.See NTYP below.
        """ # nopep8
        return self._cards[5].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[5].set_value("nid", value)

    @property
    def ntyp(self) -> int:
        """Get or set the Type of NID:
        EQ.0: node ID,
        EQ.1: node set ID,
        EQ.2: segment set ID.
        """ # nopep8
        return self._cards[5].get_value("ntyp")

    @ntyp.setter
    def ntyp(self, value: int) -> None:
        """Set the ntyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ntyp must be `None` or one of {0,1,2}.""")
        self._cards[5].set_value("ntyp", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom for excitation input(ignored if VAD=1).
        EQ. 1: x-translational degree-of-freedom x-rotational degree-of-freedom (for torque excitation, VAD=8),
        EQ. 2: y-translational degree-of-freedom or y-rotational degree-of-freedom (for torque excitation, VAD=8),
        EQ. 3: z-translational degree-of-freedom or z-rotational degree-of-freedom (for torque excitation, VAD=8),
        EQ. 4: translational movement in direction given by vector VID or rotational movement with axis given by vector VID (for torque excitation, VAD=8).
        """ # nopep8
        return self._cards[5].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        """Set the dof property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""dof must be `None` or one of {1,2,3,4}.""")
        self._cards[5].set_value("dof", value)

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
        return self._cards[5].get_value("vad")

    @vad.setter
    def vad(self, value: int) -> None:
        """Set the vad property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, None]:
            raise Exception("""vad must be `None` or one of {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14}.""")
        self._cards[5].set_value("vad", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID defining amplitude (LCFLAG=0) or real (in-phase) part (LCFLAG=1) of load as a function of frequency
        """ # nopep8
        return self._cards[5].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        """Set the lc1 property."""
        self._cards[5].set_value("lc1", value)

    @property
    def lc2(self) -> typing.Optional[int]:
        """Get or set the Load Curve ID defining phase angle (LCFLAG=0) or imaginary (out-phase) part (LCFLAG=1) of load as a function of frequency.
        """ # nopep8
        return self._cards[5].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        """Set the lc2 property."""
        self._cards[5].set_value("lc2", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor for the load.
        """ # nopep8
        return self._cards[5].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[5].set_value("sf", value)

    @property
    def vid(self) -> int:
        """Get or set the Vector ID for DOF=4 for excitation input, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[5].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        """Set the vid property."""
        self._cards[5].set_value("vid", value)

    @property
    def lcdam_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcdam."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdam:
                return kwd
        return None

    @lcdam_link.setter
    def lcdam_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdam."""
        self.lcdam = value.lcid

    @property
    def lc1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc1:
                return kwd
        return None

    @lc1_link.setter
    def lc1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc1."""
        self.lc1 = value.lcid

    @property
    def lc2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lc2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lc2:
                return kwd
        return None

    @lc2_link.setter
    def lc2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lc2."""
        self.lc2 = value.lcid

    @property
    def vid_link(self) -> typing.Optional[DefineVector]:
        """Get the DefineVector object for vid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "VECTOR"):
            if kwd.vid == self.vid:
                return kwd
        return None

    @vid_link.setter
    def vid_link(self, value: DefineVector) -> None:
        """Set the DefineVector object for vid."""
        self.vid = value.vid

