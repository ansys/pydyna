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

"""Module providing the FrequencyDomainRandomVibrationFatigue class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD0 = (
    FieldSchema("mdmin", int, 0, 10, 1),
    FieldSchema("mdmax", int, 10, 10, None),
    FieldSchema("fnmin", float, 20, 10, 0.0),
    FieldSchema("fnmax", float, 30, 10, None),
    FieldSchema("restrt", int, 40, 10, 0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
)

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD1 = (
    FieldSchema("dampf", float, 0, 10, 0.0),
    FieldSchema("lcdam", int, 10, 10, 0),
    FieldSchema("lctyp", int, 20, 10, 0),
    FieldSchema("dmpmas", float, 30, 10, 0.0),
    FieldSchema("dmpstf", float, 40, 10, 0.0),
    FieldSchema("dmptyp", int, 50, 10, 0),
)

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD2 = (
    FieldSchema("vaflag", int, 0, 10, 0),
    FieldSchema("method", int, 10, 10, 0),
    FieldSchema("unit", int, 20, 10, 0),
    FieldSchema("umlt", float, 30, 10, None),
    FieldSchema("vapsd", int, 40, 10, 0),
    FieldSchema("varms", int, 50, 10, 0),
    FieldSchema("napsd", int, 60, 10, 1),
    FieldSchema("ncpsd", int, 70, 10, 0),
)

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD3 = (
    FieldSchema("ldtyp", int, 0, 10, 0),
    FieldSchema("ipanelu", int, 10, 10, None),
    FieldSchema("ipanelv", int, 20, 10, None),
    FieldSchema("temper", float, 30, 10, 0.0),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("ldflag", int, 50, 10, 0),
    FieldSchema("icoarse", int, 60, 10, 0),
    FieldSchema("tcoarse", float, 70, 10, 0.1),
)

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD4 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, None),
    FieldSchema("dof", int, 20, 10, 0),
    FieldSchema("ldpsd", int, 30, 10, None),
    FieldSchema("ldvel", int, 40, 10, None),
    FieldSchema("ldflw", int, 50, 10, None),
    FieldSchema("ldspn", int, 60, 10, None),
    FieldSchema("cid", int, 70, 10, None),
)

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD5 = (
    FieldSchema("load_i", int, 0, 10, None),
    FieldSchema("load_j", int, 10, 10, None),
    FieldSchema("lctyp2", int, 20, 10, 0),
    FieldSchema("ldpsd1", int, 30, 10, None),
    FieldSchema("ldpsd2", int, 40, 10, None),
)

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD6 = (
    FieldSchema("mftg", int, 0, 10, 0),
    FieldSchema("nftg", int, 10, 10, 1),
    FieldSchema("sntype", int, 20, 10, 0),
    FieldSchema("texpos", float, 30, 10, 0.0),
    FieldSchema("strsf", float, 40, 10, 1.0),
    FieldSchema("inftg", int, 50, 10, None),
)

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD7 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("ptype", int, 20, 10, 0),
    FieldSchema("ltype", int, 30, 10, 0),
    FieldSchema("a", float, 40, 10, None),
    FieldSchema("b", float, 50, 10, None),
    FieldSchema("sthres", float, 60, 10, 0.0),
    FieldSchema("snlimt", int, 70, 10, 0),
)

_FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD8 = (
    FieldSchema("filename", str, 0, 80, None),
)

class FrequencyDomainRandomVibrationFatigue(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_RANDOM_VIBRATION_FATIGUE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_RANDOM_VIBRATION_FATIGUE"

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainRandomVibrationFatigue class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINRANDOMVIBRATIONFATIGUE_CARD8,
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
    def restrt(self) -> int:
        """Get or set the Restart option.
        EQ.0: A new modal analysis is performed,
        EQ.1: Restart with d3eigv.
        """ # nopep8
        return self._cards[0].get_value("restrt")

    @restrt.setter
    def restrt(self, value: int) -> None:
        """Set the restrt property."""
        if value not in [0, 1, None]:
            raise Exception("""restrt must be `None` or one of {0,1}.""")
        self._cards[0].set_value("restrt", value)

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
        """Get or set the Load Curve ID defining mode dependent modal damping coefficient, ζ.
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
        if value not in [0, 1, None]:
            raise Exception("""lctyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("lctyp", value)

    @property
    def dmpmas(self) -> float:
        """Get or set the Mass proportional damping constant α, in Rayleigh damping.
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
    def dmptyp(self) -> int:
        """Get or set the Type of damping
        EQ.0: modal damping.
        EQ.1: broadband damping.
        """ # nopep8
        return self._cards[1].get_value("dmptyp")

    @dmptyp.setter
    def dmptyp(self, value: int) -> None:
        """Set the dmptyp property."""
        if value not in [0, 1, None]:
            raise Exception("""dmptyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dmptyp", value)

    @property
    def vaflag(self) -> int:
        """Get or set the Loading type:
        EQ.0: No random vibration analysis.
        EQ.1: Base acceleration.
        EQ.2: Random pressure.
        EQ.3: Plane wave.
        EQ.4: Shock wave.
        EQ.5: Progressive wave.
        EQ.6: Reverberant wave.
        EQ.7: Turbulent boundary layer wave.
        EQ.8: Nodal force.
        """ # nopep8
        return self._cards[2].get_value("vaflag")

    @vaflag.setter
    def vaflag(self, value: int) -> None:
        """Set the vaflag property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""vaflag must be `None` or one of {0,1,2,3,4,5,6,7,8}.""")
        self._cards[2].set_value("vaflag", value)

    @property
    def method(self) -> int:
        """Get or set the method for modal response analysis.
        EQ.0: method set automatically by LS-DYNA (recommended)
        EQ.1: modal superposition method
        EQ.2: modal acceleration method
        EQ.3: modal truncation augmentation method.
        """ # nopep8
        return self._cards[2].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        """Set the method property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""method must be `None` or one of {0,1,2,3}.""")
        self._cards[2].set_value("method", value)

    @property
    def unit(self) -> int:
        """Get or set the Flag for acceleration unit conversion:
        EQ.0: use [length unit]/[time unit]2 as unit of acceleration.
        EQ.1: use g as unit for acceleration, and SI units (Newton, kg, meter,
        second, etc.) elsewhere.
        EQ.2: use g as unit for acceleration, and Engineering units (lbf,
        lbf×second2/inch, inch, second, etc.) elsewhere.
        EQ.3:use g as unit for acceleration, and units (kN, kg, mm, ms, GPa, etc.) elsewhere.
        EQ.4:	Use g as unit for acceleration, and units (Newton, ton, mm, second, MPa, etc.) elsewhere.
        EQ.-1 use g as unit for acceleration and provide the multiplier for
        converting g to [length unit]/[time unit]2.
        """ # nopep8
        return self._cards[2].get_value("unit")

    @unit.setter
    def unit(self, value: int) -> None:
        """Set the unit property."""
        if value not in [0, 1, 2, 3, 4, -1, None]:
            raise Exception("""unit must be `None` or one of {0,1,2,3,4,-1}.""")
        self._cards[2].set_value("unit", value)

    @property
    def umlt(self) -> typing.Optional[float]:
        """Get or set the Multiplier for converting g to [length unit]/[time unit]2(used only for UNIT = -1).
        """ # nopep8
        return self._cards[2].get_value("umlt")

    @umlt.setter
    def umlt(self, value: float) -> None:
        """Set the umlt property."""
        self._cards[2].set_value("umlt", value)

    @property
    def vapsd(self) -> int:
        """Get or set the Flag for PSD output:
        EQ.0: Absolute PSD output is requested.
        EQ.1: Relative PSD output is requested (used only for VAFLAG=1).
        """ # nopep8
        return self._cards[2].get_value("vapsd")

    @vapsd.setter
    def vapsd(self, value: int) -> None:
        """Set the vapsd property."""
        if value not in [0, 1, None]:
            raise Exception("""vapsd must be `None` or one of {0,1}.""")
        self._cards[2].set_value("vapsd", value)

    @property
    def varms(self) -> int:
        """Get or set the Flag for RMS output:
        EQ.0: Absolute RMS output is requested.
        EQ.1: Relative RMS output is requested (used only for VAFLAG=1).
        """ # nopep8
        return self._cards[2].get_value("varms")

    @varms.setter
    def varms(self, value: int) -> None:
        """Set the varms property."""
        if value not in [0, 1, None]:
            raise Exception("""varms must be `None` or one of {0,1}.""")
        self._cards[2].set_value("varms", value)

    @property
    def napsd(self) -> int:
        """Get or set the Number of auto PSD load definition. Card 5 is repeated "NAPSD" number of times,one for each auto PSD load definition. The default value is 1.
        """ # nopep8
        return self._cards[2].get_value("napsd")

    @napsd.setter
    def napsd(self, value: int) -> None:
        """Set the napsd property."""
        self._cards[2].set_value("napsd", value)

    @property
    def ncpsd(self) -> int:
        """Get or set the Number of cross PSD load definition. Card 6 is repeated "NCPSD" times,one for each cross PSD load definition. The default value is 0.
        """ # nopep8
        return self._cards[2].get_value("ncpsd")

    @ncpsd.setter
    def ncpsd(self, value: int) -> None:
        """Set the ncpsd property."""
        self._cards[2].set_value("ncpsd", value)

    @property
    def ldtyp(self) -> int:
        """Get or set the Excitation load (LDPSD in card 5) type:
        EQ.0: PSD.
        EQ.1: SPL (for plane wave only).
        EQ.2: time history load.
        """ # nopep8
        return self._cards[3].get_value("ldtyp")

    @ldtyp.setter
    def ldtyp(self, value: int) -> None:
        """Set the ldtyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ldtyp must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("ldtyp", value)

    @property
    def ipanelu(self) -> typing.Optional[int]:
        """Get or set the Number of strips in U direction (used only for VAFLAG=5, 6, 7).
        """ # nopep8
        return self._cards[3].get_value("ipanelu")

    @ipanelu.setter
    def ipanelu(self, value: int) -> None:
        """Set the ipanelu property."""
        self._cards[3].set_value("ipanelu", value)

    @property
    def ipanelv(self) -> typing.Optional[int]:
        """Get or set the Number of strips in V direction (used only for VAFLAG=5, 6, 7).
        """ # nopep8
        return self._cards[3].get_value("ipanelv")

    @ipanelv.setter
    def ipanelv(self, value: int) -> None:
        """Set the ipanelv property."""
        self._cards[3].set_value("ipanelv", value)

    @property
    def temper(self) -> float:
        """Get or set the Temperature.
        """ # nopep8
        return self._cards[3].get_value("temper")

    @temper.setter
    def temper(self, value: float) -> None:
        """Set the temper property."""
        self._cards[3].set_value("temper", value)

    @property
    def ldflag(self) -> int:
        """Get or set the Type of loading curves.
        EQ.0: Log-Log interpolation(default).
        EQ.1: Semi-Log interpolation.
        EQ.2: Linear-Linear interpolation.

        """ # nopep8
        return self._cards[3].get_value("ldflag")

    @ldflag.setter
    def ldflag(self, value: int) -> None:
        """Set the ldflag property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ldflag must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("ldflag", value)

    @property
    def icoarse(self) -> int:
        """Get or set the Option for PSD curve coarsening:
        EQ.0:	No coarsening, use original data (default).
        EQ.1:	Coarsening by keeping only peaks and troughs.
        EQ.2:	Coarsening by removing intermediate points whose slope change percentage is less than prescribed tolerance (TCOARSE).
        """ # nopep8
        return self._cards[3].get_value("icoarse")

    @icoarse.setter
    def icoarse(self, value: int) -> None:
        """Set the icoarse property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""icoarse must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("icoarse", value)

    @property
    def tcoarse(self) -> float:
        """Get or set the Tolerance for slope change percentage for removing intermediate points from PSD curve (default is 0.1), for ICOARSE  =  2.
        """ # nopep8
        return self._cards[3].get_value("tcoarse")

    @tcoarse.setter
    def tcoarse(self, value: float) -> None:
        """Set the tcoarse property."""
        self._cards[3].set_value("tcoarse", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the GE.0: Set ID for the panel exposed to acoustic environment,or the nodes subjected to nodal force excitation,or nodal acceleration excitation.For VAFLAG = 1,base acceleration,leave this as blank
        LT.0: used to define the cross-PSD.|SID| is the ID of the load cases.
        """ # nopep8
        return self._cards[4].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[4].set_value("sid", value)

    @property
    def stype(self) -> typing.Optional[int]:
        """Get or set the Flag specifying meaning of SID.
        EQ. 0: Node
        EQ. 1: Node Set
        EQ. 2: Segment Set
        EQ. 3: Part
        EQ. 4: Part Set
        LT.0: used to define the cross-psd.|STYPE| is the ID of the load cases.
        """ # nopep8
        return self._cards[4].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        self._cards[4].set_value("stype", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom for nodal force excitation or base acceleration(DOF = 1, 2 and 3),or wave direction:
        EQ.0: translational movement in direction given by vector VID,
        EQ.1: x-translational degree-of-freedom,
        EQ.2: y-translational degree-of-freedom,
        EQ.3: z-translational degree-of-freedom.
        """ # nopep8
        return self._cards[4].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        """Set the dof property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""dof must be `None` or one of {0,1,2,3}.""")
        self._cards[4].set_value("dof", value)

    @property
    def ldpsd(self) -> typing.Optional[int]:
        """Get or set the Load curve for PSD, SPL or time history excitation.
        """ # nopep8
        return self._cards[4].get_value("ldpsd")

    @ldpsd.setter
    def ldpsd(self, value: int) -> None:
        """Set the ldpsd property."""
        self._cards[4].set_value("ldpsd", value)

    @property
    def ldvel(self) -> typing.Optional[int]:
        """Get or set the Load curve for phase velocity.
        """ # nopep8
        return self._cards[4].get_value("ldvel")

    @ldvel.setter
    def ldvel(self, value: int) -> None:
        """Set the ldvel property."""
        self._cards[4].set_value("ldvel", value)

    @property
    def ldflw(self) -> typing.Optional[int]:
        """Get or set the Load curve for exponential decay for TBL in flow-wise direction.
        """ # nopep8
        return self._cards[4].get_value("ldflw")

    @ldflw.setter
    def ldflw(self, value: int) -> None:
        """Set the ldflw property."""
        self._cards[4].set_value("ldflw", value)

    @property
    def ldspn(self) -> typing.Optional[int]:
        """Get or set the Load curve for exponential decay for TBL in span-wise direction.
        """ # nopep8
        return self._cards[4].get_value("ldspn")

    @ldspn.setter
    def ldspn(self, value: int) -> None:
        """Set the ldspn property."""
        self._cards[4].set_value("ldspn", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Coordinate system ID for defining wave direction, see
        *DEFINE_COORDINATE_SYSTEM; or Vector ID for defining load
        direction for nodal force, or base excitation, see *DEFINE_VECTOR.
        """ # nopep8
        return self._cards[4].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[4].set_value("cid", value)

    @property
    def load_i(self) -> typing.Optional[int]:
        """Get or set the ID of load i for cross PSD.
        """ # nopep8
        return self._cards[5].get_value("load_i")

    @load_i.setter
    def load_i(self, value: int) -> None:
        """Set the load_i property."""
        self._cards[5].set_value("load_i", value)

    @property
    def load_j(self) -> typing.Optional[int]:
        """Get or set the ID of load j for cross PSD.
        """ # nopep8
        return self._cards[5].get_value("load_j")

    @load_j.setter
    def load_j(self, value: int) -> None:
        """Set the load_j property."""
        self._cards[5].set_value("load_j", value)

    @property
    def lctyp2(self) -> int:
        """Get or set the Type of load curves (LDPSD1 and LDPSD2) for defining cross PSD:
        EQ.0:LDPSD1 defines real part and LDPSD2 defines imaginary part
        EQ.1:LDPSD1 defines magnitude and LDPSD2 defines phase angle.
        """ # nopep8
        return self._cards[5].get_value("lctyp2")

    @lctyp2.setter
    def lctyp2(self, value: int) -> None:
        """Set the lctyp2 property."""
        if value not in [0, 1, None]:
            raise Exception("""lctyp2 must be `None` or one of {0,1}.""")
        self._cards[5].set_value("lctyp2", value)

    @property
    def ldpsd1(self) -> typing.Optional[int]:
        """Get or set the Load curve for real part or magnitude of cross PSD.
        """ # nopep8
        return self._cards[5].get_value("ldpsd1")

    @ldpsd1.setter
    def ldpsd1(self, value: int) -> None:
        """Set the ldpsd1 property."""
        self._cards[5].set_value("ldpsd1", value)

    @property
    def ldpsd2(self) -> typing.Optional[int]:
        """Get or set the Load curve for imaginary part or phase angle of cross PSD.
        """ # nopep8
        return self._cards[5].get_value("ldpsd2")

    @ldpsd2.setter
    def ldpsd2(self, value: int) -> None:
        """Set the ldpsd2 property."""
        self._cards[5].set_value("ldpsd2", value)

    @property
    def mftg(self) -> int:
        """Get or set the Method for random fatigue analysis (for option _FATIGUE).
        EQ.0:no fatigue analysis,
        EQ.1:Steinberg's three-band method,
        EQ.2:Dirlik method,
        EQ.3:Narrow band method,
        EQ.4:Wirsching method,
        EQ.5:Chaudhury and Dover method,
        EQ.6:Tunna method,
        EQ.7:Hancock method.
        """ # nopep8
        return self._cards[6].get_value("mftg")

    @mftg.setter
    def mftg(self, value: int) -> None:
        """Set the mftg property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""mftg must be `None` or one of {0,1,2,3,4,5,6,7}.""")
        self._cards[6].set_value("mftg", value)

    @property
    def nftg(self) -> int:
        """Get or set the Field specifying the number of S-N curves to be defined.
        GE.0: Number of S-N curves defined by card 8. Card 8 is repeated "NFTG" number of times, one for each S-N fatigue curve definition. The default value is 1.
        EQ.-999: S-N curves are defined through *MAT_ADD_FATIGUE.
        If the option FATIGUE is not used, ignore this parameter.
        """ # nopep8
        return self._cards[6].get_value("nftg")

    @nftg.setter
    def nftg(self, value: int) -> None:
        """Set the nftg property."""
        self._cards[6].set_value("nftg", value)

    @property
    def sntype(self) -> int:
        """Get or set the Stress type of S-N curve in fatigue analysis.
        EQ.0:von-mises stress
        EQ.1:maximum principal stress (not implemented)
        EQ.2:maximum shear stress (not implemented)
        EQ.-n:The nth stress component.
        """ # nopep8
        return self._cards[6].get_value("sntype")

    @sntype.setter
    def sntype(self, value: int) -> None:
        """Set the sntype property."""
        self._cards[6].set_value("sntype", value)

    @property
    def texpos(self) -> float:
        """Get or set the Exposure time (used if option FATIGUE is used)
        """ # nopep8
        return self._cards[6].get_value("texpos")

    @texpos.setter
    def texpos(self, value: float) -> None:
        """Set the texpos property."""
        self._cards[6].set_value("texpos", value)

    @property
    def strsf(self) -> float:
        """Get or set the Stress scale factor to accommodate different ordinates in S-N curve.
        EQ.1:used if the ordinate in S-N curve is stress range (default)
        EQ.2:used if the ordinate in S-N curve is stress amplitude
        """ # nopep8
        return self._cards[6].get_value("strsf")

    @strsf.setter
    def strsf(self, value: float) -> None:
        """Set the strsf property."""
        if value not in [1, 2, None]:
            raise Exception("""strsf must be `None` or one of {1,2}.""")
        self._cards[6].set_value("strsf", value)

    @property
    def inftg(self) -> typing.Optional[int]:
        """Get or set the Flag for including initial damage ratio.
        EQ.0: no initial damage ratio,
        GT.0: read existing d3ftg files to get initial damage ratio. When INFTG > 1, it means that the initial damage ratio comes from multiple loading cases (correspondingly, multiple binary databases, defined by Card 7). The value of INFTG should be<=10.
        """ # nopep8
        return self._cards[6].get_value("inftg")

    @inftg.setter
    def inftg(self, value: int) -> None:
        """Set the inftg property."""
        self._cards[6].set_value("inftg", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, or Part Set ID, or Element (solid, shell, beam, thick shell) Set ID.
        """ # nopep8
        return self._cards[7].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[7].set_value("pid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the S-N fatigue curve ID for the current Part or Part Set.
        GT. 0: S-N fatigue curve ID
        EQ. -1: S-N fatigue curve uses equation N*S**b = a
        EQ. -2: S-N fatigue curve uses equation log(S) = a-b*log(N)
        """ # nopep8
        return self._cards[7].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[7].set_value("lcid", value)

    @property
    def ptype(self) -> int:
        """Get or set the Type of PID.
        EQ. 0: Part (default)
        EQ. 1: Part Set
        EQ. 2: SET_SOLID
        EQ. 3: SET_BEAM
        EQ. 4: SET_SHELL
        EQ. 5: SET_TSHELL
        """ # nopep8
        return self._cards[7].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        """Set the ptype property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""ptype must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[7].set_value("ptype", value)

    @property
    def ltype(self) -> int:
        """Get or set the Type of LCID.
        EQ. 0: Semi-log interpolation (default)
        EQ. 1: Log-Log interpolation
        """ # nopep8
        return self._cards[7].get_value("ltype")

    @ltype.setter
    def ltype(self, value: int) -> None:
        """Set the ltype property."""
        if value not in [0, 1, None]:
            raise Exception("""ltype must be `None` or one of {0,1}.""")
        self._cards[7].set_value("ltype", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Material parameter a in S-N fatigue equation.
        """ # nopep8
        return self._cards[7].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[7].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Material parameter b in S-N fatigue equation.
        """ # nopep8
        return self._cards[7].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[7].set_value("b", value)

    @property
    def sthres(self) -> float:
        """Get or set the Fatigue threshold.
        """ # nopep8
        return self._cards[7].get_value("sthres")

    @sthres.setter
    def sthres(self, value: float) -> None:
        """Set the sthres property."""
        self._cards[7].set_value("sthres", value)

    @property
    def snlimt(self) -> int:
        """Get or set the If LCID > 0 Flag setting algorithm used when stress is lower than the lowest stress on S-N curve (if LCID > 0), or lower than STHRES (if LCID < 0).
        EQ.0: use the life at the last point on S-N curve.
        EQ.1: extrapolation from the last two points on S-N curve (only applicable if LCID > 0).
        EQ.2: infinity.
        If LCID < 0 Flag setting algorithm used when stress is lower STHRES
        EQ.0: use the life at STHRES.
        EQ.1: Ingnored. only applicable for LCID > 0.
        EQ.2: infinity.
        """ # nopep8
        return self._cards[7].get_value("snlimt")

    @snlimt.setter
    def snlimt(self, value: int) -> None:
        """Set the snlimt property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""snlimt must be `None` or one of {0,1,2}.""")
        self._cards[7].set_value("snlimt", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Path and name of existing binary database for fatigue information.
        """ # nopep8
        return self._cards[8].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[8].set_value("filename", value)

