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

"""Module providing the Contact2DAutomaticSurfaceToSurfaceMortarThermal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD0 = (
    FieldSchema("surfa", int, 0, 10, None),
    FieldSchema("surfb", int, 10, 10, None),
    FieldSchema("sfact", float, 20, 10, 1.0),
    FieldSchema("freq", int, 30, 10, 50),
    FieldSchema("fs", float, 40, 10, 0.0),
    FieldSchema("fd", float, 50, 10, 0.0),
    FieldSchema("dc", float, 60, 10, 0.0),
    FieldSchema("unused", int, 70, 10, None),
)

_CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD1 = (
    FieldSchema("tbirth", float, 0, 10, 0.0),
    FieldSchema("tdeath", float, 10, 10, 1e+20),
    FieldSchema("soa", float, 20, 10, 1.0),
    FieldSchema("sob", float, 30, 10, 1.0),
    FieldSchema("nda", int, 40, 10, 0),
    FieldSchema("ndb", int, 50, 10, 0),
    FieldSchema("cof", int, 60, 10, 0),
    FieldSchema("init", int, 70, 10, 0),
)

_CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD2 = (
    FieldSchema("k", float, 0, 10, None),
    FieldSchema("rad", float, 10, 10, None),
    FieldSchema("h", float, 20, 10, None),
    FieldSchema("lmin", float, 30, 10, None),
    FieldSchema("lmax", float, 40, 10, None),
    FieldSchema("chlm", float, 50, 10, 1.0),
    FieldSchema("bc_flag", int, 60, 10, 0),
)

_CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD3 = (
    FieldSchema("vc", float, 0, 10, 0.0),
    FieldSchema("vdc", float, 10, 10, 10.0),
    FieldSchema("ipf", int, 20, 10, 0),
    FieldSchema("slide", int, 30, 10, 0),
    FieldSchema("istiff", int, 40, 10, 0),
    FieldSchema("tiedgap", float, 50, 10, 0.0),
    FieldSchema("igapcl", int, 60, 10, 0),
    FieldSchema("tietyp", int, 70, 10, 0),
)

_CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD4 = (
    FieldSchema("sldsoa", float, 0, 10, 0.0),
    FieldSchema("sldsob", float, 10, 10, 0.0),
    FieldSchema("tdpen", float, 20, 10, 0.0),
)

class Contact2DAutomaticSurfaceToSurfaceMortarThermal(KeywordBase):
    """DYNA CONTACT_2D_AUTOMATIC_SURFACE_TO_SURFACE_MORTAR_THERMAL keyword"""

    keyword = "CONTACT"
    subkeyword = "2D_AUTOMATIC_SURFACE_TO_SURFACE_MORTAR_THERMAL"

    def __init__(self, **kwargs):
        """Initialize the Contact2DAutomaticSurfaceToSurfaceMortarThermal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACT2DAUTOMATICSURFACETOSURFACEMORTARTHERMAL_CARD4,
                **kwargs,
            ),        ]
    @property
    def surfa(self) -> typing.Optional[int]:
        """Get or set the Set ID for SURFA.  If SURFA > 0, a part set is assumed; see *SET_‌PART.  If SURFA < 0, a node set with ID equal to the absolute value of SURFA is assumed; see *SET_‌NODE. For nonsymmetric contact, this surface is the tracked surface.
        """ # nopep8
        return self._cards[0].get_value("surfa")

    @surfa.setter
    def surfa(self, value: int) -> None:
        """Set the surfa property."""
        self._cards[0].set_value("surfa", value)

    @property
    def surfb(self) -> typing.Optional[int]:
        """Get or set the Set ID to define the SURFB surface.  If SURFB > 0, a part set is assumed; see *SET_‌PART.  If SURFB < 0, a node set with ID equal to the absolute value of SURFB is assumed; see *SET_‌NODE.  Do not define for single surface contact. For nonsymmetric contact, this surface is the reference surface.
        """ # nopep8
        return self._cards[0].get_value("surfb")

    @surfb.setter
    def surfb(self, value: int) -> None:
        """Set the surfb property."""
        self._cards[0].set_value("surfb", value)

    @property
    def sfact(self) -> float:
        """Get or set the Scale factor for the penalty force stiffness (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("sfact")

    @sfact.setter
    def sfact(self, value: float) -> None:
        """Set the sfact property."""
        self._cards[0].set_value("sfact", value)

    @property
    def freq(self) -> int:
        """Get or set the Search frequency. The number of time steps between bucket sorts (default=50).
        """ # nopep8
        return self._cards[0].get_value("freq")

    @freq.setter
    def freq(self, value: int) -> None:
        """Set the freq property."""
        self._cards[0].set_value("freq", value)

    @property
    def fs(self) -> float:
        """Get or set the Static coefficient of friction (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def fd(self) -> float:
        """Get or set the Dynamic coefficient of friction (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("fd")

    @fd.setter
    def fd(self, value: float) -> None:
        """Set the fd property."""
        self._cards[0].set_value("fd", value)

    @property
    def dc(self) -> float:
        """Get or set the Exponential decay coefficient (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[0].set_value("dc", value)

    @property
    def tbirth(self) -> float:
        """Get or set the Birth time for contact (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        """Set the tbirth property."""
        self._cards[1].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for contact (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        """Set the tdeath property."""
        self._cards[1].set_value("tdeath", value)

    @property
    def soa(self) -> float:
        """Get or set the Surface offset from midline for 2D shells of SURFA surface:
        GT.0.0: scale factor applied to actual thickness,
        LT.0.0: absolute value is used as the offset.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("soa")

    @soa.setter
    def soa(self, value: float) -> None:
        """Set the soa property."""
        self._cards[1].set_value("soa", value)

    @property
    def sob(self) -> float:
        """Get or set the Surface offset from midline for 2D shells of SURFB surface:
        GT.0.0: scale factor applied to actual thickness,
        LT.0.0: absolute value is used as the offset.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("sob")

    @sob.setter
    def sob(self, value: float) -> None:
        """Set the sob property."""
        self._cards[1].set_value("sob", value)

    @property
    def nda(self) -> int:
        """Get or set the Normal direction flag for 2D shells of SURFA surface:
        EQ.0: Normal direction is determined automatically (default),
        EQ.1: Normal direction is in the positive direction,
        EQ.-1: Normal direction is in the negative direction.
        """ # nopep8
        return self._cards[1].get_value("nda")

    @nda.setter
    def nda(self, value: int) -> None:
        """Set the nda property."""
        if value not in [0, 1, -1, None]:
            raise Exception("""nda must be `None` or one of {0,1,-1}.""")
        self._cards[1].set_value("nda", value)

    @property
    def ndb(self) -> int:
        """Get or set the Normal direction flag for 2D shells of SURFB surface:
        EQ.0: Normal direction is determined automatically (default),
        EQ.1: Normal direction is in the positive direction,
        EQ.-1: Normal direction is in the negative direction.
        """ # nopep8
        return self._cards[1].get_value("ndb")

    @ndb.setter
    def ndb(self, value: int) -> None:
        """Set the ndb property."""
        if value not in [0, 1, -1, None]:
            raise Exception("""ndb must be `None` or one of {0,1,-1}.""")
        self._cards[1].set_value("ndb", value)

    @property
    def cof(self) -> int:
        """Get or set the COF: Closing/opening flag for implicit analysis.
        EQ.0: Recommended for most problems where gaps are only closing (default),
        EQ.1: Recommended when gaps are opening to avoid sticking.
        """ # nopep8
        return self._cards[1].get_value("cof")

    @cof.setter
    def cof(self, value: int) -> None:
        """Set the cof property."""
        if value not in [0, 1, None]:
            raise Exception("""cof must be `None` or one of {0,1}.""")
        self._cards[1].set_value("cof", value)

    @property
    def init(self) -> int:
        """Get or set the Special processing during initialization.
        EQ.0: No special processing,
        EQ.1: Forming option.
        """ # nopep8
        return self._cards[1].get_value("init")

    @init.setter
    def init(self, value: int) -> None:
        """Set the init property."""
        if value not in [0, 1, None]:
            raise Exception("""init must be `None` or one of {0,1}.""")
        self._cards[1].set_value("init", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity (k) of fluid between the slide surfaces. If a gap with a thickness l-gap exists between the slide surfaces, then the conductance due to thermal conductivity between the slide surfaces is
        h-cond = k/l-gap
        Note: LS- DYNA calculates l-gap based on deformation.
        """ # nopep8
        return self._cards[2].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[2].set_value("k", value)

    @property
    def rad(self) -> typing.Optional[float]:
        """Get or set the Radiation factor (f) between the slide surfaces. A radient-heat-transfer coefficient (h-rad) is calculated (see *BOUNDARY_RADIATION). If a gap exists between the slide surfaces, then the contact conductance is calculated by
        h = h-cond + h-rad.
        """ # nopep8
        return self._cards[2].get_value("rad")

    @rad.setter
    def rad(self, value: float) -> None:
        """Set the rad property."""
        self._cards[2].set_value("rad", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Heat transfer conductance (h-cont) for closed gaps. Use this heat transfer conductance for gaps in the range
        0 <= l-gap <= l-min
        where l-min is defined below.
        """ # nopep8
        return self._cards[2].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[2].set_value("h", value)

    @property
    def lmin(self) -> typing.Optional[float]:
        """Get or set the Critical gap (l-min), use the heat transfer conductance defined (HTC) for gap thicknesses less than this value.
        """ # nopep8
        return self._cards[2].get_value("lmin")

    @lmin.setter
    def lmin(self, value: float) -> None:
        """Set the lmin property."""
        self._cards[2].set_value("lmin", value)

    @property
    def lmax(self) -> typing.Optional[float]:
        """Get or set the No thermal contact if gap is greater than this value (l-max).
        """ # nopep8
        return self._cards[2].get_value("lmax")

    @lmax.setter
    def lmax(self, value: float) -> None:
        """Set the lmax property."""
        self._cards[2].set_value("lmax", value)

    @property
    def chlm(self) -> float:
        """Get or set the Is a multiplier used on the element characteristic distance for the search routine. The characteristic length is the largest interface surface element diagonal.
        EQ.0.0: Default is set to 1.0.
        """ # nopep8
        return self._cards[2].get_value("chlm")

    @chlm.setter
    def chlm(self, value: float) -> None:
        """Set the chlm property."""
        self._cards[2].set_value("chlm", value)

    @property
    def bc_flag(self) -> int:
        """Get or set the Thermal boundary condition flag:
        EQ.0: thermal boundary conditions are ON when parts are in contact
        EQ.1: thermal boundary conditions are OFF when parts are in contact.
        """ # nopep8
        return self._cards[2].get_value("bc_flag")

    @bc_flag.setter
    def bc_flag(self, value: int) -> None:
        """Set the bc_flag property."""
        if value not in [0, 1, None]:
            raise Exception("""bc_flag must be `None` or one of {0,1}.""")
        self._cards[2].set_value("bc_flag", value)

    @property
    def vc(self) -> float:
        """Get or set the Coefficient for viscous friction. This is used to limit the friction force to a maximum.
        """ # nopep8
        return self._cards[3].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        """Set the vc property."""
        self._cards[3].set_value("vc", value)

    @property
    def vdc(self) -> float:
        """Get or set the Viscous damping coefficient in percent of critical for explicit contact.
        """ # nopep8
        return self._cards[3].get_value("vdc")

    @vdc.setter
    def vdc(self, value: float) -> None:
        """Set the vdc property."""
        self._cards[3].set_value("vdc", value)

    @property
    def ipf(self) -> int:
        """Get or set the Initial penetration flag for explicit contact.
        EQ.0: Allow initial penetrations to remain
        EQ.1: Push apart initially penetrated surfaces.
        """ # nopep8
        return self._cards[3].get_value("ipf")

    @ipf.setter
    def ipf(self, value: int) -> None:
        """Set the ipf property."""
        if value not in [0, 1, None]:
            raise Exception("""ipf must be `None` or one of {0,1}.""")
        self._cards[3].set_value("ipf", value)

    @property
    def slide(self) -> int:
        """Get or set the Sliding option.
        EQ:0. Off.
        EQ.1: On.
        """ # nopep8
        return self._cards[3].get_value("slide")

    @slide.setter
    def slide(self, value: int) -> None:
        """Set the slide property."""
        if value not in [0, 1, None]:
            raise Exception("""slide must be `None` or one of {0,1}.""")
        self._cards[3].set_value("slide", value)

    @property
    def istiff(self) -> int:
        """Get or set the Stiffness scaling option.
        EQ.0: Use default option.
        EQ.1: Scale stiffness using segment masses and explicit time step (default for explicit contact).
        EQ.2: Scale stiffness using segment stiffness and dimensions (default for implicit contact)
        """ # nopep8
        return self._cards[3].get_value("istiff")

    @istiff.setter
    def istiff(self, value: int) -> None:
        """Set the istiff property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""istiff must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("istiff", value)

    @property
    def tiedgap(self) -> float:
        """Get or set the Search gap for tied contacts.
        EQ.0: Default, use 1% of the SURFB segment length
        GT.0: Use the input value
        LT.0: Use n% of the SURFB segment length where n=|TIEDGAP|.
        """ # nopep8
        return self._cards[3].get_value("tiedgap")

    @tiedgap.setter
    def tiedgap(self, value: float) -> None:
        """Set the tiedgap property."""
        self._cards[3].set_value("tiedgap", value)

    @property
    def igapcl(self) -> int:
        """Get or set the Flag to close gaps in tied contact:
        EQ.0: Default, allow gaps to remain
        EQ.1: Move SURFA nodes to SURFB segment to close gaps.
        """ # nopep8
        return self._cards[3].get_value("igapcl")

    @igapcl.setter
    def igapcl(self, value: int) -> None:
        """Set the igapcl property."""
        if value not in [0, 1, None]:
            raise Exception("""igapcl must be `None` or one of {0,1}.""")
        self._cards[3].set_value("igapcl", value)

    @property
    def tietyp(self) -> int:
        """Get or set the Flag to control constraint type of tied contact:
        EQ.0: Default, use kinematic constraints when possible
        EQ.1: Use only penalty type constraints.
        """ # nopep8
        return self._cards[3].get_value("tietyp")

    @tietyp.setter
    def tietyp(self, value: int) -> None:
        """Set the tietyp property."""
        if value not in [0, 1, None]:
            raise Exception("""tietyp must be `None` or one of {0,1}.""")
        self._cards[3].set_value("tietyp", value)

    @property
    def sldsoa(self) -> float:
        """Get or set the Solid surface offset for the SURFA surface.
        """ # nopep8
        return self._cards[4].get_value("sldsoa")

    @sldsoa.setter
    def sldsoa(self, value: float) -> None:
        """Set the sldsoa property."""
        self._cards[4].set_value("sldsoa", value)

    @property
    def sldsob(self) -> float:
        """Get or set the Solid surface offset for the SURFB surface.
        """ # nopep8
        return self._cards[4].get_value("sldsob")

    @sldsob.setter
    def sldsob(self, value: float) -> None:
        """Set the sldsob property."""
        self._cards[4].set_value("sldsob", value)

    @property
    def tdpen(self) -> float:
        """Get or set the Time span of penetration removal for 2D Mortar contacts.
        Each initial penetration will be gradually reduced linearly in time, so that it is removed by time TDPEN.
        This is the interference option analogue to MPAR1 for IGNORE = 3 in 3D automatic Mortar contacts.
        """ # nopep8
        return self._cards[4].get_value("tdpen")

    @tdpen.setter
    def tdpen(self, value: float) -> None:
        """Set the tdpen property."""
        self._cards[4].set_value("tdpen", value)

