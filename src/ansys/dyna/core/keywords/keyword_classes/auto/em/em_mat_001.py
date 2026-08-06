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

"""Module providing the EmMat001 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMMAT001_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("mtype", int, 10, 10, 0),
    FieldSchema("sigma", float, 20, 10, None),
    FieldSchema("eosid", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("eplambda", float, 50, 10, None),
    FieldSchema("deatht", float, 60, 10, 1e+28),
    FieldSchema("rdltype", int, 70, 10, 0),
)

_EMMAT001_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("epsrr", float, 20, 10, None),
    FieldSchema("eosid2", int, 30, 10, None),
    FieldSchema("epsri", float, 40, 10, None),
    FieldSchema("epsid3", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

class EmMat001(KeywordBase):
    """DYNA EM_MAT_001 keyword"""

    keyword = "EM"
    subkeyword = "MAT_001"
    _link_fields = {
        "mid": LinkType.MAT,
    }

    def __init__(self, **kwargs):
        """Initialize the EmMat001 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMMAT001_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMMAT001_CARD1,
                **kwargs,
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID. For all MTYPE except MTYPE = 3, MID must reference a *MAT material since the electromagnetic properties are added onto the *MAT properties. If MTYPE = 3, MID can be left blank for the electromagnetic properties to apply to the ICFD entire fluid or it can be the PID of a *ICFD_PART_VOL to apply to an ICFD fluid part. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def mtype(self) -> int:
        """Get or set the Defines the electromagnetism type of the material:
        EQ.0:	Air or vacuum
        EQ.1 : Insulator material.These materials have the same electromagnetism behavior as MTYPE = 0.
        EQ.2 : Conductor carrying a source.In these conductors, the eddy - current problem is solved, yielding the actual current density.Typically, this material would correspond to the coil.In Electro - Physiology, it corresponds to the tissue when solving the monodomain equations(EMSOL = 11 or 13 on * EM_CONTROL).For this case, an* EM_EP_CELLMODEL must be associated with this* EM_MAT_001.
        EQ.3:	Fluid conductor.In this case, MID refers to the ID given in* ICFD_?PART_VOL.See Remark 2.
        EQ.4:	Conductor not connected to any current or voltage source, where the eddy current problem is solved.Typically, this material would correspond to the workpiece.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""mtype must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("mtype", value)

    @property
    def sigma(self) -> typing.Optional[float]:
        """Get or set the initial electrical conductivity of the material. For the eikonal solvers (EMSOL = 14 and 15 on *EM_CONTROL), SIGMA is the wave velocity.
        """ # nopep8
        return self._cards[0].get_value("sigma")

    @sigma.setter
    def sigma(self, value: float) -> None:
        """Set the sigma property."""
        self._cards[0].set_value("sigma", value)

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Optional ID of the EOS to be used for the electrical conductivity (see *EM_EOS card).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def eplambda(self) -> typing.Optional[float]:
        """Get or set the Optional. When defined, this field activates the computation of extracellular potentials in the Purkinje network with the anisotropy ratio given by EPLAMDA
        """ # nopep8
        return self._cards[0].get_value("eplambda")

    @eplambda.setter
    def eplambda(self, value: float) -> None:
        """Set the eplambda property."""
        self._cards[0].set_value("eplambda", value)

    @property
    def deatht(self) -> float:
        """Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and will be removed from the EM solve. If a negative value is entered, a *DEFINE_?FUNCTION will be expected. The following parameters are allowed: (vx, vy, vz, temp, vol, mass, Ex, Ey, Ez, Bx, By, Bz, Fx, Fy, Fz, JHrate, time). Fx, Fy, and Fz denote the components of the Lorentz force vector. A negative value returned by the *DEFINE_?FUNCTION corresponds to a �dead� or inactive element. Once an element has been removed from the EM solve, it cannot return.
        """ # nopep8
        return self._cards[0].get_value("deatht")

    @deatht.setter
    def deatht(self, value: float) -> None:
        """Set the deatht property."""
        self._cards[0].set_value("deatht", value)

    @property
    def rdltype(self) -> int:
        """Get or set the Used for the composite Tshell batteries modeled, with **EM_RANDLES_TSHELL. RDLTYPE specifies the function of the layer associated to MID:
        EQ.0: Default. Conductor which is not part of a battery cell
        EQ.1:Current Collector Positive
        EQ.2: Positive Electrode
        EQ.3:Separator
        EQ.4:Negative Electrode
        EQ.5:Current Collector Negative
        """ # nopep8
        return self._cards[0].get_value("rdltype")

    @rdltype.setter
    def rdltype(self, value: int) -> None:
        """Set the rdltype property."""
        if value not in [0, 1, 2, 3, 4, 5, None]:
            raise Exception("""rdltype must be `None` or one of {0,1,2,3,4,5}.""")
        self._cards[0].set_value("rdltype", value)

    @property
    def epsrr(self) -> typing.Optional[float]:
        """Get or set the Real and imaginary part of the relative permeability (dielectric constant and dielectric loss terms)
        """ # nopep8
        return self._cards[1].get_value("epsrr")

    @epsrr.setter
    def epsrr(self, value: float) -> None:
        """Set the epsrr property."""
        self._cards[1].set_value("epsrr", value)

    @property
    def eosid2(self) -> typing.Optional[int]:
        """Get or set the Optional IDs defining equation of states for EPSRR and EPSRI respectfully
        """ # nopep8
        return self._cards[1].get_value("eosid2")

    @eosid2.setter
    def eosid2(self, value: int) -> None:
        """Set the eosid2 property."""
        self._cards[1].set_value("eosid2", value)

    @property
    def epsri(self) -> typing.Optional[float]:
        """Get or set the Real and imaginary part of the relative permeability (dielectric constant and dielectric loss terms)
        """ # nopep8
        return self._cards[1].get_value("epsri")

    @epsri.setter
    def epsri(self, value: float) -> None:
        """Set the epsri property."""
        self._cards[1].set_value("epsri", value)

    @property
    def epsid3(self) -> typing.Optional[int]:
        """Get or set the Optional IDs defining equation of states for EPSRR and EPSRI respectfully
        """ # nopep8
        return self._cards[1].get_value("epsid3")

    @epsid3.setter
    def epsid3(self, value: int) -> None:
        """Set the epsid3 property."""
        self._cards[1].set_value("epsid3", value)

    @property
    def mid_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid:
                return kwd
        return None

    @mid_link.setter
    def mid_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid."""
        self.mid = value.mid

