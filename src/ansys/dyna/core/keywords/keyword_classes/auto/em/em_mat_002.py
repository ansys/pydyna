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

"""Module providing the EmMat002 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_EMMAT002_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("mtype", int, 10, 10, 0),
    FieldSchema("sigma", float, 20, 10, None),
    FieldSchema("eosid", int, 30, 10, None),
    FieldSchema("murel", float, 40, 10, None),
    FieldSchema("eosmu", int, 50, 10, None),
    FieldSchema("deatht", float, 60, 10, 1e+28),
)

_EMMAT002_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("eosidsf", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("muloss", float, 40, 10, None),
    FieldSchema("eosloss", int, 50, 10, None),
)

_EMMAT002_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("epsrr", float, 20, 10, None),
    FieldSchema("eosid2", int, 30, 10, None),
    FieldSchema("epsri", float, 40, 10, None),
    FieldSchema("eosid3", int, 50, 10, None),
)

class EmMat002(KeywordBase):
    """DYNA EM_MAT_002 keyword"""

    keyword = "EM"
    subkeyword = "MAT_002"
    _link_fields = {
        "mid": LinkType.MAT,
    }

    def __init__(self, **kwargs):
        """Initialize the EmMat002 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMMAT002_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMMAT002_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMMAT002_CARD2,
                **kwargs,
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. MID must reference a *MAT material since the electromagnetic properties are added onto the *MAT properties. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def mtype(self) -> int:
        """Get or set the Electromagnetism type of the material:
        EQ.0: Air or vacuum
        EQ.1: Insulator material: these materials have the same electromagnetism behavior asM TYPE=0
        EQ.2: Conductor carrying a source. In these conductors, the eddy current problem is solved, which gives the actual current density. Typically, this would correspond to the coil.
        EQ.4: Conductor not connected to any current or voltage source, where the Eddy current problem is solved. Typically, this would correspond to the workpiece
        .
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        if value not in [0, 1, 2, 4, None]:
            raise Exception("""mtype must be `None` or one of {0,1,2,4}.""")
        self._cards[0].set_value("mtype", value)

    @property
    def sigma(self) -> typing.Optional[float]:
        """Get or set the Initial electrical conductivity of the material.
        """ # nopep8
        return self._cards[0].get_value("sigma")

    @sigma.setter
    def sigma(self, value: float) -> None:
        """Set the sigma property."""
        self._cards[0].set_value("sigma", value)

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the ID of the EOS to be used for the electrical conductivity. (see *EM_EOS card).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def murel(self) -> typing.Optional[float]:
        """Get or set the Relative permeability, which is the ratio of the permeability of a specific medium to the permeability of free space (?_r=?/?_0). For frequency-based solvers, this is the real component of the relative permeability. See Remark 2.
        """ # nopep8
        return self._cards[0].get_value("murel")

    @murel.setter
    def murel(self, value: float) -> None:
        """Set the murel property."""
        self._cards[0].set_value("murel", value)

    @property
    def eosmu(self) -> typing.Optional[int]:
        """Get or set the ID of the EOS to be used to define the nonlinear behavior of ?. It is optional and only available for EMSOL = 1 on *EM_CONTROL. Note that if EOSMU is defined, MUREL will be used for the initial value only. See *EM_EOS_PERMEABILITY.
        """ # nopep8
        return self._cards[0].get_value("eosmu")

    @eosmu.setter
    def eosmu(self, value: int) -> None:
        """Set the eosmu property."""
        self._cards[0].set_value("eosmu", value)

    @property
    def deatht(self) -> float:
        """Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and will be removed from the EM solve. If a negative value is entered, a *DEFINE_FUNCTION will be expected. The following parameters are allowed: (vx, vy, vz, temp, vol, mass, Ex, Ey, Ez, Bx, By, Bz, Fx, Fy, Fz, JHrate, time). Fx, Fy, and Fz refer to the components of the Lorentz force vector. A negative value returned by the *DEFINE_FUNCTION corresponds to a 'dead' or inactive element. Once an element has been removed from the EM solve, it cannot return
        """ # nopep8
        return self._cards[0].get_value("deatht")

    @deatht.setter
    def deatht(self, value: float) -> None:
        """Set the deatht property."""
        self._cards[0].set_value("deatht", value)

    @property
    def eosidsf(self) -> typing.Optional[int]:
        """Get or set the For EMSOL = 1, this is an optional EOS for specifying a scale factor to the permeability EOS defined in EOSMU. This field only applies if EOSMU is defined, but it is optional. If used, the EOS must be either *EM_EOS_TABULATED1 or *EM_EOS_TABULATED2. See Remark 3.
        For frequency - based solver(EMSOL = 4 and 7 on * EM_CONTROL), this directly defines how the real component of the relative permeability should behave.See Remark 4.
        """ # nopep8
        return self._cards[1].get_value("eosidsf")

    @eosidsf.setter
    def eosidsf(self, value: int) -> None:
        """Set the eosidsf property."""
        self._cards[1].set_value("eosidsf", value)

    @property
    def muloss(self) -> typing.Optional[float]:
        """Get or set the Optional magnetic losses are defined by the loss tangent. The loss tangent can either be a constant (MULOSS) or defined with an EOS (EOSLOSS). Magnetic losses are only present in frequency-based solvers (EMSOL = 4 or 7 on *EM_CONTROL). See Remark 4
        """ # nopep8
        return self._cards[1].get_value("muloss")

    @muloss.setter
    def muloss(self, value: float) -> None:
        """Set the muloss property."""
        self._cards[1].set_value("muloss", value)

    @property
    def eosloss(self) -> typing.Optional[int]:
        """Get or set the Optional magnetic losses are defined by the loss tangent. The loss tangent can either be a constant (MULOSS) or defined with an EOS (EOSLOSS). Magnetic losses are only present in frequency-based solvers (EMSOL = 4 or 7 on *EM_CONTROL). See Remark 4
        """ # nopep8
        return self._cards[1].get_value("eosloss")

    @eosloss.setter
    def eosloss(self, value: int) -> None:
        """Set the eosloss property."""
        self._cards[1].set_value("eosloss", value)

    @property
    def epsrr(self) -> typing.Optional[float]:
        """Get or set the Real and imaginary parts of the relative permeability (dielectric constant and dielectric loss terms). Only present in relevant frequency-based solvers (EMSOL = 7 on *EM_CONTROL). See Remark 5
        """ # nopep8
        return self._cards[2].get_value("epsrr")

    @epsrr.setter
    def epsrr(self, value: float) -> None:
        """Set the epsrr property."""
        self._cards[2].set_value("epsrr", value)

    @property
    def eosid2(self) -> typing.Optional[int]:
        """Get or set the Optional IDs defining equation of states for EPSRR and EPSRI, respectively
        """ # nopep8
        return self._cards[2].get_value("eosid2")

    @eosid2.setter
    def eosid2(self, value: int) -> None:
        """Set the eosid2 property."""
        self._cards[2].set_value("eosid2", value)

    @property
    def epsri(self) -> typing.Optional[float]:
        """Get or set the Real and imaginary parts of the relative permeability (dielectric constant and dielectric loss terms). Only present in relevant frequency-based solvers (EMSOL = 7 on *EM_CONTROL). See Remark 5
        """ # nopep8
        return self._cards[2].get_value("epsri")

    @epsri.setter
    def epsri(self, value: float) -> None:
        """Set the epsri property."""
        self._cards[2].set_value("epsri", value)

    @property
    def eosid3(self) -> typing.Optional[int]:
        """Get or set the Optional IDs defining equation of states for EPSRR and EPSRI, respectively
        """ # nopep8
        return self._cards[2].get_value("eosid3")

    @eosid3.setter
    def eosid3(self, value: int) -> None:
        """Set the eosid3 property."""
        self._cards[2].set_value("eosid3", value)

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

