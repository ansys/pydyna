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

"""Module providing the EmMat004 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class EmMat004(KeywordBase):
    """DYNA EM_MAT_004 keyword"""

    keyword = "EM"
    subkeyword = "MAT_004"

    def __init__(self, **kwargs):
        """Initialize the EmMat004 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "mtype",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "sigma",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eosid",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nele",
                        int,
                        40,
                        10,
                        1,
                        **kwargs,
                    ),
                    Field(
                        "murel",
                        float,
                        50,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "eosmu",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "deatht",
                        float,
                        70,
                        10,
                        1.0E28,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID: refers to MID in the *PART card.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def mtype(self) -> int:
        """Get or set the Defines the electromagnetism type of the material:
        EQ.0: Air or vacuum
        EQ.1: Insulator material: these materials have the same electromagnetism behavior as EQ.0
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
        """Get or set the ID of the EOS to be used for the electrical conductivity (see *EM_EOS cards).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def nele(self) -> int:
        """Get or set the Number of elements in the thickness of the shell. It is up to the user to make sure his mesh is fine enough to correctly capture the inductive-diffusive effects (see skin depth definition).
        """ # nopep8
        return self._cards[0].get_value("nele")

    @nele.setter
    def nele(self, value: int) -> None:
        """Set the nele property."""
        self._cards[0].set_value("nele", value)

    @property
    def murel(self) -> float:
        """Get or set the Relative permeability which is the ratio of the permeability of a specific medium to the permeability of free space.
        """ # nopep8
        return self._cards[0].get_value("murel")

    @murel.setter
    def murel(self, value: float) -> None:
        """Set the murel property."""
        self._cards[0].set_value("murel", value)

    @property
    def eosmu(self) -> typing.Optional[int]:
        """Get or set the ID of the EOS to be used to define the nonlinear behavior of μ. Note: if EOSMU is defined, MUREL will be used for the initial value only. See EM_EOS_PERMEABILITY.
        """ # nopep8
        return self._cards[0].get_value("eosmu")

    @eosmu.setter
    def eosmu(self, value: int) -> None:
        """Set the eosmu property."""
        self._cards[0].set_value("eosmu", value)

    @property
    def deatht(self) -> float:
        """Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and will be removed from the EM solve.
        If a negative value is entered, a *DEFINE_FUNCTION will be expected. The following parameters are allowed:
        (vx, vy, vz, temp, vol, mass, Ex, Ey, Ez, Bx, By, Bz, Fx, Fy, Fz, JHrate, time). Fx, Fy, and Fz refer to the components of the Lorentz force vector.
        A negative value returned by the *DEFINE_‌FUNCTION corresponds to a ‘dead’ or inactive element. Once an element has been removed from the EM solve, it cannot return.
        """ # nopep8
        return self._cards[0].get_value("deatht")

    @deatht.setter
    def deatht(self, value: float) -> None:
        """Set the deatht property."""
        self._cards[0].set_value("deatht", value)

