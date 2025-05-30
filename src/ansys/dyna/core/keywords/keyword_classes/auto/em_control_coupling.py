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

"""Module providing the EmControlCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class EmControlCoupling(KeywordBase):
    """DYNA EM_CONTROL_COUPLING keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_COUPLING"

    def __init__(self, **kwargs):
        """Initialize the EmControlCoupling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "thcoupl",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "smcoupl",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "thlcid",
                        int,
                        20,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "smlcid",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "thcplfl",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "smcplfl",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "cflag",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nflag",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "smmod",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "dfx",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dfy",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "dfz",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def thcoupl(self) -> int:
        """Get or set the Coupling to the thermal solver. When turned on, the EM solver will transfer the Joule heating terms to the solid mechanics thermal solver.
        EQ.0:Coupling on.
        EQ.1:Coupling off.
        """ # nopep8
        return self._cards[0].get_value("thcoupl")

    @thcoupl.setter
    def thcoupl(self, value: int) -> None:
        """Set the thcoupl property."""
        if value not in [0, 1, None]:
            raise Exception("""thcoupl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("thcoupl", value)

    @property
    def smcoupl(self) -> int:
        """Get or set the Coupling to the solid mechanics solver. When turned on, the EM solver will transfer the Lorentz forces to the solid mechanics solver.
        EQ.0:Coupling on.Volumic Lorentz forces are transferred
        EQ.1:Coupling off.
        EQ.2:	Coupling on. Surface magnetic forces are transferred. More accurate representation of EM forces in cases involving magnets or non-linear ferromagnets. See *EM_SOLVER_FEMBEM_MONOLITHIC.
        EQ.3:	Coupling on.Surface magnetic forces are transferred on magnets and ferromagnets while volumic Lorentz forces are transferred to regular conductors
        """ # nopep8
        return self._cards[0].get_value("smcoupl")

    @smcoupl.setter
    def smcoupl(self, value: int) -> None:
        """Set the smcoupl property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""smcoupl must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("smcoupl", value)

    @property
    def thlcid(self) -> int:
        """Get or set the Optional load curve ID. When defined, the heat rate transferred to the thermal solver will be scaled by the value returned by THLCID.

        """ # nopep8
        return self._cards[0].get_value("thlcid")

    @thlcid.setter
    def thlcid(self, value: int) -> None:
        """Set the thlcid property."""
        self._cards[0].set_value("thlcid", value)

    @property
    def smlcid(self) -> int:
        """Get or set the Optional load curve ID. When defined, the forces transferred to the solid mechanics solver will be scaled by the value returned by SMLCID.

        """ # nopep8
        return self._cards[0].get_value("smlcid")

    @smlcid.setter
    def smlcid(self, value: int) -> None:
        """Set the smlcid property."""
        self._cards[0].set_value("smlcid", value)

    @property
    def thcplfl(self) -> int:
        """Get or set the Optional load curve ID. When defined, the forces transferred to the solid mechanics solver will be scaled by the value returned by SMLCID
        """ # nopep8
        return self._cards[0].get_value("thcplfl")

    @thcplfl.setter
    def thcplfl(self, value: int) -> None:
        """Set the thcplfl property."""
        self._cards[0].set_value("thcplfl", value)

    @property
    def smcplfl(self) -> int:
        """Get or set the Coupling to the heat equation when EM quantities are solved on fluid elements. When turned on, the EM solver will transfer the Joule heating terms to the ICFD solver.
        EQ.0:	Coupling off.
        EQ.1 : Coupling on.
        """ # nopep8
        return self._cards[0].get_value("smcplfl")

    @smcplfl.setter
    def smcplfl(self, value: int) -> None:
        """Set the smcplfl property."""
        if value not in [0, 1, None]:
            raise Exception("""smcplfl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("smcplfl", value)

    @property
    def cflag(self) -> typing.Optional[int]:
        """Get or set the convergence flag, negative values call for a curve.
        """ # nopep8
        return self._cards[0].get_value("cflag")

    @cflag.setter
    def cflag(self, value: int) -> None:
        """Set the cflag property."""
        self._cards[0].set_value("cflag", value)

    @property
    def nflag(self) -> typing.Optional[int]:
        """Get or set the flag of num of max iterations for the new convergence, negative values call for a curve.
        """ # nopep8
        return self._cards[0].get_value("nflag")

    @nflag.setter
    def nflag(self, value: int) -> None:
        """Set the nflag property."""
        self._cards[0].set_value("nflag", value)

    @property
    def smmod(self) -> int:
        """Get or set the Coupling to the solid mechanics solver. When turned on, the EM solver will transfer forces to the solid mechanics solver.
        EQ.0:	Off.
        EQ.1 : Force calculation at element level is decided by * DEFINE_FUNCTION.See DFX, DFYand DFZ.
        EQ.2 : Force calculation at element level is decided by usermat routine.See dyn21em.f and user_getEMForceArray routine
        """ # nopep8
        return self._cards[1].get_value("smmod")

    @smmod.setter
    def smmod(self, value: int) -> None:
        """Set the smmod property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""smmod must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("smmod", value)

    @property
    def dfx(self) -> typing.Optional[int]:
        """Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2
        """ # nopep8
        return self._cards[1].get_value("dfx")

    @dfx.setter
    def dfx(self, value: int) -> None:
        """Set the dfx property."""
        self._cards[1].set_value("dfx", value)

    @property
    def dfy(self) -> typing.Optional[int]:
        """Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2
        """ # nopep8
        return self._cards[1].get_value("dfy")

    @dfy.setter
    def dfy(self, value: int) -> None:
        """Set the dfy property."""
        self._cards[1].set_value("dfy", value)

    @property
    def dfz(self) -> typing.Optional[int]:
        """Get or set the Define function IDs for the force three components if SMMOD=1. Arguments for the Define functions are the same as in *EM_EOS_TABULATED2
        """ # nopep8
        return self._cards[1].get_value("dfz")

    @dfz.setter
    def dfz(self, value: int) -> None:
        """Set the dfz property."""
        self._cards[1].set_value("dfz", value)

