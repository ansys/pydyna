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

class AirbagLinearFluid(KeywordBase):
    """DYNA AIRBAG_LINEAR_FLUID keyword"""

    keyword = "AIRBAG"
    subkeyword = "LINEAR_FLUID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "sidtyp",
                        int,
                        10,
                        10,
                        kwargs.get("sidtyp", 0)
                    ),
                    Field(
                        "rbid",
                        int,
                        20,
                        10,
                        kwargs.get("rbid", 0)
                    ),
                    Field(
                        "vsca",
                        float,
                        30,
                        10,
                        kwargs.get("vsca", 1.0)
                    ),
                    Field(
                        "psca",
                        float,
                        40,
                        10,
                        kwargs.get("psca", 1.0)
                    ),
                    Field(
                        "vini",
                        float,
                        50,
                        10,
                        kwargs.get("vini", 0.0)
                    ),
                    Field(
                        "mwd",
                        float,
                        60,
                        10,
                        kwargs.get("mwd", 0.0)
                    ),
                    Field(
                        "spsf",
                        float,
                        70,
                        10,
                        kwargs.get("spsf", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "bulk",
                        float,
                        0,
                        10,
                        kwargs.get("bulk")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "lcint",
                        int,
                        20,
                        10,
                        kwargs.get("lcint")
                    ),
                    Field(
                        "lcoutt",
                        int,
                        30,
                        10,
                        kwargs.get("lcoutt")
                    ),
                    Field(
                        "lcoutp",
                        int,
                        40,
                        10,
                        kwargs.get("lcoutp")
                    ),
                    Field(
                        "lcfit",
                        int,
                        50,
                        10,
                        kwargs.get("lcfit")
                    ),
                    Field(
                        "lcbulk",
                        int,
                        60,
                        10,
                        kwargs.get("lcbulk")
                    ),
                    Field(
                        "lcid",
                        int,
                        70,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p_limit",
                        float,
                        0,
                        10,
                        kwargs.get("p_limit")
                    ),
                    Field(
                        "p_limlc",
                        int,
                        10,
                        10,
                        kwargs.get("p_limlc")
                    ),
                    Field(
                        "nonull",
                        int,
                        20,
                        10,
                        kwargs.get("nonull")
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set type:
        EQ.0: segment,
        EQ.1: part IDs.
        """ # nopep8
        return self._cards[0].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sidtyp must be one of {0,1}""")
        self._cards[0].set_value("sidtyp", value)

    @property
    def rbid(self) -> int:
        """Get or set the Rigid body part ID for user defined activation subroutine:
        EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
        EQ.0: the control volume is active from time zero,
        EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
        """ # nopep8
        return self._cards[0].get_value("rbid")

    @rbid.setter
    def rbid(self, value: int) -> None:
        self._cards[0].set_value("rbid", value)

    @property
    def vsca(self) -> float:
        """Get or set the Volume scale factor, V-sca (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("vsca")

    @vsca.setter
    def vsca(self, value: float) -> None:
        self._cards[0].set_value("vsca", value)

    @property
    def psca(self) -> float:
        """Get or set the Pressure scale factor, P-sca (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("psca")

    @psca.setter
    def psca(self, value: float) -> None:
        self._cards[0].set_value("psca", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial filled volume, V-ini (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        self._cards[0].set_value("vini", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        self._cards[0].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
        """ # nopep8
        return self._cards[0].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        self._cards[0].set_value("spsf", value)

    @property
    def bulk(self) -> typing.Optional[float]:
        """Get or set the K, bulk modulus of the fluid in the control volume. Constant as a function of time. Define if LCBULK=0.  .
        """ # nopep8
        return self._cards[1].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        self._cards[1].set_value("bulk", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Density of the fluid.
        """ # nopep8
        return self._cards[1].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[1].set_value("ro", value)

    @property
    def lcint(self) -> typing.Optional[int]:
        """Get or set the F(t) input flow curve defining mass per unit time as a function of time, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcint")

    @lcint.setter
    def lcint(self, value: int) -> None:
        self._cards[1].set_value("lcint", value)

    @property
    def lcoutt(self) -> typing.Optional[int]:
        """Get or set the G(t), output flow curve defining mass per unit time as a function of time. This load curve is optional.
        """ # nopep8
        return self._cards[1].get_value("lcoutt")

    @lcoutt.setter
    def lcoutt(self, value: int) -> None:
        self._cards[1].set_value("lcoutt", value)

    @property
    def lcoutp(self) -> typing.Optional[int]:
        """Get or set the H(p), output flow curve defining mass per unit time as a function of pressure. This load curve is optional.
        """ # nopep8
        return self._cards[1].get_value("lcoutp")

    @lcoutp.setter
    def lcoutp(self, value: int) -> None:
        self._cards[1].set_value("lcoutp", value)

    @property
    def lcfit(self) -> typing.Optional[int]:
        """Get or set the L(t), added pressure as a function of time. This load curve is optional.
        """ # nopep8
        return self._cards[1].get_value("lcfit")

    @lcfit.setter
    def lcfit(self, value: int) -> None:
        self._cards[1].set_value("lcfit", value)

    @property
    def lcbulk(self) -> typing.Optional[int]:
        """Get or set the Curve defining the bulk modulus as a function of time. This load curve is optional, but if defined, the constant, BULK, is not used.
        """ # nopep8
        return self._cards[1].get_value("lcbulk")

    @lcbulk.setter
    def lcbulk(self, value: int) -> None:
        self._cards[1].set_value("lcbulk", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining pressure versus time, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def p_limit(self) -> typing.Optional[float]:
        """Get or set the Limiting value on total pressure (optional).
        """ # nopep8
        return self._cards[2].get_value("p_limit")

    @p_limit.setter
    def p_limit(self, value: float) -> None:
        self._cards[2].set_value("p_limit", value)

    @property
    def p_limlc(self) -> typing.Optional[int]:
        """Get or set the Curve defining the limiting pressure value as a function of time.
        If nonzero, P_LIMIT is ignored.
        """ # nopep8
        return self._cards[2].get_value("p_limlc")

    @p_limlc.setter
    def p_limlc(self, value: int) -> None:
        self._cards[2].set_value("p_limlc", value)

    @property
    def nonull(self) -> typing.Optional[int]:
        """Get or set the A flag to applying pressure on null material.
        EQ.0:	apply pressure everywhere inside the airbag.
        NE.0:	do not apply pressure on null material part of the airbag.
        This feature is useful in a hydroforming simulation, where typically the part set that makes up the airbag will include a part ID of null shells, defined by *MAT_NULL.
        The null shells and a deformable sheet blank will form an airbag, which upon pressurization, will push the blank into a die cavity, forming the blank.
        This feature is available in SMP from Dev 136254.
        """ # nopep8
        return self._cards[2].get_value("nonull")

    @nonull.setter
    def nonull(self, value: int) -> None:
        self._cards[2].set_value("nonull", value)

