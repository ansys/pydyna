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

class IcfdMat(KeywordBase):
    """DYNA ICFD_MAT keyword"""

    keyword = "ICFD"
    subkeyword = "MAT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "flg",
                        int,
                        10,
                        10,
                        kwargs.get("flg", 1)
                    ),
                    Field(
                        "ro",
                        float,
                        20,
                        10,
                        kwargs.get("ro", 0)
                    ),
                    Field(
                        "vis",
                        float,
                        30,
                        10,
                        kwargs.get("vis", 0)
                    ),
                    Field(
                        "st",
                        float,
                        40,
                        10,
                        kwargs.get("st", 0)
                    ),
                    Field(
                        "stsflcid",
                        int,
                        50,
                        10,
                        kwargs.get("stsflcid")
                    ),
                    Field(
                        "ca",
                        float,
                        60,
                        10,
                        kwargs.get("ca", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hc",
                        float,
                        0,
                        10,
                        kwargs.get("hc", 0)
                    ),
                    Field(
                        "tc",
                        float,
                        10,
                        10,
                        kwargs.get("tc", 0)
                    ),
                    Field(
                        "beta",
                        float,
                        20,
                        10,
                        kwargs.get("beta", 0)
                    ),
                    Field(
                        "prt",
                        float,
                        30,
                        10,
                        kwargs.get("prt", 0.85)
                    ),
                    Field(
                        "hcsflcid",
                        int,
                        40,
                        10,
                        kwargs.get("hcsflcid")
                    ),
                    Field(
                        "tcsflcid",
                        int,
                        50,
                        10,
                        kwargs.get("tcsflcid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nnmoid",
                        int,
                        0,
                        10,
                        kwargs.get("nnmoid")
                    ),
                    Field(
                        "pmmoid",
                        int,
                        10,
                        10,
                        kwargs.get("pmmoid")
                    ),
                ],
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def flg(self) -> int:
        """Get or set the Flag to choose between fully incompressible, slightly compressible,or barotropic flows.
        EQ.0 : Vacuum (free surface problems only)
        EQ.1 : Fully incompressible fluid.
        """ # nopep8
        return self._cards[0].get_value("flg")

    @flg.setter
    def flg(self, value: int) -> None:
        self._cards[0].set_value("flg", value)

    @property
    def ro(self) -> float:
        """Get or set the Flow density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def vis(self) -> float:
        """Get or set the Dynamic viscosity.
        """ # nopep8
        return self._cards[0].get_value("vis")

    @vis.setter
    def vis(self, value: float) -> None:
        self._cards[0].set_value("vis", value)

    @property
    def st(self) -> float:
        """Get or set the Surface tension coefficient.
        """ # nopep8
        return self._cards[0].get_value("st")

    @st.setter
    def st(self, value: float) -> None:
        self._cards[0].set_value("st", value)

    @property
    def stsflcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for scale factor applied on ST function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("stsflcid")

    @stsflcid.setter
    def stsflcid(self, value: int) -> None:
        self._cards[0].set_value("stsflcid", value)

    @property
    def ca(self) -> float:
        """Get or set the Contact angle
        """ # nopep8
        return self._cards[0].get_value("ca")

    @ca.setter
    def ca(self, value: float) -> None:
        self._cards[0].set_value("ca", value)

    @property
    def hc(self) -> float:
        """Get or set the Heat capacity.
        """ # nopep8
        return self._cards[1].get_value("hc")

    @hc.setter
    def hc(self, value: float) -> None:
        self._cards[1].set_value("hc", value)

    @property
    def tc(self) -> float:
        """Get or set the Thermal conductivity.
        """ # nopep8
        return self._cards[1].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        self._cards[1].set_value("tc", value)

    @property
    def beta(self) -> float:
        """Get or set the Thermal expansion coefficient used in the Boussinesq approximation for buoyancy.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[1].set_value("beta", value)

    @property
    def prt(self) -> float:
        """Get or set the Turbulent Prandlt number. Only used if K-Epsilon turbulence model selected.
        """ # nopep8
        return self._cards[1].get_value("prt")

    @prt.setter
    def prt(self, value: float) -> None:
        self._cards[1].set_value("prt", value)

    @property
    def hcsflcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for scale factor applied on HC function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[1].get_value("hcsflcid")

    @hcsflcid.setter
    def hcsflcid(self, value: int) -> None:
        self._cards[1].set_value("hcsflcid", value)

    @property
    def tcsflcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for scale factor applied on TC function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION
        """ # nopep8
        return self._cards[1].get_value("tcsflcid")

    @tcsflcid.setter
    def tcsflcid(self, value: int) -> None:
        self._cards[1].set_value("tcsflcid", value)

    @property
    def nnmoid(self) -> typing.Optional[int]:
        """Get or set the Non-Newtonian model ID. This refers to a Non-Newtonian fluid model defined using *ICFD_MODEL_NONNEWT.
        """ # nopep8
        return self._cards[2].get_value("nnmoid")

    @nnmoid.setter
    def nnmoid(self, value: int) -> None:
        self._cards[2].set_value("nnmoid", value)

    @property
    def pmmoid(self) -> typing.Optional[int]:
        """Get or set the Porous media model ID. This refers to a porous media model defined using *ICFD_MODEL_POROUS.
        """ # nopep8
        return self._cards[2].get_value("pmmoid")

    @pmmoid.setter
    def pmmoid(self, value: int) -> None:
        self._cards[2].set_value("pmmoid", value)

