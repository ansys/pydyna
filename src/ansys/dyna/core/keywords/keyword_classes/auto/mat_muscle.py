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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatMuscle(KeywordBase):
    """DYNA MAT_MUSCLE keyword"""

    keyword = "MAT"
    subkeyword = "MUSCLE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
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
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "sno",
                        float,
                        20,
                        10,
                        kwargs.get("sno")
                    ),
                    Field(
                        "srm",
                        float,
                        30,
                        10,
                        kwargs.get("srm")
                    ),
                    Field(
                        "pis",
                        float,
                        40,
                        10,
                        kwargs.get("pis")
                    ),
                    Field(
                        "ssm",
                        float,
                        50,
                        10,
                        kwargs.get("ssm")
                    ),
                    Field(
                        "cer",
                        float,
                        60,
                        10,
                        kwargs.get("cer")
                    ),
                    Field(
                        "dmp",
                        float,
                        70,
                        10,
                        kwargs.get("dmp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alm",
                        float,
                        0,
                        10,
                        kwargs.get("alm")
                    ),
                    Field(
                        "sfr",
                        float,
                        10,
                        10,
                        kwargs.get("sfr")
                    ),
                    Field(
                        "svs",
                        float,
                        20,
                        10,
                        kwargs.get("svs")
                    ),
                    Field(
                        "svr",
                        float,
                        30,
                        10,
                        kwargs.get("svr")
                    ),
                    Field(
                        "ssp",
                        float,
                        40,
                        10,
                        kwargs.get("ssp")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatMuscle.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Material density in the initial undeformed configuration.  .
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def sno(self) -> typing.Optional[float]:
        """Get or set the Initial stretch ratio,  , i.e., the current length as defined by the nodal points at t=0 divided by the initial length.  The density for the nodal mass calculation is RO/SNO, or  .
        """ # nopep8
        return self._cards[0].get_value("sno")

    @sno.setter
    def sno(self, value: float) -> None:
        self._cards[0].set_value("sno", value)

    @property
    def srm(self) -> typing.Optional[float]:
        """Get or set the Maximum strain rate..
        """ # nopep8
        return self._cards[0].get_value("srm")

    @srm.setter
    def srm(self, value: float) -> None:
        self._cards[0].set_value("srm", value)

    @property
    def pis(self) -> typing.Optional[float]:
        """Get or set the Peak isometric stress corresponding to the dimensionless value of unity in the dimensionless stress versus strain function, see SSP below
        """ # nopep8
        return self._cards[0].get_value("pis")

    @pis.setter
    def pis(self, value: float) -> None:
        self._cards[0].set_value("pis", value)

    @property
    def ssm(self) -> typing.Optional[float]:
        """Get or set the Strain when the dimensionless stress versus strain function, SSP below, reaches its maximum stress value.
        """ # nopep8
        return self._cards[0].get_value("ssm")

    @ssm.setter
    def ssm(self, value: float) -> None:
        self._cards[0].set_value("ssm", value)

    @property
    def cer(self) -> typing.Optional[float]:
        """Get or set the Constant, governing the exponential rise of SSP.  Required if SSP=0.
        """ # nopep8
        return self._cards[0].get_value("cer")

    @cer.setter
    def cer(self, value: float) -> None:
        self._cards[0].set_value("cer", value)

    @property
    def dmp(self) -> typing.Optional[float]:
        """Get or set the Damping constant
        """ # nopep8
        return self._cards[0].get_value("dmp")

    @dmp.setter
    def dmp(self, value: float) -> None:
        self._cards[0].set_value("dmp", value)

    @property
    def alm(self) -> typing.Optional[float]:
        """Get or set the Activation level vs. time.
        LT.0: absolute value gives load curve ID
        GE.0: constant value of ALM is used
        """ # nopep8
        return self._cards[1].get_value("alm")

    @alm.setter
    def alm(self, value: float) -> None:
        self._cards[1].set_value("alm", value)

    @property
    def sfr(self) -> typing.Optional[float]:
        """Get or set the Scale factor for strain rate maximum vs. the stretch ratio,  .
        LT.0: absolute value gives load curve ID
        GE.0: constant value of 1.0 is used
        """ # nopep8
        return self._cards[1].get_value("sfr")

    @sfr.setter
    def sfr(self, value: float) -> None:
        self._cards[1].set_value("sfr", value)

    @property
    def svs(self) -> typing.Optional[float]:
        """Get or set the Active dimensionless tensile stress vs. the stretch ratio,  .
        LT.0: absolute value gives load curve ID
        GE.0: constant value of 1.0 is used
        """ # nopep8
        return self._cards[1].get_value("svs")

    @svs.setter
    def svs(self, value: float) -> None:
        self._cards[1].set_value("svs", value)

    @property
    def svr(self) -> typing.Optional[float]:
        """Get or set the Active dimensionless tensile stress vs. the normalized strain rate,  .
        LT.0: absolute value gives load curve ID
        GE.0: constant value of 1.0 is used.
        """ # nopep8
        return self._cards[1].get_value("svr")

    @svr.setter
    def svr(self, value: float) -> None:
        self._cards[1].set_value("svr", value)

    @property
    def ssp(self) -> typing.Optional[float]:
        """Get or set the Isometric dimensionless stress vs. the stretch ratio,   for the parallel elastic element.
        LT.0: absolute value gives load curve ID
        EQ.0: exponential function is used (see below)
        GT.0: constant value of 0.0 is used
        """ # nopep8
        return self._cards[1].get_value("ssp")

    @ssp.setter
    def ssp(self, value: float) -> None:
        self._cards[1].set_value("ssp", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

