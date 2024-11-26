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

class MatBilkhuDuboisFoam(KeywordBase):
    """DYNA MAT_BILKHU/DUBOIS_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "BILKHU/DUBOIS_FOAM"
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
                        "ym",
                        float,
                        20,
                        10,
                        kwargs.get("ym")
                    ),
                    Field(
                        "lcpy",
                        int,
                        30,
                        10,
                        kwargs.get("lcpy")
                    ),
                    Field(
                        "lcuys",
                        int,
                        40,
                        10,
                        kwargs.get("lcuys")
                    ),
                    Field(
                        "vc",
                        float,
                        50,
                        10,
                        kwargs.get("vc")
                    ),
                    Field(
                        "pc",
                        float,
                        60,
                        10,
                        kwargs.get("pc")
                    ),
                    Field(
                        "vpc",
                        float,
                        70,
                        10,
                        kwargs.get("vpc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tsc",
                        float,
                        0,
                        10,
                        kwargs.get("tsc")
                    ),
                    Field(
                        "vtsc",
                        float,
                        10,
                        10,
                        kwargs.get("vtsc")
                    ),
                    Field(
                        "lcrate",
                        int,
                        20,
                        10,
                        kwargs.get("lcrate")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "kcon",
                        float,
                        40,
                        10,
                        kwargs.get("kcon")
                    ),
                    Field(
                        "isflg",
                        int,
                        50,
                        10,
                        kwargs.get("isflg", 0)
                    ),
                    Field(
                        "ncycle",
                        float,
                        60,
                        10,
                        kwargs.get("ncycle")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatBilkhuDuboisFoam.option_specs[0],
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def ym(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("ym")

    @ym.setter
    def ym(self, value: float) -> None:
        self._cards[0].set_value("ym", value)

    @property
    def lcpy(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving pressure for plastic yielding versus volumetric strain.
        """ # nopep8
        return self._cards[0].get_value("lcpy")

    @lcpy.setter
    def lcpy(self, value: int) -> None:
        self._cards[0].set_value("lcpy", value)

    @property
    def lcuys(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving unixial yield stress versus volumetric strain.
        """ # nopep8
        return self._cards[0].get_value("lcuys")

    @lcuys.setter
    def lcuys(self, value: int) -> None:
        self._cards[0].set_value("lcuys", value)

    @property
    def vc(self) -> typing.Optional[float]:
        """Get or set the Viscous damping coefficient (0.05 < recommended value < 0.50).
        """ # nopep8
        return self._cards[0].get_value("vc")

    @vc.setter
    def vc(self, value: float) -> None:
        self._cards[0].set_value("vc", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff. If zero, the default is set to one-tenth of p0 , the yield pressure corresponding to a volumetric strain of zero.
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        self._cards[0].set_value("pc", value)

    @property
    def vpc(self) -> typing.Optional[float]:
        """Get or set the Variable pressure cutoff as a fraction of pressure yield value. If non-zero this will override the pressure cutoff value PC.
        """ # nopep8
        return self._cards[0].get_value("vpc")

    @vpc.setter
    def vpc(self, value: float) -> None:
        self._cards[0].set_value("vpc", value)

    @property
    def tsc(self) -> typing.Optional[float]:
        """Get or set the Tension cutoff for uniaxial tensile stress. Default is zero.
        """ # nopep8
        return self._cards[1].get_value("tsc")

    @tsc.setter
    def tsc(self, value: float) -> None:
        self._cards[1].set_value("tsc", value)

    @property
    def vtsc(self) -> typing.Optional[float]:
        """Get or set the Variable tension cutoff as a fraction of the uniaxial compressive yield strength, if non-zero this will override the tension cutoff value TC.
        """ # nopep8
        return self._cards[1].get_value("vtsc")

    @vtsc.setter
    def vtsc(self, value: float) -> None:
        self._cards[1].set_value("vtsc", value)

    @property
    def lcrate(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving a scale factor for the previous yield curves, dependent upon the volumetric plastic strain.
        """ # nopep8
        return self._cards[1].get_value("lcrate")

    @lcrate.setter
    def lcrate(self, value: int) -> None:
        self._cards[1].set_value("lcrate", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson coefficient, which applies to both elastic and plastic deformations.
        """ # nopep8
        return self._cards[1].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[1].set_value("pr", value)

    @property
    def kcon(self) -> typing.Optional[float]:
        """Get or set the Stiffness coefficient for contact interface stiffness. If undefined one-third
        of Young's modulus, YM, is used. KCON is also considered in the element
        time step calculation; therefore, large values may reduce the element
        time step size.
        """ # nopep8
        return self._cards[1].get_value("kcon")

    @kcon.setter
    def kcon(self, value: float) -> None:
        self._cards[1].set_value("kcon", value)

    @property
    def isflg(self) -> int:
        """Get or set the Flag for tensile response (active only if negative abscissa are present in load curve LCUYS)
        EQ.0: load curve abscissa in tensile region correspond to volumetric strain
        EQ.1: load curve abscissa in tensile region correspond to effective strain.
        """ # nopep8
        return self._cards[1].get_value("isflg")

    @isflg.setter
    def isflg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""isflg must be one of {0,1}""")
        self._cards[1].set_value("isflg", value)

    @property
    def ncycle(self) -> typing.Optional[float]:
        """Get or set the Number of cycles to determine the average volumetric strain rate. NCYCLE is 1 by default (no smoothing) and cannot exceed 100.
        """ # nopep8
        return self._cards[1].get_value("ncycle")

    @ncycle.setter
    def ncycle(self, value: float) -> None:
        self._cards[1].set_value("ncycle", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

