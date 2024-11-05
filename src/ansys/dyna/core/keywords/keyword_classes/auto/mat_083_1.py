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

class Mat0831(KeywordBase):
    """DYNA MAT_083_1 keyword"""

    keyword = "MAT"
    subkeyword = "083_1"
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
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "kcon",
                        float,
                        30,
                        10,
                        kwargs.get("kcon")
                    ),
                    Field(
                        "tc",
                        float,
                        40,
                        10,
                        kwargs.get("tc", 1.0E+20)
                    ),
                    Field(
                        "fail",
                        float,
                        50,
                        10,
                        kwargs.get("fail", 0.0)
                    ),
                    Field(
                        "damp",
                        float,
                        60,
                        10,
                        kwargs.get("damp")
                    ),
                    Field(
                        "tbid",
                        int,
                        70,
                        10,
                        kwargs.get("tbid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "bvflag",
                        float,
                        0,
                        10,
                        kwargs.get("bvflag", 0.0)
                    ),
                    Field(
                        "sflag",
                        float,
                        10,
                        10,
                        kwargs.get("sflag", 0.0)
                    ),
                    Field(
                        "rflag",
                        float,
                        20,
                        10,
                        kwargs.get("rflag", 0.0)
                    ),
                    Field(
                        "tflag",
                        float,
                        30,
                        10,
                        kwargs.get("tflag", 0.0)
                    ),
                    Field(
                        "pvid",
                        int,
                        40,
                        10,
                        kwargs.get("pvid", 0)
                    ),
                    Field(
                        "sraf",
                        float,
                        50,
                        10,
                        kwargs.get("sraf", 0.0)
                    ),
                    Field(
                        "ref",
                        float,
                        60,
                        10,
                        kwargs.get("ref", 0.0)
                    ),
                    Field(
                        "hu",
                        float,
                        70,
                        10,
                        kwargs.get("hu", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "minr",
                        float,
                        0,
                        10,
                        kwargs.get("minr")
                    ),
                    Field(
                        "maxr",
                        float,
                        10,
                        10,
                        kwargs.get("maxr")
                    ),
                    Field(
                        "shape",
                        float,
                        20,
                        10,
                        kwargs.get("shape")
                    ),
                    Field(
                        "betat",
                        float,
                        30,
                        10,
                        kwargs.get("betat")
                    ),
                    Field(
                        "betac",
                        float,
                        40,
                        10,
                        kwargs.get("betac")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "expon",
                        float,
                        0,
                        10,
                        kwargs.get("expon", 1.0)
                    ),
                    Field(
                        "riuld",
                        float,
                        10,
                        10,
                        kwargs.get("riuld", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat0831.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def kcon(self) -> typing.Optional[float]:
        """Get or set the Optional Young's modulus used in the computation of sound speed. This will influence the time step, contact forces, hourglass stabilization forces, and numerical damping(DAMP).
        EQ.0.0: KCON is set equal to the max(E, current tangent to stress-strain curve) if TBID .ne.0. If TBID.eq.0, KCON is set equal to the maximum slope of the stress-strain curve.
        """ # nopep8
        return self._cards[0].get_value("kcon")

    @kcon.setter
    def kcon(self, value: float) -> None:
        self._cards[0].set_value("kcon", value)

    @property
    def tc(self) -> float:
        """Get or set the Tension cut-off stress.
        """ # nopep8
        return self._cards[0].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        self._cards[0].set_value("tc", value)

    @property
    def fail(self) -> float:
        """Get or set the Failure option after cutoff stress is reached:
        EQ.0.0: tensile stress remains at cut-off value (default),
        EQ.1.0: tensile stress is reset to zero.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""fail must be one of {0.0,1.0}""")
        self._cards[0].set_value("fail", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Viscous coefficient (0.05 < recommended value < 0.50) to model damping effects.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[0].set_value("damp", value)

    @property
    def tbid(self) -> typing.Optional[int]:
        """Get or set the Table ID, see *DEFINE_TABLE, for nominal stress strain data as a function of strain rate. If the table ID is provided, cards 3 and 4 may be left blank and the fit will be done internally.
        """ # nopep8
        return self._cards[0].get_value("tbid")

    @tbid.setter
    def tbid(self, value: int) -> None:
        self._cards[0].set_value("tbid", value)

    @property
    def bvflag(self) -> float:
        """Get or set the Toggle to turn bulk viscosity off or on (see Remark 1):
        LT.1.0:	No bulk viscosity(recommended)
        GE.1.0 : Bulk viscosity active.
        """ # nopep8
        return self._cards[1].get_value("bvflag")

    @bvflag.setter
    def bvflag(self, value: float) -> None:
        self._cards[1].set_value("bvflag", value)

    @property
    def sflag(self) -> float:
        """Get or set the Strain rate flag:
        EQ.0.0: true constant strain rate (default),
        EQ.1.0: engineering strain rate.
        """ # nopep8
        return self._cards[1].get_value("sflag")

    @sflag.setter
    def sflag(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""sflag must be one of {0.0,1.0}""")
        self._cards[1].set_value("sflag", value)

    @property
    def rflag(self) -> float:
        """Get or set the Strain rate evaluation flag:
        EQ.0.0: first principal direction (default),
        EQ.1.0: principal strain rates for each principal direction,
        EQ.2.0: volumetric strain rate.
        """ # nopep8
        return self._cards[1].get_value("rflag")

    @rflag.setter
    def rflag(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0]:
            raise Exception("""rflag must be one of {0.0,1.0,2.0}""")
        self._cards[1].set_value("rflag", value)

    @property
    def tflag(self) -> float:
        """Get or set the Tensile stress evaluation:
        EQ.0.0: linear in tension (default),
        EQ.1.0: input via load curves with the tensile response corresponds to negative values of stress and strain.
        """ # nopep8
        return self._cards[1].get_value("tflag")

    @tflag.setter
    def tflag(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""tflag must be one of {0.0,1.0}""")
        self._cards[1].set_value("tflag", value)

    @property
    def pvid(self) -> int:
        """Get or set the Optional load curve ID defining pressure versus volumetric strain.
        """ # nopep8
        return self._cards[1].get_value("pvid")

    @pvid.setter
    def pvid(self, value: int) -> None:
        self._cards[1].set_value("pvid", value)

    @property
    def sraf(self) -> float:
        """Get or set the Strain rate averaging flag.
        EQ.0.0: use weighted running average.
        EQ.1.0: average the last twelve values.
        """ # nopep8
        return self._cards[1].get_value("sraf")

    @sraf.setter
    def sraf(self, value: float) -> None:
        self._cards[1].set_value("sraf", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the kyword.*INITIAL_FROM_REFERENCE_GEOMETRY
        EQ. 0.0: off
        EQ.1.0: on.
        """ # nopep8
        return self._cards[1].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[1].set_value("ref", value)

    @property
    def hu(self) -> float:
        """Get or set the Hysteretic unloading factor between 0 and 1 (default=0).
        """ # nopep8
        return self._cards[1].get_value("hu")

    @hu.setter
    def hu(self, value: float) -> None:
        self._cards[1].set_value("hu", value)

    @property
    def minr(self) -> typing.Optional[float]:
        """Get or set the Ratemin, minimum strain rate of interest
        """ # nopep8
        return self._cards[2].get_value("minr")

    @minr.setter
    def minr(self, value: float) -> None:
        self._cards[2].set_value("minr", value)

    @property
    def maxr(self) -> typing.Optional[float]:
        """Get or set the Ratemax, maximum strain rate of interest
        """ # nopep8
        return self._cards[2].get_value("maxr")

    @maxr.setter
    def maxr(self, value: float) -> None:
        self._cards[2].set_value("maxr", value)

    @property
    def shape(self) -> typing.Optional[float]:
        """Get or set the Shape factor for unloading.  Active for nonzero values of the hysteretic unloading factor HU. Values less than one reduces the energy dissipation and greater than one increases dissipation, see also Figure 57.1.
        """ # nopep8
        return self._cards[2].get_value("shape")

    @shape.setter
    def shape(self, value: float) -> None:
        self._cards[2].set_value("shape", value)

    @property
    def betat(self) -> typing.Optional[float]:
        """Get or set the Decay constant for damage in tension.  The damage decays after loading in ceases according to  .
        """ # nopep8
        return self._cards[2].get_value("betat")

    @betat.setter
    def betat(self, value: float) -> None:
        self._cards[2].set_value("betat", value)

    @property
    def betac(self) -> typing.Optional[float]:
        """Get or set the Decay constant for damage in compression. .  The damage decays after loading in ceases according to  .
        """ # nopep8
        return self._cards[2].get_value("betac")

    @betac.setter
    def betac(self, value: float) -> None:
        self._cards[2].set_value("betac", value)

    @property
    def expon(self) -> float:
        """Get or set the Exponent for unloading.  Active for nonzero values of the hysteretic unloading factor HU.  Default is 1.0.
        """ # nopep8
        return self._cards[3].get_value("expon")

    @expon.setter
    def expon(self, value: float) -> None:
        self._cards[3].set_value("expon", value)

    @property
    def riuld(self) -> float:
        """Get or set the Flag for rate independent unloading, see Remark 6.
        EQ.0.0:	off,
        EQ.1.0:	on.
        """ # nopep8
        return self._cards[3].get_value("riuld")

    @riuld.setter
    def riuld(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""riuld must be one of {0.0,1.0}""")
        self._cards[3].set_value("riuld", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

