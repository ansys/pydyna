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

class Mat073(KeywordBase):
    """DYNA MAT_073 keyword"""

    keyword = "MAT"
    subkeyword = "073"
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
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "tc",
                        float,
                        40,
                        10,
                        kwargs.get("tc", 1.0E+20)
                    ),
                    Field(
                        "hu",
                        float,
                        50,
                        10,
                        kwargs.get("hu", 1.0)
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "damp",
                        float,
                        70,
                        10,
                        kwargs.get("damp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "shape",
                        float,
                        0,
                        10,
                        kwargs.get("shape", 1.0)
                    ),
                    Field(
                        "fail",
                        float,
                        10,
                        10,
                        kwargs.get("fail")
                    ),
                    Field(
                        "bvflag",
                        float,
                        20,
                        10,
                        kwargs.get("bvflag")
                    ),
                    Field(
                        "kcon",
                        float,
                        30,
                        10,
                        kwargs.get("kcon")
                    ),
                    Field(
                        "lcid2",
                        int,
                        40,
                        10,
                        kwargs.get("lcid2", 0)
                    ),
                    Field(
                        "bstart",
                        float,
                        50,
                        10,
                        kwargs.get("bstart")
                    ),
                    Field(
                        "tramp",
                        float,
                        60,
                        10,
                        kwargs.get("tramp")
                    ),
                    Field(
                        "nv",
                        int,
                        70,
                        10,
                        kwargs.get("nv", 6)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gi",
                        float,
                        0,
                        10,
                        kwargs.get("gi")
                    ),
                    Field(
                        "betai",
                        float,
                        10,
                        10,
                        kwargs.get("betai")
                    ),
                    Field(
                        "ref",
                        float,
                        20,
                        10,
                        kwargs.get("ref")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat073.option_specs[0],
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
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, for nominal stress versus strain.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def tc(self) -> float:
        """Get or set the Tension cut-off stress (default = 1.0E+20).
        """ # nopep8
        return self._cards[0].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        self._cards[0].set_value("tc", value)

    @property
    def hu(self) -> float:
        """Get or set the Hysteretic unloading factor between 0 and 1 (default=1, i.e., no energy dissipation).
        """ # nopep8
        return self._cards[0].get_value("hu")

    @hu.setter
    def hu(self, value: float) -> None:
        self._cards[0].set_value("hu", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the beta, decay constant to model creep in unloading.
        EQ:0 No relaxation (default).
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[0].set_value("beta", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Viscous coefficient (0.05 < recommended value < 0.50) to model damping effects.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        self._cards[0].set_value("damp", value)

    @property
    def shape(self) -> float:
        """Get or set the Shape factor for unloading. Active for nonzero values of the hysteretic unloading factor. Values less than one reduces the energy dissipation and greater than one increases dissipation.
        """ # nopep8
        return self._cards[1].get_value("shape")

    @shape.setter
    def shape(self, value: float) -> None:
        self._cards[1].set_value("shape", value)

    @property
    def fail(self) -> typing.Optional[float]:
        """Get or set the Failure option after cutoff stress is reached:
        EQ.0.0: tensile stress remains at cut-off value (default),
        EQ.1.0: tensile stress is reset to zero.
        """ # nopep8
        return self._cards[1].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        self._cards[1].set_value("fail", value)

    @property
    def bvflag(self) -> typing.Optional[float]:
        """Get or set the Bulk viscosity activation flag:
        EQ.0.0: no bulk viscosity (default),
        EQ.1.0: bulk viscosity active.
        """ # nopep8
        return self._cards[1].get_value("bvflag")

    @bvflag.setter
    def bvflag(self, value: float) -> None:
        self._cards[1].set_value("bvflag", value)

    @property
    def kcon(self) -> typing.Optional[float]:
        """Get or set the Stiffness coefficient for contact interface stiffness. Maximum slope in stress vs. strain curve is used. When the maximum slope is taken for the contact, the time step size for this material is reduced for stability. In some cases dt may be significantly smaller, and defining a reasonable stiffness is recommended.
        """ # nopep8
        return self._cards[1].get_value("kcon")

    @kcon.setter
    def kcon(self, value: float) -> None:
        self._cards[1].set_value("kcon", value)

    @property
    def lcid2(self) -> int:
        """Get or set the Load curve ID of relaxation curve. If LCID=0 the constants beta-i are determined via a least squares fit.This model ignores the constant stress.
        """ # nopep8
        return self._cards[1].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        self._cards[1].set_value("lcid2", value)

    @property
    def bstart(self) -> typing.Optional[float]:
        """Get or set the Fit parameter. In the fit, beta-1 is set to zero, beta-2 is set to BSTART, beta-3 is 10 times beta-k-2 , beta-4 is 100 times greater than beta-3 , and so on.
        EQ.0: BSTART= .01 (default).
        """ # nopep8
        return self._cards[1].get_value("bstart")

    @bstart.setter
    def bstart(self, value: float) -> None:
        self._cards[1].set_value("bstart", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Optional ramp time for loading.
        """ # nopep8
        return self._cards[1].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        self._cards[1].set_value("tramp", value)

    @property
    def nv(self) -> int:
        """Get or set the Number of terms in fit. Currently, the maximum number is set to 6. Values of 2 are 3 are recommended, since each term used adds significantly to the cost. Caution should be exercised when taking the results from the fit. Preferably, all generated coefficients should be positive. Negative values may lead to unstable results. Once a satisfactory fit has been achieved it is recommended that the coefficients which are written into the output file be input in future runs.
        Default is set to 6.
        """ # nopep8
        return self._cards[1].get_value("nv")

    @nv.setter
    def nv(self, value: int) -> None:
        self._cards[1].set_value("nv", value)

    @property
    def gi(self) -> typing.Optional[float]:
        """Get or set the Optional shear relaxation modulus for the i'th term.
        """ # nopep8
        return self._cards[2].get_value("gi")

    @gi.setter
    def gi(self, value: float) -> None:
        self._cards[2].set_value("gi", value)

    @property
    def betai(self) -> typing.Optional[float]:
        """Get or set the Optional decay constant for the i'th term.
        """ # nopep8
        return self._cards[2].get_value("betai")

    @betai.setter
    def betai(self, value: float) -> None:
        self._cards[2].set_value("betai", value)

    @property
    def ref(self) -> typing.Optional[float]:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the keyword: *INITIAL_FOAM_REFERENC_GEOMETRY. This option is currently restricted to 8-noded solid elements with one point integration.
        EQ.0.0: off (default),
        EQ.1.0: on.
        """ # nopep8
        return self._cards[2].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        self._cards[2].set_value("ref", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

