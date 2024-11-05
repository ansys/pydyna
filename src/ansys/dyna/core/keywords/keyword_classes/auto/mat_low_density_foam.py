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

class MatLowDensityFoam(KeywordBase):
    """DYNA MAT_LOW_DENSITY_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "LOW_DENSITY_FOAM"
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
                        kwargs.get("lcid", 0)
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
                        "ed",
                        float,
                        30,
                        10,
                        kwargs.get("ed")
                    ),
                    Field(
                        "beta1",
                        float,
                        40,
                        10,
                        kwargs.get("beta1")
                    ),
                    Field(
                        "kcon",
                        float,
                        50,
                        10,
                        kwargs.get("kcon")
                    ),
                    Field(
                        "ref",
                        float,
                        60,
                        10,
                        kwargs.get("ref")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatLowDensityFoam.option_specs[0],
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
    def lcid(self) -> int:
        """Get or set the Load curve ID, see *DEFINE_CURVE, for nominal stress versus strain.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def tc(self) -> float:
        """Get or set the Tension cut-off stress.
        """ # nopep8
        return self._cards[0].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        self._cards[0].set_value("tc", value)

    @property
    def hu(self) -> float:
        """Get or set the Hysteretic unloading factor between 0 and 1 (default = 1.0, no energy dissipation).
        """ # nopep8
        return self._cards[0].get_value("hu")

    @hu.setter
    def hu(self, value: float) -> None:
        self._cards[0].set_value("hu", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Decay constant to model creep in unloading.
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
        EQ.0.0: tensile stress remains at cut-off value,
        EQ.1.0: tensile stress is reset to zero.
        """ # nopep8
        return self._cards[1].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        self._cards[1].set_value("fail", value)

    @property
    def bvflag(self) -> typing.Optional[float]:
        """Get or set the Bulk viscosity activation flag:
        EQ.0.0: no bulk viscosity (recommended),
        EQ.1.0: bulk viscosity active.
        """ # nopep8
        return self._cards[1].get_value("bvflag")

    @bvflag.setter
    def bvflag(self, value: float) -> None:
        self._cards[1].set_value("bvflag", value)

    @property
    def ed(self) -> typing.Optional[float]:
        """Get or set the Optional Young's relaxation modulus, Ed, for rate effects.
        """ # nopep8
        return self._cards[1].get_value("ed")

    @ed.setter
    def ed(self, value: float) -> None:
        self._cards[1].set_value("ed", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Optional decay constant.
        """ # nopep8
        return self._cards[1].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        self._cards[1].set_value("beta1", value)

    @property
    def kcon(self) -> typing.Optional[float]:
        """Get or set the Stiffness coefficient for contact interface stiffness. Maximum slope in stress vs. strain curve is used. When the maximum slope is taken for the contact, the time step size for this material is reduced for stability. In some cases delta-t may be significantly smaller, and defining a reasonable stiffness is recommended.
        """ # nopep8
        return self._cards[1].get_value("kcon")

    @kcon.setter
    def kcon(self, value: float) -> None:
        self._cards[1].set_value("kcon", value)

    @property
    def ref(self) -> typing.Optional[float]:
        """Get or set the Use reference geometry to initialize the stress tensor, see also *INITIAL_FOAM_REFERENCE_ GEOMETRY. Ls-dyna now supports not only 8 noded elements. Per manual it supports Elfom= 1,2,10,15..
        EQ.0.0: off (default),
        EQ.1.0: on.
        """ # nopep8
        return self._cards[1].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        self._cards[1].set_value("ref", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

