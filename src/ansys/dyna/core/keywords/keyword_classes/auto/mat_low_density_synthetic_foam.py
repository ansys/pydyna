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

"""Module providing the MatLowDensitySyntheticFoam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatLowDensitySyntheticFoam(KeywordBase):
    """DYNA MAT_LOW_DENSITY_SYNTHETIC_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "LOW_DENSITY_SYNTHETIC_FOAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatLowDensitySyntheticFoam class."""
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcid1",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcid2",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "hu",
                        float,
                        50,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "damp",
                        float,
                        70,
                        10,
                        .05,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "fail",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "bvflag",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ed",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta1",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "kcon",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ref",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "tc",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rflag",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dtrt",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatLowDensitySyntheticFoam.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def lcid1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, for nominal stress versus strain for the first loading cycle.
        """ # nopep8
        return self._cards[0].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: int) -> None:
        """Set the lcid1 property."""
        self._cards[0].set_value("lcid1", value)

    @property
    def lcid2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, for nominal stress versus strain for the loading cycles after the first loading cycle is completed.
        """ # nopep8
        return self._cards[0].get_value("lcid2")

    @lcid2.setter
    def lcid2(self, value: int) -> None:
        """Set the lcid2 property."""
        self._cards[0].set_value("lcid2", value)

    @property
    def hu(self) -> float:
        """Get or set the Hysteretic unloading factor betwen 0 and 1, (default=1, i.e., no energy dissipation).
        """ # nopep8
        return self._cards[0].get_value("hu")

    @hu.setter
    def hu(self, value: float) -> None:
        """Set the hu property."""
        self._cards[0].set_value("hu", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the B, decay constant to model creep in unloading.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def damp(self) -> float:
        """Get or set the Viscous coefficient (.05< recommended value </.50) to model damping effects
        LT.0.0: |DAMP| is the load curve ID, which defines the damping constant as a function of the maximum strain in compression defined as: Emax = max(1-lambda1, 1-lambda2, 1-lambda3). In tension, the damping constant is set to the value corresponding to the strain at 0. The abcissia should be defined from 0 to 1.
        """ # nopep8
        return self._cards[0].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[0].set_value("damp", value)

    @property
    def shape(self) -> typing.Optional[float]:
        """Get or set the Shape factor for unloading. Active for nonzero values of the hysteretic unloading factor. Values less than one reduces the energy dissipation and greater than one increases dissipation.
        """ # nopep8
        return self._cards[1].get_value("shape")

    @shape.setter
    def shape(self, value: float) -> None:
        """Set the shape property."""
        self._cards[1].set_value("shape", value)

    @property
    def fail(self) -> float:
        """Get or set the Failure option after the cutoff stress is reached:
        EQ.0.0: tensile stress remains at cut-off value,
        EQ.1.0: tensile stress is reset to zero.
        """ # nopep8
        return self._cards[1].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        """Set the fail property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""fail must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("fail", value)

    @property
    def bvflag(self) -> float:
        """Get or set the Bulk viscosity activation flag:
        EQ.0.0: no bulk viscosity (recommended),
        EQ.1.0: bulk viscosity active.
        """ # nopep8
        return self._cards[1].get_value("bvflag")

    @bvflag.setter
    def bvflag(self, value: float) -> None:
        """Set the bvflag property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""bvflag must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("bvflag", value)

    @property
    def ed(self) -> typing.Optional[float]:
        """Get or set the Optional Young's relaxation modulus, Ed, for rate effects.
        """ # nopep8
        return self._cards[1].get_value("ed")

    @ed.setter
    def ed(self, value: float) -> None:
        """Set the ed property."""
        self._cards[1].set_value("ed", value)

    @property
    def beta1(self) -> typing.Optional[float]:
        """Get or set the Optional decay constant, B1.
        """ # nopep8
        return self._cards[1].get_value("beta1")

    @beta1.setter
    def beta1(self, value: float) -> None:
        """Set the beta1 property."""
        self._cards[1].set_value("beta1", value)

    @property
    def kcon(self) -> typing.Optional[float]:
        """Get or set the Stiffness coefficient for contact interface stiffness. If undefined the maximum slope in stress vs. strain curve is used. When the maximum slope is taken for the contact, the time step size for this material is reduced for stability. In some cases delta-t may be significantly smaller, and defining a reasonable stiffness is recommended.
        """ # nopep8
        return self._cards[1].get_value("kcon")

    @kcon.setter
    def kcon(self, value: float) -> None:
        """Set the kcon property."""
        self._cards[1].set_value("kcon", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the keyword: *INITIAL_FOAM_REFERENCE_GEOMETRY. This option is currently restarted to 8-noded solid elements with one point integration.
        EQ.0.0: off,
        EQ.1.0: on.
        """ # nopep8
        return self._cards[1].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ref must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("ref", value)

    @property
    def tc(self) -> typing.Optional[float]:
        """Get or set the Tension cut-off stress.
        """ # nopep8
        return self._cards[1].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[1].set_value("tc", value)

    @property
    def rflag(self) -> float:
        """Get or set the Rate type for input:
        EQ.0.0: LCID1 and LCID2 should be input as functions of true strain rate
        EQ.1.0: LCID1 and LCID2 should be input as functions of engineering strain rate.
        """ # nopep8
        return self._cards[2].get_value("rflag")

    @rflag.setter
    def rflag(self, value: float) -> None:
        """Set the rflag property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""rflag must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("rflag", value)

    @property
    def dtrt(self) -> typing.Optional[float]:
        """Get or set the Strain rate averaging flag:
        EQ.0.0: use weighted running average
        LT.0.0: average the last 11 values
        GT.0.0: average over the last DTRT time units.
        """ # nopep8
        return self._cards[2].get_value("dtrt")

    @dtrt.setter
    def dtrt(self, value: float) -> None:
        """Set the dtrt property."""
        self._cards[2].set_value("dtrt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

