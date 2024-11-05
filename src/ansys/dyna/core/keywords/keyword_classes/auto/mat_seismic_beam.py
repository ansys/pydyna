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

class MatSeismicBeam(KeywordBase):
    """DYNA MAT_SEISMIC_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "SEISMIC_BEAM"
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
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "asflag",
                        float,
                        40,
                        10,
                        kwargs.get("asflag", 0.0)
                    ),
                    Field(
                        "ftype",
                        int,
                        50,
                        10,
                        kwargs.get("ftype", 1)
                    ),
                    Field(
                        "degrad",
                        int,
                        60,
                        10,
                        kwargs.get("degrad", 0)
                    ),
                    Field(
                        "ifema",
                        int,
                        70,
                        10,
                        kwargs.get("ifema", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcpms",
                        int,
                        0,
                        10,
                        kwargs.get("lcpms")
                    ),
                    Field(
                        "sfs",
                        float,
                        10,
                        10,
                        kwargs.get("sfs", 1.0)
                    ),
                    Field(
                        "lcpmt",
                        int,
                        20,
                        10,
                        kwargs.get("lcpmt")
                    ),
                    Field(
                        "sft",
                        float,
                        30,
                        10,
                        kwargs.get("sft", 1.0)
                    ),
                    Field(
                        "lcat",
                        int,
                        40,
                        10,
                        kwargs.get("lcat")
                    ),
                    Field(
                        "sfat",
                        float,
                        50,
                        10,
                        kwargs.get("sfat", 1.0)
                    ),
                    Field(
                        "lcac",
                        int,
                        60,
                        10,
                        kwargs.get("lcac")
                    ),
                    Field(
                        "sfac",
                        float,
                        70,
                        10,
                        kwargs.get("sfac", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha",
                        float,
                        0,
                        10,
                        kwargs.get("alpha", 2.0)
                    ),
                    Field(
                        "beta",
                        float,
                        10,
                        10,
                        kwargs.get("beta", 2.0)
                    ),
                    Field(
                        "gamma",
                        float,
                        20,
                        10,
                        kwargs.get("gamma", 2.0)
                    ),
                    Field(
                        "delta",
                        float,
                        30,
                        10,
                        kwargs.get("delta", 4.0)
                    ),
                    Field(
                        "a",
                        float,
                        40,
                        10,
                        kwargs.get("a", 2.0)
                    ),
                    Field(
                        "b",
                        float,
                        50,
                        10,
                        kwargs.get("b", -1.0)
                    ),
                    Field(
                        "foffs",
                        float,
                        60,
                        10,
                        kwargs.get("foffs")
                    ),
                    Field(
                        "unused",
                        float,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigy",
                        float,
                        0,
                        10,
                        kwargs.get("sigy")
                    ),
                    Field(
                        "d",
                        float,
                        10,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "w",
                        float,
                        20,
                        10,
                        kwargs.get("w")
                    ),
                    Field(
                        "tf",
                        float,
                        30,
                        10,
                        kwargs.get("tf")
                    ),
                    Field(
                        "tw",
                        float,
                        40,
                        10,
                        kwargs.get("tw")
                    ),
                    Field(
                        "unused",
                        float,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "phi_t",
                        float,
                        0,
                        10,
                        kwargs.get("phi_t", 0.8)
                    ),
                    Field(
                        "phi_c",
                        float,
                        10,
                        10,
                        kwargs.get("phi_c", 0.85)
                    ),
                    Field(
                        "phi_b",
                        float,
                        20,
                        10,
                        kwargs.get("phi_b", 0.9)
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatSeismicBeam.option_specs[0],
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
        """Get or set the Young's modulus
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def asflag(self) -> float:
        """Get or set the Axial strain definition for force-strain curves, degradation and FEMA output:
        EQ.0.0: true (log) total strain
        EQ.1.0: change in length
        EQ.2.0: nominal total strain
        EQ.3.0: FEMA plastic strain ( = nominal total strain minus elastic strain).
        """ # nopep8
        return self._cards[0].get_value("asflag")

    @asflag.setter
    def asflag(self, value: float) -> None:
        if value not in [0.0, 1.0, 2.0, 3.0]:
            raise Exception("""asflag must be one of {0.0,1.0,2.0,3.0}""")
        self._cards[0].set_value("asflag", value)

    @property
    def ftype(self) -> int:
        """Get or set the Formulation type for interaction:
        EQ:1 Parabolic coefficients, axial load and biaxial bending (default).
        EQ:2 Japanese code, axial force and major axis bending.
        EQ.4:	AISC utilization calculation but no yielding
        EQ.5:	AS4100 utilization calculation but no yielding
        """ # nopep8
        return self._cards[0].get_value("ftype")

    @ftype.setter
    def ftype(self, value: int) -> None:
        if value not in [1, 2, 4, 5]:
            raise Exception("""ftype must be one of {1,2,4,5}""")
        self._cards[0].set_value("ftype", value)

    @property
    def degrad(self) -> int:
        """Get or set the Flag for degrading moment behavior
        EQ.0: Behavior as in previous versions
        EQ.1: Fatigue-type degrading moment-rotation behavior
        EQ.2: FEMA-type degrading moment-rotation behavior.
        """ # nopep8
        return self._cards[0].get_value("degrad")

    @degrad.setter
    def degrad(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""degrad must be one of {0,1,2}""")
        self._cards[0].set_value("degrad", value)

    @property
    def ifema(self) -> int:
        """Get or set the Flag for input of FEMA thresholds
        EQ.0: No input
        EQ.1: Input of rotation thresholds only
        EQ.2: Input of rotation and axial strain thresholds.
        """ # nopep8
        return self._cards[0].get_value("ifema")

    @ifema.setter
    def ifema(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""ifema must be one of {0,1,2}""")
        self._cards[0].set_value("ifema", value)

    @property
    def lcpms(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving plastic moment vs. Plastic rotation at node 2 about local s-axis.
        See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcpms")

    @lcpms.setter
    def lcpms(self, value: int) -> None:
        self._cards[1].set_value("lcpms", value)

    @property
    def sfs(self) -> float:
        """Get or set the Scale factor on s-moment at node 2.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("sfs")

    @sfs.setter
    def sfs(self, value: float) -> None:
        self._cards[1].set_value("sfs", value)

    @property
    def lcpmt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving plastic moment vs. Plastic rotation at node 2 about local t-axis.
        See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcpmt")

    @lcpmt.setter
    def lcpmt(self, value: int) -> None:
        self._cards[1].set_value("lcpmt", value)

    @property
    def sft(self) -> float:
        """Get or set the Scale factor on t-moment at node 2.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("sft")

    @sft.setter
    def sft(self, value: float) -> None:
        self._cards[1].set_value("sft", value)

    @property
    def lcat(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving axial tensile yield force vs. total tensile (elastic + plastic) strain or vs. elongation. See AOPT above. All values are positive.
        See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcat")

    @lcat.setter
    def lcat(self, value: int) -> None:
        self._cards[1].set_value("lcat", value)

    @property
    def sfat(self) -> float:
        """Get or set the Scale factor on axial tensile force.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("sfat")

    @sfat.setter
    def sfat(self, value: float) -> None:
        self._cards[1].set_value("sfat", value)

    @property
    def lcac(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving compressive yield force vs. total compressive (elastic + plastic) strain or vs. elongation. See AOPT above. All values are positive.
        See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcac")

    @lcac.setter
    def lcac(self, value: int) -> None:
        self._cards[1].set_value("lcac", value)

    @property
    def sfac(self) -> float:
        """Get or set the Scale factor on axial tensile force.
        Default is set to 1.0.
        """ # nopep8
        return self._cards[1].get_value("sfac")

    @sfac.setter
    def sfac(self, value: float) -> None:
        self._cards[1].set_value("sfac", value)

    @property
    def alpha(self) -> float:
        """Get or set the Parameter to define yield surface.
        Default is set to 2.0.
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[2].set_value("alpha", value)

    @property
    def beta(self) -> float:
        """Get or set the Parameter to define yield surface.
        Default is set to 2.0.
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[2].set_value("beta", value)

    @property
    def gamma(self) -> float:
        """Get or set the Parameter to define yield surface.
        Default is set to 2.0.
        """ # nopep8
        return self._cards[2].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[2].set_value("gamma", value)

    @property
    def delta(self) -> float:
        """Get or set the Parameter to define yield surface.
        Default is set to 4.0.
        """ # nopep8
        return self._cards[2].get_value("delta")

    @delta.setter
    def delta(self, value: float) -> None:
        self._cards[2].set_value("delta", value)

    @property
    def a(self) -> float:
        """Get or set the Parameter to define yield surface.
        Default is set to 2.0.
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[2].set_value("a", value)

    @property
    def b(self) -> float:
        """Get or set the Parameter to define yield surface.
        Default is set to -1.0.
        """ # nopep8
        return self._cards[2].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[2].set_value("b", value)

    @property
    def foffs(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("foffs")

    @foffs.setter
    def foffs(self, value: float) -> None:
        self._cards[2].set_value("foffs", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress of material.
        """ # nopep8
        return self._cards[3].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        self._cards[3].set_value("sigy", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Depth of section used to calculate interaction curve.
        """ # nopep8
        return self._cards[3].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[3].set_value("d", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Width of section used to calculate interaction curve.
        """ # nopep8
        return self._cards[3].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[3].set_value("w", value)

    @property
    def tf(self) -> typing.Optional[float]:
        """Get or set the Flange thickness of section used to calculate interaction curve.
        """ # nopep8
        return self._cards[3].get_value("tf")

    @tf.setter
    def tf(self, value: float) -> None:
        self._cards[3].set_value("tf", value)

    @property
    def tw(self) -> typing.Optional[float]:
        """Get or set the Web thickness used to calculate interaction curve.
        """ # nopep8
        return self._cards[3].get_value("tw")

    @tw.setter
    def tw(self, value: float) -> None:
        self._cards[3].set_value("tw", value)

    @property
    def phi_t(self) -> float:
        """Get or set the Factor on tensile capacity
        """ # nopep8
        return self._cards[4].get_value("phi_t")

    @phi_t.setter
    def phi_t(self, value: float) -> None:
        self._cards[4].set_value("phi_t", value)

    @property
    def phi_c(self) -> float:
        """Get or set the Factor on compression capacity
        """ # nopep8
        return self._cards[4].get_value("phi_c")

    @phi_c.setter
    def phi_c(self, value: float) -> None:
        self._cards[4].set_value("phi_c", value)

    @property
    def phi_b(self) -> float:
        """Get or set the Factor on bending capacity
        """ # nopep8
        return self._cards[4].get_value("phi_b")

    @phi_b.setter
    def phi_b(self, value: float) -> None:
        self._cards[4].set_value("phi_b", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

