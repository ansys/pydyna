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

class MatElasticSpringDiscreteBeam(KeywordBase):
    """DYNA MAT_ELASTIC_SPRING_DISCRETE_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "ELASTIC_SPRING_DISCRETE_BEAM"
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
                        "k",
                        float,
                        20,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "f0",
                        float,
                        30,
                        10,
                        kwargs.get("f0")
                    ),
                    Field(
                        "d",
                        float,
                        40,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "cdf",
                        float,
                        50,
                        10,
                        kwargs.get("cdf")
                    ),
                    Field(
                        "tdf",
                        float,
                        60,
                        10,
                        kwargs.get("tdf")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "flcid",
                        int,
                        0,
                        10,
                        kwargs.get("flcid")
                    ),
                    Field(
                        "hlcid",
                        int,
                        10,
                        10,
                        kwargs.get("hlcid")
                    ),
                    Field(
                        "c1",
                        float,
                        20,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        30,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "dle",
                        float,
                        40,
                        10,
                        kwargs.get("dle", 1.0)
                    ),
                    Field(
                        "glcid",
                        int,
                        50,
                        10,
                        kwargs.get("glcid")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatElasticSpringDiscreteBeam.option_specs[0],
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
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Stiffness coefficient.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def f0(self) -> typing.Optional[float]:
        """Get or set the Optional initial force. This option is inactive if this material is referenced in a part referenced by material type *MAT_ELASTIC_6DOF_SPRING.
        """ # nopep8
        return self._cards[0].get_value("f0")

    @f0.setter
    def f0(self, value: float) -> None:
        self._cards[0].set_value("f0", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Viscous damping coefficient.
        """ # nopep8
        return self._cards[0].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[0].set_value("d", value)

    @property
    def cdf(self) -> typing.Optional[float]:
        """Get or set the Compressive displacement at failure. Input as a positive number. After failure, no forces are carried. This option does not apply to zero length springs.
        EQ.0.0: inactive.
        """ # nopep8
        return self._cards[0].get_value("cdf")

    @cdf.setter
    def cdf(self, value: float) -> None:
        self._cards[0].set_value("cdf", value)

    @property
    def tdf(self) -> typing.Optional[float]:
        """Get or set the Tensile displacement at failure. After failure, no forces are carried.
        """ # nopep8
        return self._cards[0].get_value("tdf")

    @tdf.setter
    def tdf(self, value: float) -> None:
        self._cards[0].set_value("tdf", value)

    @property
    def flcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, defining force versus deflection for nonlinear behavior.
        """ # nopep8
        return self._cards[1].get_value("flcid")

    @flcid.setter
    def flcid(self, value: int) -> None:
        self._cards[1].set_value("flcid", value)

    @property
    def hlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, defining force versus relative velocity for nonlinear behavior (optional).
        If the origin of the curve is at (0,0) the force magnitude is identical for a given magnitude of the relative velocity, i.e., only the sign changes.
        """ # nopep8
        return self._cards[1].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        self._cards[1].set_value("hlcid", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the First damping coefficient for nonlinear behavior (optional).
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Second damping coefficient for nonlinear behavior (optional).
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[1].set_value("c2", value)

    @property
    def dle(self) -> float:
        """Get or set the Factor to scale time units.
        Default is set to unity (1.0).
        """ # nopep8
        return self._cards[1].get_value("dle")

    @dle.setter
    def dle(self, value: float) -> None:
        self._cards[1].set_value("dle", value)

    @property
    def glcid(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID, see *DEFINE_CURVE, defining a scale factor versus deflection for load curve ID, HLCID. If zero, a scale factor of unity is assumed.
        """ # nopep8
        return self._cards[1].get_value("glcid")

    @glcid.setter
    def glcid(self, value: int) -> None:
        self._cards[1].set_value("glcid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)
