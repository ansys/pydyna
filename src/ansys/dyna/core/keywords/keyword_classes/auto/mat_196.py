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

class Mat196(KeywordBase):
    """DYNA MAT_196 keyword"""

    keyword = "MAT"
    subkeyword = "196"
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
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "dospot",
                        int,
                        70,
                        10,
                        kwargs.get("dospot", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dof",
                        int,
                        0,
                        10,
                        kwargs.get("dof")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type")
                    ),
                    Field(
                        "k",
                        float,
                        20,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "d",
                        float,
                        30,
                        10,
                        kwargs.get("d")
                    ),
                    Field(
                        "cdf",
                        float,
                        40,
                        10,
                        kwargs.get("cdf")
                    ),
                    Field(
                        "tdf",
                        float,
                        50,
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
                        kwargs.get("dle")
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
                option_spec = Mat196.option_specs[0],
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
    def dospot(self) -> int:
        """Get or set the Activate thinning of tied shell elements when SPOTHIN>0 on *CONTROL_‌CONTACT.
        EQ.0:	Spot weld thinning is inactive for shells tied to discrete beams that use this material(default)
        EQ.1 : Spot weld thinning is active for shells tied to discrete beams that use this material
        """ # nopep8
        return self._cards[0].get_value("dospot")

    @dospot.setter
    def dospot(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dospot must be one of {0,1}""")
        self._cards[0].set_value("dospot", value)

    @property
    def dof(self) -> typing.Optional[int]:
        """Get or set the Active degree-of-freedom, a number between 1 and 6 inclusive. Each value of DOF can only be used once. The active degree-of-freedom is measured in the local coordinate system for the discrete beam element.
        """ # nopep8
        return self._cards[1].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        self._cards[1].set_value("dof", value)

    @property
    def type(self) -> typing.Optional[int]:
        """Get or set the The default behavior is elastic.
        For inelastic behavior input 1.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        self._cards[1].set_value("type", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Elastic loading/unloading stiffness. This is required input for inelastic behavior.
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[1].set_value("k", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Optional viscous damping coefficient.
        """ # nopep8
        return self._cards[1].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        self._cards[1].set_value("d", value)

    @property
    def cdf(self) -> typing.Optional[float]:
        """Get or set the Compressive displacement at failure. Input as a positive number. After failure, no forces are carried. This option does not apply to zero length springs.
        EQ.0.0: inactive.
        """ # nopep8
        return self._cards[1].get_value("cdf")

    @cdf.setter
    def cdf(self, value: float) -> None:
        self._cards[1].set_value("cdf", value)

    @property
    def tdf(self) -> typing.Optional[float]:
        """Get or set the Tensile displacement at failure. After failure, no forces are carried.
        EQ.0.0: inactive.
        """ # nopep8
        return self._cards[1].get_value("tdf")

    @tdf.setter
    def tdf(self, value: float) -> None:
        self._cards[1].set_value("tdf", value)

    @property
    def flcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, defining the yield force versus plastic deflection. If the origin of the curve is at (0,0) the force magnitude is identical in tension and compression, i.e., only the sign changes. If not, the yield stress in the compression is used when the spring force is negative. The plastic displacement increases monotonically in this implementation.
        The load curve is required input.
        """ # nopep8
        return self._cards[2].get_value("flcid")

    @flcid.setter
    def flcid(self, value: int) -> None:
        self._cards[2].set_value("flcid", value)

    @property
    def hlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID, see *DEFINE_CURVE, defining force versus relative velocity (optional).
        If the origin of the curve is at (0,0) the force magnitude is identical for a given magnitude of the relative velocity, i.e., only the sign changes.
        """ # nopep8
        return self._cards[2].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        self._cards[2].set_value("hlcid", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[2].set_value("c2", value)

    @property
    def dle(self) -> typing.Optional[float]:
        """Get or set the Factor to scale time units.
        """ # nopep8
        return self._cards[2].get_value("dle")

    @dle.setter
    def dle(self, value: float) -> None:
        self._cards[2].set_value("dle", value)

    @property
    def glcid(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID, see *DEFINE_CURVE, defining a scale factor versus deflection  for load curve ID, GLCID.
        If zero, a scale factor of unity is assumed.
        """ # nopep8
        return self._cards[2].get_value("glcid")

    @glcid.setter
    def glcid(self, value: int) -> None:
        self._cards[2].set_value("glcid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

