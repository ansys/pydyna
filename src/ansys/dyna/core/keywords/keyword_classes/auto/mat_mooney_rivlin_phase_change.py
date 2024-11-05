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

class MatMooneyRivlinPhaseChange(KeywordBase):
    """DYNA MAT_MOONEY-RIVLIN_PHASE_CHANGE keyword"""

    keyword = "MAT"
    subkeyword = "MOONEY-RIVLIN_PHASE_CHANGE"
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
                        "pr1",
                        float,
                        20,
                        10,
                        kwargs.get("pr1")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "b1",
                        float,
                        40,
                        10,
                        kwargs.get("b1")
                    ),
                    Field(
                        "ref",
                        float,
                        50,
                        10,
                        kwargs.get("ref", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sgl1",
                        float,
                        0,
                        10,
                        kwargs.get("sgl1")
                    ),
                    Field(
                        "sw1",
                        float,
                        10,
                        10,
                        kwargs.get("sw1")
                    ),
                    Field(
                        "st1",
                        float,
                        20,
                        10,
                        kwargs.get("st1")
                    ),
                    Field(
                        "lcid1",
                        float,
                        30,
                        10,
                        kwargs.get("lcid1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        float,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "pr2",
                        float,
                        20,
                        10,
                        kwargs.get("pr2")
                    ),
                    Field(
                        "a2",
                        float,
                        30,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "b2",
                        float,
                        40,
                        10,
                        kwargs.get("b2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sgl1",
                        float,
                        0,
                        10,
                        kwargs.get("sgl1")
                    ),
                    Field(
                        "sw1",
                        float,
                        10,
                        10,
                        kwargs.get("sw1")
                    ),
                    Field(
                        "st1",
                        float,
                        20,
                        10,
                        kwargs.get("st1")
                    ),
                    Field(
                        "lcid1",
                        float,
                        30,
                        10,
                        kwargs.get("lcid1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x1",
                        float,
                        0,
                        10,
                        kwargs.get("x1")
                    ),
                    Field(
                        "y1",
                        float,
                        10,
                        10,
                        kwargs.get("y1")
                    ),
                    Field(
                        "z1",
                        float,
                        20,
                        10,
                        kwargs.get("z1")
                    ),
                    Field(
                        "x2",
                        float,
                        30,
                        10,
                        kwargs.get("x2")
                    ),
                    Field(
                        "y2",
                        float,
                        40,
                        10,
                        kwargs.get("y2")
                    ),
                    Field(
                        "z2",
                        float,
                        50,
                        10,
                        kwargs.get("z2")
                    ),
                    Field(
                        "thkfac",
                        float,
                        60,
                        10,
                        kwargs.get("thkfac", 1.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatMooneyRivlinPhaseChange.option_specs[0],
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
        """Get or set the Material identification. A unique number or label not exceeding 8	characters must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def pr1(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (value between 0.49 and 0.5 is recommended, smaller values may not work) where i indicates the phase.
        """ # nopep8
        return self._cards[0].get_value("pr1")

    @pr1.setter
    def pr1(self, value: float) -> None:
        self._cards[0].set_value("pr1", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Constant for the i th phase, see literature and equations defined below.
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[0].set_value("a1", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the Constant for the i th phase, see literature and equations defined	below.
        """ # nopep8
        return self._cards[0].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        self._cards[0].set_value("b1", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference
        geometry is defined by the keyword:*INITIAL_FOAM_REFERENCE_GEOMETRY (see there for more details).
        EQ.0.0: off,
        EQ.1.0: on.
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        if value not in [0.0, 1.0]:
            raise Exception("""ref must be one of {0.0,1.0}""")
        self._cards[0].set_value("ref", value)

    @property
    def sgl1(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length l0 for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[1].get_value("sgl1")

    @sgl1.setter
    def sgl1(self, value: float) -> None:
        self._cards[1].set_value("sgl1", value)

    @property
    def sw1(self) -> typing.Optional[float]:
        """Get or set the Specimen width for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[1].get_value("sw1")

    @sw1.setter
    def sw1(self, value: float) -> None:
        self._cards[1].set_value("sw1", value)

    @property
    def st1(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[1].get_value("st1")

    @st1.setter
    def st1(self, value: float) -> None:
        self._cards[1].set_value("st1", value)

    @property
    def lcid1(self) -> typing.Optional[float]:
        """Get or set the Curve ID for the i th phase, see *DEFINE_CURVE, giving the force versus actual change delta L in the gauge length. See also Figure M218-2
        """ # nopep8
        return self._cards[1].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: float) -> None:
        self._cards[1].set_value("lcid1", value)

    @property
    def pr2(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (value between 0.49 and 0.5 is recommended, smaller values may not work) where i indicates the phase
        """ # nopep8
        return self._cards[2].get_value("pr2")

    @pr2.setter
    def pr2(self, value: float) -> None:
        self._cards[2].set_value("pr2", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Constant for the i th phase, see literature and equations defined below
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the Constant for the i th phase, see literature and equations defined	below
        """ # nopep8
        return self._cards[2].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        self._cards[2].set_value("b2", value)

    @property
    def sgl1(self) -> typing.Optional[float]:
        """Get or set the Specimen gauge length l0 for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[3].get_value("sgl1")

    @sgl1.setter
    def sgl1(self, value: float) -> None:
        self._cards[3].set_value("sgl1", value)

    @property
    def sw1(self) -> typing.Optional[float]:
        """Get or set the Specimen width for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[3].get_value("sw1")

    @sw1.setter
    def sw1(self, value: float) -> None:
        self._cards[3].set_value("sw1", value)

    @property
    def st1(self) -> typing.Optional[float]:
        """Get or set the Specimen thickness for the i th phase, see Figure M218-1.
        """ # nopep8
        return self._cards[3].get_value("st1")

    @st1.setter
    def st1(self, value: float) -> None:
        self._cards[3].set_value("st1", value)

    @property
    def lcid1(self) -> typing.Optional[float]:
        """Get or set the Curve ID for the i th phase, see *DEFINE_CURVE, giving the force versus actual change delta L in the gauge length. See also Figure M218-2
        """ # nopep8
        return self._cards[3].get_value("lcid1")

    @lcid1.setter
    def lcid1(self, value: float) -> None:
        self._cards[3].set_value("lcid1", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane.
        """ # nopep8
        return self._cards[4].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[4].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane.
        """ # nopep8
        return self._cards[4].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[4].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point on the phase transition plane.
        """ # nopep8
        return self._cards[4].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[4].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point.
        """ # nopep8
        return self._cards[4].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[4].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[4].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[4].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the Coordinates of a point that defines the exterior normal with the first point
        """ # nopep8
        return self._cards[4].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[4].set_value("z2", value)

    @property
    def thkfac(self) -> float:
        """Get or set the Scale factor applied to the shell thickness after the phase transformation.
        """ # nopep8
        return self._cards[4].get_value("thkfac")

    @thkfac.setter
    def thkfac(self, value: float) -> None:
        self._cards[4].set_value("thkfac", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

