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

class Mat208(KeywordBase):
    """DYNA MAT_208 keyword"""

    keyword = "MAT"
    subkeyword = "208"
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
                        "kax",
                        float,
                        20,
                        10,
                        kwargs.get("kax")
                    ),
                    Field(
                        "kshr",
                        float,
                        30,
                        10,
                        kwargs.get("kshr")
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
                        "fpre",
                        float,
                        60,
                        10,
                        kwargs.get("fpre")
                    ),
                    Field(
                        "tramp",
                        float,
                        70,
                        10,
                        kwargs.get("tramp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcax",
                        int,
                        0,
                        10,
                        kwargs.get("lcax")
                    ),
                    Field(
                        "lcshr",
                        int,
                        10,
                        10,
                        kwargs.get("lcshr")
                    ),
                    Field(
                        "fric",
                        float,
                        20,
                        10,
                        kwargs.get("fric")
                    ),
                    Field(
                        "clear",
                        float,
                        30,
                        10,
                        kwargs.get("clear")
                    ),
                    Field(
                        "dafail",
                        float,
                        40,
                        10,
                        kwargs.get("dafail", 1.E20)
                    ),
                    Field(
                        "drfail",
                        float,
                        50,
                        10,
                        kwargs.get("drfail", 1.E20)
                    ),
                    Field(
                        "damag",
                        float,
                        60,
                        10,
                        kwargs.get("damag", 0.1)
                    ),
                    Field(
                        "t0pre",
                        float,
                        70,
                        10,
                        kwargs.get("t0pre")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dacfail",
                        float,
                        0,
                        10,
                        kwargs.get("dacfail", 1.E20)
                    ),
                    Field(
                        "axshel",
                        int,
                        10,
                        10,
                        kwargs.get("axshel", 0)
                    ),
                    Field(
                        "holshr",
                        int,
                        20,
                        10,
                        kwargs.get("holshr", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat208.option_specs[0],
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
        """Get or set the Material identification. A unique number or label must be specified.
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
    def kax(self) -> typing.Optional[float]:
        """Get or set the Axial elastic stiffness (Force/Length units).
        """ # nopep8
        return self._cards[0].get_value("kax")

    @kax.setter
    def kax(self, value: float) -> None:
        self._cards[0].set_value("kax", value)

    @property
    def kshr(self) -> typing.Optional[float]:
        """Get or set the Shear elastic stiffness (Force/Length units).
        """ # nopep8
        return self._cards[0].get_value("kshr")

    @kshr.setter
    def kshr(self, value: float) -> None:
        self._cards[0].set_value("kshr", value)

    @property
    def fpre(self) -> typing.Optional[float]:
        """Get or set the Preload force.
        """ # nopep8
        return self._cards[0].get_value("fpre")

    @fpre.setter
    def fpre(self, value: float) -> None:
        self._cards[0].set_value("fpre", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Time duration during which preload is ramped up.
        """ # nopep8
        return self._cards[0].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        self._cards[0].set_value("tramp", value)

    @property
    def lcax(self) -> typing.Optional[int]:
        """Get or set the Load curve giving axial load as a function of plastic displacement (x-axis = displacement (length units), y-axis = force).
        """ # nopep8
        return self._cards[1].get_value("lcax")

    @lcax.setter
    def lcax(self, value: int) -> None:
        self._cards[1].set_value("lcax", value)

    @property
    def lcshr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or table ID giving lateral load versus displacement (xaxis
        - displacement (length units), y-axis - force). In the table case, each
        curve in the table represents lateral load versus displacement at a given
        (current) axial load, i.e. the values in the table are axial forces.
        """ # nopep8
        return self._cards[1].get_value("lcshr")

    @lcshr.setter
    def lcshr(self, value: int) -> None:
        self._cards[1].set_value("lcshr", value)

    @property
    def fric(self) -> typing.Optional[float]:
        """Get or set the Friction coefficient resisting sliding of bolt head/nut (non-dimensional).
        """ # nopep8
        return self._cards[1].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        self._cards[1].set_value("fric", value)

    @property
    def clear(self) -> typing.Optional[float]:
        """Get or set the Radial clearance (gap between bolt shank and the inner diameter of the	hole) (length units).
        """ # nopep8
        return self._cards[1].get_value("clear")

    @clear.setter
    def clear(self, value: float) -> None:
        self._cards[1].set_value("clear", value)

    @property
    def dafail(self) -> float:
        """Get or set the Axial tensile displacement to failure (length units).
        """ # nopep8
        return self._cards[1].get_value("dafail")

    @dafail.setter
    def dafail(self, value: float) -> None:
        self._cards[1].set_value("dafail", value)

    @property
    def drfail(self) -> float:
        """Get or set the Radial displacement to failure (excludes clearance).
        """ # nopep8
        return self._cards[1].get_value("drfail")

    @drfail.setter
    def drfail(self, value: float) -> None:
        self._cards[1].set_value("drfail", value)

    @property
    def damag(self) -> float:
        """Get or set the Fraction of above displacements between initiation & completion of failure.
        """ # nopep8
        return self._cards[1].get_value("damag")

    @damag.setter
    def damag(self, value: float) -> None:
        self._cards[1].set_value("damag", value)

    @property
    def t0pre(self) -> typing.Optional[float]:
        """Get or set the Time at which preload application begins.
        """ # nopep8
        return self._cards[1].get_value("t0pre")

    @t0pre.setter
    def t0pre(self, value: float) -> None:
        self._cards[1].set_value("t0pre", value)

    @property
    def dacfail(self) -> float:
        """Get or set the Axial compressive displacement at which failure is initiated (positive value, length units)
        """ # nopep8
        return self._cards[2].get_value("dacfail")

    @dacfail.setter
    def dacfail(self, value: float) -> None:
        self._cards[2].set_value("dacfail", value)

    @property
    def axshel(self) -> int:
        """Get or set the Flag to determine effect on axial response of increase of length of element due to shear displacement. In this context, shear displacement excludes sliding within the clearance gap. See notes.
        EQ.0:	Shear-induced length increase treated as axial load
        EQ.1:	Shear-induced length increase is ignored
        """ # nopep8
        return self._cards[2].get_value("axshel")

    @axshel.setter
    def axshel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""axshel must be one of {0,1}""")
        self._cards[2].set_value("axshel", value)

    @property
    def holshr(self) -> int:
        """Get or set the Flag for hole enlargement due to shear.
        EQ.0:	 Hole does not enlarge due to shear deformation.
        NE.0 : Shear deformation after bolt contacts the inner diameter of the hole enlarges the hole.
        """ # nopep8
        return self._cards[2].get_value("holshr")

    @holshr.setter
    def holshr(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""holshr must be one of {0,1}""")
        self._cards[2].set_value("holshr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

