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

"""Module providing the Mat014 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat014(KeywordBase):
    """DYNA MAT_014 keyword"""

    keyword = "MAT"
    subkeyword = "014"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat014 class."""
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
                        "g",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "bulk",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a0",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a2",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pc",
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
                        "vcr",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ref",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eps1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps8",
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
                        "eps9",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps10",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "p1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p8",
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
                        "p9",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p10",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat014.option_specs[0],
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def bulk(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus for unloading used for VCR=0.0.
        """ # nopep8
        return self._cards[0].get_value("bulk")

    @bulk.setter
    def bulk(self, value: float) -> None:
        """Set the bulk property."""
        self._cards[0].set_value("bulk", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Yield function constant for plastic yield function.
        """ # nopep8
        return self._cards[0].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        """Set the a0 property."""
        self._cards[0].set_value("a0", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Yield function constant for plastic yield function.
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Yield function constant for plastic yield function.
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[0].set_value("a2", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff for tensile fracture.
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        """Set the pc property."""
        self._cards[0].set_value("pc", value)

    @property
    def vcr(self) -> float:
        """Get or set the Volumetric crushing option:
        EQ.0.0: on (default),
        EQ.1.0: loading and unloading paths are the same.
        """ # nopep8
        return self._cards[1].get_value("vcr")

    @vcr.setter
    def vcr(self, value: float) -> None:
        """Set the vcr property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""vcr must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("vcr", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the pressure, see *INITIAL_FOAM_REFERENCE_GEOMETRY.
        EQ.0.0: off (default),
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
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 1. A maximum of 10 values are allowed and a minimum of 2 values are necessary.
        """ # nopep8
        return self._cards[2].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[2].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 2.
        """ # nopep8
        return self._cards[2].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[2].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 3.
        """ # nopep8
        return self._cards[2].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        """Set the eps3 property."""
        self._cards[2].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 4.
        """ # nopep8
        return self._cards[2].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        """Set the eps4 property."""
        self._cards[2].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 5.
        """ # nopep8
        return self._cards[2].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        """Set the eps5 property."""
        self._cards[2].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 6.
        """ # nopep8
        return self._cards[2].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        """Set the eps6 property."""
        self._cards[2].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 7.
        """ # nopep8
        return self._cards[2].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        """Set the eps7 property."""
        self._cards[2].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 8.
        """ # nopep8
        return self._cards[2].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        """Set the eps8 property."""
        self._cards[2].set_value("eps8", value)

    @property
    def eps9(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 9.
        """ # nopep8
        return self._cards[3].get_value("eps9")

    @eps9.setter
    def eps9(self, value: float) -> None:
        """Set the eps9 property."""
        self._cards[3].set_value("eps9", value)

    @property
    def eps10(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain value at point 10.
        """ # nopep8
        return self._cards[3].get_value("eps10")

    @eps10.setter
    def eps10(self, value: float) -> None:
        """Set the eps10 property."""
        self._cards[3].set_value("eps10", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 1.
        """ # nopep8
        return self._cards[4].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[4].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 2.
        """ # nopep8
        return self._cards[4].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[4].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 3.
        """ # nopep8
        return self._cards[4].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[4].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 4.
        """ # nopep8
        return self._cards[4].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[4].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 5.
        """ # nopep8
        return self._cards[4].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        """Set the p5 property."""
        self._cards[4].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 6.
        """ # nopep8
        return self._cards[4].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        """Set the p6 property."""
        self._cards[4].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 7.
        """ # nopep8
        return self._cards[4].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        """Set the p7 property."""
        self._cards[4].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 8.
        """ # nopep8
        return self._cards[4].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        """Set the p8 property."""
        self._cards[4].set_value("p8", value)

    @property
    def p9(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 9.
        """ # nopep8
        return self._cards[5].get_value("p9")

    @p9.setter
    def p9(self, value: float) -> None:
        """Set the p9 property."""
        self._cards[5].set_value("p9", value)

    @property
    def p10(self) -> typing.Optional[float]:
        """Get or set the Pressure corresponding to volumetric strain at point 10.
        """ # nopep8
        return self._cards[5].get_value("p10")

    @p10.setter
    def p10(self, value: float) -> None:
        """Set the p10 property."""
        self._cards[5].set_value("p10", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

