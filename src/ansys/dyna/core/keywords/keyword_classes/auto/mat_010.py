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

"""Module providing the Mat010 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat010(KeywordBase):
    """DYNA MAT_010 keyword"""

    keyword = "MAT"
    subkeyword = "010"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat010 class."""
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
                        "sigy",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eh",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pc",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fs",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "charl",
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
                    Field(
                        "eps11",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps12",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps13",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps14",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps15",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "eps16",
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
                        "es1",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es4",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es5",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es6",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es7",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es8",
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
                        "es9",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es10",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es11",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es12",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es13",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es14",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es15",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "es16",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat010.option_specs[0],
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
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def eh(self) -> typing.Optional[float]:
        """Get or set the Plastic hardening modulus.
        """ # nopep8
        return self._cards[0].get_value("eh")

    @eh.setter
    def eh(self, value: float) -> None:
        """Set the eh property."""
        self._cards[0].set_value("eh", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Pressure cutoff (<= 0.0).
        EQ.0: a cutoff of -infinity is assumed.
        """ # nopep8
        return self._cards[0].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        """Set the pc property."""
        self._cards[0].set_value("pc", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Failure strain for erosion.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def charl(self) -> typing.Optional[float]:
        """Get or set the Characteristic element thickness for deletion.  This applies to 2D solid elements that lie on a boundary of a part.  If the boundary element thins down due to stretching or compression, and if it thins to a value less than CHARL, the element will be deleted.  The primary application of this option is to predict the break-up of axisymmetric shaped charge jets.
        """ # nopep8
        return self._cards[0].get_value("charl")

    @charl.setter
    def charl(self, value: float) -> None:
        """Set the charl property."""
        self._cards[0].set_value("charl", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the First effective plastic strain point (True). Define up to 16 values. Care must be taken that the full range of strains expected in the analysis is covered. Linear extrapolation is used if the strain values exceed the maximum input value.
        """ # nopep8
        return self._cards[1].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[1].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Second effective plastic strain point (True).
        """ # nopep8
        return self._cards[1].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[1].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Third effective plastic strain point (True).
        """ # nopep8
        return self._cards[1].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        """Set the eps3 property."""
        self._cards[1].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Fourth effective plastic strain point (True).
        """ # nopep8
        return self._cards[1].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        """Set the eps4 property."""
        self._cards[1].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Fifth effective plastic strain point (True).
        """ # nopep8
        return self._cards[1].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        """Set the eps5 property."""
        self._cards[1].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Sixth effective plastic strain point (True).
        """ # nopep8
        return self._cards[1].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        """Set the eps6 property."""
        self._cards[1].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Seventh effective plastic strain point (True).
        """ # nopep8
        return self._cards[1].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        """Set the eps7 property."""
        self._cards[1].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Eight effective plastic strain point (True).
        """ # nopep8
        return self._cards[1].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        """Set the eps8 property."""
        self._cards[1].set_value("eps8", value)

    @property
    def eps9(self) -> typing.Optional[float]:
        """Get or set the Ninth effective plastic strain point (True).
        """ # nopep8
        return self._cards[2].get_value("eps9")

    @eps9.setter
    def eps9(self, value: float) -> None:
        """Set the eps9 property."""
        self._cards[2].set_value("eps9", value)

    @property
    def eps10(self) -> typing.Optional[float]:
        """Get or set the Tenth effective plastic strain point (True).
        """ # nopep8
        return self._cards[2].get_value("eps10")

    @eps10.setter
    def eps10(self, value: float) -> None:
        """Set the eps10 property."""
        self._cards[2].set_value("eps10", value)

    @property
    def eps11(self) -> typing.Optional[float]:
        """Get or set the Eleventh effective plastic strain point (True).
        """ # nopep8
        return self._cards[2].get_value("eps11")

    @eps11.setter
    def eps11(self, value: float) -> None:
        """Set the eps11 property."""
        self._cards[2].set_value("eps11", value)

    @property
    def eps12(self) -> typing.Optional[float]:
        """Get or set the Twelfth effective plastic strain point (True).
        """ # nopep8
        return self._cards[2].get_value("eps12")

    @eps12.setter
    def eps12(self, value: float) -> None:
        """Set the eps12 property."""
        self._cards[2].set_value("eps12", value)

    @property
    def eps13(self) -> typing.Optional[float]:
        """Get or set the Thirteenth effective plastic strain point (True).
        """ # nopep8
        return self._cards[2].get_value("eps13")

    @eps13.setter
    def eps13(self, value: float) -> None:
        """Set the eps13 property."""
        self._cards[2].set_value("eps13", value)

    @property
    def eps14(self) -> typing.Optional[float]:
        """Get or set the Fourteenth effective plastic strain point (True).
        """ # nopep8
        return self._cards[2].get_value("eps14")

    @eps14.setter
    def eps14(self, value: float) -> None:
        """Set the eps14 property."""
        self._cards[2].set_value("eps14", value)

    @property
    def eps15(self) -> typing.Optional[float]:
        """Get or set the Fifteenth effective plastic strain point (True).
        """ # nopep8
        return self._cards[2].get_value("eps15")

    @eps15.setter
    def eps15(self, value: float) -> None:
        """Set the eps15 property."""
        self._cards[2].set_value("eps15", value)

    @property
    def eps16(self) -> typing.Optional[float]:
        """Get or set the Sixteenth effective plastic strain point (True).
        """ # nopep8
        return self._cards[2].get_value("eps16")

    @eps16.setter
    def eps16(self, value: float) -> None:
        """Set the eps16 property."""
        self._cards[2].set_value("eps16", value)

    @property
    def es1(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain. Define up to 16 values.
        """ # nopep8
        return self._cards[3].get_value("es1")

    @es1.setter
    def es1(self, value: float) -> None:
        """Set the es1 property."""
        self._cards[3].set_value("es1", value)

    @property
    def es2(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[3].get_value("es2")

    @es2.setter
    def es2(self, value: float) -> None:
        """Set the es2 property."""
        self._cards[3].set_value("es2", value)

    @property
    def es3(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[3].get_value("es3")

    @es3.setter
    def es3(self, value: float) -> None:
        """Set the es3 property."""
        self._cards[3].set_value("es3", value)

    @property
    def es4(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[3].get_value("es4")

    @es4.setter
    def es4(self, value: float) -> None:
        """Set the es4 property."""
        self._cards[3].set_value("es4", value)

    @property
    def es5(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[3].get_value("es5")

    @es5.setter
    def es5(self, value: float) -> None:
        """Set the es5 property."""
        self._cards[3].set_value("es5", value)

    @property
    def es6(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[3].get_value("es6")

    @es6.setter
    def es6(self, value: float) -> None:
        """Set the es6 property."""
        self._cards[3].set_value("es6", value)

    @property
    def es7(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[3].get_value("es7")

    @es7.setter
    def es7(self, value: float) -> None:
        """Set the es7 property."""
        self._cards[3].set_value("es7", value)

    @property
    def es8(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[3].get_value("es8")

    @es8.setter
    def es8(self, value: float) -> None:
        """Set the es8 property."""
        self._cards[3].set_value("es8", value)

    @property
    def es9(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[4].get_value("es9")

    @es9.setter
    def es9(self, value: float) -> None:
        """Set the es9 property."""
        self._cards[4].set_value("es9", value)

    @property
    def es10(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[4].get_value("es10")

    @es10.setter
    def es10(self, value: float) -> None:
        """Set the es10 property."""
        self._cards[4].set_value("es10", value)

    @property
    def es11(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[4].get_value("es11")

    @es11.setter
    def es11(self, value: float) -> None:
        """Set the es11 property."""
        self._cards[4].set_value("es11", value)

    @property
    def es12(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[4].get_value("es12")

    @es12.setter
    def es12(self, value: float) -> None:
        """Set the es12 property."""
        self._cards[4].set_value("es12", value)

    @property
    def es13(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[4].get_value("es13")

    @es13.setter
    def es13(self, value: float) -> None:
        """Set the es13 property."""
        self._cards[4].set_value("es13", value)

    @property
    def es14(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[4].get_value("es14")

    @es14.setter
    def es14(self, value: float) -> None:
        """Set the es14 property."""
        self._cards[4].set_value("es14", value)

    @property
    def es15(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[4].get_value("es15")

    @es15.setter
    def es15(self, value: float) -> None:
        """Set the es15 property."""
        self._cards[4].set_value("es15", value)

    @property
    def es16(self) -> typing.Optional[float]:
        """Get or set the Effective stress corresponding to the plastic strain.
        """ # nopep8
        return self._cards[4].get_value("es16")

    @es16.setter
    def es16(self, value: float) -> None:
        """Set the es16 property."""
        self._cards[4].set_value("es16", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

