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

class DefineFibers(KeywordBase):
    """DYNA DEFINE_FIBERS keyword"""

    keyword = "DEFINE"
    subkeyword = "FIBERS"
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
                        "idf",
                        int,
                        0,
                        10,
                        kwargs.get("idf")
                    ),
                    Field(
                        "idp",
                        int,
                        10,
                        10,
                        kwargs.get("idp")
                    ),
                    Field(
                        "numf",
                        int,
                        20,
                        10,
                        kwargs.get("numf")
                    ),
                    Field(
                        "n1",
                        int,
                        30,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        40,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "efb",
                        float,
                        50,
                        10,
                        kwargs.get("efb")
                    ),
                    Field(
                        "shr",
                        float,
                        50,
                        10,
                        kwargs.get("shr")
                    ),
                    Field(
                        "hrgls",
                        float,
                        50,
                        10,
                        kwargs.get("hrgls", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alpha1",
                        float,
                        0,
                        10,
                        kwargs.get("alpha1")
                    ),
                    Field(
                        "alpha2",
                        float,
                        10,
                        10,
                        kwargs.get("alpha2")
                    ),
                    Field(
                        "alpha3",
                        float,
                        20,
                        10,
                        kwargs.get("alpha3")
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
                ],
            ),
            OptionCardSet(
                option_spec = DefineFibers.option_specs[0],
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
    def idf(self) -> typing.Optional[int]:
        """Get or set the ID of a fiber set to be defined, must be unique number.
        """ # nopep8
        return self._cards[0].get_value("idf")

    @idf.setter
    def idf(self, value: int) -> None:
        self._cards[0].set_value("idf", value)

    @property
    def idp(self) -> typing.Optional[int]:
        """Get or set the Part ID of the matrix material associated with the fiber set.
        """ # nopep8
        return self._cards[0].get_value("idp")

    @idp.setter
    def idp(self, value: int) -> None:
        self._cards[0].set_value("idp", value)

    @property
    def numf(self) -> typing.Optional[int]:
        """Get or set the Number of fiber orientations.
        """ # nopep8
        return self._cards[0].get_value("numf")

    @numf.setter
    def numf(self, value: int) -> None:
        self._cards[0].set_value("numf", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Direction from Node 1 (N1) to Node 2 (N2) defines the reference fiber orientation.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Direction from Node 1 (N1) to Node 2 (N2) defines the reference fiber orientation.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def efb(self) -> typing.Optional[float]:
        """Get or set the Effective stiffness of the fiber in its orientation, which typically equals to
        Young’s Modulus times fiber cross sectional area fraction.
        Fiber cross sectional area fraction is typically between 0.25 and 0.5.
        """ # nopep8
        return self._cards[0].get_value("efb")

    @efb.setter
    def efb(self, value: float) -> None:
        self._cards[0].set_value("efb", value)

    @property
    def shr(self) -> typing.Optional[float]:
        """Get or set the Shear stiffness of the fiber:
        GT.0:	shear stiffness,
        LT.0:	|SHR| is theload curve ID defining shear stiffness vs. shear strain.
        """ # nopep8
        return self._cards[0].get_value("shr")

    @shr.setter
    def shr(self, value: float) -> None:
        self._cards[0].set_value("shr", value)

    @property
    def hrgls(self) -> float:
        """Get or set the Hourglass coefficient for stiffness type hourglass control. Default=1.0.
        """ # nopep8
        return self._cards[0].get_value("hrgls")

    @hrgls.setter
    def hrgls(self, value: float) -> None:
        self._cards[0].set_value("hrgls", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Initial orientation angles of the first, second and third fibers relative
        to reference fiber orientation defined by N1-N2, respectively.
        """ # nopep8
        return self._cards[1].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        self._cards[1].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Initial orientation angles of the first, second and third fibers relative
        to reference fiber orientation defined by N1-N2, respectively.
        """ # nopep8
        return self._cards[1].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        self._cards[1].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Initial orientation angles of the first, second and third fibers relative
        to reference fiber orientation defined by N1-N2, respectively.
        """ # nopep8
        return self._cards[1].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        self._cards[1].set_value("alpha3", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[2].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[2].set_value("y1", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[2].set_value("z1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[2].set_value("y2", value)

    @property
    def z2(self) -> typing.Optional[float]:
        """Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
        The Z1 and Z2 coordinate values must be defined close to the part.
        Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
        """ # nopep8
        return self._cards[2].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[2].set_value("z2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

