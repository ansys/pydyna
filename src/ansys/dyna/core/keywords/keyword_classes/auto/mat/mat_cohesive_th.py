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

"""Module providing the MatCohesiveTh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATCOHESIVETH_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("roflg", float, 20, 10, 0.0),
    FieldSchema("intfail", float, 30, 10, None),
    FieldSchema("sigmax", float, 40, 10, None),
    FieldSchema("nls", float, 50, 10, None),
    FieldSchema("tls", float, 60, 10, None),
    FieldSchema("tls2", float, 70, 10, None),
)

_MATCOHESIVETH_CARD1 = (
    FieldSchema("lamda1", float, 0, 10, None),
    FieldSchema("lamda2", float, 10, 10, None),
    FieldSchema("lamdaf", float, 20, 10, None),
    FieldSchema("stfsf", float, 30, 10, None),
    FieldSchema("isw", int, 40, 10, -1),
    FieldSchema("alpha1", float, 50, 10, None),
    FieldSchema("alpha2", float, 60, 10, None),
)

_MATCOHESIVETH_CARD2 = (
    FieldSchema("dr", float, 0, 10, None),
    FieldSchema("alpha3", float, 10, 10, None),
)

class MatCohesiveTh(KeywordBase):
    """DYNA MAT_COHESIVE_TH keyword"""

    keyword = "MAT"
    subkeyword = "COHESIVE_TH"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatCohesiveTh class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATCOHESIVETH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCOHESIVETH_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCOHESIVETH_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatCohesiveTh.option_specs[0],
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
    def roflg(self) -> float:
        """Get or set the Flag stating whether density is specified per unit area or volume:
        EQ.0:	Specified density is per unit volume(default).
        EQ.1 : Specified density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
        """ # nopep8
        return self._cards[0].get_value("roflg")

    @roflg.setter
    def roflg(self, value: float) -> None:
        """Set the roflg property."""
        if value not in [0, 1, None]:
            raise Exception("""roflg must be `None` or one of {0,1}.""")
        self._cards[0].set_value("roflg", value)

    @property
    def intfail(self) -> typing.Optional[float]:
        """Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
        LT.0.0:	Employs a Newton - Cotes integration scheme and the element will be deleted when | INTFAIL | integration points have failed.
        EQ.0.0 : Employs a Newton - Cotes integration scheme and the element will not be deleted even if it satisfies the failure criterion.
        GT.0.0 : Employs a Gauss integration scheme and the element will be deleted when INTFAIL integration points have failed.
        """ # nopep8
        return self._cards[0].get_value("intfail")

    @intfail.setter
    def intfail(self, value: float) -> None:
        """Set the intfail property."""
        self._cards[0].set_value("intfail", value)

    @property
    def sigmax(self) -> typing.Optional[float]:
        """Get or set the Peak traction.
        """ # nopep8
        return self._cards[0].get_value("sigmax")

    @sigmax.setter
    def sigmax(self, value: float) -> None:
        """Set the sigmax property."""
        self._cards[0].set_value("sigmax", value)

    @property
    def nls(self) -> typing.Optional[float]:
        """Get or set the Length scale (maximum separation) in the normal direction.
        """ # nopep8
        return self._cards[0].get_value("nls")

    @nls.setter
    def nls(self, value: float) -> None:
        """Set the nls property."""
        self._cards[0].set_value("nls", value)

    @property
    def tls(self) -> typing.Optional[float]:
        """Get or set the Length scale (maximum separation) in the tangential direction.
        """ # nopep8
        return self._cards[0].get_value("tls")

    @tls.setter
    def tls(self, value: float) -> None:
        """Set the tls property."""
        self._cards[0].set_value("tls", value)

    @property
    def tls2(self) -> typing.Optional[float]:
        """Get or set the Length scale (maximum separation) in the tear direction, only for XFEM shell.
        """ # nopep8
        return self._cards[0].get_value("tls2")

    @tls2.setter
    def tls2(self, value: float) -> None:
        """Set the tls2 property."""
        self._cards[0].set_value("tls2", value)

    @property
    def lamda1(self) -> typing.Optional[float]:
        """Get or set the Scaled distance to peak traction (Lambda_1).
        """ # nopep8
        return self._cards[1].get_value("lamda1")

    @lamda1.setter
    def lamda1(self, value: float) -> None:
        """Set the lamda1 property."""
        self._cards[1].set_value("lamda1", value)

    @property
    def lamda2(self) -> typing.Optional[float]:
        """Get or set the Scaled distance to beginning of softening (Lambda_2).
        """ # nopep8
        return self._cards[1].get_value("lamda2")

    @lamda2.setter
    def lamda2(self, value: float) -> None:
        """Set the lamda2 property."""
        self._cards[1].set_value("lamda2", value)

    @property
    def lamdaf(self) -> typing.Optional[float]:
        """Get or set the Scaled distance for failure (Lambda_fail).
        """ # nopep8
        return self._cards[1].get_value("lamdaf")

    @lamdaf.setter
    def lamdaf(self, value: float) -> None:
        """Set the lamdaf property."""
        self._cards[1].set_value("lamdaf", value)

    @property
    def stfsf(self) -> typing.Optional[float]:
        """Get or set the Penetration stiffness multiplier.
        """ # nopep8
        return self._cards[1].get_value("stfsf")

    @stfsf.setter
    def stfsf(self, value: float) -> None:
        """Set the stfsf property."""
        self._cards[1].set_value("stfsf", value)

    @property
    def isw(self) -> int:
        """Get or set the EQ. -1: initially rigid cohesive law (type I)
        EQ. -2: initially rigid cohesive law (type II), only for XFEM shell.
        """ # nopep8
        return self._cards[1].get_value("isw")

    @isw.setter
    def isw(self, value: int) -> None:
        """Set the isw property."""
        if value not in [-1, -2, None]:
            raise Exception("""isw must be `None` or one of {-1,-2}.""")
        self._cards[1].set_value("isw", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Ratio of maximum mode II shear traction to normal traction, only for XFEM shell.
        """ # nopep8
        return self._cards[1].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[1].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Ratio of maximum mode III shear traction to normal traction (available in shell), only for XFEM shell.
        """ # nopep8
        return self._cards[1].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[1].set_value("alpha2", value)

    @property
    def dr(self) -> typing.Optional[float]:
        """Get or set the Critical rotation scale (available in shell), only for XFEM shell.
        """ # nopep8
        return self._cards[2].get_value("dr")

    @dr.setter
    def dr(self, value: float) -> None:
        """Set the dr property."""
        self._cards[2].set_value("dr", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Ratio of maximum bending moment  to normal traction (available in shell), only for XFEM shell.
        """ # nopep8
        return self._cards[2].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[2].set_value("alpha3", value)

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

