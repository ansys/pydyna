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

"""Module providing the Mat258 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT258_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("a", float, 50, 10, None),
    FieldSchema("ksi", float, 60, 10, None),
)

_MAT258_CARD1 = (
    FieldSchema("theta1", float, 0, 10, None),
    FieldSchema("q1", float, 10, 10, None),
    FieldSchema("theta2", float, 20, 10, None),
    FieldSchema("q2", float, 30, 10, None),
    FieldSchema("theta3", float, 40, 10, None),
    FieldSchema("q3", float, 50, 10, None),
)

_MAT258_CARD2 = (
    FieldSchema("cs", float, 0, 10, None),
    FieldSchema("pdots", float, 10, 10, None),
)

_MAT258_CARD3 = (
    FieldSchema("dcrit", float, 0, 10, None),
    FieldSchema("wcb", float, 10, 10, None),
    FieldSchema("wcl", float, 20, 10, None),
    FieldSchema("wcs", float, 30, 10, None),
    FieldSchema("cc", float, 40, 10, None),
    FieldSchema("phi", float, 50, 10, None),
    FieldSchema("gamma", float, 60, 10, None),
    FieldSchema("thick", float, 70, 10, None),
)

class Mat258(KeywordBase):
    """DYNA MAT_258 keyword"""

    keyword = "MAT"
    subkeyword = "258"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat258 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT258_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT258_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT258_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT258_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat258.option_specs[0],
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
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Exponent of Hershey yield criterion.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def ksi(self) -> typing.Optional[float]:
        """Get or set the Coefficient governing critical strain increment for substepping.
        """ # nopep8
        return self._cards[0].get_value("ksi")

    @ksi.setter
    def ksi(self, value: float) -> None:
        """Set the ksi property."""
        self._cards[0].set_value("ksi", value)

    @property
    def theta1(self) -> typing.Optional[float]:
        """Get or set the Initial hardening modulus of R_i.
        """ # nopep8
        return self._cards[1].get_value("theta1")

    @theta1.setter
    def theta1(self, value: float) -> None:
        """Set the theta1 property."""
        self._cards[1].set_value("theta1", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Saturation value of R_i.
        """ # nopep8
        return self._cards[1].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        """Set the q1 property."""
        self._cards[1].set_value("q1", value)

    @property
    def theta2(self) -> typing.Optional[float]:
        """Get or set the Initial hardening modulus of R_i.
        """ # nopep8
        return self._cards[1].get_value("theta2")

    @theta2.setter
    def theta2(self, value: float) -> None:
        """Set the theta2 property."""
        self._cards[1].set_value("theta2", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Saturation value of R_i.
        """ # nopep8
        return self._cards[1].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        """Set the q2 property."""
        self._cards[1].set_value("q2", value)

    @property
    def theta3(self) -> typing.Optional[float]:
        """Get or set the Initial hardening modulus of R_i.
        """ # nopep8
        return self._cards[1].get_value("theta3")

    @theta3.setter
    def theta3(self, value: float) -> None:
        """Set the theta3 property."""
        self._cards[1].set_value("theta3", value)

    @property
    def q3(self) -> typing.Optional[float]:
        """Get or set the Saturation value of R_i.
        """ # nopep8
        return self._cards[1].get_value("q3")

    @q3.setter
    def q3(self, value: float) -> None:
        """Set the q3 property."""
        self._cards[1].set_value("q3", value)

    @property
    def cs(self) -> typing.Optional[float]:
        """Get or set the Rate sensitivity of flow stress.
        """ # nopep8
        return self._cards[2].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        """Set the cs property."""
        self._cards[2].set_value("cs", value)

    @property
    def pdots(self) -> typing.Optional[float]:
        """Get or set the Reference strain rate.
        """ # nopep8
        return self._cards[2].get_value("pdots")

    @pdots.setter
    def pdots(self, value: float) -> None:
        """Set the pdots property."""
        self._cards[2].set_value("pdots", value)

    @property
    def dcrit(self) -> typing.Optional[float]:
        """Get or set the Critical damage.
        """ # nopep8
        return self._cards[3].get_value("dcrit")

    @dcrit.setter
    def dcrit(self, value: float) -> None:
        """Set the dcrit property."""
        self._cards[3].set_value("dcrit", value)

    @property
    def wcb(self) -> typing.Optional[float]:
        """Get or set the Constant defining the damage evolution.
        """ # nopep8
        return self._cards[3].get_value("wcb")

    @wcb.setter
    def wcb(self, value: float) -> None:
        """Set the wcb property."""
        self._cards[3].set_value("wcb", value)

    @property
    def wcl(self) -> typing.Optional[float]:
        """Get or set the Constant defining the damage evolution.
        """ # nopep8
        return self._cards[3].get_value("wcl")

    @wcl.setter
    def wcl(self, value: float) -> None:
        """Set the wcl property."""
        self._cards[3].set_value("wcl", value)

    @property
    def wcs(self) -> typing.Optional[float]:
        """Get or set the Constant defining the damage evolution.
        """ # nopep8
        return self._cards[3].get_value("wcs")

    @wcs.setter
    def wcs(self, value: float) -> None:
        """Set the wcs property."""
        self._cards[3].set_value("wcs", value)

    @property
    def cc(self) -> typing.Optional[float]:
        """Get or set the Constant defining the damage evolution.
        """ # nopep8
        return self._cards[3].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        """Set the cc property."""
        self._cards[3].set_value("cc", value)

    @property
    def phi(self) -> typing.Optional[float]:
        """Get or set the Constant defining the damage evolution.
        """ # nopep8
        return self._cards[3].get_value("phi")

    @phi.setter
    def phi(self, value: float) -> None:
        """Set the phi property."""
        self._cards[3].set_value("phi", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Constant defining the damage evolution.
        """ # nopep8
        return self._cards[3].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[3].set_value("gamma", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Element thickness if using shell formulation 16.
        """ # nopep8
        return self._cards[3].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        """Set the thick property."""
        self._cards[3].set_value("thick", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

