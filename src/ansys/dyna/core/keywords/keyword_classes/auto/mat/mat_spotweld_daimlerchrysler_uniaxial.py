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

"""Module providing the MatSpotweldDaimlerchryslerUniaxial class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATSPOTWELDDAIMLERCHRYSLERUNIAXIAL_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("dt", float, 60, 10, None),
    FieldSchema("tfail", float, 70, 10, None),
)

_MATSPOTWELDDAIMLERCHRYSLERUNIAXIAL_CARD1 = (
    FieldSchema("efail", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("unused", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("unused", float, 50, 10, None),
    FieldSchema("unused", float, 60, 10, None),
    FieldSchema("nf", float, 70, 10, None),
)

_MATSPOTWELDDAIMLERCHRYSLERUNIAXIAL_CARD2 = (
    FieldSchema("rs", float, 0, 10, None),
    FieldSchema("asff", int, 10, 10, None),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("true_t", float, 30, 10, None),
    FieldSchema("con_id", int, 40, 10, None),
    FieldSchema("rfiltf", float, 50, 10, None),
    FieldSchema("jtol", float, 60, 10, None),
)

_MATSPOTWELDDAIMLERCHRYSLERUNIAXIAL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatSpotweldDaimlerchryslerUniaxial(KeywordBase):
    """DYNA MAT_SPOTWELD_DAIMLERCHRYSLER_UNIAXIAL keyword"""

    keyword = "MAT"
    subkeyword = "SPOTWELD_DAIMLERCHRYSLER_UNIAXIAL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatSpotweldDaimlerchryslerUniaxial class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSPOTWELDDAIMLERCHRYSLERUNIAXIAL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSPOTWELDDAIMLERCHRYSLERUNIAXIAL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSPOTWELDDAIMLERCHRYSLERUNIAXIAL_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatSpotweldDaimlerchryslerUniaxial.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATSPOTWELDDAIMLERCHRYSLERUNIAXIAL_OPTION0_CARD0,
                        **kwargs,
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
        """Get or set the Young's modulus: LT.0.0:	|"E"| is the Young's modulus. E < 0 invokes uniaxial stress for solid spot welds with the transverse stresses assumed to be zero. See Remark 1.
        This is for when the keyword option is unset (<BLANK>) only..
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
    def dt(self) -> typing.Optional[float]:
        """Get or set the Time step size for mass scaling, Delta t.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

    @property
    def tfail(self) -> typing.Optional[float]:
        """Get or set the Failure time if nonzero. If zero this option is ignored.
        """ # nopep8
        return self._cards[0].get_value("tfail")

    @tfail.setter
    def tfail(self, value: float) -> None:
        """Set the tfail property."""
        self._cards[0].set_value("tfail", value)

    @property
    def efail(self) -> typing.Optional[float]:
        """Get or set the Effective plastic strain in weld material at failure.  The spot weld element is deleted when the plastic strain at each integration point exceeds EFAIL.  If zero, failure due to effective plastic strain is not considered.
        """ # nopep8
        return self._cards[1].get_value("efail")

    @efail.setter
    def efail(self, value: float) -> None:
        """Set the efail property."""
        self._cards[1].set_value("efail", value)

    @property
    def nf(self) -> typing.Optional[float]:
        """Get or set the Number of force vectors stored for filtering (default = 0). Default is recommended unless oscillatory resultant forces are observed in the time history databases. Even though these welds should not oscillate significantly, this option was added for consistency with the other spot weld options. NF affects the storage since it is necessary to store the resultant forces as history variables. When NF is nonzero, the resultants in the output databases are filtered.
        """ # nopep8
        return self._cards[1].get_value("nf")

    @nf.setter
    def nf(self, value: float) -> None:
        """Set the nf property."""
        self._cards[1].set_value("nf", value)

    @property
    def rs(self) -> typing.Optional[float]:
        """Get or set the Rupture strain.  See Remark 2
        """ # nopep8
        return self._cards[2].get_value("rs")

    @rs.setter
    def rs(self, value: float) -> None:
        """Set the rs property."""
        self._cards[2].set_value("rs", value)

    @property
    def asff(self) -> typing.Optional[int]:
        """Get or set the Weld assembly simultaneous failure flag (see Remark 4):
        EQ.0:	Damaged elements fail individually.
        EQ.1 : Damaged elements fail when first reaches failure criterion.
        """ # nopep8
        return self._cards[2].get_value("asff")

    @asff.setter
    def asff(self, value: int) -> None:
        """Set the asff property."""
        self._cards[2].set_value("asff", value)

    @property
    def true_t(self) -> typing.Optional[float]:
        """Get or set the True weld thickness for single hexahedron solid weld elements. See Remark 3
        """ # nopep8
        return self._cards[2].get_value("true_t")

    @true_t.setter
    def true_t(self, value: float) -> None:
        """Set the true_t property."""
        self._cards[2].set_value("true_t", value)

    @property
    def con_id(self) -> typing.Optional[int]:
        """Get or set the Connection ID of *DEFINE_CONNECTION card. A negative CON_ID deactivates failure; see Remark 6
        """ # nopep8
        return self._cards[2].get_value("con_id")

    @con_id.setter
    def con_id(self, value: int) -> None:
        """Set the con_id property."""
        self._cards[2].set_value("con_id", value)

    @property
    def rfiltf(self) -> typing.Optional[float]:
        """Get or set the Smoothing factor on the effective strain rate (default is 0.0), potentially used in table DSIGY<0 and in functions for PRUL.ge.2 (see *DEFINE_CONNECTION_PROPERTIES).
        """ # nopep8
        return self._cards[2].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        """Set the rfiltf property."""
        self._cards[2].set_value("rfiltf", value)

    @property
    def jtol(self) -> typing.Optional[float]:
        """Get or set the Tolerance value for relative volume change (default: JTOL = 0.01). Solid element spot welds with a Jacobian less than JTOL will be eroded
        """ # nopep8
        return self._cards[2].get_value("jtol")

    @jtol.setter
    def jtol(self, value: float) -> None:
        """Set the jtol property."""
        self._cards[2].set_value("jtol", value)

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

