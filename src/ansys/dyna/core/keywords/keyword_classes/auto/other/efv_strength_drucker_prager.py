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

"""Module providing the EfvStrengthDruckerPrager class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTHDRUCKERPRAGER_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g", float, 10, 10, None),
    FieldSchema("yldtyp", int, 20, 10, 0),
)

_EFVSTRENGTHDRUCKERPRAGER_CARD1 = (
    FieldSchema("p1", float, 0, 10, None),
    FieldSchema("p2", float, 10, 10, None),
    FieldSchema("p3", float, 20, 10, None),
    FieldSchema("p4", float, 30, 10, None),
    FieldSchema("p5", float, 40, 10, None),
    FieldSchema("p6", float, 50, 10, None),
    FieldSchema("p7", float, 60, 10, None),
    FieldSchema("p8", float, 70, 10, None),
)

_EFVSTRENGTHDRUCKERPRAGER_CARD2 = (
    FieldSchema("p9", float, 0, 10, None),
    FieldSchema("p10", float, 10, 10, None),
)

_EFVSTRENGTHDRUCKERPRAGER_CARD3 = (
    FieldSchema("sigy1", float, 0, 10, None),
    FieldSchema("sigy2", float, 10, 10, None),
    FieldSchema("sigy3", float, 20, 10, None),
    FieldSchema("sigy4", float, 30, 10, None),
    FieldSchema("sigy5", float, 40, 10, None),
    FieldSchema("sigy6", float, 50, 10, None),
    FieldSchema("sigy7", float, 60, 10, None),
    FieldSchema("sigy8", float, 70, 10, None),
)

_EFVSTRENGTHDRUCKERPRAGER_CARD4 = (
    FieldSchema("sigy9", float, 0, 10, None),
    FieldSchema("sigy10", float, 10, 10, None),
)

_EFVSTRENGTHDRUCKERPRAGER_CARD5 = (
    FieldSchema("sigyp0", float, 0, 10, None),
    FieldSchema("theta", float, 10, 10, None),
)

_EFVSTRENGTHDRUCKERPRAGER_CARD6 = (
    FieldSchema("sigyut", float, 0, 10, None),
    FieldSchema("sigyuc", float, 10, 10, None),
)

class EfvStrengthDruckerPrager(KeywordBase):
    """DYNA EFV_STRENGTH_DRUCKER_PRAGER keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_DRUCKER_PRAGER"

    def __init__(self, **kwargs):
        """Initialize the EfvStrengthDruckerPrager class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHDRUCKERPRAGER_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHDRUCKERPRAGER_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHDRUCKERPRAGER_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHDRUCKERPRAGER_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHDRUCKERPRAGER_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHDRUCKERPRAGER_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHDRUCKERPRAGER_CARD6,
                **kwargs,
            ),
        ]
    @property
    def strid(self) -> typing.Optional[int]:
        """Get or set the Strength model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT)...
        """ # nopep8
        return self._cards[0].get_value("strid")

    @strid.setter
    def strid(self, value: int) -> None:
        """Set the strid property."""
        self._cards[0].set_value("strid", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, G
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def yldtyp(self) -> int:
        """Get or set the Flag for the type of yield condition:
        EQ.0: Piecewise (see Remark 3)
        EQ.1: Linear(see Remark 1)
        EQ.2: Stassi(see Remark 2)
        """ # nopep8
        return self._cards[0].get_value("yldtyp")

    @yldtyp.setter
    def yldtyp(self, value: int) -> None:
        """Set the yldtyp property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""yldtyp must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("yldtyp", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[1].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[1].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[1].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[1].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[1].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[1].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        """Set the p5 property."""
        self._cards[1].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[1].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        """Set the p6 property."""
        self._cards[1].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[1].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        """Set the p7 property."""
        self._cards[1].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[1].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        """Set the p8 property."""
        self._cards[1].set_value("p8", value)

    @property
    def p9(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[2].get_value("p9")

    @p9.setter
    def p9(self, value: float) -> None:
        """Set the p9 property."""
        self._cards[2].set_value("p9", value)

    @property
    def p10(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified.
        """ # nopep8
        return self._cards[2].get_value("p10")

    @p10.setter
    def p10(self, value: float) -> None:
        """Set the p10 property."""
        self._cards[2].set_value("p10", value)

    @property
    def sigy1(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[3].get_value("sigy1")

    @sigy1.setter
    def sigy1(self, value: float) -> None:
        """Set the sigy1 property."""
        self._cards[3].set_value("sigy1", value)

    @property
    def sigy2(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[3].get_value("sigy2")

    @sigy2.setter
    def sigy2(self, value: float) -> None:
        """Set the sigy2 property."""
        self._cards[3].set_value("sigy2", value)

    @property
    def sigy3(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[3].get_value("sigy3")

    @sigy3.setter
    def sigy3(self, value: float) -> None:
        """Set the sigy3 property."""
        self._cards[3].set_value("sigy3", value)

    @property
    def sigy4(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[3].get_value("sigy4")

    @sigy4.setter
    def sigy4(self, value: float) -> None:
        """Set the sigy4 property."""
        self._cards[3].set_value("sigy4", value)

    @property
    def sigy5(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[3].get_value("sigy5")

    @sigy5.setter
    def sigy5(self, value: float) -> None:
        """Set the sigy5 property."""
        self._cards[3].set_value("sigy5", value)

    @property
    def sigy6(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[3].get_value("sigy6")

    @sigy6.setter
    def sigy6(self, value: float) -> None:
        """Set the sigy6 property."""
        self._cards[3].set_value("sigy6", value)

    @property
    def sigy7(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[3].get_value("sigy7")

    @sigy7.setter
    def sigy7(self, value: float) -> None:
        """Set the sigy7 property."""
        self._cards[3].set_value("sigy7", value)

    @property
    def sigy8(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[3].get_value("sigy8")

    @sigy8.setter
    def sigy8(self, value: float) -> None:
        """Set the sigy8 property."""
        self._cards[3].set_value("sigy8", value)

    @property
    def sigy9(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[4].get_value("sigy9")

    @sigy9.setter
    def sigy9(self, value: float) -> None:
        """Set the sigy9 property."""
        self._cards[4].set_value("sigy9", value)

    @property
    def sigy10(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure piecewise curve. Up to 10 values may be specified
        """ # nopep8
        return self._cards[4].get_value("sigy10")

    @sigy10.setter
    def sigy10(self, value: float) -> None:
        """Set the sigy10 property."""
        self._cards[4].set_value("sigy10", value)

    @property
    def sigyp0(self) -> typing.Optional[float]:
        """Get or set the Yield stress at zero pressure. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("sigyp0")

    @sigyp0.setter
    def sigyp0(self, value: float) -> None:
        """Set the sigyp0 property."""
        self._cards[5].set_value("sigyp0", value)

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the Slope,0, in degrees of the linear yield function. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        """Set the theta property."""
        self._cards[5].set_value("theta", value)

    @property
    def sigyut(self) -> typing.Optional[float]:
        """Get or set the Yield stress in uniaxial tension0. See Remark 2.
        """ # nopep8
        return self._cards[6].get_value("sigyut")

    @sigyut.setter
    def sigyut(self, value: float) -> None:
        """Set the sigyut property."""
        self._cards[6].set_value("sigyut", value)

    @property
    def sigyuc(self) -> typing.Optional[float]:
        """Get or set the Yield stress in uniaxial compression. See Remark 2.
        """ # nopep8
        return self._cards[6].get_value("sigyuc")

    @sigyuc.setter
    def sigyuc(self, value: float) -> None:
        """Set the sigyuc property."""
        self._cards[6].set_value("sigyuc", value)

