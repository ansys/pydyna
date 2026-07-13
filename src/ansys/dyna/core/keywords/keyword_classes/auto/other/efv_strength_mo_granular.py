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

"""Module providing the EfvStrengthMoGranular class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTHMOGRANULAR_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD1 = (
    FieldSchema("p1", float, 0, 10, None),
    FieldSchema("p2", float, 10, 10, None),
    FieldSchema("p3", float, 20, 10, None),
    FieldSchema("p4", float, 30, 10, None),
    FieldSchema("p5", float, 40, 10, None),
    FieldSchema("p6", float, 50, 10, None),
    FieldSchema("p7", float, 60, 10, None),
    FieldSchema("p8", float, 70, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD2 = (
    FieldSchema("p9", float, 0, 10, None),
    FieldSchema("p10", float, 10, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD3 = (
    FieldSchema("sigyp1", float, 0, 10, None),
    FieldSchema("sigyp2", float, 10, 10, None),
    FieldSchema("sigyp3", float, 20, 10, None),
    FieldSchema("sigyp4", float, 30, 10, None),
    FieldSchema("sigyp5", float, 40, 10, None),
    FieldSchema("sigyp6", float, 50, 10, None),
    FieldSchema("sigyp7", float, 60, 10, None),
    FieldSchema("sigyp8", float, 70, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD4 = (
    FieldSchema("sigyp9", float, 0, 10, None),
    FieldSchema("sigyp10", float, 10, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD5 = (
    FieldSchema("rhoys1", float, 0, 10, None),
    FieldSchema("rhoys2", float, 10, 10, None),
    FieldSchema("rhoys3", float, 20, 10, None),
    FieldSchema("rhoys4", float, 30, 10, None),
    FieldSchema("rhoys5", float, 40, 10, None),
    FieldSchema("rhoys6", float, 50, 10, None),
    FieldSchema("rhoys7", float, 60, 10, None),
    FieldSchema("rhoys8", float, 70, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD6 = (
    FieldSchema("rhoys9", float, 0, 10, None),
    FieldSchema("rhoys10", float, 10, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD7 = (
    FieldSchema("sigyrh1", float, 0, 10, None),
    FieldSchema("sigyrh2", float, 10, 10, None),
    FieldSchema("sigyrh3", float, 20, 10, None),
    FieldSchema("sigyrh4", float, 30, 10, None),
    FieldSchema("sigyrh5", float, 40, 10, None),
    FieldSchema("sigyrh6", float, 50, 10, None),
    FieldSchema("sigyrh7", float, 60, 10, None),
    FieldSchema("sigyrh8", float, 70, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD8 = (
    FieldSchema("sigyrh9", float, 0, 10, None),
    FieldSchema("sigyrh10", float, 10, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD9 = (
    FieldSchema("rhog1", float, 0, 10, None),
    FieldSchema("rhog2", float, 10, 10, None),
    FieldSchema("rhog3", float, 20, 10, None),
    FieldSchema("rhog4", float, 30, 10, None),
    FieldSchema("rhog5", float, 40, 10, None),
    FieldSchema("rhog6", float, 50, 10, None),
    FieldSchema("rhog7", float, 60, 10, None),
    FieldSchema("rhog8", float, 70, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD10 = (
    FieldSchema("rhog9", float, 0, 10, None),
    FieldSchema("rhog10", float, 10, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD11 = (
    FieldSchema("g1", float, 0, 10, None),
    FieldSchema("g2", float, 10, 10, None),
    FieldSchema("g3", float, 20, 10, None),
    FieldSchema("g4", float, 30, 10, None),
    FieldSchema("g5", float, 40, 10, None),
    FieldSchema("g6", float, 50, 10, None),
    FieldSchema("g7", float, 60, 10, None),
    FieldSchema("g8", float, 70, 10, None),
)

_EFVSTRENGTHMOGRANULAR_CARD12 = (
    FieldSchema("g9", float, 0, 10, None),
    FieldSchema("g10", float, 10, 10, None),
)

class EfvStrengthMoGranular(KeywordBase):
    """DYNA EFV_STRENGTH_MO_GRANULAR keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_MO_GRANULAR"

    def __init__(self, **kwargs):
        """Initialize the EfvStrengthMoGranular class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD7,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD8,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD9,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD10,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD11,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTHMOGRANULAR_CARD12,
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
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[1].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[1].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[1].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[1].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        """Set the p5 property."""
        self._cards[1].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        """Set the p6 property."""
        self._cards[1].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        """Set the p7 property."""
        self._cards[1].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        """Set the p8 property."""
        self._cards[1].set_value("p8", value)

    @property
    def p9(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[2].get_value("p9")

    @p9.setter
    def p9(self, value: float) -> None:
        """Set the p9 property."""
        self._cards[2].set_value("p9", value)

    @property
    def p10(self) -> typing.Optional[float]:
        """Get or set the Pressure values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[2].get_value("p10")

    @p10.setter
    def p10(self, value: float) -> None:
        """Set the p10 property."""
        self._cards[2].set_value("p10", value)

    @property
    def sigyp1(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("sigyp1")

    @sigyp1.setter
    def sigyp1(self, value: float) -> None:
        """Set the sigyp1 property."""
        self._cards[3].set_value("sigyp1", value)

    @property
    def sigyp2(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("sigyp2")

    @sigyp2.setter
    def sigyp2(self, value: float) -> None:
        """Set the sigyp2 property."""
        self._cards[3].set_value("sigyp2", value)

    @property
    def sigyp3(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("sigyp3")

    @sigyp3.setter
    def sigyp3(self, value: float) -> None:
        """Set the sigyp3 property."""
        self._cards[3].set_value("sigyp3", value)

    @property
    def sigyp4(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("sigyp4")

    @sigyp4.setter
    def sigyp4(self, value: float) -> None:
        """Set the sigyp4 property."""
        self._cards[3].set_value("sigyp4", value)

    @property
    def sigyp5(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("sigyp5")

    @sigyp5.setter
    def sigyp5(self, value: float) -> None:
        """Set the sigyp5 property."""
        self._cards[3].set_value("sigyp5", value)

    @property
    def sigyp6(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("sigyp6")

    @sigyp6.setter
    def sigyp6(self, value: float) -> None:
        """Set the sigyp6 property."""
        self._cards[3].set_value("sigyp6", value)

    @property
    def sigyp7(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("sigyp7")

    @sigyp7.setter
    def sigyp7(self, value: float) -> None:
        """Set the sigyp7 property."""
        self._cards[3].set_value("sigyp7", value)

    @property
    def sigyp8(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[3].get_value("sigyp8")

    @sigyp8.setter
    def sigyp8(self, value: float) -> None:
        """Set the sigyp8 property."""
        self._cards[3].set_value("sigyp8", value)

    @property
    def sigyp9(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[4].get_value("sigyp9")

    @sigyp9.setter
    def sigyp9(self, value: float) -> None:
        """Set the sigyp9 property."""
        self._cards[4].set_value("sigyp9", value)

    @property
    def sigyp10(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of pressure curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[4].get_value("sigyp10")

    @sigyp10.setter
    def sigyp10(self, value: float) -> None:
        """Set the sigyp10 property."""
        self._cards[4].set_value("sigyp10", value)

    @property
    def rhoys1(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("rhoys1")

    @rhoys1.setter
    def rhoys1(self, value: float) -> None:
        """Set the rhoys1 property."""
        self._cards[5].set_value("rhoys1", value)

    @property
    def rhoys2(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("rhoys2")

    @rhoys2.setter
    def rhoys2(self, value: float) -> None:
        """Set the rhoys2 property."""
        self._cards[5].set_value("rhoys2", value)

    @property
    def rhoys3(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("rhoys3")

    @rhoys3.setter
    def rhoys3(self, value: float) -> None:
        """Set the rhoys3 property."""
        self._cards[5].set_value("rhoys3", value)

    @property
    def rhoys4(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("rhoys4")

    @rhoys4.setter
    def rhoys4(self, value: float) -> None:
        """Set the rhoys4 property."""
        self._cards[5].set_value("rhoys4", value)

    @property
    def rhoys5(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("rhoys5")

    @rhoys5.setter
    def rhoys5(self, value: float) -> None:
        """Set the rhoys5 property."""
        self._cards[5].set_value("rhoys5", value)

    @property
    def rhoys6(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("rhoys6")

    @rhoys6.setter
    def rhoys6(self, value: float) -> None:
        """Set the rhoys6 property."""
        self._cards[5].set_value("rhoys6", value)

    @property
    def rhoys7(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("rhoys7")

    @rhoys7.setter
    def rhoys7(self, value: float) -> None:
        """Set the rhoys7 property."""
        self._cards[5].set_value("rhoys7", value)

    @property
    def rhoys8(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[5].get_value("rhoys8")

    @rhoys8.setter
    def rhoys8(self, value: float) -> None:
        """Set the rhoys8 property."""
        self._cards[5].set_value("rhoys8", value)

    @property
    def rhoys9(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[6].get_value("rhoys9")

    @rhoys9.setter
    def rhoys9(self, value: float) -> None:
        """Set the rhoys9 property."""
        self._cards[6].set_value("rhoys9", value)

    @property
    def rhoys10(self) -> typing.Optional[float]:
        """Get or set the Density values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[6].get_value("rhoys10")

    @rhoys10.setter
    def rhoys10(self, value: float) -> None:
        """Set the rhoys10 property."""
        self._cards[6].set_value("rhoys10", value)

    @property
    def sigyrh1(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[7].get_value("sigyrh1")

    @sigyrh1.setter
    def sigyrh1(self, value: float) -> None:
        """Set the sigyrh1 property."""
        self._cards[7].set_value("sigyrh1", value)

    @property
    def sigyrh2(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[7].get_value("sigyrh2")

    @sigyrh2.setter
    def sigyrh2(self, value: float) -> None:
        """Set the sigyrh2 property."""
        self._cards[7].set_value("sigyrh2", value)

    @property
    def sigyrh3(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[7].get_value("sigyrh3")

    @sigyrh3.setter
    def sigyrh3(self, value: float) -> None:
        """Set the sigyrh3 property."""
        self._cards[7].set_value("sigyrh3", value)

    @property
    def sigyrh4(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[7].get_value("sigyrh4")

    @sigyrh4.setter
    def sigyrh4(self, value: float) -> None:
        """Set the sigyrh4 property."""
        self._cards[7].set_value("sigyrh4", value)

    @property
    def sigyrh5(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[7].get_value("sigyrh5")

    @sigyrh5.setter
    def sigyrh5(self, value: float) -> None:
        """Set the sigyrh5 property."""
        self._cards[7].set_value("sigyrh5", value)

    @property
    def sigyrh6(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[7].get_value("sigyrh6")

    @sigyrh6.setter
    def sigyrh6(self, value: float) -> None:
        """Set the sigyrh6 property."""
        self._cards[7].set_value("sigyrh6", value)

    @property
    def sigyrh7(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[7].get_value("sigyrh7")

    @sigyrh7.setter
    def sigyrh7(self, value: float) -> None:
        """Set the sigyrh7 property."""
        self._cards[7].set_value("sigyrh7", value)

    @property
    def sigyrh8(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[7].get_value("sigyrh8")

    @sigyrh8.setter
    def sigyrh8(self, value: float) -> None:
        """Set the sigyrh8 property."""
        self._cards[7].set_value("sigyrh8", value)

    @property
    def sigyrh9(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[8].get_value("sigyrh9")

    @sigyrh9.setter
    def sigyrh9(self, value: float) -> None:
        """Set the sigyrh9 property."""
        self._cards[8].set_value("sigyrh9", value)

    @property
    def sigyrh10(self) -> typing.Optional[float]:
        """Get or set the Yield stress values in the yield stress as a function of density curve. Up to 10 values may be specified. See Remark 1
        """ # nopep8
        return self._cards[8].get_value("sigyrh10")

    @sigyrh10.setter
    def sigyrh10(self, value: float) -> None:
        """Set the sigyrh10 property."""
        self._cards[8].set_value("sigyrh10", value)

    @property
    def rhog1(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhog1")

    @rhog1.setter
    def rhog1(self, value: float) -> None:
        """Set the rhog1 property."""
        self._cards[9].set_value("rhog1", value)

    @property
    def rhog2(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhog2")

    @rhog2.setter
    def rhog2(self, value: float) -> None:
        """Set the rhog2 property."""
        self._cards[9].set_value("rhog2", value)

    @property
    def rhog3(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhog3")

    @rhog3.setter
    def rhog3(self, value: float) -> None:
        """Set the rhog3 property."""
        self._cards[9].set_value("rhog3", value)

    @property
    def rhog4(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhog4")

    @rhog4.setter
    def rhog4(self, value: float) -> None:
        """Set the rhog4 property."""
        self._cards[9].set_value("rhog4", value)

    @property
    def rhog5(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhog5")

    @rhog5.setter
    def rhog5(self, value: float) -> None:
        """Set the rhog5 property."""
        self._cards[9].set_value("rhog5", value)

    @property
    def rhog6(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhog6")

    @rhog6.setter
    def rhog6(self, value: float) -> None:
        """Set the rhog6 property."""
        self._cards[9].set_value("rhog6", value)

    @property
    def rhog7(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhog7")

    @rhog7.setter
    def rhog7(self, value: float) -> None:
        """Set the rhog7 property."""
        self._cards[9].set_value("rhog7", value)

    @property
    def rhog8(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhog8")

    @rhog8.setter
    def rhog8(self, value: float) -> None:
        """Set the rhog8 property."""
        self._cards[9].set_value("rhog8", value)

    @property
    def rhog9(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[10].get_value("rhog9")

    @rhog9.setter
    def rhog9(self, value: float) -> None:
        """Set the rhog9 property."""
        self._cards[10].set_value("rhog9", value)

    @property
    def rhog10(self) -> typing.Optional[float]:
        """Get or set the Density values in the shear modulus as a function of density curve. Up to 10 values may be specified. The density values are at zero pressure. See Remark 2
        """ # nopep8
        return self._cards[10].get_value("rhog10")

    @rhog10.setter
    def rhog10(self, value: float) -> None:
        """Set the rhog10 property."""
        self._cards[10].set_value("rhog10", value)

    @property
    def g1(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        """Set the g1 property."""
        self._cards[11].set_value("g1", value)

    @property
    def g2(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        """Set the g2 property."""
        self._cards[11].set_value("g2", value)

    @property
    def g3(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("g3")

    @g3.setter
    def g3(self, value: float) -> None:
        """Set the g3 property."""
        self._cards[11].set_value("g3", value)

    @property
    def g4(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("g4")

    @g4.setter
    def g4(self, value: float) -> None:
        """Set the g4 property."""
        self._cards[11].set_value("g4", value)

    @property
    def g5(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("g5")

    @g5.setter
    def g5(self, value: float) -> None:
        """Set the g5 property."""
        self._cards[11].set_value("g5", value)

    @property
    def g6(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("g6")

    @g6.setter
    def g6(self, value: float) -> None:
        """Set the g6 property."""
        self._cards[11].set_value("g6", value)

    @property
    def g7(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("g7")

    @g7.setter
    def g7(self, value: float) -> None:
        """Set the g7 property."""
        self._cards[11].set_value("g7", value)

    @property
    def g8(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("g8")

    @g8.setter
    def g8(self, value: float) -> None:
        """Set the g8 property."""
        self._cards[11].set_value("g8", value)

    @property
    def g9(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[12].get_value("g9")

    @g9.setter
    def g9(self, value: float) -> None:
        """Set the g9 property."""
        self._cards[12].set_value("g9", value)

    @property
    def g10(self) -> typing.Optional[float]:
        """Get or set the Shear modulus values in the shear modulus as a function of density curve. Up to 10 values may be specified. See Remark 2
        """ # nopep8
        return self._cards[12].get_value("g10")

    @g10.setter
    def g10(self, value: float) -> None:
        """Set the g10 property."""
        self._cards[12].set_value("g10", value)

