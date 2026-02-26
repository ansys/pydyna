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

"""Module providing the MatAle03 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATALE03_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("iadiab", int, 10, 10, 0),
    FieldSchema("runiv", float, 20, 10, None),
)

_MATALE03_CARD1 = (
    FieldSchema("cvmass1", float, 0, 10, None),
    FieldSchema("cvmass2", float, 10, 10, None),
    FieldSchema("cvmass3", float, 20, 10, None),
    FieldSchema("cvmass4", float, 30, 10, None),
    FieldSchema("cvmass5", float, 40, 10, None),
    FieldSchema("cvmass6", float, 50, 10, None),
    FieldSchema("cvmass7", float, 60, 10, None),
    FieldSchema("cvmass8", float, 70, 10, None),
)

_MATALE03_CARD2 = (
    FieldSchema("cpmass1", float, 0, 10, None),
    FieldSchema("cpmass2", float, 10, 10, None),
    FieldSchema("cpmass3", float, 20, 10, None),
    FieldSchema("cpmass4", float, 30, 10, None),
    FieldSchema("cpmass5", float, 40, 10, None),
    FieldSchema("cpmass6", float, 50, 10, None),
    FieldSchema("cpmass7", float, 60, 10, None),
    FieldSchema("cpmass8", float, 70, 10, None),
)

_MATALE03_CARD3 = (
    FieldSchema("molwt1", float, 0, 10, None),
    FieldSchema("molwt2", float, 10, 10, None),
    FieldSchema("molwt3", float, 20, 10, None),
    FieldSchema("molwt4", float, 30, 10, None),
    FieldSchema("molwt5", float, 40, 10, None),
    FieldSchema("molwt6", float, 50, 10, None),
    FieldSchema("molwt7", float, 60, 10, None),
    FieldSchema("molwt8", float, 70, 10, None),
)

_MATALE03_CARD4 = (
    FieldSchema("cpmole1", float, 0, 10, None),
    FieldSchema("cpmole2", float, 10, 10, None),
    FieldSchema("cpmole3", float, 20, 10, None),
    FieldSchema("cpmole4", float, 30, 10, None),
    FieldSchema("cpmole5", float, 40, 10, None),
    FieldSchema("cpmole6", float, 50, 10, None),
    FieldSchema("cpmole7", float, 60, 10, None),
    FieldSchema("cpmole8", float, 70, 10, None),
)

_MATALE03_CARD5 = (
    FieldSchema("b1", float, 0, 10, None),
    FieldSchema("b2", float, 10, 10, None),
    FieldSchema("b3", float, 20, 10, None),
    FieldSchema("b4", float, 30, 10, None),
    FieldSchema("b5", float, 40, 10, None),
    FieldSchema("b6", float, 50, 10, None),
    FieldSchema("b7", float, 60, 10, None),
    FieldSchema("b8", float, 70, 10, None),
)

_MATALE03_CARD6 = (
    FieldSchema("c1", float, 0, 10, None),
    FieldSchema("c2", float, 10, 10, None),
    FieldSchema("c3", float, 20, 10, None),
    FieldSchema("c4", float, 30, 10, None),
    FieldSchema("c5", float, 40, 10, None),
    FieldSchema("c6", float, 50, 10, None),
    FieldSchema("c7", float, 60, 10, None),
    FieldSchema("c8", float, 70, 10, None),
)

_MATALE03_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAle03(KeywordBase):
    """DYNA MAT_ALE_03 keyword"""

    keyword = "MAT"
    subkeyword = "ALE_03"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatAle03 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATALE03_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATALE03_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATALE03_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATALE03_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATALE03_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATALE03_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATALE03_CARD6,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAle03.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATALE03_OPTION0_CARD0,
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
    def iadiab(self) -> int:
        """Get or set the This flag (default=0) is used to turn ON/OFF adiabatic compression logics for an ideal gas (remark 5).
        EQ.0:  OFF (default)
        EQ.1:  ON
        .
        """ # nopep8
        return self._cards[0].get_value("iadiab")

    @iadiab.setter
    def iadiab(self, value: int) -> None:
        """Set the iadiab property."""
        if value not in [0, 1, None]:
            raise Exception("""iadiab must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iadiab", value)

    @property
    def runiv(self) -> typing.Optional[float]:
        """Get or set the Universal gas constant in per-mole unit (8.31447 J/(mole*K)).
        """ # nopep8
        return self._cards[0].get_value("runiv")

    @runiv.setter
    def runiv(self, value: float) -> None:
        """Set the runiv property."""
        self._cards[0].set_value("runiv", value)

    @property
    def cvmass1(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[1].get_value("cvmass1")

    @cvmass1.setter
    def cvmass1(self, value: float) -> None:
        """Set the cvmass1 property."""
        self._cards[1].set_value("cvmass1", value)

    @property
    def cvmass2(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[1].get_value("cvmass2")

    @cvmass2.setter
    def cvmass2(self, value: float) -> None:
        """Set the cvmass2 property."""
        self._cards[1].set_value("cvmass2", value)

    @property
    def cvmass3(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[1].get_value("cvmass3")

    @cvmass3.setter
    def cvmass3(self, value: float) -> None:
        """Set the cvmass3 property."""
        self._cards[1].set_value("cvmass3", value)

    @property
    def cvmass4(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[1].get_value("cvmass4")

    @cvmass4.setter
    def cvmass4(self, value: float) -> None:
        """Set the cvmass4 property."""
        self._cards[1].set_value("cvmass4", value)

    @property
    def cvmass5(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[1].get_value("cvmass5")

    @cvmass5.setter
    def cvmass5(self, value: float) -> None:
        """Set the cvmass5 property."""
        self._cards[1].set_value("cvmass5", value)

    @property
    def cvmass6(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[1].get_value("cvmass6")

    @cvmass6.setter
    def cvmass6(self, value: float) -> None:
        """Set the cvmass6 property."""
        self._cards[1].set_value("cvmass6", value)

    @property
    def cvmass7(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[1].get_value("cvmass7")

    @cvmass7.setter
    def cvmass7(self, value: float) -> None:
        """Set the cvmass7 property."""
        self._cards[1].set_value("cvmass7", value)

    @property
    def cvmass8(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[1].get_value("cvmass8")

    @cvmass8.setter
    def cvmass8(self, value: float) -> None:
        """Set the cvmass8 property."""
        self._cards[1].set_value("cvmass8", value)

    @property
    def cpmass1(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[2].get_value("cpmass1")

    @cpmass1.setter
    def cpmass1(self, value: float) -> None:
        """Set the cpmass1 property."""
        self._cards[2].set_value("cpmass1", value)

    @property
    def cpmass2(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[2].get_value("cpmass2")

    @cpmass2.setter
    def cpmass2(self, value: float) -> None:
        """Set the cpmass2 property."""
        self._cards[2].set_value("cpmass2", value)

    @property
    def cpmass3(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[2].get_value("cpmass3")

    @cpmass3.setter
    def cpmass3(self, value: float) -> None:
        """Set the cpmass3 property."""
        self._cards[2].set_value("cpmass3", value)

    @property
    def cpmass4(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[2].get_value("cpmass4")

    @cpmass4.setter
    def cpmass4(self, value: float) -> None:
        """Set the cpmass4 property."""
        self._cards[2].set_value("cpmass4", value)

    @property
    def cpmass5(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[2].get_value("cpmass5")

    @cpmass5.setter
    def cpmass5(self, value: float) -> None:
        """Set the cpmass5 property."""
        self._cards[2].set_value("cpmass5", value)

    @property
    def cpmass6(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[2].get_value("cpmass6")

    @cpmass6.setter
    def cpmass6(self, value: float) -> None:
        """Set the cpmass6 property."""
        self._cards[2].set_value("cpmass6", value)

    @property
    def cpmass7(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[2].get_value("cpmass7")

    @cpmass7.setter
    def cpmass7(self, value: float) -> None:
        """Set the cpmass7 property."""
        self._cards[2].set_value("cpmass7", value)

    @property
    def cpmass8(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
        """ # nopep8
        return self._cards[2].get_value("cpmass8")

    @cpmass8.setter
    def cpmass8(self, value: float) -> None:
        """Set the cpmass8 property."""
        self._cards[2].set_value("cpmass8", value)

    @property
    def molwt1(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[3].get_value("molwt1")

    @molwt1.setter
    def molwt1(self, value: float) -> None:
        """Set the molwt1 property."""
        self._cards[3].set_value("molwt1", value)

    @property
    def molwt2(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[3].get_value("molwt2")

    @molwt2.setter
    def molwt2(self, value: float) -> None:
        """Set the molwt2 property."""
        self._cards[3].set_value("molwt2", value)

    @property
    def molwt3(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[3].get_value("molwt3")

    @molwt3.setter
    def molwt3(self, value: float) -> None:
        """Set the molwt3 property."""
        self._cards[3].set_value("molwt3", value)

    @property
    def molwt4(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[3].get_value("molwt4")

    @molwt4.setter
    def molwt4(self, value: float) -> None:
        """Set the molwt4 property."""
        self._cards[3].set_value("molwt4", value)

    @property
    def molwt5(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[3].get_value("molwt5")

    @molwt5.setter
    def molwt5(self, value: float) -> None:
        """Set the molwt5 property."""
        self._cards[3].set_value("molwt5", value)

    @property
    def molwt6(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[3].get_value("molwt6")

    @molwt6.setter
    def molwt6(self, value: float) -> None:
        """Set the molwt6 property."""
        self._cards[3].set_value("molwt6", value)

    @property
    def molwt7(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[3].get_value("molwt7")

    @molwt7.setter
    def molwt7(self, value: float) -> None:
        """Set the molwt7 property."""
        self._cards[3].set_value("molwt7", value)

    @property
    def molwt8(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
        """ # nopep8
        return self._cards[3].get_value("molwt8")

    @molwt8.setter
    def molwt8(self, value: float) -> None:
        """Set the molwt8 property."""
        self._cards[3].set_value("molwt8", value)

    @property
    def cpmole1(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
        """ # nopep8
        return self._cards[4].get_value("cpmole1")

    @cpmole1.setter
    def cpmole1(self, value: float) -> None:
        """Set the cpmole1 property."""
        self._cards[4].set_value("cpmole1", value)

    @property
    def cpmole2(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
        """ # nopep8
        return self._cards[4].get_value("cpmole2")

    @cpmole2.setter
    def cpmole2(self, value: float) -> None:
        """Set the cpmole2 property."""
        self._cards[4].set_value("cpmole2", value)

    @property
    def cpmole3(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
        """ # nopep8
        return self._cards[4].get_value("cpmole3")

    @cpmole3.setter
    def cpmole3(self, value: float) -> None:
        """Set the cpmole3 property."""
        self._cards[4].set_value("cpmole3", value)

    @property
    def cpmole4(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
        """ # nopep8
        return self._cards[4].get_value("cpmole4")

    @cpmole4.setter
    def cpmole4(self, value: float) -> None:
        """Set the cpmole4 property."""
        self._cards[4].set_value("cpmole4", value)

    @property
    def cpmole5(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
        """ # nopep8
        return self._cards[4].get_value("cpmole5")

    @cpmole5.setter
    def cpmole5(self, value: float) -> None:
        """Set the cpmole5 property."""
        self._cards[4].set_value("cpmole5", value)

    @property
    def cpmole6(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
        """ # nopep8
        return self._cards[4].get_value("cpmole6")

    @cpmole6.setter
    def cpmole6(self, value: float) -> None:
        """Set the cpmole6 property."""
        self._cards[4].set_value("cpmole6", value)

    @property
    def cpmole7(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
        """ # nopep8
        return self._cards[4].get_value("cpmole7")

    @cpmole7.setter
    def cpmole7(self, value: float) -> None:
        """Set the cpmole7 property."""
        self._cards[4].set_value("cpmole7", value)

    @property
    def cpmole8(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable"A" in the equation in remark 2.
        """ # nopep8
        return self._cards[4].get_value("cpmole8")

    @cpmole8.setter
    def cpmole8(self, value: float) -> None:
        """Set the cpmole8 property."""
        self._cards[4].set_value("cpmole8", value)

    @property
    def b1(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
        """ # nopep8
        return self._cards[5].get_value("b1")

    @b1.setter
    def b1(self, value: float) -> None:
        """Set the b1 property."""
        self._cards[5].set_value("b1", value)

    @property
    def b2(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
        """ # nopep8
        return self._cards[5].get_value("b2")

    @b2.setter
    def b2(self, value: float) -> None:
        """Set the b2 property."""
        self._cards[5].set_value("b2", value)

    @property
    def b3(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
        """ # nopep8
        return self._cards[5].get_value("b3")

    @b3.setter
    def b3(self, value: float) -> None:
        """Set the b3 property."""
        self._cards[5].set_value("b3", value)

    @property
    def b4(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
        """ # nopep8
        return self._cards[5].get_value("b4")

    @b4.setter
    def b4(self, value: float) -> None:
        """Set the b4 property."""
        self._cards[5].set_value("b4", value)

    @property
    def b5(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
        """ # nopep8
        return self._cards[5].get_value("b5")

    @b5.setter
    def b5(self, value: float) -> None:
        """Set the b5 property."""
        self._cards[5].set_value("b5", value)

    @property
    def b6(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
        """ # nopep8
        return self._cards[5].get_value("b6")

    @b6.setter
    def b6(self, value: float) -> None:
        """Set the b6 property."""
        self._cards[5].set_value("b6", value)

    @property
    def b7(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
        """ # nopep8
        return self._cards[5].get_value("b7")

    @b7.setter
    def b7(self, value: float) -> None:
        """Set the b7 property."""
        self._cards[5].set_value("b7", value)

    @property
    def b8(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
        """ # nopep8
        return self._cards[5].get_value("b8")

    @b8.setter
    def b8(self, value: float) -> None:
        """Set the b8 property."""
        self._cards[5].set_value("b8", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
        """ # nopep8
        return self._cards[6].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[6].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
        """ # nopep8
        return self._cards[6].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[6].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
        """ # nopep8
        return self._cards[6].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[6].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
        """ # nopep8
        return self._cards[6].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        """Set the c4 property."""
        self._cards[6].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
        """ # nopep8
        return self._cards[6].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        """Set the c5 property."""
        self._cards[6].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
        """ # nopep8
        return self._cards[6].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        """Set the c6 property."""
        self._cards[6].set_value("c6", value)

    @property
    def c7(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
        """ # nopep8
        return self._cards[6].get_value("c7")

    @c7.setter
    def c7(self, value: float) -> None:
        """Set the c7 property."""
        self._cards[6].set_value("c7", value)

    @property
    def c8(self) -> typing.Optional[float]:
        """Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
        """ # nopep8
        return self._cards[6].get_value("c8")

    @c8.setter
    def c8(self, value: float) -> None:
        """Set the c8 property."""
        self._cards[6].set_value("c8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

