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

"""Module providing the EfvEosPorous class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOSPOROUS_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("csol", float, 10, 10, None),
    FieldSchema("cpor", float, 20, 10, None),
    FieldSchema("rhofc", float, 30, 10, None),
)

_EFVEOSPOROUS_CARD1 = (
    FieldSchema("rho1", float, 0, 10, None),
    FieldSchema("rho2", float, 10, 10, None),
    FieldSchema("rho3", float, 20, 10, None),
    FieldSchema("rho4", float, 30, 10, None),
    FieldSchema("rho5", float, 40, 10, None),
    FieldSchema("rho6", float, 50, 10, None),
    FieldSchema("rho7", float, 60, 10, None),
    FieldSchema("rho8", float, 70, 10, None),
)

_EFVEOSPOROUS_CARD2 = (
    FieldSchema("rho1", float, 0, 10, None),
    FieldSchema("rho2", float, 10, 10, None),
)

_EFVEOSPOROUS_CARD3 = (
    FieldSchema("rho1", float, 0, 10, None),
    FieldSchema("rho2", float, 10, 10, None),
    FieldSchema("rho3", float, 20, 10, None),
    FieldSchema("rho4", float, 30, 10, None),
    FieldSchema("rho5", float, 40, 10, None),
    FieldSchema("rho6", float, 50, 10, None),
    FieldSchema("rho7", float, 60, 10, None),
    FieldSchema("rho8", float, 70, 10, None),
)

_EFVEOSPOROUS_CARD4 = (
    FieldSchema("rho1", float, 0, 10, None),
    FieldSchema("rho2", float, 10, 10, None),
)

class EfvEosPorous(KeywordBase):
    """DYNA EFV_EOS_POROUS keyword"""

    keyword = "EFV"
    subkeyword = "EOS_POROUS"

    def __init__(self, **kwargs):
        """Initialize the EfvEosPorous class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOSPOROUS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSPOROUS_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSPOROUS_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSPOROUS_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSPOROUS_CARD4,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def csol(self) -> typing.Optional[float]:
        """Get or set the Speed of sound of the solid (non-porous) material
        """ # nopep8
        return self._cards[0].get_value("csol")

    @csol.setter
    def csol(self, value: float) -> None:
        """Set the csol property."""
        self._cards[0].set_value("csol", value)

    @property
    def cpor(self) -> typing.Optional[float]:
        """Get or set the Speed of sound of the porous material
        """ # nopep8
        return self._cards[0].get_value("cpor")

    @cpor.setter
    def cpor(self, value: float) -> None:
        """Set the cpor property."""
        self._cards[0].set_value("cpor", value)

    @property
    def rhofc(self) -> typing.Optional[float]:
        """Get or set the Crush failure density
        """ # nopep8
        return self._cards[0].get_value("rhofc")

    @rhofc.setter
    def rhofc(self, value: float) -> None:
        """Set the rhofc property."""
        self._cards[0].set_value("rhofc", value)

    @property
    def rho1(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[1].get_value("rho1")

    @rho1.setter
    def rho1(self, value: float) -> None:
        """Set the rho1 property."""
        self._cards[1].set_value("rho1", value)

    @property
    def rho2(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[1].get_value("rho2")

    @rho2.setter
    def rho2(self, value: float) -> None:
        """Set the rho2 property."""
        self._cards[1].set_value("rho2", value)

    @property
    def rho3(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[1].get_value("rho3")

    @rho3.setter
    def rho3(self, value: float) -> None:
        """Set the rho3 property."""
        self._cards[1].set_value("rho3", value)

    @property
    def rho4(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[1].get_value("rho4")

    @rho4.setter
    def rho4(self, value: float) -> None:
        """Set the rho4 property."""
        self._cards[1].set_value("rho4", value)

    @property
    def rho5(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[1].get_value("rho5")

    @rho5.setter
    def rho5(self, value: float) -> None:
        """Set the rho5 property."""
        self._cards[1].set_value("rho5", value)

    @property
    def rho6(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[1].get_value("rho6")

    @rho6.setter
    def rho6(self, value: float) -> None:
        """Set the rho6 property."""
        self._cards[1].set_value("rho6", value)

    @property
    def rho7(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[1].get_value("rho7")

    @rho7.setter
    def rho7(self, value: float) -> None:
        """Set the rho7 property."""
        self._cards[1].set_value("rho7", value)

    @property
    def rho8(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[1].get_value("rho8")

    @rho8.setter
    def rho8(self, value: float) -> None:
        """Set the rho8 property."""
        self._cards[1].set_value("rho8", value)

    @property
    def rho1(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[2].get_value("rho1")

    @rho1.setter
    def rho1(self, value: float) -> None:
        """Set the rho1 property."""
        self._cards[2].set_value("rho1", value)

    @property
    def rho2(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[2].get_value("rho2")

    @rho2.setter
    def rho2(self, value: float) -> None:
        """Set the rho2 property."""
        self._cards[2].set_value("rho2", value)

    @property
    def rho1(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[3].get_value("rho1")

    @rho1.setter
    def rho1(self, value: float) -> None:
        """Set the rho1 property."""
        self._cards[3].set_value("rho1", value)

    @property
    def rho2(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[3].get_value("rho2")

    @rho2.setter
    def rho2(self, value: float) -> None:
        """Set the rho2 property."""
        self._cards[3].set_value("rho2", value)

    @property
    def rho3(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[3].get_value("rho3")

    @rho3.setter
    def rho3(self, value: float) -> None:
        """Set the rho3 property."""
        self._cards[3].set_value("rho3", value)

    @property
    def rho4(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[3].get_value("rho4")

    @rho4.setter
    def rho4(self, value: float) -> None:
        """Set the rho4 property."""
        self._cards[3].set_value("rho4", value)

    @property
    def rho5(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[3].get_value("rho5")

    @rho5.setter
    def rho5(self, value: float) -> None:
        """Set the rho5 property."""
        self._cards[3].set_value("rho5", value)

    @property
    def rho6(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[3].get_value("rho6")

    @rho6.setter
    def rho6(self, value: float) -> None:
        """Set the rho6 property."""
        self._cards[3].set_value("rho6", value)

    @property
    def rho7(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[3].get_value("rho7")

    @rho7.setter
    def rho7(self, value: float) -> None:
        """Set the rho7 property."""
        self._cards[3].set_value("rho7", value)

    @property
    def rho8(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[3].get_value("rho8")

    @rho8.setter
    def rho8(self, value: float) -> None:
        """Set the rho8 property."""
        self._cards[3].set_value("rho8", value)

    @property
    def rho1(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[4].get_value("rho1")

    @rho1.setter
    def rho1(self, value: float) -> None:
        """Set the rho1 property."""
        self._cards[4].set_value("rho1", value)

    @property
    def rho2(self) -> typing.Optional[float]:
        """Get or set the Density values in the pressure as a function of density curve
        """ # nopep8
        return self._cards[4].get_value("rho2")

    @rho2.setter
    def rho2(self, value: float) -> None:
        """Set the rho2 property."""
        self._cards[4].set_value("rho2", value)

