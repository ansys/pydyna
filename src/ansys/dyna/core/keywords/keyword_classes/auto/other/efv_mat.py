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

"""Module providing the EfvMat class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVMAT_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("rhoref", float, 10, 10, None),
    FieldSchema("eosid", int, 20, 10, None),
    FieldSchema("strid", int, 30, 10, None),
    FieldSchema("failid", int, 40, 10, None),
)

_EFVMAT_CARD1 = (
    FieldSchema("erode", int, 0, 10, 0),
    FieldSchema("p1", float, 10, 10, None),
)

_EFVMAT_CARD2 = (
    FieldSchema("expnmax", float, 0, 10, 1e-10),
    FieldSchema("rhofmin", float, 10, 10, 0.0001),
    FieldSchema("cmin", float, 20, 10, 1e-10),
    FieldSchema("tmax", float, 30, 10, 1e+20),
)

class EfvMat(KeywordBase):
    """DYNA EFV_MAT keyword"""

    keyword = "EFV"
    subkeyword = "MAT"

    def __init__(self, **kwargs):
        """Initialize the EfvMat class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVMAT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVMAT_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVMAT_CARD2,
                **kwargs,
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified (see *PART)
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def rhoref(self) -> typing.Optional[float]:
        """Get or set the Reference material density
        """ # nopep8
        return self._cards[0].get_value("rhoref")

    @rhoref.setter
    def rhoref(self, value: float) -> None:
        """Set the rhoref property."""
        self._cards[0].set_value("rhoref", value)

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state (EOS) identification for Efv materials. This ID gives the EOS model for the material. It is required. See *EFV_EOS_OPTION.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def strid(self) -> typing.Optional[int]:
        """Get or set the Strength model identification for Efv materials. This ID gives the strength model for the material. It is optional. See *EFV_STRENGTH_OPTION.
        """ # nopep8
        return self._cards[0].get_value("strid")

    @strid.setter
    def strid(self, value: int) -> None:
        """Set the strid property."""
        self._cards[0].set_value("strid", value)

    @property
    def failid(self) -> typing.Optional[int]:
        """Get or set the Failure model identification for Efv materials. This ID gives the failure model for the material. It is optional. See *EFV_FAILURE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("failid")

    @failid.setter
    def failid(self, value: int) -> None:
        """Set the failid property."""
        self._cards[0].set_value("failid", value)

    @property
    def erode(self) -> int:
        """Get or set the Erosion criterion:
        EQ.0: No erosion
        EQ.1: Instantaneous Geometric Strain
        EQ.-1: Incremental Geometric Strain
        EQ.2: Plastic Strain
        EQ.3: Time step
        EQ.4: Failure
        EQ.5: User erosion model
        """ # nopep8
        return self._cards[1].get_value("erode")

    @erode.setter
    def erode(self, value: int) -> None:
        """Set the erode property."""
        if value not in [0, 1, -1, 2, 3, 4, 5, None]:
            raise Exception("""erode must be `None` or one of {0,1,-1,2,3,4,5}.""")
        self._cards[1].set_value("erode", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Parameter for erosion criterion:
        ERODE.EQ.1: Geometric strain limit.Recommended values are 0.75 < P1 < 3.0.The default value is 1.5.
        ERODE.EQ.2: Erosion strain.The default is 1.01x1020.
        ERODE.EQ.3: Minimum time step(units: �s).The default is 1.0 x 10 - 10 �s.
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[1].set_value("p1", value)

    @property
    def expnmax(self) -> float:
        """Get or set the Maximum expansion of the material
        """ # nopep8
        return self._cards[2].get_value("expnmax")

    @expnmax.setter
    def expnmax(self, value: float) -> None:
        """Set the expnmax property."""
        self._cards[2].set_value("expnmax", value)

    @property
    def rhofmin(self) -> float:
        """Get or set the Minimum density factor
        """ # nopep8
        return self._cards[2].get_value("rhofmin")

    @rhofmin.setter
    def rhofmin(self, value: float) -> None:
        """Set the rhofmin property."""
        self._cards[2].set_value("rhofmin", value)

    @property
    def cmin(self) -> float:
        """Get or set the Minimum speed of sound (units: cm/�s)
        """ # nopep8
        return self._cards[2].get_value("cmin")

    @cmin.setter
    def cmin(self, value: float) -> None:
        """Set the cmin property."""
        self._cards[2].set_value("cmin", value)

    @property
    def tmax(self) -> float:
        """Get or set the Maximum allowed temperature (units: K)
        """ # nopep8
        return self._cards[2].get_value("tmax")

    @tmax.setter
    def tmax(self, value: float) -> None:
        """Set the tmax property."""
        self._cards[2].set_value("tmax", value)

