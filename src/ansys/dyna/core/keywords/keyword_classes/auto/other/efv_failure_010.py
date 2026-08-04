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

"""Module providing the EfvFailure010 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILURE010_CARD0 = (
    FieldSchema("failid", int, 0, 10, None),
    FieldSchema("t", float, 10, 10, None),
    FieldSchema("jhtyp", int, 20, 10, 0),
)

_EFVFAILURE010_CARD1 = (
    FieldSchema("d1", float, 0, 10, None),
    FieldSchema("d2", float, 10, 10, None),
    FieldSchema("beta", float, 20, 10, None),
    FieldSchema("dtyp", int, 30, 10, 0),
    FieldSchema("tfail", int, 40, 10, 0),
)

_EFVFAILURE010_CARD2 = (
    FieldSchema("epfmax", float, 0, 10, None),
    FieldSchema("p3", float, 10, 10, None),
    FieldSchema("beta", float, 20, 10, None),
    FieldSchema("dtyp", int, 30, 10, 0),
    FieldSchema("tfail", int, 40, 10, 0),
)

class EfvFailure010(KeywordBase):
    """DYNA EFV_FAILURE_010 keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_010"

    def __init__(self, **kwargs):
        """Initialize the EfvFailure010 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILURE010_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVFAILURE010_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVFAILURE010_CARD2,
                **kwargs,
            ),
        ]
    @property
    def failid(self) -> typing.Optional[int]:
        """Get or set the Failure model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("failid")

    @failid.setter
    def failid(self, value: int) -> None:
        """Set the failid property."""
        self._cards[0].set_value("failid", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Hydrostatic tensile limit, T
        """ # nopep8
        return self._cards[0].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[0].set_value("t", value)

    @property
    def jhtyp(self) -> int:
        """Get or set the Failure model type (see Remark 1):
        EQ.0: Continuous, JH2.See Remark Error!Reference source not found.of *EFV_STRENGTH_JOHNSON_HOLMQUIST.
        EQ.1: Segmented, JH1.See Remark Error!Reference source not found.of *EFV_STRENGTH_JOHNSON_HOLMQUIST.
        """ # nopep8
        return self._cards[0].get_value("jhtyp")

    @jhtyp.setter
    def jhtyp(self, value: int) -> None:
        """Set the jhtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""jhtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("jhtyp", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Damage constant D_1. See Remark Error! Reference source not found. of *EFV_STRENGTH_JOHNSON_HOLMQUIST
        """ # nopep8
        return self._cards[1].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[1].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Damage constant D_2. See Remark Error! Reference source not found. of *EFV_STRENGTH_JOHNSON_HOLMQUIST
        """ # nopep8
        return self._cards[1].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[1].set_value("d2", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Bulking constant, b. See Remark 2.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

    @property
    def dtyp(self) -> int:
        """Get or set the Damage type (see Remark 3):
        EQ.0: Gradual
        EQ.1: Instantaneous
        """ # nopep8
        return self._cards[1].get_value("dtyp")

    @dtyp.setter
    def dtyp(self, value: int) -> None:
        """Set the dtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""dtyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("dtyp", value)

    @property
    def tfail(self) -> int:
        """Get or set the Tensile failure:
        EQ.0: Principal stress
        EQ.1: Hydro(Pmin)
        """ # nopep8
        return self._cards[1].get_value("tfail")

    @tfail.setter
    def tfail(self, value: int) -> None:
        """Set the tfail property."""
        if value not in [0, 1, None]:
            raise Exception("""tfail must be `None` or one of {0,1}.""")
        self._cards[1].set_value("tfail", value)

    @property
    def epfmax(self) -> typing.Optional[float]:
        """Get or set the Plastic strain for fracture at compressive pressure P_3,ee_(F,max )**p. See Figure Error! Reference source not found. of *EFV_STRENGTH_JOHNSON_HOLMQUIST
        """ # nopep8
        return self._cards[2].get_value("epfmax")

    @epfmax.setter
    def epfmax(self, value: float) -> None:
        """Set the epfmax property."""
        self._cards[2].set_value("epfmax", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Maximum compressive pressure strength, P_3. See Figure Error! Reference source not found. of *EFV_STRENGTH_JOHNSON_HOLMQUIST.
        """ # nopep8
        return self._cards[2].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[2].set_value("p3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Bulking constant, b (see Remark 2
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[2].set_value("beta", value)

    @property
    def dtyp(self) -> int:
        """Get or set the Damage type (see Remark 3):
        EQ.0: Gradual
        EQ.1: Instantaneous
        """ # nopep8
        return self._cards[2].get_value("dtyp")

    @dtyp.setter
    def dtyp(self, value: int) -> None:
        """Set the dtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""dtyp must be `None` or one of {0,1}.""")
        self._cards[2].set_value("dtyp", value)

    @property
    def tfail(self) -> int:
        """Get or set the Tensile failure:
        EQ.0: Principal stress
        EQ.1: Hydro(Pmin)
        """ # nopep8
        return self._cards[2].get_value("tfail")

    @tfail.setter
    def tfail(self, value: int) -> None:
        """Set the tfail property."""
        if value not in [0, 1, None]:
            raise Exception("""tfail must be `None` or one of {0,1}.""")
        self._cards[2].set_value("tfail", value)

