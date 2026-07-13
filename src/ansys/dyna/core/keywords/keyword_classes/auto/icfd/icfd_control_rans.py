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

"""Module providing the IcfdControlRans class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLRANS_CARD0 = (
    FieldSchema("kato", int, 0, 10, 0),
    FieldSchema("pklim", int, 10, 10, 0),
    FieldSchema("clim", float, 20, 10, 20.0),
    FieldSchema("lrc", int, 30, 10, None),
)

_ICFDCONTROLRANS_CARD1 = (
    FieldSchema("krob", int, 0, 10, 0),
    FieldSchema("erob", int, 10, 10, 0),
)

_ICFDCONTROLRANS_CARD2 = (
    FieldSchema("isbuo", int, 0, 10, 0),
    FieldSchema("isc3", int, 10, 10, 0),
    FieldSchema("c3val", float, 20, 10, None),
)

class IcfdControlRans(KeywordBase):
    """DYNA ICFD_CONTROL_RANS keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_RANS"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlRans class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLRANS_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLRANS_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLRANS_CARD2,
                **kwargs,
            ),
        ]
    @property
    def kato(self) -> int:
        """Get or set the Kato-Launder turbulent production term modification. See Remark 1.
        EQ.0:	Default.Same as 2.
        EQ.1 : Modify the term.
        EQ.2 : Do not modify the term.
        """ # nopep8
        return self._cards[0].get_value("kato")

    @kato.setter
    def kato(self, value: int) -> None:
        """Set the kato property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""kato must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("kato", value)

    @property
    def pklim(self) -> int:
        """Get or set the Turbulent production term limiter flag. See Remark 2.
        EQ.0:	Default.Same as 1.
        EQ.1 : Turn on.
        EQ.2 : Turn off.
        """ # nopep8
        return self._cards[0].get_value("pklim")

    @pklim.setter
    def pklim(self, value: int) -> None:
        """Set the pklim property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""pklim must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("pklim", value)

    @property
    def clim(self) -> float:
        """Get or set the Production term limiter scaling coefficient.
        """ # nopep8
        return self._cards[0].get_value("clim")

    @clim.setter
    def clim(self, value: float) -> None:
        """Set the clim property."""
        self._cards[0].set_value("clim", value)

    @property
    def lrc(self) -> typing.Optional[int]:
        """Get or set the Low Reynolds correction flag that is applicable to k � ? turbulence models. See Remark 3.
        EQ.0:	Default.Same as 2.
        EQ.1 : Apply the correction.
        EQ.2 : Do not apply the correction.
        """ # nopep8
        return self._cards[0].get_value("lrc")

    @lrc.setter
    def lrc(self, value: int) -> None:
        """Set the lrc property."""
        self._cards[0].set_value("lrc", value)

    @property
    def krob(self) -> int:
        """Get or set the Option to choose between applying Dirichlet boundary conditions at the wall or Robin boundary conditions for the k or ? ? solve. See Remark 4.
        EQ.0:	Default.Same as 1.
        EQ.1 : Dirichlet boundary condition
        EQ.2 : Robin boundary condition
        """ # nopep8
        return self._cards[1].get_value("krob")

    @krob.setter
    def krob(self, value: int) -> None:
        """Set the krob property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""krob must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("krob", value)

    @property
    def erob(self) -> int:
        """Get or set the Option to choose between applying Dirichlet boundary conditions at the wall or Robin boundary conditions for the ?/? solve. See Remark 4.
        EQ.0:	Default.Same as 1.
        EQ.1 : Dirichlet boundary condition
        EQ.2 : Robin boundary condition
        """ # nopep8
        return self._cards[1].get_value("erob")

    @erob.setter
    def erob(self, value: int) -> None:
        """Set the erob property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""erob must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("erob", value)

    @property
    def isbuo(self) -> int:
        """Get or set the Flag to add free convection terms to the k � ? and k� ? models. See Remark 5.
        EQ.0:	Default.Same as 2.
        EQ.1 : Include free convection terms.
        EQ.2 : Do not include free convection terms.
        """ # nopep8
        return self._cards[2].get_value("isbuo")

    @isbuo.setter
    def isbuo(self, value: int) -> None:
        """Set the isbuo property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""isbuo must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("isbuo", value)

    @property
    def isc3(self) -> int:
        """Get or set the Flag to set a specific C_3?  with C3VAL or have the solver automatically calculate this quantity (see Remark 5):
        EQ.0:	Set C_3?  to 0.
        EQ.1 : Automatically estimate C_3?
        EQ.2 : Set the C_3?  estimate with C3VAL
        """ # nopep8
        return self._cards[2].get_value("isc3")

    @isc3.setter
    def isc3(self, value: int) -> None:
        """Set the isc3 property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""isc3 must be `None` or one of {0,1,2}.""")
        self._cards[2].set_value("isc3", value)

    @property
    def c3val(self) -> typing.Optional[float]:
        """Get or set the C_3?  if ISC3 = 2
        """ # nopep8
        return self._cards[2].get_value("c3val")

    @c3val.setter
    def c3val(self, value: float) -> None:
        """Set the c3val property."""
        self._cards[2].set_value("c3val", value)

