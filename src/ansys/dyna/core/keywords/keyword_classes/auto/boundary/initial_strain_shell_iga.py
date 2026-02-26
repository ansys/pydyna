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

"""Module providing the InitialStrainShellIga class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INITIALSTRAINSHELLIGA_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("nplane", int, 10, 10, None),
    FieldSchema("nthk", int, 20, 10, 0),
    FieldSchema("large", int, 30, 10, 0),
)

_INITIALSTRAINSHELLIGA_CARD1 = (
    FieldSchema("r", float, 0, 10, None),
    FieldSchema("s", float, 10, 10, None),
    FieldSchema("t", float, 20, 10, None),
)

_INITIALSTRAINSHELLIGA_CARD2 = (
    FieldSchema("epsxx", float, 0, 10, 0.0),
    FieldSchema("epsyy", float, 10, 10, 0.0),
    FieldSchema("epszz", float, 20, 10, 0.0),
    FieldSchema("epsxy", float, 30, 10, 0.0),
    FieldSchema("epsyz", float, 40, 10, 0.0),
    FieldSchema("epszx", float, 50, 10, 0.0),
    FieldSchema("thki", float, 60, 10, 0.0),
)

class InitialStrainShellIga(KeywordBase):
    """DYNA INITIAL_STRAIN_SHELL_IGA keyword"""

    keyword = "INITIAL"
    subkeyword = "STRAIN_SHELL_IGA"

    def __init__(self, **kwargs):
        """Initialize the InitialStrainShellIga class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINSHELLIGA_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINSHELLIGA_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINSHELLIGA_CARD2,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the iga element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def nplane(self) -> typing.Optional[int]:
        """Get or set the Number of in-plane integration points being output.
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        """Set the nplane property."""
        self._cards[0].set_value("nplane", value)

    @property
    def nthk(self) -> int:
        """Get or set the Flag for initialization of thicknesses at in-plane IPs:
        EQ.0:	o
        ffEQ.1 : on.
        """ # nopep8
        return self._cards[0].get_value("nthk")

    @nthk.setter
    def nthk(self, value: int) -> None:
        """Set the nthk property."""
        if value not in [0, 1, None]:
            raise Exception("""nthk must be `None` or one of {0,1}.""")
        self._cards[0].set_value("nthk", value)

    @property
    def large(self) -> int:
        """Get or set the Large format flag:
        EQ.0:	off
        EQ.1 : on.Each strain field is twice as long for higher precision.
        """ # nopep8
        return self._cards[0].get_value("large")

    @large.setter
    def large(self, value: int) -> None:
        """Set the large property."""
        if value not in [0, 1, None]:
            raise Exception("""large must be `None` or one of {0,1}.""")
        self._cards[0].set_value("large", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Parametric r-coordinate of location of in-plane integration point (with respect to iga shell definition)
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[1].set_value("r", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Parametric s-coordinate of location of in-plane integration point (with respect to iga shell definition)
        """ # nopep8
        return self._cards[1].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        """Set the s property."""
        self._cards[1].set_value("s", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate of through thickness integration point between -1 and 1 inclusive.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[1].set_value("t", value)

    @property
    def epsxx(self) -> float:
        """Get or set the Define the xx strain component. The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epsxx")

    @epsxx.setter
    def epsxx(self, value: float) -> None:
        """Set the epsxx property."""
        self._cards[2].set_value("epsxx", value)

    @property
    def epsyy(self) -> float:
        """Get or set the Define the yy strain component.The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epsyy")

    @epsyy.setter
    def epsyy(self, value: float) -> None:
        """Set the epsyy property."""
        self._cards[2].set_value("epsyy", value)

    @property
    def epszz(self) -> float:
        """Get or set the Define the zz strain component.The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epszz")

    @epszz.setter
    def epszz(self, value: float) -> None:
        """Set the epszz property."""
        self._cards[2].set_value("epszz", value)

    @property
    def epsxy(self) -> float:
        """Get or set the Define the xy strain component.The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epsxy")

    @epsxy.setter
    def epsxy(self, value: float) -> None:
        """Set the epsxy property."""
        self._cards[2].set_value("epsxy", value)

    @property
    def epsyz(self) -> float:
        """Get or set the Define the yz strain component.The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epsyz")

    @epsyz.setter
    def epsyz(self, value: float) -> None:
        """Set the epsyz property."""
        self._cards[2].set_value("epsyz", value)

    @property
    def epszx(self) -> float:
        """Get or set the Define the zx strain componentThe strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epszx")

    @epszx.setter
    def epszx(self, value: float) -> None:
        """Set the epszx property."""
        self._cards[2].set_value("epszx", value)

    @property
    def thki(self) -> float:
        """Get or set the The thickness value at in-plane integration point
        """ # nopep8
        return self._cards[2].get_value("thki")

    @thki.setter
    def thki(self, value: float) -> None:
        """Set the thki property."""
        self._cards[2].set_value("thki", value)

