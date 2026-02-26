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

"""Module providing the InitialStrainTshell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INITIALSTRAINTSHELL_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
)

_INITIALSTRAINTSHELL_CARD1 = (
    FieldSchema("epsxx", float, 0, 10, 0.0),
    FieldSchema("epsyy", float, 10, 10, 0.0),
    FieldSchema("epszz", float, 20, 10, 0.0),
    FieldSchema("epsxy", float, 30, 10, 0.0),
    FieldSchema("epsyz", float, 40, 10, 0.0),
    FieldSchema("epszx", float, 50, 10, 0.0),
)

_INITIALSTRAINTSHELL_CARD2 = (
    FieldSchema("epsxx", float, 0, 10, 0.0),
    FieldSchema("epsyy", float, 10, 10, 0.0),
    FieldSchema("epszz", float, 20, 10, 0.0),
    FieldSchema("epsxy", float, 30, 10, 0.0),
    FieldSchema("epsyz", float, 40, 10, 0.0),
    FieldSchema("epszx", float, 50, 10, 0.0),
)

class InitialStrainTshell(KeywordBase):
    """DYNA INITIAL_STRAIN_TSHELL keyword"""

    keyword = "INITIAL"
    subkeyword = "STRAIN_TSHELL"

    def __init__(self, **kwargs):
        """Initialize the InitialStrainTshell class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINTSHELL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINTSHELL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALSTRAINTSHELL_CARD2,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the TShell element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def epsxx(self) -> float:
        """Get or set the Define the xx strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("epsxx")

    @epsxx.setter
    def epsxx(self, value: float) -> None:
        """Set the epsxx property."""
        self._cards[1].set_value("epsxx", value)

    @property
    def epsyy(self) -> float:
        """Get or set the Define the yy strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("epsyy")

    @epsyy.setter
    def epsyy(self, value: float) -> None:
        """Set the epsyy property."""
        self._cards[1].set_value("epsyy", value)

    @property
    def epszz(self) -> float:
        """Get or set the Define the zz strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("epszz")

    @epszz.setter
    def epszz(self, value: float) -> None:
        """Set the epszz property."""
        self._cards[1].set_value("epszz", value)

    @property
    def epsxy(self) -> float:
        """Get or set the Define the xy strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("epsxy")

    @epsxy.setter
    def epsxy(self, value: float) -> None:
        """Set the epsxy property."""
        self._cards[1].set_value("epsxy", value)

    @property
    def epsyz(self) -> float:
        """Get or set the Define the yz strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("epsyz")

    @epsyz.setter
    def epsyz(self, value: float) -> None:
        """Set the epsyz property."""
        self._cards[1].set_value("epsyz", value)

    @property
    def epszx(self) -> float:
        """Get or set the Define the zx strain component at inner integration (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("epszx")

    @epszx.setter
    def epszx(self, value: float) -> None:
        """Set the epszx property."""
        self._cards[1].set_value("epszx", value)

    @property
    def epsxx(self) -> float:
        """Get or set the Define the xx strain component at outer integration (global cartesian system).
        """ # nopep8
        return self._cards[2].get_value("epsxx")

    @epsxx.setter
    def epsxx(self, value: float) -> None:
        """Set the epsxx property."""
        self._cards[2].set_value("epsxx", value)

    @property
    def epsyy(self) -> float:
        """Get or set the Define the yy strain component at outer integration (global cartesian system).
        """ # nopep8
        return self._cards[2].get_value("epsyy")

    @epsyy.setter
    def epsyy(self, value: float) -> None:
        """Set the epsyy property."""
        self._cards[2].set_value("epsyy", value)

    @property
    def epszz(self) -> float:
        """Get or set the Define the zz strain component at outer integration (global cartesian system).
        """ # nopep8
        return self._cards[2].get_value("epszz")

    @epszz.setter
    def epszz(self, value: float) -> None:
        """Set the epszz property."""
        self._cards[2].set_value("epszz", value)

    @property
    def epsxy(self) -> float:
        """Get or set the Define the xy strain component at outer integration (global cartesian system).
        """ # nopep8
        return self._cards[2].get_value("epsxy")

    @epsxy.setter
    def epsxy(self, value: float) -> None:
        """Set the epsxy property."""
        self._cards[2].set_value("epsxy", value)

    @property
    def epsyz(self) -> float:
        """Get or set the Define the yz strain component at outer integration (global cartesian system).
        """ # nopep8
        return self._cards[2].get_value("epsyz")

    @epsyz.setter
    def epsyz(self, value: float) -> None:
        """Set the epsyz property."""
        self._cards[2].set_value("epsyz", value)

    @property
    def epszx(self) -> float:
        """Get or set the Define the zx strain component at outer integration (global cartesian system).
        """ # nopep8
        return self._cards[2].get_value("epszx")

    @epszx.setter
    def epszx(self, value: float) -> None:
        """Set the epszx property."""
        self._cards[2].set_value("epszx", value)

